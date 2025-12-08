# Issue #40: Meta Pieces Implementation Plan

## Overview

Implement the ability to fuse two or more adjacent pieces into a single "meta piece" that behaves as one unit for rendering and separation.

## Architecture Decision

**Approach: Generation-Time Edge Skipping**

After investigation, the recommended approach is to skip internal edge generation for fused piece pairs during the puzzle generation phase, rather than trying to hide edges at render time.

**Rationale:**
1. Edges are embedded as bezier curves in piece SVG paths
2. Simply hiding edges would leave deformed piece boundaries (tabs with nothing to connect to)
3. Generation-time fusion creates clean merged boundaries
4. Works naturally with existing separation/offset mechanism

## Implementation Phases

### Phase 1: Unified Adjacency API

**Goal:** Create a consistent way to query piece neighbors across all puzzle types.

**New File:** `R/adjacency_api.R`

```r
#' Get neighbors of a piece
#' @param piece_id Piece identifier (string or integer depending on type)
#' @param puzzle_result Output from generate_puzzle()
#' @return Data frame with columns: direction, neighbor_id, is_boundary, shared_edge_key
#' @export
get_piece_neighbors <- function(piece_id, puzzle_result)

#' Check if two pieces are adjacent
#' @export
are_pieces_adjacent <- function(piece_id_a, piece_id_b, puzzle_result)

#' Get the shared edge key between two adjacent pieces
#' @export
get_shared_edge_key <- function(piece_id_a, piece_id_b, puzzle_result)

#' Validate a fusion group (all pieces must be connected)
#' @export
validate_fusion_group <- function(piece_ids, puzzle_result)
```

**Type-specific implementations:**
- `get_rect_neighbors()` - Grid-based (new)
- `get_hex_neighbors_unified()` - Wrap existing `get_hex_neighbor()`
- `get_concentric_neighbors_unified()` - Wrap existing `get_concentric_neighbor()`

**Direction Naming Convention:**
| Rectangular | Hexagonal | Concentric |
|-------------|-----------|------------|
| N, E, S, W | 0-5 (sides) | INNER, RIGHT, OUTER, LEFT |

---

### Phase 2: Edge Fusion Mechanism

**Goal:** Track which edges should be skipped during generation.

**Data Structures:**

```r
# Fusion specification (user input)
fusion_groups <- list(
  c("piece_0_0", "piece_1_0"),           # Rectangular: fuse two horizontal neighbors
  c("piece_0_0", "piece_1_0", "piece_0_1", "piece_1_1")  # 2x2 block
)

# Internal representation
fused_edges <- list(
  "piece_0_0-E" = TRUE,   # Skip piece_0_0's east edge
  "piece_1_0-W" = TRUE    # Skip piece_1_0's west edge (complementary)
)
```

**New Functions:**

```r
#' Convert fusion groups to fused edge set
#' @param fusion_groups List of piece ID vectors to fuse
#' @param puzzle_result Puzzle structure with adjacency info
#' @return Set of edge keys to skip during generation
compute_fused_edges <- function(fusion_groups, puzzle_result)

#' Check if an edge should be fused (skipped)
#' @param piece_id Current piece
#' @param direction Edge direction
#' @param fused_edges Set from compute_fused_edges()
#' @return TRUE if edge should be skipped
is_edge_fused <- function(piece_id, direction, fused_edges)
```

---

### Phase 3: Update Piece Generation

**Goal:** Modify generation to skip fused edges and create merged boundaries.

#### 3.1 Rectangular (`R/puzzle_core_clean.R`)

**Modify `generate_all_edges()`:**
```r
generate_all_edges <- function(xn, yn, ..., fused_edges = NULL) {
  # For each potential edge:
  edge_key <- sprintf("piece_%d_%d-%s", xi, yi, direction)
  if (!is.null(fused_edges) && edge_key %in% fused_edges) {
    # Skip this edge - pieces will share a straight boundary
    edges[[...]] <- NULL
  } else {
    # Generate normal bezier edge
    edges[[...]] <- generate_edge_segment(...)
  }
}
```

**Modify `generate_single_piece()`:**
```r
generate_single_piece <- function(xi, yi, puzzle_structure, fused_edges = NULL) {
  # When building path, check fused_edges:
  edge_key <- sprintf("piece_%d_%d-E", xi, yi)
  if (!is.null(fused_edges) && edge_key %in% fused_edges) {
    # Don't close the path on this side - continue to fused neighbor
    # This requires knowing the full fusion group
  }
}
```

**Challenge:** Fused pieces need merged paths, not separate paths.

**Solution:** Generate meta-pieces as single units:
```r
generate_meta_piece <- function(piece_ids, puzzle_structure, fused_edges) {
  # 1. Get bounding box of all pieces in group
  # 2. Walk the outer boundary (skip internal edges)
  # 3. Return single merged SVG path
}
```

#### 3.2 Hexagonal (`R/hexagonal_edge_generation_fixed.R`)

**Modify edge map generation:**
```r
generate_hex_edge_map <- function(..., fused_edges = NULL) {
  # When building edge_map:
  for (piece_id in 1:num_pieces) {
    for (side in 0:5) {
      edge_key <- sprintf("%d-%d", piece_id, side)
      if (!is.null(fused_edges) && edge_key %in% fused_edges) {
        # Mark as internal fused edge - will be skipped in path building
        edge_map[[edge_key]]$fused <- TRUE
      }
    }
  }
}
```

#### 3.3 Concentric (`R/concentric_edge_generation.R`)

Similar approach to hexagonal - mark fused edges in edge map.

---

### Phase 4: Meta-Piece Path Generation

**Goal:** Create unified paths for fused piece groups.

**Algorithm for Merged Boundary:**
1. Identify all pieces in fusion group
2. Find the outer boundary vertices (vertices not shared only within the group)
3. Walk the boundary in order, collecting edge segments
4. Skip any edge that connects two pieces within the group
5. Return single closed SVG path

**New Function:**
```r
#' Generate merged path for a fusion group
#' @param fusion_group Vector of piece IDs to merge
#' @param puzzle_structure Full puzzle with edges
#' @param fused_edges Edge skip set
#' @return Single SVG path string for the meta-piece
generate_meta_piece_path <- function(fusion_group, puzzle_structure, fused_edges)
```

**Boundary Walking Algorithm (Rectangular):**
```r
walk_fusion_boundary <- function(fusion_group, xn, yn) {
  # 1. Find all grid positions in group
  positions <- lapply(fusion_group, parse_piece_id)  # -> list of c(xi, yi)

  # 2. Find bounding rectangle
  min_x <- min(sapply(positions, `[`, 1))
  max_x <- max(sapply(positions, `[`, 1))
  min_y <- min(sapply(positions, `[`, 2))
  max_y <- max(sapply(positions, `[`, 2))

  # 3. Walk perimeter, only including edges that face outward
  boundary_edges <- list()
  for each edge on perimeter:
    if neighbor is NOT in fusion_group:
      add edge to boundary_edges

  # 4. Assemble into single path
  return(assemble_boundary_path(boundary_edges))
}
```

---

### Phase 5: Integration with Unified Pipeline

**Goal:** Wire meta-pieces through the existing pipeline.

#### 5.1 Update `generate_puzzle()` API

```r
generate_puzzle <- function(
  type = "rectangular",
  ...,
  fusion_groups = NULL,    # NEW: List of piece ID vectors to fuse
  vanish_internal = TRUE   # NEW: Whether fused edges disappear (vs dashed lines)
) {
  # Validate fusion groups
  if (!is.null(fusion_groups)) {
    fusion_groups <- validate_fusion_groups(fusion_groups, type, grid)
  }

  # Pass to internal generation
  pieces_result <- generate_pieces_internal(
    ...,
    fusion_groups = fusion_groups,
    vanish_internal = vanish_internal
  )
}
```

#### 5.2 Update `generate_pieces_internal()`

```r
generate_pieces_internal <- function(..., fusion_groups = NULL, vanish_internal = TRUE) {
  # Compute fused edges from fusion groups
  if (!is.null(fusion_groups)) {
    fused_edges <- compute_fused_edges(fusion_groups, type, grid)
  }

  # Pass to type-specific generator
  if (type == "rectangular") {
    result <- generate_rectangular_pieces_with_fusion(
      ..., fusion_groups = fusion_groups, fused_edges = fused_edges
    )
  } # ... etc for other types
}
```

#### 5.3 Update Piece List Structure

When fusion is active, the piece list changes:
- Individual pieces in fusion groups are replaced by single meta-pieces
- Meta-piece inherits properties:
  - `id`: Combined ID like `"meta_0_0+1_0"` or `"meta_1"`
  - `path`: Merged boundary path
  - `center`: Centroid of all constituent pieces
  - `constituent_pieces`: List of original piece IDs (for reference)
  - `type`: Same as puzzle type

```r
meta_piece <- list(
  id = "meta_1",
  path = "M 0 0 L 200 0 C ... Z",  # Merged outer boundary
  center = c(100, 50),
  constituent_pieces = c("piece_0_0", "piece_1_0"),
  type = "rectangular"
)
```

#### 5.4 Update Positioning

`apply_piece_positioning()` should handle meta-pieces naturally:
- Meta-piece has single center → translates as unit
- Offset calculation uses meta-piece position, not constituent positions

---

### Phase 6: Shiny UI Integration

**Goal:** Allow users to select pieces to fuse in the Shiny app.

#### 6.1 UI Components

```r
# In UI: Add fusion controls panel
fusion_panel <- conditionalPanel(
  condition = "input.enable_fusion",
  checkboxInput("enable_fusion", "Enable Piece Fusion", FALSE),

  # Option 1: Predefined patterns
  selectInput("fusion_pattern", "Fusion Pattern",
    choices = c("None", "2x2 Corners", "Center Block", "Custom")
  ),


  # Option 2: Click-to-select (advanced)
  # Would require interactive SVG with piece selection

  # Preview of fusion groups
  verbatimTextOutput("fusion_preview")
)
```

#### 6.2 Server Logic

```r
# In server: Handle fusion
fusion_groups <- reactive({
  if (!input$enable_fusion) return(NULL)

  if (input$fusion_pattern == "2x2 Corners") {
    # Generate corner fusion groups based on grid size
    generate_corner_fusion(input$rows, input$cols)
  } else if (input$fusion_pattern == "Custom") {
    # Parse custom input
    parse_custom_fusion(input$custom_fusion_text)
  }
})

# Pass to generate_puzzle()
puzzle <- generate_puzzle(
  ...,
  fusion_groups = fusion_groups()
)
```

---

### Phase 7: Testing Strategy

#### 7.1 Unit Tests

**File:** `tests/testthat/test-adjacency-api.R`
```r
test_that("rectangular adjacency works", {
  result <- generate_puzzle(type = "rectangular", grid = c(3, 3))
  neighbors <- get_piece_neighbors("piece_1_1", result)
  expect_equal(nrow(neighbors), 4)  # Center piece has 4 neighbors
})

test_that("are_pieces_adjacent works", {
  result <- generate_puzzle(type = "rectangular", grid = c(2, 2))
  expect_true(are_pieces_adjacent("piece_0_0", "piece_1_0", result))
  expect_false(are_pieces_adjacent("piece_0_0", "piece_1_1", result))  # Diagonal
})
```

**File:** `tests/testthat/test-meta-pieces.R`
```r
test_that("simple 2-piece fusion works for rectangular", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    fusion_groups = list(c("piece_0_0", "piece_1_0"))
  )
  expect_equal(length(result$pieces), 3)  # 4 - 2 + 1 meta = 3
})

test_that("meta piece path is valid closed path", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    fusion_groups = list(c("piece_0_0", "piece_1_0"))
  )
  meta <- result$pieces[[1]]  # Assuming first
  expect_true(grepl("^M.*Z$", meta$path))  # Starts with M, ends with Z
})

test_that("fusion works with separation", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    fusion_groups = list(c("piece_0_0", "piece_1_0")),
    offset = 10
  )
  # Meta piece should be separated from others
  expect_true(length(result$pieces) == 3)
})
```

#### 7.2 Visual Tests

Create test script that generates visual outputs:
```r
# tests/visual/test-fusion-visual.R

# Test 1: 2x2 rectangular with horizontal fusion
generate_puzzle(
  type = "rectangular", grid = c(2, 2),
  fusion_groups = list(c("piece_0_0", "piece_1_0")),
  save_files = TRUE,
  output_dir = "tests/visual/output"
)

# Test 2: 3x3 rectangular with 2x2 block fusion
generate_puzzle(
  type = "rectangular", grid = c(3, 3),
  fusion_groups = list(c("piece_0_0", "piece_1_0", "piece_0_1", "piece_1_1")),
  save_files = TRUE
)

# Test 3: Hexagonal with adjacent fusion
generate_puzzle(
  type = "hexagonal", grid = c(3),
  fusion_groups = list(c(1, 2)),  # Center + one ring-1 piece
  save_files = TRUE
)
```

---

## File Changes Summary

| File | Change Type | Description |
|------|-------------|-------------|
| `R/adjacency_api.R` | **NEW** | Unified adjacency API |
| `R/puzzle_core_clean.R` | MODIFY | Add fusion support to rectangular generation |
| `R/hexagonal_edge_generation_fixed.R` | MODIFY | Add fusion support to hexagonal |
| `R/concentric_edge_generation.R` | MODIFY | Add fusion support to concentric |
| `R/unified_piece_generation.R` | MODIFY | Pass fusion params, generate meta-pieces |
| `R/piece_positioning.R` | MINOR | Handle meta-piece positioning (should work as-is) |
| `R/unified_renderer.R` | MINOR | Handle meta-piece rendering (should work as-is) |
| `R/jigsawR_clean.R` | MODIFY | Add `fusion_groups` and `vanish_internal` params |
| `inst/shiny-app/app.R` | MODIFY | Add fusion UI controls |
| `tests/testthat/test-adjacency-api.R` | **NEW** | Adjacency API tests |
| `tests/testthat/test-meta-pieces.R` | **NEW** | Meta-piece fusion tests |

---

## Implementation Order

### Sprint 1: Foundation (Adjacency API)
1. Create `R/adjacency_api.R` with type dispatch
2. Implement `get_rect_neighbors()` (new)
3. Wrap `get_hex_neighbors_unified()`
4. Wrap `get_concentric_neighbors_unified()`
5. Add unit tests

### Sprint 2: Rectangular Fusion
1. Implement `compute_fused_edges()` for rectangular
2. Modify `generate_all_edges()` to skip fused edges
3. Implement `generate_meta_piece_path()` for rectangular
4. Update `generate_pieces_internal()` for rectangular fusion
5. Add tests and visual validation

### Sprint 3: Hexagonal & Concentric Fusion
1. Extend fusion to hexagonal puzzles
2. Extend fusion to concentric puzzles
3. Handle complex cases (ring boundaries, angular overlaps)
4. Add tests

### Sprint 4: API & Shiny Integration
1. Add `fusion_groups` parameter to `generate_puzzle()`
2. Add Shiny UI for fusion selection
3. Add predefined fusion patterns
4. Final testing and documentation

---

## Design Decisions (Resolved)

### 1. Internal Edge Style (Configurable)

The appearance of internal edges within fused groups is configurable:

| Style | Description |
|-------|-------------|
| `"none"` | Internal edges completely vanish (default) |
| `"dashed"` | Internal edges shown as dashed lines |
| `"solid"` | Internal edges shown as solid lines |

Additionally, an **opacity slider** (0.0 - 1.0) controls the transparency of internal edges when style is "dashed" or "solid".

```r
generate_puzzle(
  ...,
  fusion_groups = list(c(1, 2)),
  fusion_style = "dashed",      # "none", "dashed", "solid"
  fusion_opacity = 0.3          # 0.0 (invisible) to 1.0 (fully visible)
)
```

### 2. Validation Rules

- **Adjacency Required**: All pieces in a fusion group must be connected (each piece adjacent to at least one other in the group)
- **No Overlapping Groups**: A piece can only belong to ONE fusion group
- **No Maximum Size**: Fusion groups can be any size (up to all pieces)

### 3. Input Format

Simple, flexible input format:
- Single group: `"1,2"` → fuse pieces 1 and 2
- Multiple groups: `"(1,2),(7,8,9)"` → fuse 1+2 and separately fuse 7+8+9
- Programmatic: `list(c(1, 2), c(7, 8, 9))`

```r
# String input (Shiny-friendly)
fusion_groups = "(1,2),(7,8,9)"

# List input (programmatic)
fusion_groups = list(c(1, 2), c(7, 8, 9))
```

### 4. Meta-Piece Naming

Simple sequential naming:
- `meta_1`, `meta_2`, `meta_3`, ...
- Original piece IDs stored in `constituent_pieces` attribute

```r
meta_piece <- list(
  id = "meta_1",
  path = "M 0 0 L 200 0 ...",
  center = c(100, 50),
  constituent_pieces = c(1, 2),  # Original piece IDs
  type = "rectangular"
)
```

### 5. Separation Behavior

- Fused pieces **stay together** when offset > 0 (move as single unit)
- No "explode" option needed (user can simply not fuse if they want separation)

---

## Success Criteria

- [ ] Unified adjacency API works for all puzzle types
- [ ] Two adjacent rectangular pieces can be fused
- [ ] 2x2 block of rectangular pieces can be fused
- [ ] Meta-piece paths are valid closed SVG paths
- [ ] Meta-pieces separate correctly with offset
- [ ] Hexagonal adjacent pieces can be fused
- [ ] Concentric adjacent pieces can be fused
- [ ] Shiny app has basic fusion controls
- [ ] All tests pass
- [ ] Visual output matches expected appearance
