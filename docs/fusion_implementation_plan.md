# Implementation Plan: Fusion for Hexagonal and Concentric Puzzles

## Executive Summary

This plan outlines the steps to enable fusion (meta-pieces) for hexagonal and concentric puzzle types, following the pattern established for rectangular puzzles.

**Current State:**
- ✅ Rectangular: Full fusion support
- ❌ Hexagonal: Infrastructure ready, fusion generation not implemented
- ❌ Concentric: Infrastructure ready, fusion generation not implemented

**Recommendation:** Implement concentric first (simpler 4-edge topology), then hexagonal (6-edge topology).

---

## Phase 1: Concentric Fusion (Estimated: 3 components)

### 1.1 Edge Naming Convention

**Existing convention** (from `concentric_geometry.R:169-173`):
| Edge Index | Name | Description |
|-----------|------|-------------|
| 1 | INNER | Connects to inner ring or center |
| 2 | RIGHT | Next piece in same ring (clockwise) |
| 3 | OUTER | Connects to outer ring or boundary |
| 4 | LEFT | Previous piece in same ring |

**Center piece** (hexagon type): 6 edges numbered 1-6, each connecting to ring 1 pieces.

**Edge key format**: `"{piece_id}-{edge_name}"` (e.g., `"5-INNER"`, `"1-3"` for center)

### 1.2 Adjacency Functions

**File:** `R/adjacency_api.R`

**Functions to add:**

```r
#' Get concentric piece neighbors
#' @param piece_id Piece ID (1-based)
#' @param rings Number of rings in puzzle
#' @param center_shape "hexagon" or "circle"
#' @return Data frame with direction, neighbor_id, is_boundary
get_concentric_neighbors <- function(piece_id, rings, center_shape = "hexagon") {
  # Use existing get_concentric_neighbor() from concentric_geometry.R
  # Returns neighbors for all 4 edges (or 6 for center)
}

#' Get complementary edge for concentric
#' @param direction Edge direction
#' @return Opposite direction
get_concentric_complementary_direction <- function(direction) {
  switch(direction,
    "INNER" = "OUTER",
    "OUTER" = "INNER",
    "LEFT" = "RIGHT",
    "RIGHT" = "LEFT",
    # Center piece edges (1-6) - opposite is (n + 3) %% 6
    as.character((as.integer(direction) + 2) %% 6 + 1)
  )
}
```

### 1.3 Fused Edge Computation

**File:** `R/adjacency_api.R`

**Function to add:**

```r
#' Compute fused edges for concentric puzzles
#' @param fusion_groups List of piece ID vectors
#' @param puzzle_result Puzzle result with pieces and parameters
#' @return List with fused_edges, edge_to_group, piece_to_group
compute_concentric_fused_edges <- function(fusion_groups, puzzle_result) {
  # Pattern: Same as compute_fused_edges() but:
  # - Use get_concentric_neighbors() instead of get_piece_neighbors()
  # - Handle center piece 6-edge case
  # - Use concentric complementary directions
}
```

### 1.4 Piece Generation Update

**File:** `R/unified_piece_generation.R`

**Function:** `generate_concentric_pieces_internal()` (lines 351-464)

**Changes:**
1. Remove "not yet implemented" comment
2. Call `compute_concentric_fused_edges()` if fusion_groups provided
3. For each piece, set `fused_edges` metadata:
   ```r
   # For trapezoid pieces (ring > 0)
   fused_edges <- list(INNER = FALSE, RIGHT = FALSE, OUTER = FALSE, LEFT = FALSE)
   if (!is.null(fused_edge_data)) {
     fused_edges$INNER <- is_edge_fused(piece_id, "INNER", fused_edge_data)
     fused_edges$RIGHT <- is_edge_fused(piece_id, "RIGHT", fused_edge_data)
     fused_edges$OUTER <- is_edge_fused(piece_id, "OUTER", fused_edge_data)
     fused_edges$LEFT <- is_edge_fused(piece_id, "LEFT", fused_edge_data)
   }

   # For center piece (ring 0, hexagon type)
   # fused_edges = list(`1` = FALSE, `2` = FALSE, ..., `6` = FALSE)
   ```

### 1.5 Path Splitting

**File:** `R/unified_renderer.R`

**Function to add:**

```r
#' Split concentric piece path into individual edge paths
#' @param path SVG path string
#' @param piece Piece object with ring_pos
#' @return List with INNER, RIGHT, OUTER, LEFT edge paths
split_concentric_path_into_edges <- function(path, piece) {
  # Path construction order: M, INNER, RIGHT, OUTER, LEFT, Z
  # Use vertex positions to identify edge boundaries
  # Similar to split_rect_path_into_edges but with different corner detection
}
```

### 1.6 Renderer Integration

**File:** `R/unified_renderer.R`

**Update `render_pieces_with_fusion_styled()`:**
- Detect piece type from `piece$type`
- Call appropriate path splitting function:
  - `"rectangular"` → `split_rect_path_into_edges()`
  - `"concentric"` → `split_concentric_path_into_edges()`
  - `"hexagonal"` → `split_hex_path_into_edges()`

---

## Phase 2: Hexagonal Fusion (Estimated: 3 components)

### 2.1 Edge Naming Convention

**Existing convention** (from `hexagonal_neighbors.R:10-21`):
| Side | Direction | Angle |
|------|-----------|-------|
| 0 | Right | 0° |
| 1 | Upper-right | 60° |
| 2 | Upper-left | 120° |
| 3 | Left | 180° |
| 4 | Lower-left | 240° |
| 5 | Lower-right | 300° |

**Edge key format**: `"{piece_id}-{side}"` (e.g., `"7-3"` for piece 7, side 3)

### 2.2 Adjacency Functions

**File:** `R/adjacency_api.R`

**Functions to add:**

```r
#' Get hexagonal piece neighbors
#' @param piece_id Piece ID (1-based)
#' @param rings Number of rings
#' @return Data frame with side, neighbor_id, is_boundary
get_hex_neighbors_for_fusion <- function(piece_id, rings) {
  # Use existing get_hex_neighbor() from hexagonal_neighbors.R
  # Return neighbors for all 6 sides
}

#' Get complementary side for hexagonal
#' @param side Side number (0-5)
#' @return Opposite side number
get_hex_complementary_side <- function(side) {
  (side + 3) %% 6  # Opposite side is +3 mod 6
}
```

### 2.3 Fused Edge Computation

**File:** `R/adjacency_api.R`

**Function to add:**

```r
#' Compute fused edges for hexagonal puzzles
#' @param fusion_groups List of piece ID vectors
#' @param puzzle_result Puzzle result with pieces and parameters
#' @return List with fused_edges, edge_to_group, piece_to_group
compute_hex_fused_edges <- function(fusion_groups, puzzle_result) {
  # Pattern: Same as compute_fused_edges() but:
  # - Use get_hex_neighbors_for_fusion()
  # - Handle 6 sides instead of 4
  # - Use (side + 3) %% 6 for complementary
}
```

### 2.4 Piece Generation Update

**File:** `R/unified_piece_generation.R`

**Function:** `generate_hex_pieces_internal()` (lines 225-348)

**Changes:**
1. Remove "not yet implemented" comment
2. Call `compute_hex_fused_edges()` if fusion_groups provided
3. For each piece, set `fused_edges` metadata:
   ```r
   fused_edges <- list(`0` = FALSE, `1` = FALSE, `2` = FALSE,
                       `3` = FALSE, `4` = FALSE, `5` = FALSE)
   if (!is.null(fused_edge_data)) {
     for (side in 0:5) {
       fused_edges[[as.character(side)]] <- is_edge_fused(piece_id, side, fused_edge_data)
     }
   }
   ```

### 2.5 Path Splitting

**File:** `R/unified_renderer.R`

**Function to add:**

```r
#' Split hexagonal piece path into individual edge paths
#' @param path SVG path string
#' @param piece Piece object
#' @return List with edges 0-5
split_hex_path_into_edges <- function(path, piece) {
  # Path construction: Counter-clockwise, sides 0→5
  # Each side is one segment (L or C commands)
  # Use vertex positions from edge map if available
}
```

### 2.6 Renderer Integration

Same as Phase 1.6 - already covered in the type-based dispatch.

---

## Phase 3: Testing

### 3.1 Unit Tests

**File:** `tests/testthat/test-fusion-concentric.R`
```r
test_that("concentric fusion works with offset=0", { ... })
test_that("concentric fusion works with offset>0", { ... })
test_that("concentric fused pieces move together", { ... })
test_that("concentric center piece fusion works", { ... })
```

**File:** `tests/testthat/test-fusion-hexagonal.R`
```r
test_that("hexagonal fusion works with offset=0", { ... })
test_that("hexagonal fusion works with offset>0", { ... })
test_that("hexagonal fused pieces move together", { ... })
test_that("hexagonal center piece fusion works", { ... })
```

### 3.2 Integration Tests

- Test all three puzzle types with same fusion_groups format
- Verify Shiny app displays fusion controls for all types
- Visual verification of edge hiding/styling

### 3.3 Edge Cases

- Center piece fusion (piece 1 with ring 1 pieces)
- Multi-ring fusion (pieces spanning rings)
- Full puzzle fusion (all pieces in one group)
- Empty fusion groups
- Invalid piece IDs in fusion groups

---

## Phase 4: Documentation

### 4.1 Update Function Documentation
- Add fusion examples to `generate_puzzle()` for all types
- Document edge naming conventions

### 4.2 Update CLAUDE.md
- Add fusion support status for each puzzle type
- Document edge naming conventions

---

## Implementation Order

1. **Concentric adjacency functions** (1.2) - builds on existing `get_concentric_neighbor()`
2. **Concentric fused edge computation** (1.3) - copy pattern from rectangular
3. **Concentric piece generation update** (1.4) - integrate into generation loop
4. **Concentric path splitting** (1.5) - simpler than hexagonal
5. **Renderer integration** (1.6) - type-based dispatch
6. **Concentric tests** (3.1) - verify implementation
7. **Hexagonal adjacency functions** (2.2) - builds on existing `get_hex_neighbor()`
8. **Hexagonal fused edge computation** (2.3) - copy pattern from concentric
9. **Hexagonal piece generation update** (2.4) - integrate into generation loop
10. **Hexagonal path splitting** (2.5) - 6-edge version
11. **Hexagonal tests** (3.1) - verify implementation
12. **Documentation updates** (4.1, 4.2)

---

## Risks and Mitigations

| Risk | Mitigation |
|------|------------|
| Path splitting complexity | Use existing edge map data instead of parsing paths |
| Center piece special cases | Handle separately with explicit edge counts |
| Multi-segment edges (concentric OUTER) | May need segment grouping logic |
| Performance with large puzzles | Reuse existing neighbor detection infrastructure |

---

## Success Criteria

1. All existing tests continue to pass (901 tests)
2. New fusion tests pass for concentric and hexagonal
3. Shiny app displays fusion correctly for all puzzle types
4. Edge styling (none/dashed/solid) works for all types
5. Fused pieces move together when separated
