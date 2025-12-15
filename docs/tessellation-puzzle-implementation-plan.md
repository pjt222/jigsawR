# Tessellation Puzzle Implementation Plan

## Issues: #41 (Random Shapes) and #42 (Voronoi)

**Version Tag:** v0.3.0 (created before this work)

---

## Overview

This plan outlines the implementation of two new puzzle types:
1. **Voronoi Puzzles** (Issue #42) - Fermat spiral + Voronoi tessellation
2. **Random Shape Puzzles** (Issue #41) - Polygonal base + Delaunay triangulation

Both share common infrastructure for handling arbitrary polygon edges with bezier tabs.

---

## Architecture

### Package Dependencies

Add to `DESCRIPTION` Suggests:
```
Suggests:
    ...
    deldir,     # Voronoi tessellation (Issue #42)
    RCDT        # Constrained Delaunay (Issue #41)
```

### New Files Structure

```
R/
├── tessellation_edge_generation.R  # Shared: arbitrary polygon tab generation
├── voronoi_puzzle.R                # Issue #42: Voronoi puzzle type
├── voronoi_topology.R              # Issue #42: Voronoi adjacency
├── random_puzzle.R                 # Issue #41: Random shape puzzle type
└── random_topology.R               # Issue #41: Random shape adjacency

tests/testthat/
├── test-voronoi-puzzle.R
└── test-random-puzzle.R
```

---

## Phase 1: Shared Edge Generation Utilities

### File: `R/tessellation_edge_generation.R`

Generalize existing `generate_hex_bezier_edge()` for arbitrary polygons:

```r
#' Generate bezier edge with tab between two arbitrary points
#'
#' @param v1 Start vertex c(x, y)
#' @param v2 End vertex c(x, y)
#' @param seed Random seed for reproducibility
#' @param edge_id Unique edge identifier
#' @param tabsize Tab size percentage (default: 20)
#' @param jitter Jitter percentage (default: 4)
#' @param tab_direction 1 for outward, -1 for inward
#' @return List with forward and reverse SVG paths
generate_polygon_edge <- function(v1, v2, seed, edge_id,
                                   tabsize = 20, jitter = 4,
                                   tab_direction = 1) {
  # Implementation based on hexagonal_bezier_generation.R
  # ...
}

#' Build edge map for tessellation
#'
#' Creates shared edge paths for all adjacent cell pairs
#'
#' @param cells List of cells with vertices
#' @param adjacency Adjacency data (which cells share edges)
#' @param seed Base seed
#' @param tabsize Tab size percentage
#' @param jitter Jitter percentage
#' @return Named list of edge paths
build_tessellation_edge_map <- function(cells, adjacency, seed,
                                         tabsize = 20, jitter = 4) {
  # For each adjacent pair, generate complementary edges
  # ...
}
```

---

## Phase 2: Voronoi Puzzle Type (Issue #42)

### File: `R/voronoi_puzzle.R`

#### API Design
```r
generate_puzzle(
  type = "voronoi",
  seed = 42,
  grid = c(50),           # Number of Voronoi cells (OR c(width_cells, height_cells))
  size = c(300, 300),     # Puzzle dimensions (width, height) OR c(diameter) for circular
  boundary = "rectangular", # "rectangular" or "circular"
  point_distribution = "fermat", # "fermat", "uniform", or "custom"
  tabsize = 20,
  jitter = 4,
  offset = 0,
  ...
)
```

#### Implementation Steps

1. **Point Generation**
   - `generate_fermat_spiral_points(n, size)` - Fermat spiral distribution
   - `generate_uniform_points(n, size)` - Random uniform distribution
   - `generate_jittered_grid_points(n, size)` - Grid with jitter

2. **Voronoi Tessellation**
   - Use `deldir::deldir()` for tessellation
   - Use `deldir::tile.list()` to extract cell polygons
   - Handle boundary clipping

3. **Edge Processing**
   - Build edge map from adjacent cells
   - Add bezier tabs to all internal edges
   - Keep boundary edges as straight lines or follow boundary shape

4. **Piece Assembly**
   - Convert cells to standard piece objects
   - Calculate centers
   - Return standardized result

#### Core Functions

```r
#' Generate Voronoi pieces
generate_voronoi_pieces_internal <- function(seed, grid, size, tabsize, jitter,
                                              boundary = "rectangular",
                                              point_distribution = "fermat",
                                              fusion_groups = NULL,
                                              fusion_style = "none",
                                              fusion_opacity = 1.0) {
  # 1. Generate seed points
  n_cells <- if (length(grid) == 1) grid else grid[1] * grid[2]
  points <- switch(point_distribution,
    fermat = generate_fermat_points(n_cells, size),
    uniform = generate_uniform_points(n_cells, size, seed),
    jittered = generate_jittered_grid_points(grid, size, seed)
  )

  # 2. Compute Voronoi tessellation
  vor <- deldir::deldir(points$x, points$y, rw = get_boundary_rect(size))
  tiles <- deldir::tile.list(vor)

  # 3. Build adjacency from Voronoi
  adjacency <- extract_voronoi_adjacency(vor)

  # 4. Generate edge map with tabs
  edge_map <- build_tessellation_edge_map(tiles, adjacency, seed, tabsize, jitter)

  # 5. Assemble pieces
  pieces <- assemble_voronoi_pieces(tiles, edge_map, boundary, size)

  # Return standardized result
  list(
    pieces = pieces,
    canvas_size = size,
    type = "voronoi",
    parameters = list(...),
    fusion_data = NULL
  )
}
```

### File: `R/voronoi_topology.R`

```r
#' Get Voronoi cell neighbors
#'
#' Uses Delaunay triangulation dual to find adjacent cells
get_voronoi_neighbors <- function(piece_id, puzzle_result, include_boundary = TRUE) {
  # Voronoi adjacency is stored during generation
  # Return from puzzle_result$adjacency
}
```

---

## Phase 3: Random Shape Puzzle Type (Issue #41)

### File: `R/random_puzzle.R`

#### API Design
```r
generate_puzzle(
  type = "random",
  seed = 42,
  n_corner = 4,           # Base shape corners (3=triangle, 4=rectangle, ...)
  grid = c(20),           # Approximate number of pieces
  size = c(300, 200),     # Base shape dimensions
  min_piece_size = 30,    # Minimum piece dimension (optional)
  tabsize = 20,
  jitter = 4,
  offset = 0,
  ...
)
```

#### Implementation Steps

1. **Base Shape Generation**
   - `generate_base_polygon(n_corner, size)` - Create n-gon boundary
   - Support rectangle special case for n_corner = 4

2. **Node Placement**
   - Generate random points inside polygon
   - Add boundary vertices to point set
   - Respect minimum piece size constraint

3. **Constrained Delaunay Triangulation**
   - Use `RCDT::delaunay()` with constraint edges
   - Each triangle becomes a puzzle piece

4. **Edge Processing**
   - Internal edges get bezier tabs
   - Boundary edges follow polygon boundary

5. **Piece Assembly**
   - Convert triangles to standard piece objects
   - Calculate centers
   - Return standardized result

#### Core Functions

```r
#' Generate random shape pieces
generate_random_pieces_internal <- function(seed, n_corner, grid, size,
                                             tabsize, jitter, min_piece_size = NULL,
                                             fusion_groups = NULL,
                                             fusion_style = "none",
                                             fusion_opacity = 1.0) {
  # 1. Generate base polygon
  boundary <- generate_base_polygon(n_corner, size)

  # 2. Generate interior nodes
  n_pieces <- if (length(grid) == 1) grid else grid[1]
  interior_points <- generate_interior_nodes(n_pieces, boundary, seed, min_piece_size)

  # 3. Combine boundary vertices + interior points
  all_points <- rbind(boundary$vertices, interior_points)

  # 4. Constrained Delaunay triangulation
  edges <- boundary$constraint_edges
  del <- RCDT::delaunay(all_points, edges = edges)

  # 5. Extract triangles
  triangles <- extract_triangles(del, all_points)

  # 6. Build edge map with tabs
  adjacency <- extract_triangle_adjacency(del)
  edge_map <- build_tessellation_edge_map(triangles, adjacency, seed, tabsize, jitter)

  # 7. Assemble pieces
  pieces <- assemble_triangle_pieces(triangles, edge_map, boundary)

  # Return standardized result
  list(
    pieces = pieces,
    canvas_size = size,
    type = "random",
    parameters = list(...),
    fusion_data = NULL
  )
}
```

---

## Phase 4: Integration

### Updates to `R/jigsawR_clean.R`

Add type dispatch in `generate_puzzle()`:
```r
generate_puzzle <- function(..., type = "rectangular", ...) {
  # Validation
  valid_types <- c("rectangular", "hexagonal", "concentric", "voronoi", "random")
  if (!type %in% valid_types) {
    cli::cli_abort("Invalid puzzle type: {type}")
  }

  # Check dependencies for new types
  if (type == "voronoi" && !requireNamespace("deldir", quietly = TRUE)) {
    cli::cli_abort("Package 'deldir' required for voronoi puzzles. Install with: install.packages('deldir')")
  }
  if (type == "random" && !requireNamespace("RCDT", quietly = TRUE)) {
    cli::cli_abort("Package 'RCDT' required for random puzzles. Install with: install.packages('RCDT')")
  }

  # ...existing dispatch...
}
```

### Updates to `R/unified_piece_generation.R`

```r
generate_pieces_internal <- function(type, ...) {
  if (type == "voronoi") {
    return(generate_voronoi_pieces_internal(...))
  } else if (type == "random") {
    return(generate_random_pieces_internal(...))
  }
  # ...existing types...
}
```

### Updates to `R/piece_positioning.R`

```r
apply_piece_positioning <- function(piece_result, offset, ...) {
  if (piece_result$type == "voronoi") {
    positioned <- apply_voronoi_positioning(piece_result, offset)
  } else if (piece_result$type == "random") {
    positioned <- apply_random_positioning(piece_result, offset)
  }
  # ...existing types...
}
```

### Updates to `R/adjacency_api.R`

```r
get_piece_neighbors <- function(piece_id, puzzle_result, ...) {
  type <- puzzle_result$type
  if (type == "voronoi") {
    return(get_voronoi_neighbors(piece_id, puzzle_result, ...))
  } else if (type == "random") {
    return(get_random_neighbors(piece_id, puzzle_result, ...))
  }
  # ...existing types...
}
```

---

## Phase 5: Shiny App Integration

### Updates to `inst/shiny-app/app.R`

Add UI elements:
- Type selector: Add "Voronoi" and "Random" options
- Conditional parameters:
  - Voronoi: n_cells, boundary shape, point distribution
  - Random: n_corner, n_pieces, min_piece_size

```r
# Type-specific UI
conditionalPanel(
  condition = "input.puzzle_type == 'voronoi'",
  numericInput("n_cells", "Number of Cells", 50, min = 10, max = 200),
  selectInput("boundary", "Boundary", choices = c("rectangular", "circular")),
  selectInput("point_dist", "Point Distribution", choices = c("fermat", "uniform", "jittered"))
)

conditionalPanel(
  condition = "input.puzzle_type == 'random'",
  numericInput("n_corner", "Base Shape Corners", 4, min = 3, max = 8),
  numericInput("n_pieces", "Approx. Pieces", 20, min = 5, max = 100)
)
```

---

## Phase 6: Testing

### Test Files

**`tests/testthat/test-voronoi-puzzle.R`:**
- Test Fermat spiral point generation
- Test uniform point generation
- Test Voronoi tessellation produces valid cells
- Test edge map generation with tabs
- Test piece assembly produces valid SVG
- Test boundary handling (rectangular, circular)
- Test fusion groups work correctly
- Test seed reproducibility

**`tests/testthat/test-random-puzzle.R`:**
- Test base polygon generation for n_corner = 3,4,5,6
- Test interior node placement inside polygon
- Test constrained Delaunay produces valid triangles
- Test edge map generation with tabs
- Test piece assembly produces valid SVG
- Test minimum piece size constraint
- Test fusion groups work correctly
- Test seed reproducibility

---

## Implementation Order

1. **Phase 1**: Shared edge generation utilities (foundation)
2. **Phase 2**: Voronoi puzzle (simpler, cells already defined by deldir)
3. **Phase 3**: Random shapes (builds on Phase 1, more complex)
4. **Phase 4**: Integration (dispatch updates)
5. **Phase 5**: Shiny app (UI additions)
6. **Phase 6**: Testing (comprehensive validation)

---

## Technical Notes

### Tab Direction Consistency

For two adjacent cells A and B sharing an edge:
- Cell A's edge: uses `forward` path with tab_direction = 1 (outward)
- Cell B's edge: uses `reverse` path with tab_direction = -1 (inward)

Edge map stores both directions:
```r
edge_map[["E{minId}-{maxId}"]] <- list(
  forward = "C...",  # For smaller ID piece
  reverse = "C...",  # For larger ID piece
  start = c(x, y),
  end = c(x, y)
)
```

### Boundary Handling

**Rectangular boundary:**
- Clip Voronoi cells to rectangle
- Boundary edges are straight lines

**Circular boundary (Voronoi only):**
- Clip cells to circle
- Boundary edges follow arc

**Polygon boundary (Random only):**
- Boundary vertices are part of triangulation
- Boundary edges are straight segments along polygon

### Performance Considerations

- deldir is fast for up to ~1000 points
- RCDT uses CDT library (fast C++ implementation)
- Edge map prevents duplicate tab generation
- Pre-calculate adjacency during tessellation

---

## Acceptance Criteria

### Issue #42 (Voronoi)
- [x] Fermat spiral point generation works
- [ ] Voronoi tessellation produces valid cells
- [ ] All cells converted to puzzle pieces with tabs
- [ ] Rectangular and circular boundaries supported
- [ ] Offset separation works correctly
- [ ] Integrated with Shiny app
- [ ] Test suite validates output

### Issue #41 (Random Shapes)
- [ ] Base shapes work for n_corner = 3, 4, 5, 6
- [ ] Random node placement is seed-reproducible
- [ ] Constrained Delaunay produces valid triangles
- [ ] Tabs added to all internal edges
- [ ] Minimum piece size constraint respected
- [ ] Integrated with Shiny app
- [ ] Test suite validates output
