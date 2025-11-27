# Hexagonal Edge Complementarity Analysis

**Date**: 2025-11-26
**Status**: Analysis complete, implementation needed

## Current Implementation Status

✅ **Completed**:
- Direct bezier generation for hexagonal pieces
- Flat-top honeycomb orientation (all pieces parallel)
- Tab generation with deterministic seed
- 6-edge hexagon piece assembly

❌ **Missing**:
- Edge complementarity between adjacent pieces
- Shared edge generation (like rectangular puzzle)

## The Problem

Current implementation generates **independent edges** for each piece:
- Each piece calculates `edge_id = piece_id * 10 + side`
- Adjacent pieces use **different edge_ids**
- Result: Pieces won't fit together (different tab shapes)

### Example
```
Piece 1, side 0: edge_id = 10
Piece 2, side 3: edge_id = 23  <-- Different!
```

These should share the same edge with complementary paths (forward/reverse).

## The Solution Pattern (from Rectangular Puzzle)

The rectangular puzzle uses this pattern:

### 1. Pre-generate All Edges
```r
edges <- generate_all_edges(xn, yn)
# Returns: list(
#   horizontal = list(...),  # Edges between rows
#   vertical = list(...)     # Edges between columns
# )
```

### 2. Each Edge Has Forward/Reverse Paths
```r
edge <- list(
  forward = "C ...",   # Path from v1 to v2
  reverse = "C ...",   # Path from v2 to v1 (complementary)
  start = c(x1, y1),
  end = c(x2, y2)
)
```

### 3. Build Pieces from Pre-generated Edges
```r
# Top edge (forward)
edge <- edges$horizontal[[yi]][[xi + 1]]
path <- paste0(path, edge$forward)

# Bottom edge (reverse - from adjacent piece below)
edge <- edges$horizontal[[yi + 1]][[xi + 1]]
path <- paste0(path, edge$reverse)
```

## Required Changes for Hexagonal Puzzle

### 1. Create `generate_all_hex_edges()`
Pre-generate all unique edges in the puzzle, organized by:
- Ring boundaries (edges between rings)
- Within-ring boundaries (edges between adjacent pieces in same ring)

### 2. Edge Organization Structure
```r
edges <- list(
  # Edges between center and ring 1
  ring_0_to_1 = list(
    edge_1 = list(forward = "...", reverse = "..."),
    edge_2 = list(...),
    ... # 6 edges
  ),

  # Edges within ring 1 (between adjacent pieces)
  ring_1_internal = list(
    edge_1 = list(...),  # Between pieces 2-3
    edge_2 = list(...),  # Between pieces 3-4
    ... # 6 edges
  ),

  # Edges between ring 1 and ring 2
  ring_1_to_2 = list(
    edge_1 = list(...),
    ... # 18 edges (6 pieces * 3 connections each)
  ),

  # ... and so on
)
```

### 3. Neighbor Mapping
Need function to determine which edge a piece uses for each side:
```r
get_hex_edge_reference <- function(piece_id, side, rings) {
  # Returns: list(
  #   category = "ring_1_internal",
  #   index = 3,
  #   direction = "forward"  # or "reverse"
  # )
}
```

## Hexagonal Neighbor Patterns

### Center Piece (Ring 0)
- 6 neighbors: all pieces in ring 1
- Side 0 → Piece 2
- Side 1 → Piece 3
- Side 2 → Piece 4
- Side 3 → Piece 5
- Side 4 → Piece 6
- Side 5 → Piece 7

### Ring 1 Pieces (Example: Piece 2)
- 6 neighbors total:
  - Side 3: Center (piece 1)
  - Side 2: Previous in ring (piece 7)
  - Side 4: Next in ring (piece 3)
  - Sides 0, 1, 5: Ring 2 pieces (8, 9, 19)

### Ring n Pieces (General)
Each piece has:
- 1 edge toward inner ring
- 2 edges to adjacent pieces in same ring
- 3 edges toward outer ring (or border for last ring)

## Implementation Plan

### Phase 1: Edge Pre-generation
Create `R/hexagonal_edge_pregeneration.R`:
```r
#' Generate all unique edges for hexagonal puzzle
#'
#' @param rings Number of rings
#' @param seed Random seed
#' @param diameter Puzzle diameter
#' @param tabsize Tab size percentage
#' @param jitter Jitter percentage
#' @return Structured list of all edges
generate_all_hex_edges <- function(rings, seed, diameter, tabsize, jitter) {
  # 1. Calculate total number of unique edges
  # 2. For each ring boundary, generate edges
  # 3. For each ring, generate internal edges
  # 4. Store with forward/reverse paths
}
```

### Phase 2: Edge Lookup
Create `R/hexagonal_edge_lookup.R`:
```r
#' Get edge reference for a piece side
#'
#' @param piece_id Piece ID
#' @param side Side number (0-5)
#' @param rings Number of rings
#' @return Edge reference (category, index, direction)
get_hex_edge_reference <- function(piece_id, side, rings) {
  # Use topology info to determine:
  # - Which edge category this belongs to
  # - Index within that category
  # - Forward or reverse direction
}
```

### Phase 3: Update Piece Generation
Modify `generate_hex_piece_bezier()`:
```r
generate_hex_piece_bezier <- function(piece_id, rings, seed, diameter, ..., edges) {
  # REMOVED: generate_hex_bezier_edge() calls
  # ADDED: Look up pre-generated edges

  for (side in 0:5) {
    edge_ref <- get_hex_edge_reference(piece_id, side, rings)
    edge_data <- edges[[edge_ref$category]][[edge_ref$index]]

    if (edge_ref$direction == "forward") {
      edge_paths[[side + 1]] <- edge_data$forward
    } else {
      edge_paths[[side + 1]] <- edge_data$reverse
    }
  }

  # Assemble piece from looked-up edges
}
```

## Estimated Timeline

- **Phase 1** (Edge pre-generation): 2 days
  - Calculate edge counts for each ring
  - Generate edges with proper seed management
  - Test edge structure

- **Phase 2** (Edge lookup): 1 day
  - Implement neighbor logic
  - Test lookup function

- **Phase 3** (Integration): 1 day
  - Modify piece generation
  - Update test scripts
  - Visual verification

**Total**: 4 days for full edge complementarity

## Alternative: Simplified Approach

For a quicker solution, we could:
1. Accept that current pieces don't fit together
2. Document as "placeholder bezier pieces"
3. Defer full complementarity for later

This would allow us to:
- ✅ See bezier curves with tabs
- ✅ Understand tab parameters
- ✅ Test visual appearance
- ❌ But pieces won't actually fit together

## Recommendation

Given the complexity, I recommend:
1. **Document current state** as "Phase 1: Bezier curves implemented"
2. **User decision**: Continue to Phase 2 (complementarity) or defer?
3. **If continue**: Implement full edge pre-generation system
4. **If defer**: Add `use_bezier = FALSE` option to separation function, keep placeholder mode as default

The current bezier implementation is valuable even without complementarity - it demonstrates the approach and can be upgraded later.
