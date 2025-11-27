# Hexagonal Puzzle Rotation Infrastructure Plan
**Date**: 2025-11-26
**Status**: Active Planning Document
**Context**: Following successful placeholder rotation fix

## Executive Summary

The placeholder rotation fix has validated that the topology infrastructure is ready for real piece generation using a **Hybrid Direct Generation** approach. This document outlines how rotation will work in the direct generation system.

## What We Just Fixed (Placeholder Rotation)

### The Problem
All placeholder hexagons were oriented the same way (pointing up at 0°), when they should point radially outward from the center.

### The Solution (5 Lines of Code)
```r
# 1. Added rotation parameter to create_hex_placeholder()
create_hex_placeholder <- function(cx, cy, radius, rotation = 0) {
  angle <- i * pi / 3 + rotation  # Apply rotation to vertices
}

# 2. Get rotation from topology
ring_info <- map_piece_id_to_ring(i, rings)
piece_rotation <- ring_info$angle

# 3. Pass rotation to placeholder
hex_path <- create_hex_placeholder(cx, cy, radius, piece_rotation)
```

### Key Insight
The topology utilities already provide:
- ✅ Piece-to-ring mapping
- ✅ Direction-based positioning (angle)
- ✅ Cartesian coordinates (x, y)
- ✅ Rotation angle per piece

This infrastructure can be **reused for real piece generation**.

## Rotation in Direct Generation Approach

### Architecture Overview

```
Topology Layer (Exists)           Generation Layer (To Build)
─────────────────────────────────────────────────────────────
map_piece_id_to_ring()     →     generate_hex_piece()
  ├─ ring: 0, 1, 2...            ├─ Use ring for edge selection
  ├─ position: 0...6*ring-1      ├─ Use position for neighbors
  └─ angle: radians              └─ Use angle for rotation
                                      │
hex_ring_to_cartesian()     →        ├─ generate_edge_segment()
  ├─ x coordinate                    │   with rotation parameter
  └─ y coordinate                    │
                                     │
calculate_hex_piece_position() →    └─ Transform coordinates
  ├─ separation_factor                  - Rotate control points
  └─ base_spacing                       - Apply to bezier curves
```

### How Rotation Will Work

#### 1. Edge Generation with Rotation
Following the rectangular pattern but adding rotation:

```r
generate_edge_segment <- function(seed, edge_params, rotation = 0) {
  # Generate base edge (unrotated)
  base_edge <- generate_base_bezier_curve(seed, edge_params)

  # If rotation needed, transform control points
  if (rotation != 0) {
    rotated_edge <- rotate_bezier_curve(base_edge, rotation)
    return(rotated_edge)
  }

  return(base_edge)
}
```

#### 2. Piece Assembly with Rotation Context
```r
generate_hex_piece <- function(piece_id, rings, seed, params) {
  # Get topology info (includes rotation)
  ring_info <- map_piece_id_to_ring(piece_id, rings)
  position <- calculate_hex_piece_position(piece_id, rings, ...)

  # Determine piece type (center, inner, outer, edge)
  piece_type <- classify_hex_piece(ring_info$ring, ring_info$position)

  # Generate edges with rotation context
  if (piece_type == "center") {
    # Center piece: 6 edges, no rotation needed
    edges <- generate_hex_center_edges(seed, params)
  } else {
    # Other pieces: may need rotation
    edges <- generate_hex_piece_edges(
      seed = seed,
      piece_id = piece_id,
      rotation = ring_info$angle,
      params = params
    )
  }

  # Assemble piece path
  path <- assemble_hex_piece_path(edges, position)
  return(path)
}
```

#### 3. Coordinate Transformation Pipeline
The hexagonal puzzle already has coordinate transformation functions that handle rotation:

```r
# From hexagonal_puzzle.R
hex_rotate <- function(rot) {
  function(pos) {
    x <- pos[1]
    y <- pos[2]
    c(
      cos(rot) * x - sin(rot) * y,
      sin(rot) * x + cos(rot) * y
    )
  }
}
```

**We can reuse this for individual pieces!**

### Rotation Types in Hexagonal Puzzles

#### Type 1: Border Rotation (Original Generator)
- Applied 6 times at 60° intervals
- Creates hexagonal envelope
- Only affects outermost pieces
- **Used in**: Connected puzzle generation

#### Type 2: Piece Radial Rotation (New)
- Applied per-piece based on position
- Points pieces outward from center
- Varies continuously around rings
- **Used in**: Separated individual pieces

#### Type 3: Edge Orientation (Complementary)
- Forward vs. reverse paths
- Ensures pieces fit together
- Independent of visual rotation
- **Used in**: Both connected and separated

## Implementation Strategy

### Phase 1: Rotation Utilities (1 day)
Build reusable rotation transformation functions:

```r
# R/rotation_utils.R

#' Rotate a 2D point around origin
#' @param x X coordinate
#' @param y Y coordinate
#' @param angle Rotation angle in radians
#' @return List with rotated (x, y)
rotate_point <- function(x, y, angle) {
  list(
    x = cos(angle) * x - sin(angle) * y,
    y = sin(angle) * x + cos(angle) * y
  )
}

#' Rotate bezier control points
#' @param control_points List of (x, y) control points
#' @param angle Rotation angle in radians
#' @param center Center of rotation (default: origin)
#' @return List of rotated control points
rotate_bezier_curve <- function(control_points, angle, center = c(0, 0)) {
  # Translate to origin if needed
  translated <- lapply(control_points, function(pt) {
    c(pt[1] - center[1], pt[2] - center[2])
  })

  # Rotate
  rotated <- lapply(translated, function(pt) {
    rotate_point(pt[1], pt[2], angle)
  })

  # Translate back
  final <- lapply(rotated, function(pt) {
    c(pt$x + center[1], pt$y + center[2])
  })

  return(final)
}

#' Transform SVG path data with rotation
#' @param path_string SVG path string (e.g., "M 0 0 L 10 10")
#' @param angle Rotation angle in radians
#' @return Rotated SVG path string
rotate_svg_path <- function(path_string, angle) {
  # Parse path to control points
  points <- parse_svg_path(path_string)

  # Rotate points
  rotated <- rotate_bezier_curve(points, angle)

  # Rebuild path
  rebuild_svg_path(rotated)
}
```

### Phase 2: Hexagonal Edge Generation (2-3 days)
Adapt rectangular edge generation for hexagonal geometry:

```r
# R/hexagonal_edge_generation.R

#' Generate hexagonal edge segment
#' @param seed Random seed
#' @param edge_id Edge identifier
#' @param edge_type "side" or "border"
#' @param rotation Rotation angle for this piece
#' @param params Edge parameters (tabsize, jitter, etc.)
#' @return List with forward and reverse paths
generate_hex_edge_segment <- function(seed, edge_id, edge_type,
                                      rotation = 0, params) {

  # Generate base edge (horizontal, unrotated)
  base_edge <- generate_base_hex_edge(seed, edge_id, edge_type, params)

  # Apply rotation if needed
  if (abs(rotation) > 1e-6) {
    forward_path <- rotate_svg_path(base_edge$forward, rotation)
    reverse_path <- rotate_svg_path(base_edge$reverse, rotation)
  } else {
    forward_path <- base_edge$forward
    reverse_path <- base_edge$reverse
  }

  return(list(
    forward = forward_path,
    reverse = reverse_path,
    rotation = rotation
  ))
}
```

### Phase 3: Hexagonal Piece Assembly (2 days)
Build pieces from rotated edges:

```r
# R/hexagonal_piece_assembly.R

#' Classify hexagonal piece type
#' @param ring Ring number (0 = center)
#' @param position Position in ring
#' @param total_rings Total rings in puzzle
#' @return Character: "center", "inner", "outer", "edge"
classify_hex_piece <- function(ring, position, total_rings) {
  if (ring == 0) return("center")
  if (ring == total_rings - 1) return("edge")
  return("inner")
}

#' Generate individual hexagonal piece
#' @param piece_id Piece ID (1 to num_pieces)
#' @param rings Total rings in puzzle
#' @param seed Random seed
#' @param params Generation parameters
#' @return List with piece SVG path and metadata
generate_hex_piece <- function(piece_id, rings, seed, params) {
  # Get topology
  ring_info <- map_piece_id_to_ring(piece_id, rings)
  position <- calculate_hex_piece_position(piece_id, rings,
                                           params$base_spacing)

  # Classify piece
  piece_type <- classify_hex_piece(ring_info$ring, ring_info$position, rings)

  # Generate edges (6 edges for hexagonal piece)
  edges <- list()
  for (side in 0:5) {
    edge_id <- calculate_hex_edge_id(piece_id, side, rings)
    edge_type <- if (piece_type == "edge" && is_border_side(side))
                    "border" else "side"

    edges[[side + 1]] <- generate_hex_edge_segment(
      seed = seed,
      edge_id = edge_id,
      edge_type = edge_type,
      rotation = ring_info$angle,
      params = params
    )
  }

  # Assemble piece
  piece_path <- assemble_hex_piece_path(edges, position)

  return(list(
    id = piece_id,
    ring = ring_info$ring,
    position = ring_info$position,
    rotation = ring_info$angle,
    center_x = position$x,
    center_y = position$y,
    path = piece_path,
    type = piece_type
  ))
}
```

### Phase 4: Integration & Testing (1-2 days)
- Test with 2-ring puzzle (7 pieces)
- Test with 3-ring puzzle (19 pieces)
- Verify rotation angles are correct
- Check complementary edges still fit

## Key Differences: Rectangular vs. Hexagonal Rotation

| Aspect | Rectangular Puzzles | Hexagonal Puzzles |
|--------|-------------------|-------------------|
| **Piece Shape** | 4 edges (square/rect) | 6 edges (hexagon) |
| **Edge Rotation** | 90° increments | 60° increments |
| **Grid Structure** | Orthogonal (x, y) | Radial (ring, angle) |
| **Rotation Needed** | No (axis-aligned) | Yes (radial layout) |
| **Center Piece** | N/A or same as others | Special (6-fold symmetry) |
| **Border Pieces** | Straight edge | Rotated envelope |

## Validation Criteria

How to verify rotation is working correctly:

### Visual Tests
1. **Placeholder comparison**: Rotated placeholders match piece orientations
2. **Tab alignment**: Tabs point toward/away from center appropriately
3. **Border alignment**: Edge pieces follow hexagonal envelope
4. **Ring consistency**: Pieces in same ring have evenly distributed rotations

### Programmatic Tests
```r
test_that("hexagonal piece rotation matches topology", {
  piece <- generate_hex_piece(piece_id = 5, rings = 3, seed = 42, params)
  ring_info <- map_piece_id_to_ring(5, 3)

  expect_equal(piece$rotation, ring_info$angle)
})

test_that("rotation angles are evenly distributed in ring", {
  rings <- 3
  ring_1_pieces <- 2:7  # Ring 1 has 6 pieces

  angles <- sapply(ring_1_pieces, function(id) {
    info <- map_piece_id_to_ring(id, rings)
    info$angle
  })

  # Should be 0, π/3, 2π/3, π, 4π/3, 5π/3
  expected <- seq(0, 5) * pi / 3
  expect_equal(angles, expected, tolerance = 1e-6)
})
```

## Connection to Original Challenge

### Why Rotation Matters for Parse-and-Extract
If we went with the parse-and-extract approach, we'd need to:
1. Parse bezier curves from monolithic SVG
2. **Rotate extracted paths** to separate orientation
3. Handle intersection detection between rotated curves
4. Maintain complementary edge relationships through rotation

**Complexity**: High - requires bezier intersection math

### Why Rotation is Easier with Direct Generation
With direct generation approach:
1. Generate edges at correct rotation **from the start**
2. Use existing `hex_rotate()` transformation function
3. Apply rotation to control points before bezier calculation
4. Complementary edges naturally align (same rotation context)

**Complexity**: Low - reuse existing rotation infrastructure

## Timeline Estimate

| Phase | Duration | Deliverable |
|-------|----------|-------------|
| Rotation utilities | 1 day | `rotation_utils.R` |
| Edge generation | 2-3 days | `hexagonal_edge_generation.R` |
| Piece assembly | 2 days | `hexagonal_piece_assembly.R` |
| Integration & testing | 1-2 days | Working 2-ring, 3-ring tests |
| **Total** | **6-8 days** | Full hexagonal individual pieces |

## Next Steps

1. ✅ **Fix placeholder rotation** (DONE)
2. 📝 **Create this planning document** (DONE)
3. ⏭️ **Get user approval** for Hybrid Direct Generation approach
4. 🔨 **Implement rotation utilities** (Phase 1)
5. 🔨 **Build edge generation** (Phase 2)
6. 🔨 **Implement piece assembly** (Phase 3)
7. 🧪 **Test and validate** (Phase 4)

## References

- **Successful Placeholder Fix**: `R/hexagonal_separation.R` (lines 163-212, 234-252)
- **Topology Infrastructure**: `R/hexagonal_topology.R`
- **Existing Rotation**: `R/hexagonal_puzzle.R` (`hex_rotate()` function)
- **Rectangular Pattern**: `R/individual_pieces.R` (reference implementation)
- **Planning Context**: `docs/planning/hexagonal/2025-11-26-revised-plan.md`

## Conclusion

The placeholder rotation fix proves that:
1. Topology infrastructure is robust and ready
2. Rotation information is readily available
3. Direct generation is the natural approach
4. We can reuse existing rotation functions

**Recommendation**: Proceed with Hybrid Direct Generation using rotation infrastructure shown here.
