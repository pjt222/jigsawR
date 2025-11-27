# Hexagonal Puzzles Development Guide

**Last Updated**: 2025-11-27
**Status**: Working implementation with bezier curves and complementary edges

This guide consolidates insights, troubleshooting patterns, and development knowledge from the hexagonal puzzle implementation.

## Table of Contents

1. [Architecture Overview](#architecture-overview)
2. [Key Insights](#key-insights)
3. [The Honeycomb Principle](#the-honeycomb-principle)
4. [Implementation Approaches](#implementation-approaches)
5. [File Structure](#file-structure)
6. [Common Issues and Solutions](#common-issues-and-solutions)
7. [Code Snippets](#code-snippets)
8. [Testing Patterns](#testing-patterns)

---

## Architecture Overview

### Core Components

```
R/hexagonal_puzzle.R          # Original hexagonal puzzle generator
R/hexagonal_topology.R        # Ring-based coordinate utilities
R/hexagonal_neighbors.R       # Neighbor mapping for adjacent pieces
R/hexagonal_bezier_generation.R  # Bezier curve generation
R/hexagonal_edge_generation_fixed.R  # Fixed edge generation with complementarity
R/hexagonal_separation.R      # Separation layout with bezier support
```

### Data Flow

```
User Input (rings, seed, diameter)
    ↓
init_hex_jigsaw()           # Initialize environment
    ↓
Topology Calculation        # map_piece_id_to_ring()
    ↓
Edge Pre-generation        # generate_hex_edge_map()
    ↓
Piece Assembly             # generate_hex_pieces_with_edge_map()
    ↓
SVG Output                 # generate_separated_hexagonal_svg()
```

---

## Key Insights

### 1. Paths Intersect, Not Connect

**Discovery**: Hexagonal paths are continuous curves that INTERSECT at interior points, not endpoints.

```
WRONG assumption:
  Horizontal path ends at (120, 76.7)
  Vertical path starts at (120, 76.7)
  → They "connect"

REALITY:
  Horizontal path: M 45 76.7 C ... 120 76.7 C ... 195 76.7 (continuous)
  Point (120, 76.7) is INSIDE the path, not an endpoint
  → They INTERSECT, requiring bezier-bezier intersection math
```

**Impact**: Parse-and-extract approach requires bezier intersection algorithms (7-10 days). Hybrid direct generation approach is simpler (5-7 days).

### 2. Ring-Based Topology

Hexagonal puzzles have a natural ring structure:
- **Ring 0**: 1 piece (center)
- **Ring 1**: 6 pieces
- **Ring 2**: 12 pieces
- **Ring n**: 6n pieces
- **Total for n rings**: 3n(n-1) + 1 pieces

```r
# Map piece ID to ring structure
ring_info <- map_piece_id_to_ring(piece_id, rings)
# Returns: $ring, $position_in_ring, $angle, $direction
```

### 3. Piece Numbering

Pieces are numbered 1 to N, following the hexagonal grid iteration pattern:

```r
# The iteration order from hex_gen_dh()
n <- rings
yl <- 2 * n - 1
piece_id <- 1
for (yi in seq(-yl + 2, yl - 2, by = 2)) {
  xl <- 2 * n - 1 - (abs(yi) - 1) / 2
  for (xi in seq(-xl + 1, xl - 2, by = 1)) {
    # piece_id corresponds to this (xi, yi) position
    piece_id <- piece_id + 1
  }
}
```

---

## The Honeycomb Principle

### Critical Realization

**Hexagonal puzzles are honeycomb grids, NOT pinwheels.**

```
WRONG (Pinwheel):          CORRECT (Honeycomb):
      ___                     ___     ___     ___
     /   \                   /   \   /   \   /   \
  __/     \__               /     \_/     \_/     \
 /  \     /  \              \     / \     / \     /
/    \___/    \              \___/   \___/   \___/
\    /   \    /              /   \   /   \   /   \
 \__/     \__/              /     \_/     \_/     \

Each piece rotated         All pieces SAME orientation
based on position          Like floor tiles/bee's web
```

### Implications

1. **All hexagons have the same orientation** (rotation = 0)
2. **Only positions vary, not rotations**
3. **Edges naturally align** between adjacent pieces
4. **Tab placement is consistent** across all pieces

```r
# CORRECT: All pieces same orientation
piece_rotation <- 0  # Honeycomb structure

# WRONG: Position-based rotation
piece_rotation <- ring_info$angle  # Creates pinwheel
```

---

## Implementation Approaches

### Approach 1: Parse and Extract (NOT RECOMMENDED)

Parse complete SVG paths, find intersections, trace piece boundaries.

**Problems**:
- Requires bezier-bezier intersection algorithms
- Floating point precision issues
- Complex path splitting at interior points
- 7-10 days implementation time

### Approach 2: Hybrid Direct Generation (RECOMMENDED)

Modify generation to optionally return structured edge data.

**Benefits**:
- Follows proven rectangular puzzle pattern
- Clean, deterministic
- No intersection math needed
- 5-7 days implementation time
- Backward compatible

```r
# Hybrid approach pattern
hex_gen_dh <- function(return_edges = FALSE) {
  if (return_edges) {
    # Return structured edge data (NEW)
    edges <- list()
    for (yi in ...) {
      for (xi in ...) {
        edges[[length(edges) + 1]] <- hex_hlineseg_data(xi, yi)
      }
    }
    return(edges)
  } else {
    # Existing behavior (BACKWARD COMPATIBLE)
    str <- ""
    # ... concatenate paths
    return(str)
  }
}
```

---

## File Structure

### Core Files (Tracked)

| File | Purpose |
|------|---------|
| `R/hexagonal_puzzle.R` | Original puzzle generator (complete puzzles) |
| `R/hexagonal_topology.R` | Ring-based coordinate utilities |
| `R/hexagonal_neighbors.R` | Neighbor mapping |
| `R/hexagonal_bezier_generation.R` | Bezier curve generation |
| `R/hexagonal_edge_generation_fixed.R` | Fixed edge generation |
| `R/hexagonal_separation.R` | Separation layout |
| `R/hexagonal_piece_extraction.R` | Extraction utilities (partial) |

### Archive Locations

```
R/scripts_archive/development/hexagonal_2025-11-26/
  ├── test_*.R           # Test scripts
  ├── hexagonal_*.R      # Experimental implementations
  └── rotation_utils.R   # Rotation utilities

R/scripts_archive/development/hexagonal_work/
  └── [Previous archived work]
```

---

## Common Issues and Solutions

### Issue 1: Pieces Don't Align (Pinwheel Effect)

**Symptom**: Adjacent pieces have different rotations, edges don't match.

**Solution**: Set `piece_rotation = 0` for all pieces (honeycomb principle).

```r
# In hexagonal_separation.R
piece_rotation <- 0  # All pieces same orientation
```

### Issue 2: Edges Not Complementary

**Symptom**: Adjacent pieces don't have matching tabs/blanks.

**Solution**: Use pre-generated edge map with forward/reverse paths.

```r
# Pre-generate edges
edge_data <- generate_hex_edge_map(rings, seed, diameter, tabsize, jitter)

# For piece A's edge: use forward path
# For piece B's shared edge: use reverse path (complementary)
```

### Issue 3: Path Connectivity Testing Fails

**Symptom**: `build_segment_graph()` returns 0 connections.

**Cause**: Hexagonal paths are continuous curves that intersect at interior points, not endpoints.

**Solution**: Use direct generation approach instead of parse-and-extract.

### Issue 4: Wrong Hexagon Orientation

**Symptom**: Hexagons appear "pointy-top" instead of "flat-top".

**Solution**: Apply base offset of π/6 (30°) for flat-top orientation.

```r
# In create_hex_placeholder()
base_angle_offset <- pi / 6  # 30° for flat-top
```

### Issue 5: Position Calculation Wrong

**Symptom**: Pieces overlap or are positioned incorrectly.

**Solution**: Use topology utilities with proper spacing calculation.

```r
base_spacing <- piece_radius * 2
separation_factor <- 1.0 + (offset / base_spacing)

pos <- calculate_hex_piece_position(piece_id, rings, diameter,
                                    base_spacing * separation_factor)
```

---

## Code Snippets

### Calculate Piece Count

```r
# For n rings: 3n(n-1) + 1 pieces
num_pieces <- 3 * rings * (rings - 1) + 1
# rings=2: 7 pieces
# rings=3: 19 pieces
# rings=4: 37 pieces
```

### Map Piece ID to Ring

```r
map_piece_id_to_ring <- function(piece_id, rings) {
  if (piece_id == 1) {
    return(list(ring = 0, position_in_ring = 0, angle = 0, direction = 0))
  }

  remaining <- piece_id - 1
  ring <- 1
  while (remaining > 6 * ring) {
    remaining <- remaining - 6 * ring
    ring <- ring + 1
  }

  position_in_ring <- remaining - 1
  direction <- floor(position_in_ring / ring)
  position_in_direction <- position_in_ring %% ring

  angle <- (direction * 60 + position_in_direction * 60 / ring) * pi / 180

  list(ring = ring, position_in_ring = position_in_ring,
       angle = angle, direction = direction)
}
```

### Create Hexagon Path

```r
create_hex_placeholder <- function(center_x, center_y, radius, rotation = 0) {
  base_angle_offset <- pi / 6  # Flat-top orientation

  vertices <- vapply(0:5, function(i) {
    angle <- base_angle_offset + rotation + i * pi / 3
    c(center_x + radius * cos(angle),
      center_y + radius * sin(angle))
  }, numeric(2))

  sprintf("M %.2f %.2f L %.2f %.2f L %.2f %.2f L %.2f %.2f L %.2f %.2f L %.2f %.2f Z",
          vertices[1,1], vertices[2,1],
          vertices[1,2], vertices[2,2],
          vertices[1,3], vertices[2,3],
          vertices[1,4], vertices[2,4],
          vertices[1,5], vertices[2,5],
          vertices[1,6], vertices[2,6])
}
```

### Debug Connectivity

```r
# Test if paths connect at endpoints
cat("Testing connectivity with different tolerances:\n")
for (tol in c(0.1, 0.5, 1.0, 2.0)) {
  graph <- build_segment_graph(all_segs, tolerance = tol)
  connections <- sapply(graph, function(n) length(n$connections))
  cat(sprintf("  Tolerance %.1f: min=%d, max=%d, mean=%.1f\n",
              tol, min(connections), max(connections), mean(connections)))
}
```

---

## Testing Patterns

### Visual Verification

```r
# Generate test SVG
svg <- generate_separated_hexagonal_svg(
  rings = 2,
  seed = 42,
  diameter = 240,
  offset = 10,
  arrangement = "hexagonal",
  use_bezier = TRUE
)

# Save and inspect
writeLines(svg, "output/test_hexagonal.svg")
```

### Edge Complementarity Test

```r
edge_data <- generate_hex_edge_map(rings = 2, seed = 42, diameter = 240)

# Test adjacent pieces
test_pairs <- list(
  list(p1 = 1, s1 = 0, p2 = 2, s2 = 3),  # Center to ring 1
  list(p1 = 2, s1 = 2, p2 = 7, s2 = 4)   # Ring 1 adjacent
)

for (pair in test_pairs) {
  edge1 <- edge_data$piece_edge_map[[sprintf("%d-%d", pair$p1, pair$s1)]]
  edge2 <- edge_data$piece_edge_map[[sprintf("%d-%d", pair$p2, pair$s2)]]

  is_complementary <- edge1$forward == edge2$reverse &&
                      edge1$edge_key == edge2$edge_key
  cat(sprintf("Pieces %d-%d <-> %d-%d: %s\n",
              pair$p1, pair$s1, pair$p2, pair$s2,
              if(is_complementary) "COMPLEMENTARY" else "NOT COMPLEMENTARY"))
}
```

### Topology Verification

```r
# Verify piece positions
for (i in 1:7) {  # 2-ring puzzle
  ring_info <- map_piece_id_to_ring(i, rings = 2)
  pos <- calculate_hex_piece_position(i, rings = 2, diameter = 240)
  cat(sprintf("Piece %d: ring=%d, pos_in_ring=%d, center=(%.1f, %.1f)\n",
              i, ring_info$ring, ring_info$position_in_ring,
              pos$x, pos$y))
}
```

---

## References

### Original Implementation
- JavaScript source: https://github.com/Draradech/jigsaw
- Rectangular R implementation: `R/puzzle_core_clean.R`

### Related GitHub Issues
- #6: Piece boundary extraction
- #7: Positioning and transformation (CLOSED)
- #8: SVG generation and file I/O
- #9: Testing and validation
- #10: Complete feature epic
- #27: Implementation approach decision (CLOSED)

### Archive Locations
- Planning documents: `docs/planning/hexagonal/`
- Experimental code: `R/scripts_archive/development/hexagonal_2025-11-26/`
- Temp analysis: Archived (extracted to this guide)

---

*This guide is a living document. Update as new insights are discovered.*
