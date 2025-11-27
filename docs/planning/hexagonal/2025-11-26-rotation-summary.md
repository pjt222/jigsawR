# Hexagonal Puzzle Rotation: Summary and Infrastructure
**Date**: 2025-11-26
**Purpose**: Quick reference for rotation concepts and implementation

## What is Rotation in Hexagonal Puzzles?

### Visual Concept
```
Without Rotation (Wrong):        With Rotation (Correct):
All pieces point UP              Pieces point OUTWARD

    △                                 △
   ╱ ╲                               ╱ ╲
  ╱   ╲          Ring 1            ╱   ╲
 △  △  △         Center            ◁  ●  ▷
  ╲   ╱                             ╲   ╱
   ╲ ╱                               ╲ ╱
    △                                 ▽

All same angle = boring         Natural radial layout
```

### Mathematical Concept
**Rotation** = Angular position of piece in polar coordinates
- Center piece (ID 1): angle = 0°
- Ring 1, piece 2: angle = 0° (→ right)
- Ring 1, piece 3: angle = 60° (↗ upper-right)
- Ring 1, piece 4: angle = 120° (↖ upper-left)
- Ring 1, piece 5: angle = 180° (← left)
- Ring 1, piece 6: angle = 240° (↙ lower-left)
- Ring 1, piece 7: angle = 300° (↘ lower-right)

## Three Types of Rotation

### 1. Border Rotation (Original Generator)
**Purpose**: Create hexagonal boundary
**Applies to**: Outermost ring only
**Mechanism**: Generate puzzle 6 times, rotate by 60° each time
**Code**: `hex_rotate()` in `hexagonal_puzzle.R`

```r
# From hexagonal_puzzle.R (lines 268-273)
for (rot in 0:5) {
  gen_hexagon(seed = seed, rot = rot * pi / 3)
}
```

**Used in**: Connected puzzle generation (original algorithm)

### 2. Piece Radial Rotation (New - For Separation)
**Purpose**: Orient pieces outward from center
**Applies to**: All pieces (except center)
**Mechanism**: Calculate angle from piece position in ring
**Code**: `map_piece_id_to_ring()` provides `angle` field

```r
# From hexagonal_topology.R
ring_info <- map_piece_id_to_ring(piece_id, rings)
rotation <- ring_info$angle  # Radians, 0 to 2π
```

**Used in**: Separated individual pieces (placeholder fix)

### 3. Edge Orientation (Complementary Paths)
**Purpose**: Ensure pieces fit together
**Applies to**: All shared edges
**Mechanism**: Forward vs. reverse bezier paths
**Code**: Same as rectangular puzzles

```r
# Conceptual (not yet implemented for hexagonal)
edge <- generate_edge_segment(seed, edge_id)
# Returns: list(forward = "M...", reverse = "M...")
```

**Used in**: Both connected and separated (for fitting)

## Current Implementation Status

### ✅ Completed: Placeholder Rotation
**File**: `R/hexagonal_separation.R`
**Lines**: 163-212 (generation loop), 234-252 (rotation function)

```r
# Get rotation from topology
ring_info <- map_piece_id_to_ring(i, rings)
piece_rotation <- ring_info$angle

# Apply to placeholder hexagon
hex_path <- create_hex_placeholder(cx, cy, radius, piece_rotation)

# In create_hex_placeholder:
angle <- i * pi / 3 + rotation  # Rotate vertices
```

**Result**: Hexagons now point radially outward ✓

### 🔨 In Planning: Real Piece Rotation
**Approach**: Hybrid Direct Generation (recommended)
**Key insight**: Reuse topology angle for edge generation

```r
# Planned implementation
generate_hex_piece <- function(piece_id, rings, seed, params) {
  ring_info <- map_piece_id_to_ring(piece_id, rings)

  # Generate edges with rotation context
  edges <- generate_hex_piece_edges(
    rotation = ring_info$angle,  # ← Same angle from topology
    ...
  )
}
```

## Infrastructure Already Available

### From `hexagonal_topology.R`:
```r
map_piece_id_to_ring(piece_id, rings)
# Returns: list(
#   ring = 0..rings-1,
#   position = 0..6*ring-1,
#   angle = 0..2π  ← THIS IS THE ROTATION
# )

hex_ring_to_cartesian(ring, angle, ring_spacing)
# Converts: (ring, angle) → (x, y)
# Already handles polar-to-cartesian conversion

calculate_hex_piece_position(piece_id, rings, base_spacing)
# Complete pipeline: piece_id → (x, y) position
```

### From `hexagonal_puzzle.R`:
```r
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
# Standard 2D rotation matrix
# Can be reused for individual piece generation!
```

## How Rotation Helps Real Piece Implementation

### Problem Without Rotation Understanding
❌ All pieces would be generated in same orientation
❌ Separated layout would look artificial
❌ Pieces wouldn't reflect their spatial relationships
❌ Hard to visualize how pieces fit together

### Solution With Rotation Infrastructure
✅ Each piece generated at correct orientation
✅ Natural radial layout in separation
✅ Visual correspondence to position in puzzle
✅ Foundation ready for direct generation approach

## Code Examples

### Example 1: Get Rotation for Any Piece
```r
# Load topology utilities
source("R/hexagonal_topology.R")

# Get rotation for piece 5 in 3-ring puzzle
ring_info <- map_piece_id_to_ring(5, rings = 3)
cat(sprintf("Piece 5: ring=%d, position=%d, angle=%.2f° (%.3f rad)\n",
            ring_info$ring,
            ring_info$position,
            ring_info$angle * 180 / pi,
            ring_info$angle))

# Output: Piece 5: ring=1, position=3, angle=180.00° (3.142 rad)
```

### Example 2: Rotate a Point
```r
# Using hex_rotate from hexagonal_puzzle.R
rotate_fn <- hex_rotate(pi / 3)  # 60° rotation
point <- c(10, 0)                 # Point at (10, 0)
rotated <- rotate_fn(point)       # Apply rotation
# Result: (5, 8.66) - rotated 60° counterclockwise
```

### Example 3: Apply to Bezier Control Points
```r
# Conceptual (to be implemented)
rotate_bezier_curve <- function(control_points, angle) {
  rotate_fn <- hex_rotate(angle)
  lapply(control_points, rotate_fn)
}

# Usage in edge generation
base_edge_controls <- list(c(0,0), c(5,2), c(10,0))
rotated_edge <- rotate_bezier_curve(base_edge_controls, pi/3)
```

## Key Insights from Placeholder Fix

### 1. Topology is Complete
The topology utilities provide ALL needed information:
- Ring structure ✓
- Angular position ✓
- Rotation angle ✓
- Cartesian position ✓

### 2. Rotation is Simple
Just one parameter: `angle` from topology
- No complex calculations needed
- Already in correct units (radians)
- Ready to use in generation

### 3. Infrastructure is Reusable
Functions built for placeholders → work for real pieces
- `map_piece_id_to_ring()` → provides rotation
- `hex_rotate()` → applies rotation
- Same pattern for both

### 4. Direct Generation is Natural Fit
Since we're already using topology for position...
- → Use same topology for rotation
- → Generate edges at correct angle from start
- → No need to parse/extract/rotate separately

## Comparison: Parse-and-Extract vs. Direct Generation

### Parse-and-Extract with Rotation
```r
# 1. Generate monolithic puzzle
puzzle <- generate_hex_jigsaw_svg(seed, rings, ...)

# 2. Parse SVG to find pieces
pieces <- parse_svg_paths(puzzle$svg)

# 3. Extract piece boundaries
piece_5_path <- extract_piece_boundary(pieces, id = 5)

# 4. Rotate extracted piece ⚠️ COMPLEX
ring_info <- map_piece_id_to_ring(5, rings)
rotated_path <- rotate_svg_path(piece_5_path, ring_info$angle)
# ^ Requires bezier intersection math if paths overlap

# 5. Translate to position
final_path <- translate_svg_path(rotated_path, target_x, target_y)
```

**Complexity**: Steps 3-4 involve parsing bezier intersections

### Direct Generation with Rotation
```r
# 1. Get topology info (includes rotation)
ring_info <- map_piece_id_to_ring(5, rings)
position <- calculate_hex_piece_position(5, rings, spacing)

# 2. Generate piece with rotation baked in
piece <- generate_hex_piece(
  piece_id = 5,
  rings = rings,
  rotation = ring_info$angle,  # ← Rotation from start
  position = position
)

# Done! Piece is already at correct rotation and position
```

**Complexity**: No parsing or intersection math needed

## Testing Rotation

### Visual Test
```r
# Generate separated layout
svg <- generate_separated_hexagonal_svg(
  rings = 3,
  arrangement = "hexagonal"
)

# Check in SVG viewer:
# - Piece 1 (center): points up (0°)
# - Piece 2: points right (0°)
# - Piece 3: points upper-right (60°)
# - Piece 5: points left (180°)
```

### Programmatic Test
```r
test_that("rotation matches topology", {
  # For each piece in ring 1
  for (id in 2:7) {
    ring_info <- map_piece_id_to_ring(id, 3)
    expected_angle <- (id - 2) * pi / 3  # 0, 60, 120, 180, 240, 300

    expect_equal(ring_info$angle, expected_angle, tolerance = 1e-6)
  }
})
```

## Files Reference

### Core Topology (Rotation Source)
- `R/hexagonal_topology.R` - Map piece to ring/angle

### Rotation Implementation
- `R/hexagonal_puzzle.R` - `hex_rotate()` function
- `R/hexagonal_separation.R` - Placeholder rotation (lines 196-198, 238)

### Planning Documents
- `docs/planning/hexagonal/2025-11-26-rotation-infrastructure-plan.md` - Detailed plan
- `docs/planning/hexagonal/2025-11-26-status-update.md` - Executive summary
- `docs/planning/hexagonal/2025-11-26-revised-plan.md` - Implementation options

## Next Actions

1. ✅ **Understand rotation** (this document)
2. ⏭️ **Get user approval** for direct generation approach
3. 🔨 **Implement `rotation_utils.R`** (reusable functions)
4. 🔨 **Add rotation to edge generation**
5. 🔨 **Test with real hexagonal pieces**

## Quick Lookup

**Q: How do I get rotation angle for a piece?**
```r
ring_info <- map_piece_id_to_ring(piece_id, rings)
angle <- ring_info$angle  # In radians
```

**Q: How do I rotate a point?**
```r
rotate_fn <- hex_rotate(angle)
rotated <- rotate_fn(c(x, y))
```

**Q: What's the rotation of the center piece?**
```r
# Center piece (ID 1) has rotation = 0
# It has 6-fold symmetry, so rotation doesn't matter visually
```

**Q: Are rotations the same in each ring?**
```r
# No! Each piece has unique angle based on position
# Ring 1: 6 pieces, angles = 0°, 60°, 120°, 180°, 240°, 300°
# Ring 2: 12 pieces, angles = 0°, 30°, 60°, 90°, ...
```

## Conclusion

Rotation is:
- ✅ Already calculated (topology utilities)
- ✅ Easy to apply (reuse `hex_rotate()`)
- ✅ Essential for natural layout
- ✅ Proven working (placeholder fix)
- ✅ Ready for real pieces (direct generation)

The placeholder rotation fix validates that we have all the infrastructure needed for the Hybrid Direct Generation approach.
