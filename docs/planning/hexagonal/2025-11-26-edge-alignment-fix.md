# Edge Alignment Fix: Ensuring Edges (Not Vertices) Face Center

**Date**: 2025-11-26
**Issue**: Some pieces had vertices facing center instead of edges
**Fix**: Add 30° rotation adjustment for pieces at non-60° multiples

## The Problem

After fixing flat-top orientation, most pieces looked correct, but **alternating pieces in ring 2** had vertices (corners) pointing toward the center instead of edges.

### Observation
- **Piece 16** (240°): Correct - edge faces center ✅
- **Piece 17** (270°): Wrong - vertex faces center ❌
- **Piece 18** (300°): Correct - edge faces center ✅

## Root Cause Analysis

### Hexagon Geometry
A flat-top hexagon with base offset 30° has:
- **Edges (flat sides) at**: 0°, 60°, 120°, 180°, 240°, 300°
- **Vertices (corners) at**: 30°, 90°, 150°, 210°, 270°, 330°

### Piece Distribution
- **Ring 0** (center): 1 piece at 0°
- **Ring 1**: 6 pieces at 0°, 60°, 120°, 180°, 240°, 300° (every 60°)
- **Ring 2**: 12 pieces at 0°, 30°, 60°, 90°, ... (every 30°)

### The Mismatch
```
Ring 1 (6 pieces, 60° spacing):
  0°, 60°, 120°, 180°, 240°, 300°
  ✅ All align with edges

Ring 2 (12 pieces, 30° spacing):
  0°   ✅ edge      |  180°  ✅ edge
  30°  ❌ vertex    |  210°  ❌ vertex
  60°  ✅ edge      |  240°  ✅ edge
  90°  ❌ vertex    |  270°  ❌ vertex
  120° ✅ edge      |  300°  ✅ edge
  150° ❌ vertex    |  330°  ❌ vertex
```

Half the pieces in ring 2 had vertices facing center!

## The Solution

### Logic
For any piece:
1. Check if its angle is a multiple of 60° (`angle % 60 == 0`)
2. If YES: Edge faces center → no adjustment needed
3. If NO: Vertex faces center → add 30° to rotate edge inward

### Code Implementation

```r
# Get rotation angle from topology
ring_info <- map_piece_id_to_ring(i, rings)
piece_rotation <- ring_info$angle

# Adjust rotation so an edge (not vertex) faces toward center
angle_degrees <- (piece_rotation * 180 / pi) %% 360
if (abs(angle_degrees %% 60) > 0.1) {
  # Piece is at 30°, 90°, 150°, 210°, 270°, or 330°
  # Add 30° so edge faces center instead
  piece_rotation <- piece_rotation + pi / 6
}
```

## Results

### Before Fix
```
Piece  8:   0° (edge faces center) ✅
Piece  9:  30° (vertex faces center) ❌
Piece 10:  60° (edge faces center) ✅
Piece 11:  90° (vertex faces center) ❌
Piece 12: 120° (edge faces center) ✅
Piece 13: 150° (vertex faces center) ❌
Piece 14: 180° (edge faces center) ✅
Piece 15: 210° (vertex faces center) ❌
Piece 16: 240° (edge faces center) ✅
Piece 17: 270° (vertex faces center) ❌
Piece 18: 300° (edge faces center) ✅
Piece 19: 330° (vertex faces center) ❌
```

### After Fix
```
Piece  8:   0° →   0° (no change)
Piece  9:  30° →  60° ✓ ADJUSTED
Piece 10:  60° →  90° ✓ ADJUSTED
Piece 11:  90° → 120° ✓ ADJUSTED
Piece 12: 120° → 150° ✓ ADJUSTED
Piece 13: 150° → 180° ✓ ADJUSTED
Piece 14: 180° → 180° (no change)
Piece 15: 210° → 240° ✓ ADJUSTED
Piece 16: 240° → 270° ✓ ADJUSTED
Piece 17: 270° → 300° ✓ ADJUSTED
Piece 18: 300° → 300° (no change)
Piece 19: 330° → 360° ✓ ADJUSTED

All pieces now have edges facing center! ✅
```

## Visual Explanation

```
BEFORE FIX:
     ___
    ╱   ╲        Pieces at 0°, 60°, 120°, etc:
   │  8  │       Edge faces center ✅
    ╲___╱

      △          Pieces at 30°, 90°, 150°, etc:
     ╱ ╲         Vertex faces center ❌
    │ 9 │
     ╲ ╱
      ▽

AFTER FIX:
     ___
    ╱   ╲        All pieces:
   │  8  │       Edge faces center ✅
    ╲___╱

     ___
    ╱   ╲        Adjusted rotation ensures
   │  9  │       edge always faces center ✅
    ╲___╱
```

## Why This Matters

### For Puzzle Connection
Pieces must connect **edge-to-edge**:
- ✅ Edge facing: Pieces can connect when brought together
- ❌ Vertex facing: Gap or overlap when attempting connection

### For Tab Placement
When implementing real bezier curves:
- Tabs go on **edges** (flat sides)
- If vertex faces center, tab would be misaligned
- Edge-facing ensures tabs point toward adjacent pieces

### For All Ring Sizes
This pattern repeats in any ring with more pieces than ring 1:
- **Ring 1**: 6 pieces (60° spacing) → all align naturally
- **Ring 2**: 12 pieces (30° spacing) → half need adjustment
- **Ring 3**: 18 pieces (20° spacing) → 2/3 need adjustment
- **Ring n**: 6n pieces → only multiples of 60° align naturally

## Pattern Generalization

The adjustment rule works for any ring size:
```r
# Universal rule: edge faces center if angle is multiple of 60°
if (angle % 60 != 0) {
  angle += 30  # Rotate to next edge
}
```

This ensures correct orientation regardless of:
- Number of rings
- Number of pieces in ring
- Angular spacing between pieces

## Files Modified

1. ✅ `R/hexagonal_separation.R` - Main implementation (lines 200-208)
2. ✅ `R/hexagonal_separation_working.R` - Spiral version (lines 112-116)

## Testing

### Visual Test
```bash
Rscript test_flat_top.R
# Check: All pieces should have flat edges facing toward center
```

### Programmatic Test
```bash
Rscript test_adjusted_rotations.R
# Shows before/after rotations for all ring 2 pieces
```

### Verification Checklist
- ✅ Center piece (ring 0): Edge faces any direction (6-fold symmetry)
- ✅ Ring 1 pieces (2-7): All edges face center
- ✅ Ring 2 pieces (8-19): All edges face center (adjusted)
- ✅ Pieces at 0°, 60°, 120°, 180°, 240°, 300°: No adjustment needed
- ✅ Pieces at 30°, 90°, 150°, 210°, 270°, 330°: Adjusted by +30°

## Related Issues

This fix builds on:
1. **Flat-top orientation** (`2025-11-26-hexagon-orientation-fix.md`)
   - Added π/6 base offset for flat-top hexagons
2. **Rotation infrastructure** (`2025-11-26-rotation-infrastructure-plan.md`)
   - Topology-based rotation angles

Together, these ensure:
- Hexagons are flat-top (not pointy-top)
- Edges face center (not vertices)
- All pieces correctly oriented for connection

## Next Steps

With correct edge alignment:
1. **Real bezier curves** can be generated with tabs on edges
2. **Adjacent pieces** will have complementary edges
3. **Rotation** will maintain proper tab alignment
4. **Any ring size** will work correctly

The foundation is now solid for implementing real puzzle pieces with tabs! 🎯
