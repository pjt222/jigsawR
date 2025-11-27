# Hexagonal Bezier Curve Implementation Plan
**Date**: 2025-11-26
**Status**: Ready to implement
**Approach**: Hybrid Direct Generation with honeycomb structure

## Key Insight: Honeycomb Simplification

With the honeycomb insight, implementation is **much simpler** than originally thought:

- ✅ All pieces have same orientation (no per-piece rotation)
- ✅ Can reuse hexagonal puzzle's edge generation (`hex_gentab`)
- ✅ Edges naturally complementary (shared between adjacent pieces)
- ✅ No complex intersection math needed

## Architecture Overview

```
Input: piece_id, rings, seed
         ↓
   [Topology] → ring, position, (x, y)
         ↓
   [Edge IDs] → Identify 6 edges for this piece
         ↓
   [Generate] → Use hex_gentab for each edge
         ↓
   [Assemble] → Build piece path from 6 edges
         ↓
   Output: SVG path with bezier curves + tabs
```

## Implementation Strategy

### Option A: Direct Generation (Recommended)
**Reuse existing hex_gentab logic directly**

Advantages:
- ✅ Proven algorithm (already works in connected puzzle)
- ✅ Deterministic (same seed = same edges)
- ✅ Complementary edges automatic
- ✅ Simpler than parse-and-extract

Disadvantages:
- ⚠️ Need to understand hexagonal grid coordinates
- ⚠️ Need to map piece_id to grid coordinates

### Option B: Hybrid (Fallback)
**Generate connected puzzle, then extract with better understanding**

Advantages:
- ✅ Can verify against known-working puzzle
- ✅ Easier to debug (compare with connected)

Disadvantages:
- ❌ More complex than needed now
- ❌ Honeycomb insight makes this unnecessary

## Chosen Approach: Direct Generation

With honeycomb structure, we can generate edges directly using the same
logic as the connected puzzle, just applied to individual pieces.

## Implementation Steps

### Phase 1: Understand Hexagonal Grid Coordinates (1 day)

**Goal**: Map piece_id to hexagonal grid (xi, yi) coordinates

The hexagonal puzzle uses a coordinate system where:
- Grid iterates over `yi` (rows) and `xi` (columns)
- Edges are generated based on these coordinates
- Need to reverse-engineer this mapping from piece_id

**Tasks**:
1. Study how `hex_gen_dh()` and `hex_gen_dv()` iterate
2. Create mapping: `piece_id → (xi, yi, edge_type)`
3. Understand how hexagonal grid indices work
4. Test mapping for all pieces in a 3-ring puzzle

**Deliverable**: `map_piece_to_hex_grid()` function

### Phase 2: Edge Generation Infrastructure (2 days)

**Goal**: Generate individual edges using hex_gentab logic

**Tasks**:
1. Extract edge generation to reusable function:
   ```r
   generate_hex_edge_bezier <- function(v1, v2, seed, edge_id) {
     # Initialize environment with seed + edge_id
     # Call hex_gentab(v1, v2, isnew=TRUE)
     # Return forward and reverse paths
   }
   ```

2. Create edge identification system:
   ```r
   identify_piece_edges <- function(piece_id, rings) {
     # Returns 6 edge IDs (or edge generation parameters)
     # Includes info about which edges are:
     #   - Shared with adjacent pieces (use existing edge)
     #   - Border edges (straight or special)
   }
   ```

3. Handle edge types:
   - **Internal edges**: Use `hex_hlineseg` or `hex_vlineseg`
   - **Border edges**: Use `hex_blineseg` or straight lines
   - **Shared edges**: Forward for one piece, reverse for neighbor

**Deliverable**: `R/hexagonal_bezier_edges.R`

### Phase 3: Piece Assembly (1 day)

**Goal**: Combine 6 edges into complete piece

**Tasks**:
1. Create piece assembly function:
   ```r
   generate_hex_piece_with_bezier <- function(piece_id, rings, seed, params) {
     # Get grid coordinates
     grid_coords <- map_piece_to_hex_grid(piece_id, rings)

     # Identify 6 edges
     edges <- identify_piece_edges(piece_id, rings)

     # Generate bezier paths for each edge
     edge_paths <- lapply(edges, generate_hex_edge_bezier)

     # Assemble into piece
     piece_path <- assemble_hex_piece_bezier(edge_paths)

     return(piece_path)
   }
   ```

2. Handle edge continuity (ensuring smooth path)
3. Add piece positioning (for separated layout)

**Deliverable**: Complete piece generation with real bezier curves

### Phase 4: Testing & Validation (1 day)

**Goal**: Verify pieces are correct

**Tasks**:
1. Visual tests:
   - Generate 2-ring puzzle (7 pieces)
   - Generate 3-ring puzzle (19 pieces)
   - Verify tabs are visible
   - Check edge complementarity

2. Comparison tests:
   - Generate connected puzzle with original function
   - Generate individual pieces with new function
   - Compare edges visually (should match)

3. Edge cases:
   - Center piece (6 edges, all shared)
   - Border pieces (some straight edges)
   - Adjacent piece pairs (edges should be complementary)

**Deliverable**: Working hexagonal individual pieces with tabs! 🎉

## Key Technical Challenges

### Challenge 1: Grid Coordinate Mapping

**Problem**: Need to map piece_id → (xi, yi) for hex grid

**Solution approach**:
```r
# Study the iteration pattern from hex_gen_dh:
# for yi in seq(-yl + 2, yl - 2, by = 2)
#   for xi in seq(-xl + 1, xl - 2, by = 1)

# Build reverse mapping:
map_piece_to_hex_grid <- function(piece_id, rings) {
  n <- rings
  yl <- 2 * n - 1

  current_id <- 1
  for (yi in seq(-yl + 2, yl - 2, by = 2)) {
    xl <- 2 * n - 1 - (abs(yi) - 1) / 2
    for (xi in seq(-xl + 1, xl - 2, by = 1)) {
      if (current_id == piece_id) {
        return(list(xi = xi, yi = yi))
      }
      current_id <- current_id + 1
    }
  }
}
```

### Challenge 2: Edge Identification

**Problem**: Which edges does each piece have?

**Solution approach**:
- Each hexagon has 6 edges (numbered 0-5)
- Determine for each edge:
  - Is it shared with another piece? (get shared edge ID)
  - Is it a border? (different generation)
  - Which direction? (forward or reverse)

```r
# For piece at (xi, yi), edges are:
# 0: right edge (shared with piece at xi+1, yi)
# 1: upper-right edge
# 2: upper-left edge
# 3: left edge (shared with piece at xi-1, yi)
# 4: lower-left edge
# 5: lower-right edge
```

### Challenge 3: Bezier Curve Reuse

**Problem**: How to reuse existing hex_gentab function?

**Solution approach**:
1. Initialize hex environment with seed
2. Set tab parameters (b, c, d, e, t from environment)
3. Call hex_gentab with appropriate vertices
4. Extract SVG path string
5. Parse into forward/reverse paths if needed

**Alternative**: Extract bezier math into pure function

## Code Structure

```
R/
├── hexagonal_bezier_edges.R       # NEW: Edge generation
│   ├── map_piece_to_hex_grid()
│   ├── identify_piece_edges()
│   ├── generate_hex_edge_bezier()
│   └── hex_gentab_wrapper()
│
├── hexagonal_piece_assembly.R     # NEW: Piece assembly
│   ├── generate_hex_piece_with_bezier()
│   ├── assemble_hex_piece_bezier()
│   └── apply_piece_position()
│
├── hexagonal_puzzle.R             # EXISTING: Keep as-is
│   └── (Connected puzzle generation)
│
├── hexagonal_separation.R         # UPDATE: Add bezier option
│   └── generate_separated_hexagonal_svg(use_bezier = FALSE)
│
└── hexagonal_topology.R           # EXISTING: Already good
    └── (Topology utilities)
```

## User-Facing Options

### Shiny App / API
```r
# Option 1: Placeholder hexagons (current)
generate_separated_hexagonal_svg(
  rings = 3,
  seed = 42,
  use_bezier = FALSE  # Simple hexagons
)

# Option 2: Real bezier curves with tabs (new)
generate_separated_hexagonal_svg(
  rings = 3,
  seed = 42,
  use_bezier = TRUE   # Full puzzle pieces
)
```

### Rationale for Keeping Both
- **Placeholder mode**: Fast preview, debugging, understanding layout
- **Bezier mode**: Real puzzle pieces for printing/cutting
- **Testing**: Can compare both modes visually
- **Performance**: Placeholder is faster for large puzzles

## Timeline Estimate

| Phase | Duration | Deliverable |
|-------|----------|-------------|
| Grid coordinate mapping | 1 day | `map_piece_to_hex_grid()` |
| Edge generation | 2 days | `generate_hex_edge_bezier()` |
| Piece assembly | 1 day | Complete pieces with tabs |
| Testing & validation | 1 day | Verified working implementation |
| **Total** | **5 days** | **Hexagonal individual pieces!** |

## Success Criteria

- ✅ Generate individual hexagonal pieces with bezier curves
- ✅ Pieces have visible tabs (not just straight edges)
- ✅ Adjacent pieces have complementary edges
- ✅ Deterministic (same seed = same puzzle)
- ✅ Works for 2-ring, 3-ring, larger puzzles
- ✅ Option to use placeholder or bezier mode
- ✅ Visual verification: pieces look like real jigsaw pieces

## Risks & Mitigation

### Risk 1: Grid coordinate mapping complexity
**Mitigation**: Start with 2-ring puzzle (simple), verify manually

### Risk 2: Edge identification errors
**Mitigation**: Extensive logging, visual comparison with connected puzzle

### Risk 3: Bezier curve integration issues
**Mitigation**: Test edge generation in isolation first

### Risk 4: Performance concerns
**Mitigation**: Keep placeholder option, benchmark both modes

## Next Action

**Ready to start Phase 1**: Map piece_id to hexagonal grid coordinates

Shall I proceed with implementing the grid coordinate mapping?
