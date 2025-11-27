# Hexagonal Individual Pieces - Revised Implementation Plan

**Date**: 2025-11-26
**Status**: Revised after Phase 1 findings
**Original Roadmap**: HEXAGONAL_IMPLEMENTATION_ROADMAP.md (Approach 1)
**Findings**: HEXAGONAL_PHASE1_FINDINGS.md

## Executive Summary

After beginning Phase 1 (Parse and Extract), critical testing revealed that **hexagonal paths don't connect at endpoints - they intersect**. This makes Approach 1 significantly more complex than anticipated (7-10 days instead of 3 days).

**RECOMMENDATION**: Switch to **Hybrid Approach** - modifying hexagonal generation to optionally return individual edges, following the successful rectangular puzzle pattern.

## Why the Change?

### Original Plan (Approach 1)
- Parse complete SVG paths
- Build connectivity graph from endpoints
- Trace piece boundaries through graph
- **Estimated**: 3 days

### Reality Discovered
- Paths are continuous curves that intersect
- Must calculate bezier-bezier intersections
- Must split paths at interior points
- Much more complex math and precision issues
- **Actual Estimate**: 7-10 days, high bug risk

### Rectangular Puzzle Success
- Generates edges individually by grid position
- No parsing or extraction needed
- Clean, deterministic, maintainable
- **Proven pattern that works**

## Revised Approach: Hybrid Direct Generation

### Strategy
Modify hexagonal generation to **optionally return individual edge data** instead of concatenated strings.

### Benefits
✅ Backward compatible - existing complete puzzle generation unchanged
✅ Follows proven rectangular pattern
✅ Lower complexity and risk
✅ More maintainable long-term
✅ Guaranteed complementary edges
✅ No floating point intersection issues

### Implementation Overview

```r
# Step 1: Add edge data structure
hex_hlineseg_data <- function(xi, yi) {
  # Return edge information
  list(
    type = "horizontal",
    coords = c(xi, yi),
    start_point = c(x1, y1),
    end_point = c(x2, y2),
    path_forward = "M x1 y1 C ... x2 y2",
    path_reverse = "M x2 y2 C ... x1 y1",  # For adjacent piece
    has_tab = .hex_jigsaw_env$dir,
    grid_position = c(xi, yi)
  )
}

# Step 2: Modify generation functions
hex_gen_dh <- function(return_edges = FALSE) {
  if (return_edges) {
    edges <- list()
    for (yi in ...) {
      for (xi in ...) {
        edges[[length(edges) + 1]] <- hex_hlineseg_data(xi, yi)
      }
    }
    return(edges)
  } else {
    # Existing behavior for complete puzzles
    str <- ""
    for (yi in ...) {
      for (xi in ...) {
        str <- paste0(str, hex_hlineseg(xi, yi, isnew))
      }
    }
    return(str)
  }
}

# Step 3: Build pieces from edges
build_hexagonal_piece <- function(piece_id, edges_h, edges_v, border_edges) {
  # Identify which edges bound this piece
  piece_edges <- find_piece_edges(piece_id, edges_h, edges_v, border_edges)

  # Assemble into closed path
  piece_path <- assemble_hexagonal_piece_path(piece_edges)

  return(piece_path)
}
```

## Revised Timeline

### Week 1: Direct Edge Generation (3-4 days)

**Day 1: Understand Hexagonal Coordinate System**
- Study hex_hlineseg() and hex_vlineseg() implementations
- Map hexagonal grid coordinates to piece boundaries
- Identify which edges bound which pieces
- Create coordinate system documentation

**Day 2: Implement Edge Data Functions**
- Create `hex_hlineseg_data()` - returns edge info instead of string
- Create `hex_vlineseg_data()` - returns edge info instead of string
- Create `hex_border_data()` - returns border edge info
- Test with 2-ring puzzle (should get 6 horizontal + 12 vertical + 6 border edges)

**Day 3: Modify Generation Functions**
- Add `return_edges` parameter to hex_gen_dh(), hex_gen_dv(), hex_gen_db()
- Implement conditional logic (string vs. edge list)
- Test backward compatibility (complete puzzle generation still works)
- Test edge generation (get structured edge data)

**Day 4: Map Pieces to Edges**
- Implement `identify_piece_topology()` - which edges bound which pieces
- For 2-ring: center piece (6 edges) + 6 ring pieces (4-5 edges each)
- Create piece-to-edge mapping structure
- Verify with visual diagram

### Week 2: Piece Assembly and Integration (2-3 days)

**Day 5: Piece Path Assembly**
- Implement `assemble_hexagonal_piece_path()`
- Combine edges in correct order (clockwise)
- Handle edge direction (forward vs. reverse)
- Close path with Z command
- Test: extract center piece from 2-ring puzzle

**Day 6: Complete Extraction Function**
- Implement `generate_hexagonal_individual_pieces()`
- Generate all pieces for a puzzle
- Return list of piece data (paths, positions, colors)
- Test with 2-ring (7 pieces) and 3-ring (19 pieces)

**Day 7: Separation and Integration**
- Implement path translation for separated layout
- Replace placeholder separation with real pieces
- Update `R/hexagonal_separation.R`
- Test in Shiny app

### Week 3: Testing and Polish (2 days)

**Day 8: Comprehensive Testing**
- Test 2-ring, 3-ring, 4-ring puzzles
- Test with warping (do_warp = TRUE)
- Test with truncation (do_trunc = TRUE)
- Verify edge complementarity
- Visual inspection

**Day 9: Documentation and Cleanup**
- Document hexagonal coordinate system
- Add roxygen2 documentation
- Create examples
- Update CLAUDE.md
- Close GitHub issues

## Technical Details

### Hexagonal Coordinate System

For a hexagonal puzzle with `rings = n`:
- Center piece at (0, 0)
- Ring 1: 6 pieces around center
- Ring 2: 12 pieces around ring 1
- Ring k: 6k pieces

**Hexagonal axial coordinates (q, r)**:
- q: column offset
- r: row offset
- Six directions: E, NE, NW, W, SW, SE

### Edge Identification

Each piece is bounded by edges:
- **Center piece** (6 edges): All edges radiate outward
- **Ring pieces** (3-6 edges): Mix of radial and tangential edges
- **Edge pieces** (include border): Some edges are border segments

### Edge Sharing

Adjacent pieces share edges:
- Piece A has edge from (x1, y1) to (x2, y2) with tab UP
- Piece B has same edge from (x2, y2) to (x1, y1) with tab DOWN (reversed)
- Must ensure complementarity

## Files to Create/Modify

### New Functions (in R/hexagonal_individual_pieces.R)
- `hex_hlineseg_data()` - Edge data for horizontal edge
- `hex_vlineseg_data()` - Edge data for vertical edge
- `hex_border_segment_data()` - Edge data for border segment
- `identify_piece_topology()` - Map pieces to edges
- `assemble_hexagonal_piece_path()` - Build piece from edges
- `generate_hexagonal_individual_pieces()` - Main function

### Modified Functions
- `hex_gen_dh(return_edges = FALSE)` - Conditional return
- `hex_gen_dv(return_edges = FALSE)` - Conditional return
- `hex_gen_db(return_edges = FALSE)` - Conditional return

### Updated Files
- `R/hexagonal_separation.R` - Use real pieces instead of placeholders
- `inst/shiny-app/app.R` - Already enabled, should just work
- `CLAUDE.md` - Update status

### Testing
- `tests/test_hexagonal_individual.R` - New test suite
- `temp/test_hex_edge_generation.R` - Development testing

## Success Criteria

### Must Have
✅ Generate individual edges with forward/reverse paths
✅ Map pieces to their bounding edges correctly
✅ Assemble closed piece paths
✅ Extract all pieces from 2-ring puzzle (7 pieces)
✅ Extract all pieces from 3-ring puzzle (19 pieces)
✅ Adjacent pieces have complementary edges
✅ Separated layout works with real pieces
✅ Shiny app displays real pieces

### Should Have
- Works with 4-ring and 5-ring puzzles
- Handles warped puzzles
- Handles truncated puzzles
- Good performance (< 5 seconds)

## Risk Mitigation

**Risk**: Hexagonal coordinate system is complex
- Mitigation: Study existing generation code carefully
- Mitigation: Start with 2-ring (simpler topology)
- Mitigation: Create visual diagrams

**Risk**: Edge-to-piece mapping is non-trivial
- Mitigation: Use existing hex_gen_dh/dv loop structure as guide
- Mitigation: Test incrementally (center piece first)

**Risk**: Breaking existing complete puzzle generation
- Mitigation: Add parameter, don't change default behavior
- Mitigation: Test backward compatibility extensively

## Comparison with Original Roadmap

| Aspect | Original (Approach 1) | Revised (Hybrid) |
|--------|----------------------|------------------|
| **Complexity** | Very high (intersections) | Moderate (refactoring) |
| **Timeline** | 7-10 days | 5-7 days |
| **Risk** | High (math precision) | Medium (refactoring) |
| **Maintainability** | Low (complex extraction) | High (follows rectangular) |
| **Backward Compat** | Yes | Yes |
| **Proven Pattern** | No | Yes (rectangular) |

## Next Steps

1. **Get approval** for revised approach
2. **Study hexagonal generation** code in detail
3. **Start Day 1**: Understand coordinate system
4. **Create feature branch**: `feature/hexagonal-direct-generation`
5. **Begin implementation** following timeline

## Related Documents

- Original roadmap: `HEXAGONAL_IMPLEMENTATION_ROADMAP.md`
- Phase 1 findings: `HEXAGONAL_PHASE1_FINDINGS.md`
- Rectangular reference: `R/puzzle_core_clean.R`
- Hexagonal generation: `R/hexagonal_puzzle.R`
- GitHub issues: #6, #7, #8, #9, #10
