# Hexagonal Individual Pieces - Complete Implementation Roadmap

**Created**: 2025-11-26
**Status**: Planning Phase
**Goal**: Implement real hexagonal piece extraction and separation (not placeholders)

## Executive Summary

### Current Situation
- ✅ Hexagonal complete puzzle generation works perfectly
- ✅ Separation functionality exists but uses placeholder hexagons
- ❌ Real individual piece extraction NOT implemented
- ❌ GitHub Issues #6, #7, #8, #9, #10 remain open

### What We Achieved Today (2025-11-26)
- ✅ Fixed Shiny app crash (added compatibility stub)
- ✅ Created working separation with placeholders (stopgap solution)
- ✅ Identified that placeholder approach is insufficient
- ✅ Analyzed the real problem and created this roadmap

### The Real Challenge
Hexagonal puzzles are fundamentally different from rectangular:
- **Rectangular**: Grid-based, generate pieces directly (4 edges per piece)
- **Hexagonal**: Topology-based, must extract from complete puzzle (3-6 edges per piece)
- **Complexity**: Requires graph theory, coordinate mapping, and topology understanding

## GitHub Issues Overview

### Primary Epic
**Issue #10**: Feature: Complete hexagonal individual piece extraction
- Status: OPEN
- Labels: epic, enhancement, hexagonal-puzzles
- Milestone: Version 0.2.0
- Estimated: 5-8 days total effort

### Sub-Issues (Sequential Dependencies)

**Issue #6**: Implement hexagonal piece boundary extraction
- Priority: **CRITICAL** - Foundation for everything else
- Estimated: 2-3 days
- Status: Not started
- Blocks: Issues #7, #8, #9

**Issue #7**: Implement hexagonal piece positioning and transformation
- Priority: High
- Estimated: 1-2 days
- Status: Placeholder solution exists (needs replacement)
- Depends on: Issue #6

**Issue #8**: Implement hexagonal SVG generation and file saving
- Priority: High
- Estimated: 1 day
- Status: Not started
- Depends on: Issue #6

**Issue #9**: Test and validate hexagonal individual piece extraction
- Priority: High
- Estimated: 1-2 days
- Status: Cannot start until #6, #7, #8 complete
- Depends on: All above

### Related Issues

**Issue #11**: Review and document hexagonal development artifacts archive
- Priority: Low
- Many experimental attempts archived in `R/scripts_archive/development/hexagonal_work/`
- Useful for understanding what was tried

**Issue #25**: Enhancement: Add PNG download capability to Shiny app
- Priority: Medium
- Related but independent of hexagonal work

## Technical Analysis

### Problem Statement

The hexagonal puzzle generator creates 3 complete SVG paths:
1. **Horizontal cuts**: Multiple M...C...C... sequences forming horizontal divisions
2. **Vertical cuts**: Multiple M...C...C... sequences forming vertical divisions
3. **Border**: The outer hexagon (or circle if warped)

These paths are interconnected. Individual pieces are formed by the **closed regions** created by these intersecting paths.

**Goal**: Extract each closed region as a separate SVG path.

### Why This Is Hard

1. **Topological Complexity**
   - Hexagonal grid has non-obvious connectivity
   - Center piece: 6 edges
   - Ring pieces: 3-6 edges (varies by position)
   - Edge pieces: Include part of border

2. **Path Parsing Challenge**
   - Paths contain multiple subpaths (M commands)
   - Must identify which segments bound which pieces
   - Segments shared by adjacent pieces

3. **Coordinate Precision**
   - Floating point coordinates must match within tolerance
   - Transformations (warp, rotate, scale) complicate matching
   - Control points must be preserved exactly

4. **Complementary Edges**
   - Adjacent pieces share edges (one has tab, other has blank)
   - Must ensure edge direction reversal
   - Critical for pieces to fit together

### Approaches Considered

#### Approach 1: Parse and Extract (Current)
**Strategy**: Parse complete paths, build connectivity graph, trace piece boundaries

**Pros**:
- Works with existing generation code
- No changes to core hexagonal generation
- Maintains backward compatibility

**Cons**:
- Complex graph theory implementation needed
- Coordinate matching is error-prone
- Debugging is difficult
- High implementation complexity

**Status**: Partially implemented (path splitting works)

**Estimate**: 3-4 days for complete solution

#### Approach 2: Direct Generation (Like Rectangular)
**Strategy**: Modify hexagonal generation to create pieces directly, not complete puzzle

**Pros**:
- Clean, deterministic
- Guaranteed complementary edges
- Follows proven rectangular pattern
- Easier to maintain

**Cons**:
- Requires rewriting core generation logic
- Original JS code tightly coupled to complete path generation
- Risk of introducing bugs in working code
- Complex coordinate transformations

**Status**: Not started (significant refactor required)

**Estimate**: 5-7 days for complete solution

#### Approach 3: JavaScript Bridge
**Strategy**: Use original JavaScript implementation via Node.js

**Pros**:
- Original implementation already works
- Known to be correct
- Could leverage via system() calls

**Cons**:
- Adds Node.js dependency
- Cross-platform compatibility issues
- Not pure R solution
- Performance overhead

**Status**: Not explored

**Estimate**: 2-3 days (if JS has individual piece support)

#### Approach 4: Manual Topology Mapping
**Strategy**: Hard-code piece topology for small ring counts (2-3 rings)

**Pros**:
- Could work for common cases quickly
- Deterministic piece structure
- No graph algorithms needed

**Cons**:
- Not scalable to larger puzzles
- Maintenance nightmare
- Violates DRY principle
- Breaks for warped/truncated puzzles

**Status**: Not recommended

**Estimate**: N/A (bad approach)

## Recommended Implementation Plan

### Phase 1: Enhanced Path Extraction (Issue #6)
**Duration**: 3 days
**Priority**: CRITICAL

#### Day 1: Build Connectivity Graph
1. **Complete path splitting** (already started)
   - ✅ Split by M commands (DONE)
   - Extract all segment endpoints
   - Build segment lookup by coordinates

2. **Implement connectivity detection**
   ```r
   find_connecting_segments(point, segments, tolerance = 0.1)
   # Returns list of segments that start or end at this point
   ```

3. **Build adjacency graph**
   ```r
   # Graph structure: segment_id -> list of connected segment_ids
   connectivity_graph <- build_segment_graph(all_segments)
   ```

4. **Test with 2-ring puzzle**
   - Verify all segments connected
   - Visualize connectivity (optional)

#### Day 2: Implement Piece Tracing
1. **Cycle detection algorithm**
   ```r
   find_cycles(graph, max_cycle_length)
   # Returns list of cycles (each is a list of segment_ids)
   ```

2. **Piece boundary tracing**
   ```r
   trace_piece_boundary(start_segment, connectivity_graph)
   # Follow edges until returning to start
   # Handle clockwise vs counterclockwise
   ```

3. **Path assembly**
   ```r
   assemble_piece_path(segment_list, direction)
   # Combine segments into closed SVG path
   # Handle segment reversal for direction
   # Add Z command to close
   ```

4. **Test with 2-ring puzzle**
   - Extract center piece (6 edges)
   - Extract one ring piece
   - Verify closed paths

#### Day 3: Complete Extraction for All Pieces
1. **Extract all pieces systematically**
   ```r
   extract_all_pieces(connectivity_graph, expected_piece_count)
   # Identify all cycles
   # Filter to valid pieces (correct size)
   # Handle degenerate cases
   ```

2. **Piece identification**
   - Map extracted pieces to logical positions
   - Identify center piece (largest cycle)
   - Order ring pieces clockwise
   - Label pieces consistently

3. **Edge complementarity verification**
   - Check adjacent pieces share edges
   - Verify one has tab, other has blank
   - Test with visual inspection

4. **Test with 3-ring puzzle**
   - Should extract 19 pieces
   - Verify all pieces valid
   - Check no missing pieces

#### Deliverables
- `extract_individual_hexagonal_piece_paths()` fully implemented
- Works for 2-ring and 3-ring puzzles
- Returns list of pieces with SVG paths
- Documented with examples

### Phase 2: Positioning and Transformation (Issue #7)
**Duration**: 1 day
**Priority**: High
**Depends on**: Phase 1 complete

#### Morning: Path Transformation
1. **Implement coordinate translation**
   ```r
   translate_svg_path(path_string, dx, dy)
   # Parse path
   # Add offsets to all coordinates
   # Preserve control points
   # Rebuild path string
   ```

2. **Implement piece retrieval**
   ```r
   get_individual_hexagonal_piece_path(piece_index, ...)
   # Call extraction function
   # Return specific piece by index
   # Cache results for performance
   ```

3. **Test transformations**
   - Translate piece to (0, 0)
   - Translate to (100, 100)
   - Verify visual output correct

#### Afternoon: Grid Layout
1. **Replace placeholder logic in `generate_separated_hexagonal_svg()`**
   - Use real piece paths instead of hexagons
   - Apply cumulative offset layout
   - Maintain rectangular grid arrangement

2. **Test separated layout**
   - Generate 2-ring separated (7 pieces)
   - Generate 3-ring separated (19 pieces)
   - Verify spacing correct
   - Check no overlaps

3. **Integration with Shiny app**
   - Test separated mode in app
   - Verify real pieces render
   - Check different offset values

#### Deliverables
- Placeholder separation replaced with real pieces
- `transform_hexagonal_piece_to_position()` implemented
- Separated layout works for laser cutting
- Shiny app fully functional

### Phase 3: SVG Generation and File I/O (Issue #8)
**Duration**: 1 day
**Priority**: High
**Depends on**: Phase 1 complete

#### Morning: Combined SVG
1. **Implement `create_hexagonal_individual_pieces_svg()`**
   - Calculate proper viewBox
   - Generate SVG header
   - Add all pieces with colors
   - Include piece labels/numbers
   - Close SVG structure

2. **Color palette application**
   - Cycle through colors
   - Option for custom color mapping
   - Support for gradient fills (future)

3. **Test combined visualization**
   - Render with colors
   - Verify all pieces visible
   - Check scale and proportions

#### Afternoon: Individual Files
1. **Implement `save_hexagonal_individual_pieces()`**
   - Calculate bounding box per piece
   - Generate individual SVG files
   - Proper file naming convention
   - Add metadata (seed, ring, etc.)

2. **Bounding box calculation**
   ```r
   calculate_piece_bounds(piece_path)
   # Parse coordinates
   # Find min/max X, Y
   # Add padding
   # Return viewBox string
   ```

3. **File operations**
   - Create output directory
   - Write each piece file
   - Return list of filenames
   - Handle errors gracefully

4. **Test file generation**
   - Generate 7 files for 2-ring
   - Open in SVG viewer
   - Verify laser-cutting ready

#### Deliverables
- Combined visualization SVG
- Individual piece files
- Proper bounding boxes
- User-friendly file naming

### Phase 4: Testing and Validation (Issue #9)
**Duration**: 2 days
**Priority**: High
**Depends on**: Phases 1, 2, 3 complete

#### Day 1: Functional Testing
1. **Test suite creation**
   - File: `tests/testthat/test-hexagonal-individual.R`
   - Test 2-ring extraction (7 pieces)
   - Test 3-ring extraction (19 pieces)
   - Test 4-ring extraction (37 pieces)

2. **Edge complementarity tests**
   ```r
   test_that("adjacent pieces have complementary edges", {
     pieces <- extract_hex_pieces(...)
     # Check piece 1 and piece 2 share edge
     # Verify one has tab, other has blank
     # Confirm direction reversal
   })
   ```

3. **Path validity tests**
   ```r
   test_that("all piece paths are valid SVG", {
     pieces <- extract_hex_pieces(...)
     for (piece in pieces) {
       expect_true(is_valid_svg_path(piece$path))
       expect_true(is_closed_path(piece$path))
     }
   })
   ```

4. **Separation tests**
   ```r
   test_that("separated pieces don't overlap", {
     svg <- generate_separated_hexagonal_svg(...)
     # Check bounding boxes don't intersect
     # Verify minimum separation maintained
   })
   ```

#### Day 2: Visual and Integration Testing
1. **Manual visual inspection**
   - Generate test puzzles
   - Open in SVG viewer
   - Check piece shapes correct
   - Verify tabs and blanks match
   - Test actual fitting (if printing)

2. **Shiny app integration testing**
   - Test all hexagonal modes
   - Verify UI updates correctly
   - Check file downloads work
   - Test error handling

3. **Performance benchmarking**
   ```r
   benchmark_hexagonal_extraction()
   # Measure time for 2-ring, 3-ring, 4-ring, 5-ring
   # Identify bottlenecks
   # Document performance characteristics
   ```

4. **Documentation verification**
   - All functions documented (roxygen2)
   - Examples run without errors
   - README updated
   - CLAUDE.md reflects reality

5. **Edge case testing**
   - Warped puzzles (do_warp = TRUE)
   - Truncated puzzles (do_trunc = TRUE)
   - Circular puzzles
   - Very small puzzles (2 rings)
   - Large puzzles (5+ rings)

#### Deliverables
- Comprehensive test suite
- All tests passing
- Performance benchmarks documented
- Visual validation complete
- Documentation updated

## Alternative Paths and Contingencies

### If Phase 1 Proves Too Difficult

**Option A**: Implement Approach 2 (Direct Generation)
- Refactor `hex_gen_dh()` and `hex_gen_dv()` to generate per-piece
- Follow rectangular pattern more closely
- Timeline: Add 2-3 days

**Option B**: Simplify to Common Cases Only
- Hard-code topology for 2-ring and 3-ring
- Document limitation clearly
- Provide warning for larger puzzles
- Timeline: Save 1-2 days but limited solution

**Option C**: Use Placeholder Solution
- Accept current hexagon placeholders
- Document as "layout preview only"
- Focus efforts elsewhere
- Timeline: No additional work

### Risk Mitigation

**Risk**: Coordinate matching fails due to precision
- Mitigation: Implement robust tolerance-based matching
- Fallback: Round coordinates to nearest 0.01

**Risk**: Cycle detection finds wrong cycles
- Mitigation: Validate cycle size matches expected piece edge count
- Fallback: Manual correction for problematic puzzles

**Risk**: Performance too slow for large puzzles
- Mitigation: Profile and optimize hot paths
- Fallback: Cache results, lazy evaluation

**Risk**: Complementary edges don't match perfectly
- Mitigation: Use exact same path data with reversal
- Fallback: Add small overlap/gap correction

## Resource Requirements

### Development Time
- **Minimum**: 5 days (if everything goes smoothly)
- **Expected**: 7 days (with debugging and refinement)
- **Maximum**: 10 days (if major issues arise)

### Dependencies
- ✅ R (already have)
- ✅ bezier_utils.R (already have)
- ✅ hexagonal_puzzle.R (already have)
- ✅ SVG viewer for testing (already have)
- ❌ Graph theory library (may need igraph - optional)

### Expertise Needed
- R programming ✅
- SVG path manipulation ✅
- Graph theory/algorithms ⚠️ (may need to learn)
- Hexagonal coordinate systems ⚠️ (need to understand better)
- Topology/geometry ⚠️ (moderate difficulty)

## Success Metrics

### Must Have (Required for Completion)
1. ✅ Extract all pieces from 2-ring puzzle (7 pieces)
2. ✅ Extract all pieces from 3-ring puzzle (19 pieces)
3. ✅ Adjacent pieces have complementary edges
4. ✅ Individual SVG files can be saved
5. ✅ Separated layout works (no placeholders)
6. ✅ All tests pass
7. ✅ Shiny app works with real pieces

### Should Have (Highly Desired)
1. Works for 4-ring (37 pieces) and 5-ring (61 pieces)
2. Performance acceptable (< 5 seconds per puzzle)
3. Handles warped and truncated puzzles
4. Good documentation and examples
5. Visual validation confirms correctness

### Nice to Have (Future Enhancements)
1. Custom piece coloring
2. Alternative layout arrangements
3. Export to other formats (DXF for laser cutters)
4. Interactive piece highlighting
5. Assembly instructions generation

## Timeline and Milestones

### Week 1: Foundation (Phase 1)
- **Day 1**: Connectivity graph building
- **Day 2**: Piece tracing implementation
- **Day 3**: Complete extraction, test with 2-3 rings
- **Milestone**: Can extract pieces from simple puzzles

### Week 2: Integration (Phases 2-3)
- **Day 4**: Path transformation and positioning
- **Day 5**: SVG generation and file I/O
- **Day 6**: Replace placeholders, integrate with Shiny
- **Milestone**: Real pieces working in separation mode

### Week 2: Quality (Phase 4)
- **Day 7**: Comprehensive testing
- **Day 8**: Bug fixes and edge cases
- **Milestone**: Production-ready implementation

### Optional Week 3: Polish
- **Day 9-10**: Performance optimization
- **Day 11**: Documentation and examples
- **Day 12**: User testing and feedback
- **Milestone**: Version 0.2.0 ready for release

## Next Immediate Steps

1. **Decision Point**: Commit to this implementation plan
   - Discuss with stakeholders
   - Confirm timeline acceptable
   - Allocate dedicated time

2. **Environment Setup**
   - Create feature branch: `feature/hexagonal-real-pieces`
   - Set up testing framework
   - Prepare test data (2-ring, 3-ring puzzles)

3. **Start Phase 1, Day 1**
   - Implement connectivity graph
   - Test with simple 2-ring puzzle
   - Document findings

4. **Daily Standup**
   - Track progress vs plan
   - Identify blockers early
   - Adjust timeline as needed

## Related Documentation

- **Technical Plan**: This document
- **Original Issue**: GitHub Issue #10
- **Sub-Issues**: GitHub Issues #6, #7, #8, #9
- **Reference Code**: `R/individual_pieces.R` (rectangular)
- **Archive**: `R/scripts_archive/development/hexagonal_work/` (previous attempts)
- **Current Status**: `HEXAGONAL_FIXES_SUMMARY.md` (what was done today)

## Conclusion

This is a **significant engineering effort** requiring careful planning and execution. The problem is **solvable** but not trivial.

**Recommended approach**: Phase 1 (Parse and Extract) with contingency for Phase 2 approach if needed.

**Key to success**: Incremental progress with frequent testing and validation.

**Expected outcome**: Full-featured hexagonal individual piece extraction matching rectangular puzzle capabilities.
