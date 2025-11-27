# Hexagonal Piece Extraction - Phase 1 Findings

**Date**: 2025-11-26
**Phase**: Phase 1, Day 1 - Connectivity Graph Building
**Status**: Critical discovery - approach needs revision

## What Was Attempted

Following the roadmap, I began implementing Phase 1 (Parse and Extract approach):
1. ✅ Split paths by M commands into segments
2. ✅ Implemented `build_segment_graph()` for connectivity
3. ✅ Implemented `extract_unique_endpoints()` for junction point analysis
4. ❌ Discovered fundamental issue with the approach

## Critical Discovery

### The Problem: Paths Don't Connect at Endpoints - They INTERSECT

**Original Assumption** (incorrect):
- Horizontal, vertical, and border paths would share endpoints
- Build connectivity graph by matching endpoints
- Trace cycles through connected segments

**Reality** (discovered through testing):
- Horizontal and vertical paths are CONTINUOUS CURVES
- They pass THROUGH intersection points but don't START/END there
- Example from 2-ring puzzle:
  - Horizontal path: `M 45 76.7 C ... 120 76.7 C ... 195 76.7` (one continuous curve)
  - Vertical path: `M 120 33.4 C ... 120 76.7` (separate curve)
  - Point (120, 76.7) is INSIDE the horizontal path, not an endpoint

### Evidence from Testing

```
DEBUGGING CONNECTIVITY GRAPH
============================

Total segments: 7

First 5 segment endpoints:
  Segment 1: (45.00, 76.70) -> (195.00, 76.70)
  Segment 2: (45.00, 163.30) -> (195.00, 163.30)
  Segment 3: (120.00, 33.40) -> (120.00, 76.70)
  Segment 4: (82.50, 98.35) -> (82.50, 141.65)
  Segment 5: (157.50, 98.35) -> (157.50, 141.65)

Testing connectivity with different tolerances:
  Tolerance 0.1: min=0, max=0, mean=0.0
  Tolerance 0.5: min=0, max=0, mean=0.0
  Tolerance 1.0: min=0, max=0, mean=0.0
  Tolerance 2.0: min=0, max=0, mean=0.0
```

**Result**: ZERO connections found because endpoints don't match.

### Why This Happens

Looking at the hexagonal generation code structure:

```r
hex_gen_dh <- function() {
  str <- ""
  for (yi in ...) {
    isnew <- TRUE
    for (xi in ...) {
      str <- paste0(str, hex_hlineseg(xi, yi, isnew))
      isnew <- FALSE  # Only first segment gets M command
    }
  }
  return(str)
}
```

The `isnew` flag means:
- First edge in a row gets `M x y` (move command)
- Subsequent edges are CONCATENATED (no new M command)
- Result: ONE CONTINUOUS PATH for entire row

This is exactly like drawing with a pen:
- `M 45 76.7` = "move pen to (45, 76.7)"
- `C ... 120 76.7` = "draw curve to (120, 76.7)"
- `C ... 195 76.7` = "continue drawing to (195, 76.7)" - NO lift!

## What This Means for Extraction

### Approach 1 (Parse and Extract) is Much More Complex Than Expected

**Roadmap Estimate**: 3 days
**Actual Complexity**: 5-7 days minimum

**Additional Requirements Discovered**:
1. **Path Intersection Calculation**
   - Find where bezier curves intersect with other bezier curves
   - Requires solving parametric equations
   - Numerically complex and error-prone

2. **Path Splitting at Interior Points**
   - Can't just split by M commands
   - Must split paths at computed intersection points
   - Requires bezier curve subdivision algorithms

3. **Intersection Point Precision**
   - Floating point precision issues
   - Multiple paths may intersect at "approximately" the same point
   - Need robust clustering algorithm

4. **Path Direction Tracking**
   - After splitting, must track which direction each segment goes
   - Critical for assembling piece boundaries correctly

### Comparison with Rectangular Approach

**Rectangular Puzzle** (puzzle_core_clean.R):
```r
generate_all_edges <- function(xn, yn) {
  edges <- list(horizontal = list(), vertical = list())

  # Generate horizontal edges
  for (yi in 1:(yn - 1)) {
    for (xi in 0:(xn - 1)) {
      # Calculate edge endpoints
      x1 <- ... ; x2 <- ... ; y <- ...

      # Generate individual edge segment
      edge_path <- generate_edge_segment(c(x1, y), c(x2, y), vertical = FALSE)
      row_edges[[xi + 1]] <- edge_path
    }
    edges$horizontal[[yi]] <- row_edges
  }
  # ... similar for vertical
}
```

**Key Differences**:
- Generates edges INDIVIDUALLY by grid position
- Each edge stored separately
- No parsing or extraction needed
- Clean, deterministic

**Hexagonal Puzzle** (hexagonal_puzzle.R):
```r
hex_gen_dh <- function() {
  str <- ""
  for (yi in ...) {
    for (xi in ...) {
      str <- paste0(str, hex_hlineseg(xi, yi, isnew))
    }
  }
  return(str)  # ONE BIG STRING
}
```

**Problems**:
- Concatenates all edges into one string
- Individual edges lost in concatenation
- Requires complex extraction to recover individual edges

## Recommendations

### Option A: Continue with Approach 1 (Parse and Extract)
**Pros**:
- No changes to core hexagonal generation
- Backward compatible

**Cons**:
- MUCH more complex than anticipated
- Requires bezier intersection algorithms
- Error-prone floating point math
- 5-7 days minimum (not 3 days)
- High risk of bugs

**Estimate**: 7-10 days total

### Option B: Switch to Approach 2 (Direct Generation)
**Pros**:
- Clean, following rectangular pattern
- Deterministic and maintainable
- Guaranteed correct complementary edges
- Once working, very robust

**Cons**:
- Requires refactoring hex_gen_dh/dv
- Must understand hexagonal coordinate system
- Risk of breaking existing complete puzzle generation

**Estimate**: 5-7 days total

### Option C: Hybrid Approach
**Strategy**: Modify hexagonal generation to OPTIONALLY return individual edges

```r
hex_gen_dh <- function(return_edges = FALSE) {
  if (return_edges) {
    # Return list of individual edge data
    edges <- list()
    for (yi in ...) {
      for (xi in ...) {
        edge_data <- hex_hlineseg_data(xi, yi)
        edges[[length(edges) + 1]] <- edge_data
      }
    }
    return(edges)
  } else {
    # Existing behavior - concatenate into string
    str <- ""
    for (yi in ...) {
      for (xi in ...) {
        str <- paste0(str, hex_hlineseg(xi, yi, isnew))
      }
    }
    return(str)
  }
}
```

**Pros**:
- Backward compatible (existing code still works)
- Enables direct edge access for individual pieces
- Follows rectangular pattern
- Lower risk

**Cons**:
- Still requires understanding hexagonal coordinate system
- Need to create `hex_hlineseg_data()` variants

**Estimate**: 4-6 days total

## Recommended Next Step

**Switch to Approach 2 (Direct Generation) or Hybrid Approach**

**Rationale**:
1. Parse-and-extract is fundamentally more complex than anticipated
2. Rectangular puzzles prove direct generation works well
3. Lower risk of precision/intersection bugs
4. More maintainable long-term
5. Better matches the existing successful pattern

**Next Actions**:
1. Discuss with stakeholders about approach change
2. If approved, study hexagonal coordinate system thoroughly
3. Create `generate_all_hexagonal_edges()` function
4. Follow rectangular implementation pattern
5. Test with 2-ring puzzle first

## Code Implemented (Approach 1 - Partial)

### What Works
- ✅ `split_path_by_move()` - Splits paths by M commands
- ✅ `points_match()` - Coordinate tolerance matching
- ✅ `build_segment_graph()` - Creates connectivity graph structure
- ✅ `extract_unique_endpoints()` - Identifies junction points
- ✅ Testing framework in `temp/` directory

### What's Missing (for Approach 1)
- ❌ Bezier-bezier intersection calculation
- ❌ Path splitting at interior points
- ❌ Robust intersection point clustering
- ❌ Cycle detection algorithm
- ❌ Piece boundary assembly
- ❌ Edge complementarity verification

### Files Modified
- `R/hexagonal_piece_extraction.R` - Added graph building functions
- `temp/test_path_splitting.R` - Basic testing
- `temp/debug_connectivity.R` - Debugging connectivity
- `temp/analyze_hex_paths.R` - Path structure analysis

## Lessons Learned

1. **Always test assumptions early** - The "paths connect at endpoints" assumption should have been verified before implementing graph algorithms

2. **Study the generation code first** - Understanding how paths are created reveals extraction complexity

3. **Rectangular patterns don't directly translate** - Hexagonal topology is fundamentally different

4. **Parse-and-extract vs. Direct-generation trade-off**:
   - Parse-and-extract: Preserves existing code, but complex extraction
   - Direct-generation: More refactoring, but cleaner result

## References

- Roadmap: `HEXAGONAL_IMPLEMENTATION_ROADMAP.md`
- Rectangular implementation: `R/puzzle_core_clean.R:generate_all_edges()`
- Hexagonal generation: `R/hexagonal_puzzle.R:hex_gen_dh(), hex_gen_dv()`
- Testing: `temp/debug_connectivity.R`, `temp/analyze_hex_paths.R`
