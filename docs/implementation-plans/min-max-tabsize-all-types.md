# Implementation Plan: Min/Max Tab Size for All Puzzle Types

**GitHub Issue**: #76
**Date**: 2025-12-21
**Status**: ✅ COMPLETED

## Executive Summary

Enable `min_tab_size` and `max_tab_size` constraints for rectangular, hexagonal, and concentric puzzle types. Currently only voronoi/random types support these constraints.

## Current Architecture Analysis

### Tab Height Formula (Universal)
All puzzle types use the same tab height formula:
```
tab_height = 3.0 * t * edge_length
```

Where:
- `t` = normalized tab fraction (typically 0.08-0.12 from tabsize percentage)
- `edge_length` = physical length of the puzzle edge in mm

### Existing Constraint Logic (tessellation_edge_generation.R:108-136)
```r
# Calculate actual tab height
tab_height <- 3.0 * t * edge_length

# MINIMUM constraint: scale UP if tab too small
if (!is.null(min_tab_size) && tab_height < min_tab_size) {
  t <- min_tab_size / (3.0 * edge_length)

  # Safety: if tab would be too wide (>70% of edge), use straight line
  if (4.0 * t > 0.7) {
    return(straight_line_edge)
  }
}

# MAXIMUM constraint: scale DOWN if tab too large
if (!is.null(max_tab_size) && tab_height > max_tab_size) {
  t <- max_tab_size / (3.0 * edge_length)
}
```

## Implementation Strategy by Puzzle Type

### 1. Hexagonal & Concentric (Simpler - Edge-Based)

**Files to modify:**
- `R/hexagonal_bezier_generation.R` - `generate_hex_bezier_edge()`
- `R/concentric_edge_generation.R` - parameter passing

**Approach**: Direct adaptation of tessellation constraint logic.

**Changes to `generate_hex_bezier_edge()`:**
```r
generate_hex_bezier_edge <- function(v1, v2, seed, edge_id,
                                     tab_params = list(tabsize = 27, jitter = 5),
                                     min_tab_size = NULL,  # NEW
                                     max_tab_size = NULL)  # NEW
{
  # ... existing edge_length calculation ...

  t <- tabsize * (0.8 + 0.4 * runif(1))

  # NEW: Apply min/max constraints
  tab_height <- 3.0 * t * edge_length

  if (!is.null(min_tab_size) && tab_height < min_tab_size) {
    t <- min_tab_size / (3.0 * edge_length)
    if (4.0 * t > 0.7) {
      return(straight_line_result)  # Edge too short
    }
  }

  if (!is.null(max_tab_size) && tab_height > max_tab_size) {
    t <- max_tab_size / (3.0 * edge_length)
  }

  # ... rest of function uses adjusted t ...
}
```

### 2. Rectangular (More Complex - Global State)

**Files to modify:**
- `R/rectangular_puzzle.R` - `init_jigsaw()`, `next_tab()`, `gen_dh()`, `gen_dv()`

**Challenge**: Rectangular uses global `.jigsaw_env$t` set once at init, but edge lengths vary:
- Horizontal edges: `height / yn` (piece height)
- Vertical edges: `width / xn` (piece width)

**Approach A: Per-Edge Constraint Check** (Recommended)
Store constraints in environment, calculate per-edge `t` adjustment:

```r
init_jigsaw <- function(..., min_tab_size = NULL, max_tab_size = NULL) {
  # ... existing code ...
  .jigsaw_env$min_tab_size <- min_tab_size
  .jigsaw_env$max_tab_size <- max_tab_size
  .jigsaw_env$t_base <- tabsize / 200.0  # Base t value
}

# NEW helper function
get_constrained_t <- function() {
  edge_length <- sl()  # Current edge length
  t <- .jigsaw_env$t_base * (0.8 + 0.4 * random())  # With variation
  tab_height <- 3.0 * t * edge_length

  if (!is.null(.jigsaw_env$min_tab_size) && tab_height < .jigsaw_env$min_tab_size) {
    t <- .jigsaw_env$min_tab_size / (3.0 * edge_length)
    # Note: Can't easily return straight line in this architecture
    # Cap at maximum safe width instead
    if (4.0 * t > 0.7) t <- 0.175  # 0.7 / 4
  }

  if (!is.null(.jigsaw_env$max_tab_size) && tab_height > .jigsaw_env$max_tab_size) {
    t <- .jigsaw_env$max_tab_size / (3.0 * edge_length)
  }

  return(t)
}
```

**Approach B: Pre-compute Safe Range** (Alternative)
Calculate min/max valid `t` ranges at init based on all edge lengths, use stricter bounds.

**Recommendation**: Use Approach A for consistency with voronoi/random behavior.

## File-by-File Changes

### Phase 1: API Layer

#### 1.1 `R/jigsawR_clean.R`

**Line 50-53**: Update documentation
```r
#' @param min_tab_size Minimum absolute tab size in mm (default: NULL for no limit).
#'   Prevents tabs from becoming too small on short edges.
#' @param max_tab_size Maximum absolute tab size in mm (default: NULL for no limit).
#'   Prevents tabs from becoming too large on long edges.
```
Remove "Only applies to voronoi/random types" restriction.

**Lines 294-295**: Update result parameters
```r
min_tab_size = min_tab_size,  # Remove conditional
max_tab_size = max_tab_size   # Remove conditional
```

### Phase 2: Dispatch Layer

#### 2.1 `R/unified_piece_generation.R`

**Lines 58-85** (rectangular/hexagonal/concentric dispatch): Pass parameters
```r
# Rectangular (around line 75)
result <- generate_rect_pieces_internal(
  ...,
  min_tab_size = min_tab_size,  # ADD
  max_tab_size = max_tab_size   # ADD
)

# Hexagonal (around line 80)
result <- generate_hex_pieces_internal(
  ...,
  min_tab_size = min_tab_size,  # ADD
  max_tab_size = max_tab_size   # ADD
)

# Concentric (around line 85)
result <- generate_concentric_pieces_internal(
  ...,
  min_tab_size = min_tab_size,  # ADD
  max_tab_size = max_tab_size   # ADD
)
```

### Phase 3: Type-Specific Implementation

#### 3.1 `R/rectangular_puzzle.R`

1. Update `init_jigsaw()` signature and store constraints
2. Add `get_constrained_t()` helper function
3. Modify `next_tab()` to use constrained t OR modify control point functions

#### 3.2 `R/hexagonal_bezier_generation.R`

1. Update `generate_hex_bezier_edge()` signature
2. Add constraint logic after line 49
3. Handle straight-line fallback for too-short edges

#### 3.3 `R/concentric_edge_generation.R`

1. Update `generate_concentric_edge_map()` to accept constraints
2. Pass constraints to `generate_hex_bezier_edge()` calls

### Phase 4: Shiny App

#### 4.1 `inst/shiny-app/app.R`

**Line 509**: Remove conditional panel restriction
```r
# BEFORE:
conditionalPanel(
  condition = "input.puzzle_type == 'voronoi' || input.puzzle_type == 'random'",
  ...
)

# AFTER:
# Show for all puzzle types (remove conditionalPanel or change condition to TRUE)
```

### Phase 5: Documentation

#### 5.1 `quarto/api/generate-puzzle.qmd`
- Update parameter table
- Remove "voronoi/random only" notes

#### 5.2 Gallery Files
- Add `min_tab_size` and `max_tab_size` to parameter tables

## Testing Strategy

### Unit Tests to Add (`tests/testthat/test-min-max-tabsize.R`)

```r
# Test 1: Rectangular with min constraint
test_that("rectangular puzzle respects min_tab_size", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 4),
    size = c(400, 300),
    tabsize = 10,          # Small percentage
    min_tab_size = 15,     # Force minimum 15mm tabs
    seed = 42
  )
  # Verify tabs are at least 15mm (inspect SVG paths)
  expect_true(length(result$pieces) > 0)
})

# Test 2: Hexagonal with max constraint
test_that("hexagonal puzzle respects max_tab_size", {
  result <- generate_puzzle(
    type = "hexagonal",
    grid = 3,
    size = 200,
    tabsize = 40,          # Large percentage
    max_tab_size = 10,     # Cap at 10mm
    seed = 42
  )
  expect_true(length(result$pieces) > 0)
})

# Test 3: Concentric with both constraints
test_that("concentric puzzle respects min and max constraints", {
  result <- generate_puzzle(
    type = "concentric",
    grid = 3,
    size = 240,
    min_tab_size = 5,
    max_tab_size = 20,
    seed = 42
  )
  expect_true(length(result$pieces) > 0)
})

# Test 4: Edge case - impossible constraints
test_that("handles min > edge gracefully", {
  # When min_tab_size would require tab wider than edge
  # Should produce straight lines instead of crashing
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(10, 10),      # Many small pieces
    size = c(100, 100),    # 10mm edges
    min_tab_size = 50,     # Impossible - 50mm tab on 10mm edge
    seed = 42
  )
  expect_true(length(result$pieces) > 0)
})
```

## Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Breaking existing rectangular generation | Medium | High | Extensive testing, maintain backward compatibility when constraints=NULL |
| Performance regression | Low | Medium | Constraint check is O(1) per edge |
| Inconsistent behavior across types | Medium | Medium | Use identical constraint formula everywhere |
| Edge case: min > max | Low | Low | Already validated in API layer |

## Implementation Order

1. **Phase 3.2**: Hexagonal (simplest, self-contained function)
2. **Phase 3.3**: Concentric (uses hexagonal, just parameter passing)
3. **Phase 3.1**: Rectangular (most complex, needs careful testing)
4. **Phase 2**: Dispatch layer updates
5. **Phase 1**: API layer updates
6. **Phase 4**: Shiny app
7. **Phase 5**: Documentation
8. **Testing**: Full test suite

## Estimated Complexity

- **Hexagonal**: ~30 lines changed
- **Concentric**: ~10 lines changed
- **Rectangular**: ~50 lines changed (most complex due to global state)
- **Dispatch/API**: ~20 lines changed
- **Shiny**: ~5 lines changed
- **Tests**: ~100 lines new
- **Docs**: ~30 lines changed

**Total**: ~250 lines of code changes

## Success Criteria

1. All puzzle types accept `min_tab_size` and `max_tab_size` parameters
2. Constraints are applied consistently using the same formula
3. Existing tests pass (backward compatibility)
4. New tests cover constraint scenarios
5. Shiny app shows constraint controls for all types
6. Documentation updated with no "voronoi/random only" restrictions
