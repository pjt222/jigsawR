# jigsawR Performance Optimization Plan

## Baseline Performance (December 2025)

| Puzzle Type | Size | Pieces | Median Time |
|-------------|------|--------|-------------|
| Rectangular | 5x5 | 25 | **50.3ms** |
| Rectangular | 8x8 | 64 | **104ms** |
| Hexagonal | 3 rings | 19 | **302ms** |
| Hexagonal | 5 rings | 61 | **1.06s** |
| Concentric | 3 rings | 19 | **92.4ms** |

### Key Observations
1. **Hexagonal puzzles are 6x slower** than rectangular for similar piece counts
2. **Scaling**: Hexagonal 61 pieces takes 1.06s vs rectangular 64 pieces at 104ms
3. **Target**: Achieve 2x+ speedup for hexagonal puzzles (GitHub Issue #48)

---

## Identified Hotspots

### HIGH PRIORITY

| # | Issue | File:Line | Impact |
|---|-------|-----------|--------|
| 1 | String concat in loops | rectangular_puzzle.R:134-146 | O(n²) → O(n) |
| 2 | Vector grow-on-append | unified_piece_generation.R:376 | O(n²) → O(n) |
| 3 | Neighbor search O(n²) | unified_renderer.R:1226 | O(n²) → O(1) |
| 4 | SVG path parsing per render | unified_renderer.R:1147 | n×parse → 1×cache |
| 5 | Hex topology recalc | unified_piece_generation.R:306 | per-render → once |

### MEDIUM PRIORITY

| # | Issue | File:Line | Impact |
|---|-------|-----------|--------|
| 6 | Repeated bounds calc | unified_piece_generation.R:373-389 | 3×calc → 1×cache |
| 7 | Environment lookups | hexagonal_puzzle.R:90-120 | micro-optimization |

---

## Implementation Plan

### Phase 1: Quick Wins (Target: 1.5-2x speedup)

**Estimated effort: 1-2 days**

#### 1.1 Pre-allocate Vectors Instead of Grow-on-Append
**Location**: `unified_piece_generation.R:373-389`, `piece_positioning.R:287-299`

```r
# BEFORE (O(n²))
all_path_x <- c()
for (piece in pieces) {
  all_path_x <- c(all_path_x, x_coords)  # Growing vector
}

# AFTER (O(n))
all_path_x <- vector("numeric", estimated_size)
idx <- 1
for (piece in pieces) {
  n <- length(x_coords)
  all_path_x[idx:(idx + n - 1)] <- x_coords
  idx <- idx + n
}
# Or use list + unlist pattern
coords_list <- lapply(pieces, extract_coords)
all_path_x <- unlist(coords_list)
```

#### 1.2 Build Piece ID Lookup Hash Map
**Location**: `unified_renderer.R:1200-1324`

```r
# BEFORE (O(n) per lookup)
for (np in pieces) {
  np_id <- np$id %||% which(...)
  if (np_id == neighbor_id) { ... }
}

# AFTER (O(1) per lookup)
piece_lookup <- new.env(hash = TRUE)
for (i in seq_along(pieces)) {
  piece_lookup[[as.character(pieces[[i]]$id)]] <- i
}
# Lookup:
idx <- piece_lookup[[as.character(neighbor_id)]]
neighbor_piece <- pieces[[idx]]
```

#### 1.3 Cache Environment Variables in Local Scope
**Location**: `hexagonal_puzzle.R:90-120`

```r
# BEFORE (env lookup per call)
hex_p1 <- function() list(l = hex_l(0.2), w = hex_w(.hex_jigsaw_env$a))

# AFTER (local variable)
hex_gentab_optimized <- function(v1, v2, isnew) {
  env <- .hex_jigsaw_env  # Single lookup
  a <- env$a; b <- env$b; c <- env$c; d <- env$d; e <- env$e
  flip <- env$flip; t <- env$t
  # Use local variables throughout
}
```

---

### Phase 2: Caching (Target: additional 1.5x)

**Estimated effort: 3-5 days**

#### 2.1 Cache Parsed SVG Paths at Generation Time
**Priority**: HIGH
**Estimated impact**: Significant - path is parsed 3-5x per piece during rendering

**Problem**: `parse_svg_path()` is called repeatedly on the same path:
- `calculate_piece_bounds()` (unified_renderer.R:388) - label positioning
- `split_rect_path_into_edges()` (unified_renderer.R:526) - rectangular edge extraction
- `split_concentric_path_into_edges()` (unified_renderer.R:619) - concentric edge extraction
- `split_hex_path_into_edges()` (unified_renderer.R:926) - hexagonal edge extraction

**Implementation Steps**:

1. **Modify piece generation** (unified_piece_generation.R):
```r
# In generate_rectangular_pieces_internal(), generate_hexagonal_pieces_internal(), etc.:
piece <- list(
  id = piece_id,
  path = piece_path,
  parsed_segments = parse_svg_path(piece_path),  # Cache once at generation
  center = c(center_x, center_y),
  ...
)
```

2. **Modify consumers** (unified_renderer.R):
```r
# In split_*_path_into_edges() and calculate_piece_bounds():
segments <- if (!is.null(piece$parsed_segments)) {
  piece$parsed_segments
} else {
  parse_svg_path(piece$path)  # Fallback for backwards compatibility
}
```

**Files to modify**:
- `R/unified_piece_generation.R` - 3 locations (rect, hex, concentric piece creation)
- `R/unified_renderer.R` - 4 locations (calculate_piece_bounds, 3x split_*_path_into_edges)

#### 2.2 Pre-compute Hex Topology-to-Geometry Mappings
**Priority**: MEDIUM
**Estimated impact**: Moderate - complex mapping computed per piece

**Problem**: `topo_to_geo_map` is computed at lines 303-327 during hexagonal piece generation. The mapping logic is repeated.

**Implementation Steps**:

1. **Extract mapping computation** (unified_piece_generation.R):
```r
# Create helper function
compute_hex_topo_geo_map <- function(piece_id, hex_pieces, n_rings) {
  # Extract mapping logic from lines 303-327
  topo_to_geo_map <- list()
  # ... mapping computation ...
  return(topo_to_geo_map)
}
```

2. **Store in piece object**:
```r
piece <- list(
  id = piece_id,
  path = hp$path,
  topo_to_geo_map = compute_hex_topo_geo_map(piece_id, hex_pieces, n_rings),
  ...
)
```

**Files to modify**:
- `R/unified_piece_generation.R` - extract and cache mapping

#### 2.3 Fix remaining grow-on-append in calculate_piece_bounds
**Priority**: LOW
**Location**: `unified_renderer.R:390-404`

```r
# BEFORE (O(n²))
xs <- c()
ys <- c()
for (seg in segments) {
  xs <- c(xs, seg$x)
  ys <- c(ys, seg$y)
}

# AFTER (O(n))
xs <- unlist(lapply(segments, function(seg) {
  if (seg$type == "C") c(seg$cp1x, seg$cp2x, seg$x)
  else if (seg$type %in% c("M", "L", "A")) seg$x
  else NULL
}), use.names = FALSE)
```

#### 2.4 Single-Pass Canvas Size Calculation ✅ (Done in Phase 1.1)
Already implemented via `calculate_pieces_bounds()` in bezier_utils.R.

---

### Phase 3: Parallelization (Target: 2-4x on multi-core)

**Estimated effort: 1 week**

#### 3.1 Parallel Piece Generation
**Location**: `unified_piece_generation.R:147-202` (rectangular), `278-366` (hexagonal)

```r
# Using furrr for parallel processing
library(furrr)
plan(multisession, workers = parallel::detectCores() - 1)

pieces <- furrr::future_map(seq_len(n_pieces), function(i) {
  generate_single_piece_data(i, ...)
}, .options = furrr_options(seed = TRUE))
```

#### 3.2 Parallel Batch Generation
**Location**: `jigsawR_clean.R:391-443` (generate_puzzle_batch)

Already has infrastructure; enable parallel backend by default.

---

## Acceptance Criteria

From GitHub Issue #48:
- [x] Benchmark current performance as baseline ✓
- [x] Implement at least one optimization technique ✓ (Phase 1.1, 1.2, 2.1, 2.3)
- [x] Achieve measurable speedup ✓ (15.67x on split_path operations via Phase 2.1)
- [x] All existing tests pass ✓ (1432 tests passing)
- [x] No breaking changes to public API ✓

---

## Testing Strategy

1. **Unit tests**: Verify optimized functions produce identical output
2. **Integration tests**: Run existing test suite after each change
3. **Benchmark tests**: Compare before/after timing for all puzzle types
4. **Regression tests**: Verify SVG output byte-for-byte identical (same seed)

---

## Rollback Plan

All optimizations will be:
1. Implemented in separate commits with clear descriptions
2. Feature-flagged where appropriate (e.g., `options(jigsawR.parallel = TRUE)`)
3. Documented with before/after benchmarks

---

## Files to Modify

### Phase 1
- `R/unified_piece_generation.R`
- `R/unified_renderer.R`
- `R/piece_positioning.R`
- `R/hexagonal_puzzle.R`

### Phase 2
- `R/unified_piece_generation.R`
- `R/unified_renderer.R`
- `R/bezier_utils.R`

### Phase 3
- `R/unified_piece_generation.R`
- `R/jigsawR_clean.R`
- `DESCRIPTION` (add furrr to Suggests)

---

## Progress Tracking

| Phase | Task | Status | Speedup |
|-------|------|--------|---------|
| 1.1 | Pre-allocate vectors | ✅ Complete | TBD |
| 1.2 | Piece ID hash map | ✅ Complete | TBD |
| 1.3 | Cache env variables | ⏭️ Skipped | Minimal impact |
| 2.1 | Cache parsed paths | ✅ Complete | **15.67x** (on split_path ops) |
| 2.2 | Pre-compute hex mappings | ⏭️ Skipped | Already cached via fused_edges |
| 2.3 | Fix grow-on-append bounds | ✅ Complete | O(n²) → O(n) |
| 3.1 | Parallel piece gen | ⏭️ Deferred | Complex (RNG state) |
| 3.2 | Parallel batch gen | ✅ Complete | ~Nx (N workers) |

### Phase 1 Implementation Notes (December 2025)

**1.1 Pre-allocate vectors** - Replaced O(n²) grow-on-append patterns with O(n) list+unlist:
- `R/bezier_utils.R`: Added `extract_path_coords()`, `extract_all_piece_coords()`, `calculate_pieces_bounds()`
- `R/unified_piece_generation.R`: Hexagonal and concentric canvas calculation
- `R/piece_positioning.R`: Concentric and hexagonal positioning bounds

**1.2 Piece ID hash map** - Replaced O(n²) neighbor lookups with O(1) hash map:
- `R/unified_renderer.R`: Added `piece_lookup` environment and `get_piece_by_id()` helper in `render_pieces_with_fusion_styled()`

**1.3 Cache env variables** - Skipped: Would require significant restructuring of hexagonal_puzzle.R closures for minimal gain.

### Phase 2 Implementation Notes (December 2025)

**2.1 Cache parsed paths** - Added `parsed_segments` caching at piece generation time:
- `R/unified_piece_generation.R`: Added `parsed_segments = parse_svg_path(path)` to all 3 piece types (rect, hex, conc)
- `R/unified_renderer.R`: Updated 6 functions to use cached segments:
  - `split_rect_path_into_edges()` (line 526)
  - `split_concentric_path_into_edges()` (line 623)
  - `split_hex_path_into_edges()` (line 935)
  - `calculate_path_bounding_box_center()` (line 373)
  - `calculate_piece_bounds()` (line 1630)
- **Measured speedup**: 15.67x on `split_*_path_into_edges` operations (470ms → 30ms for 100 calls)

**2.2 Pre-compute hex mappings** - Skipped: Analysis showed `topo_to_geo_map` is already computed once during hexagonal piece generation and results stored in `fused_edges`. No additional caching needed.

**2.3 Fix grow-on-append in calculate_piece_bounds** - Replaced O(n²) vector concatenation with O(n) list+unlist:
```r
# Before (O(n²))
xs <- c()
for (seg in segments) { xs <- c(xs, seg$x) }

# After (O(n))
coord_lists <- lapply(segments, function(seg) list(x = seg$x, y = seg$y))
xs <- unlist(lapply(coord_lists, `[[`, "x"), use.names = FALSE)
```

### Phase 3 Implementation Notes (December 2025)

**3.1 Parallel piece generation** - Deferred: The piece generators use environment-based RNG state (`.rect_jigsaw_env`, `.hex_jigsaw_env`) which is problematic for parallelization. Would require significant restructuring to make generators functional (stateless). Given that Phase 2 caching already provides major speedup, this is deferred for future consideration.

**3.2 Parallel batch generation** - Added optional parallel execution to `generate_puzzle_batch()`:
- `R/jigsawR_clean.R`: Updated function with `parallel` and `workers` parameters
- Uses `furrr::future_map()` with `future::multisession` backend
- Gracefully falls back to sequential if:
  - `furrr`/`future` packages not available
  - `jigsawR` package not installed (required for workers)
- **Expected speedup**: Near-linear with worker count (N workers → ~Nx speedup)
- **Requirements**: Package must be installed (`devtools::install()`) for parallel mode

**Usage example**:
```r
# Sequential (works with devtools::load_all())
results <- generate_puzzle_batch(variations)

# Parallel (requires installed package)
results <- generate_puzzle_batch(variations, parallel = TRUE, workers = 4)
```

---

## References

- GitHub Issue #48: Performance optimization
- `tests/benchmark/baseline_benchmark.R`: Comprehensive benchmark suite
- `R/performance_utils.R`: Existing optimization utilities
