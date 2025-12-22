# jigsawR Architecture & Development Insights

This document contains detailed architectural documentation and development insights from the jigsawR project. It serves as a reference for understanding the mathematical foundations and lessons learned during development.

**Cross-reference**: See `CLAUDE.md` in project root for essential daily development guidance.

## Table of Contents
1. [Hexagonal Warp/Trunc Architecture](#hexagonal-warptrunc-architecture)
2. [Concentric Ring Mode Architecture](#concentric-ring-mode-architecture)
3. [Key Development Insights](#key-development-insights)
4. [Development History](#development-history)

---

## Hexagonal Warp/Trunc Architecture

The hexagonal puzzle supports five boundary modes controlled by `do_warp`, `do_trunc`, and `do_circular_border`:

| UI Option | do_warp | do_trunc | do_circular_border | Boundary Shape | Border Edges |
|-----------|---------|----------|-------------------|----------------|--------------|
| Zigzag (Original) | FALSE | FALSE | FALSE | Zigzag hexagon | Straight (L) |
| Clean Hexagon | FALSE | TRUE | FALSE | Clean hexagon | Straight (L) |
| Warped Zigzag | TRUE | FALSE | FALSE | Warped zigzag | Straight (L) |
| Warped Hexagon | TRUE | TRUE | FALSE | Circular vertices | Straight (L) |
| Perfect Circle | TRUE | TRUE | TRUE | Perfect circle | Arcs (A) |

### Warped Hexagon vs Perfect Circle

Verified 2025-12-04:
- Both modes project boundary vertices to the same circle radius (`diameter/2`)
- The difference is in how border edges connect these vertices:
  - **Warped Hexagon**: `L -80.30 59.60` (straight line to endpoint)
  - **Perfect Circle**: `A 100.00 100.00 0 0 1 -80.30 59.60` (arc following circle)
- Visual difference is **subtle for high ring counts** (5-7 rings) because arc segments are short (~8-10°)
- Visual difference is **noticeable for low ring counts** (2-3 rings) because arc segments are longer (~30-60°)
- Arc "sag" (bulge at midpoint) formula: `sag = radius * (1 - cos(arc_angle/2))`
  - 7-ring, 200mm diameter: ~0.5mm sag per segment (nearly invisible)
  - 3-ring, 200mm diameter: ~3mm sag per segment (visible on close inspection)
  - 2-ring, 300mm diameter: ~20mm sag per segment (clearly visible)

### Key Implementation Details

#### 1. Warp Formula (`R/hexagonal_topology.R:apply_hex_warp`)
```r
# CRITICAL: Must use DIVISION, not multiplication
angl <- atan2(y, x) + pi
angl60 <- angl %% (pi / 3)
angl30 <- abs((pi / 6) - angl60)
l <- sqrt(0.75) / cos(angl30)
return(list(x = x / l, y = y / l))  # DIVIDE by l
```
- The original JavaScript uses division to push edge midpoints outward
- Multiplication (incorrect) would compress corners inward instead

#### 2. Warp Application Scope (`R/hexagonal_edge_generation_fixed.R`)
- Warp must be applied to ALL vertices (internal + boundary), not just boundary
- This matches the deprecated complete mode behavior

#### 3. Piece Radius Formula (`R/hexagonal_edge_generation_fixed.R`)
```r
# CORRECT formula: diameter / (4 * rings - 2)
piece_radius <- diameter / (4 * rings - 2)
```
- This ensures boundary vertices reach exactly `diameter/2` after warp transformation
- The old formula `diameter / (rings * 4)` produced coordinates ~73-80% of target size
- Derived by solving: find piece_radius where max(warped_boundary_dist) = diameter/2
- The factor `4*rings - 2` is exact for all ring counts (verified for rings 2-12)

#### 4. Boundary Projection (when both warp+trunc)
- After warping, boundary vertices are at different distances from origin
- Project them onto the target circle: `projected = (v / |v|) * circle_radius`
- This ensures all border arcs connect smoothly on a perfect circle

#### 5. Arc Radius (when both warp+trunc)
- Use consistent `circle_radius = diameter / 2` for ALL border arcs
- Don't calculate per-edge radii (causes gaps between arcs)

#### 6. Transformation Order (verified 2025-12-03)
- Separation/offset is ALWAYS applied AFTER warp/border transformations
- Pipeline order in `generate_puzzle()`:
  1. Vertex calculation (original hexagonal grid)
  2. Boundary detection (before transformations)
  3. Warp transformation (if `do_warp=TRUE`) → applies to ALL vertices
  4. Circular border projection (if `do_circular_border=TRUE`)
  5. Edge generation (using transformed vertices)
  6. Piece assembly at compact positions
  7. Separation/offset applied via `apply_piece_positioning()` (translation only)
- Separation is purely translational - edge shapes never change, only positions
- This ensures consistent piece shapes across all separation levels

#### 7. Floating Point Precision in Vertex Keys (fixed 2025-12-04)
- Vertex sharing uses string keys like `"x,y"` to match shared vertices between pieces
- **Problem**: Floating point errors caused key mismatches:
  - Values like `-1.776357e-15` (epsilon near zero) formatted as `"-0.0"` instead of `"0.0"`
  - Interior vertices incorrectly identified as boundary vertices
  - `apply_hex_trunc` then projected interior vertices to the puzzle boundary
- **Solution**: `make_vertex_key()` function in `R/hexagonal_edge_generation_fixed.R`:
  ```r
  make_vertex_key <- function(x, y) {
    x_rounded <- round(x, 1)  # Round BEFORE formatting
    y_rounded <- round(y, 1)
    sprintf("%.1f,%.1f", x_rounded + 0.0, y_rounded + 0.0)  # +0.0 normalizes -0.0
  }
  ```
- This ensures vertices that should match (e.g., shared by 3 pieces) always produce identical keys

#### 8. Label Centering on Geometric Bounding Box (fixed 2025-12-05)
- **Problem**: Piece labels appeared increasingly offset from piece centers for outer pieces
- **Root cause**: Labels used **stored lattice centers** (calculated before warp/truncation), while SVG gradient fills use **geometric bounding box centers** (from actual transformed paths)
- For warped hexagonal pieces, these differ significantly - error increases with distance from puzzle center
- **Solution**: Calculate label position from actual SVG path geometry:
  ```r
  calculate_path_bounding_box_center <- function(path) {
    segments <- parse_svg_path(path)
    xs <- c(); ys <- c()
    for (seg in segments) {
      if (seg$type %in% c("M", "L")) { xs <- c(xs, seg$x); ys <- c(ys, seg$y) }
      else if (seg$type == "C") { xs <- c(xs, seg$cp1x, seg$cp2x, seg$x); ys <- c(ys, seg$cp1y, seg$cp2y, seg$y) }
      else if (seg$type == "A") { xs <- c(xs, seg$x); ys <- c(ys, seg$y) }
    }
    c(x = (min(xs) + max(xs)) / 2, y = (min(ys) + max(ys)) / 2)
  }
  ```
- Now labels align with where SVG `objectBoundingBox` gradients appear centered
- Files: `R/unified_renderer.R` (calculate_path_bounding_box_center, render_piece_label)

---

## Concentric Ring Mode Architecture

Added 2025-12-04. The concentric ring mode creates puzzles where all pieces have **constant radial height** (distance toward center). This creates trapezoidal pieces that get **wider toward the outside**, resembling a dartboard or target pattern.

### Key Characteristics
- **Center piece**: Hexagon or circle (configurable via `center_shape`)
- **Outer pieces**: Trapezoids with 4 vertices (INNER, RIGHT, OUTER, LEFT edges)
- **Piece count**: Same formula as hexagonal: `3 * rings * (rings - 1) + 1`
- **Piece height**: `diameter / (2 * rings)` - constant for all pieces

### Edge Types
| Edge | Description | Has Tab? |
|------|-------------|----------|
| INNER | Connects to inner ring or center | Yes (always) |
| RIGHT | Connects to next piece in same ring (circumferential) | Yes |
| OUTER | Connects to outer ring or boundary | Yes (unless boundary) |
| LEFT | Connects to previous piece in same ring (circumferential) | Yes |

### Geometry Calculations (`R/concentric_geometry.R`)
```r
# Trapezoid vertices for piece in ring r, position p:
inner_radius <- r * piece_height
outer_radius <- (r + 1) * piece_height
arc_angle <- 2 * pi / (6 * r)  # Each ring r has 6*r pieces

# 4 vertices in clockwise order:
V1: (inner_radius * cos(start_angle), inner_radius * sin(start_angle))  # inner-start
V2: (inner_radius * cos(end_angle), inner_radius * sin(end_angle))      # inner-end
V3: (outer_radius * cos(end_angle), outer_radius * sin(end_angle))      # outer-end
V4: (outer_radius * cos(start_angle), outer_radius * sin(start_angle))  # outer-start
```

### Files
- `R/concentric_geometry.R`: Vertex calculations, neighbor detection
- `R/concentric_edge_generation.R`: Edge map with bezier tabs, path building

### Usage
```r
generate_puzzle(
  type = "concentric",    # Top-level puzzle type
  grid = c(3),            # 3 rings = 19 pieces
  size = c(240),          # 240mm diameter
  center_shape = "hexagon"  # or "circle"
)
```

---

## Key Development Insights

These insights emerged during development and capture lessons learned for future reference.

### 1. Type Architecture Matters
- Initially implemented concentric as `concentric_mode=TRUE` within hexagonal type
- This led to confusion: Is concentric a modifier or a fundamentally different puzzle?
- **Insight**: If a feature requires its own piece geometry, edge generation, and positioning logic, it should be a top-level type, not a boolean flag
- Clean API: `type="concentric"` is clearer than `type="hexagonal", concentric_mode=TRUE`

### 2. Hard Breaks vs Deprecation
- Chose "hard break" (remove parameter entirely) over deprecation warnings
- **Rationale**: The codebase is pre-1.0 and the old API had very limited use
- Deprecation warnings add complexity for minimal benefit in early development
- Clear error messages ("Invalid type 'X'. Must be one of: ...") guide users

### 3. Type Dispatch Patterns
- Three-way dispatch in R is cleanest with explicit `if/else if/else`:
  ```r
  if (type == "concentric") { ... }
  else if (type == "hexagonal") { ... }
  else { ... }  # rectangular
  ```
- `switch()` works but obscures the default case
- Store type-specific parameters conditionally: `center_shape = if (type == "concentric") value else NULL`

### 4. Shiny UI for Multiple Types
- Radio buttons for mutually exclusive types work better than nested conditionals
- Each type gets its own `conditionalPanel` with type-specific options
- Separate input IDs per type (e.g., `rings` vs `rings_conc`) avoid value conflicts

### 5. Testing Strategy for Type Refactoring
- Test each type individually first
- Test that type-specific parameters are only set for their type
- Test that invalid types produce clear errors
- Generate visual output files for human verification

### 6. Edge Direction Debugging
- Circle center edges were drawn clockwise instead of counter-clockwise
- **Root cause**: `is_forward = FALSE` in edge generation
- **Lesson**: Edge direction bugs manifest as visual artifacts (overlapping, gaps)
- Debug by examining raw path coordinates, not just rendered output

### 7. SVG Arc Translation (2025-12-05)
- **Problem**: Perfect Circle boundary arcs appeared distorted when pieces separated
- **Wrong assumption**: "Arcs can't be translated - convert to lines"
- **Correct math**: Arc translation IS valid - translate endpoints, keep radii constant
- **Proof**: `|P₁' - C₂| = |(P₁ + Δ) - (C₁ + Δ)| = |P₁ - C₁| = r`
- **Insight**: SVG renderer calculates arc center from endpoints + radii. When both endpoints translate by (dx, dy), the center also moves by (dx, dy)
- **Analogy**: Moving a slice of pie outward - the curved edge maintains its exact shape
- **Lesson**: Question mathematical assumptions in comments; verify with proofs

### 8. SVG Arc Sweep Flag for Boundary Direction (2025-12-05)
- **Discovery**: Initial "Perfect Circle" boundary bent inward (concave) instead of outward (convex)
- **Root cause**: SVG arc sweep-flag controls arc direction
- **SVG arc format**: `A rx ry x-rotation large-arc-flag sweep-flag x y`
- **sweep-flag values**:
  - `0` = counter-clockwise (arc curves left when traveling from start to end)
  - `1` = clockwise (arc curves right when traveling from start to end)
- **Implementation**: For outward-facing (convex) boundary arcs traveling clockwise around puzzle:
  - Forward direction (clockwise): `sweep=0` (arc bulges outward)
  - Reverse direction (counter-clockwise): `sweep=1` (arc bulges outward)
- **For inward-facing (concave)**: Swap the values
- **Feature opportunity**: What seemed like a bug became a configurable feature (`boundary_facing` parameter)
- **Lesson**: "Bugs" that produce interesting visual effects can become features with a simple toggle

### 9. Parameter Flow Debugging in Shiny Apps (2025-12-05)
- **Problem**: Background gradient colors were ignored; always fell back to palette
- **Root cause**: Field name mismatch between Shiny and renderer
  - Shiny sent: `list(center = "#ff0000", middle = "#00ff00", edge = "#0000ff")`
  - Renderer expected: `gradient_spec$center_color`, `gradient_spec$edge_color`
- **Discovery method**: Spawned 3 QA subagents to trace parameter flow:
  1. Config → defaults
  2. UI → input controls
  3. Server → function calls
- **Lesson**: When a feature "doesn't work", trace the full data flow from UI to renderer
- **Fix pattern**: Support both naming conventions with null-coalescing:
  ```r
  center_color <- gradient_spec$center %||% gradient_spec$center_color
  ```

### 10. SVG Radial Gradients for Piece Fills (2025-12-05)
- **Challenge**: Each puzzle piece needs its own centered radial gradient
- **Solution**: Use `objectBoundingBox` (SVG default for gradientUnits)
- **How it works**: With `objectBoundingBox`, `cx="50%" cy="50%"` means "center of each element's bounding box"
- **Implementation**: Define one gradient, reference it in all pieces - each gets centered gradient
- **Key insight**: Unlike background gradients (absolute coordinates), piece gradients need relative coordinates
- **Code pattern**:
  ```xml
  <defs>
    <radialGradient id="pieceFillGradient" cx="50%" cy="50%" r="70%">
      <stop offset="0%" stop-color="#ffffff"/>
      <stop offset="50%" stop-color="#cccccc"/>
      <stop offset="100%" stop-color="#333333"/>
    </radialGradient>
  </defs>
  <path d="..." fill="url(#pieceFillGradient)"/>
  ```

### 11. 3-Stop Gradients for Richer Visual Effects (2025-12-05)
- **Standard SVG**: Supports unlimited color stops at any offset
- **UI design**: 3 stops (center/middle/edge at 0%/50%/100%) covers most use cases
- **Backward compatibility**: Make middle color optional; fall back to 2-stop if not provided
- **Implementation**: Check for middle color existence before building gradient def

### 12. Test Suite Migration Strategy (2025-12-05)
- **Context**: Migrated 40+ standalone test files to testthat framework
- **Categorization approach**: Spawn parallel subagents to categorize files into DELETE/ARCHIVE/KEEP
- **DELETE criteria**: Functionality now covered by testthat suite (17 files deleted)
- **ARCHIVE criteria**: Debugging scripts with historical value (69 files moved to `tests/debug_archive/`)
- **KEEP criteria**: Unique functionality not covered by unified API tests (7 files retained)
- **Key insight**: Most standalone tests were development artifacts; testthat provides better structure
- **Result**: 796 passing tests with comprehensive coverage

### 13. testthat Best Practices for R Packages (2025-12-05)
- **Use `skip_if_not()` for optional functions**: Legacy or deprecated functions may not exist
  ```r
  skip_if_not(exists("legacy_function"), "legacy_function not available")
  ```
- **Avoid `expect_gte`/`expect_lte` with `info` parameter**: Not supported in all testthat versions
  ```r
  # BAD: expect_gte(x, 0, info = "reason")
  # GOOD: expect_true(x >= 0, label = "x >= 0")
  ```
- **Test result structure, not implementation details**: Focus on `result$pieces`, `result$svg_content`
- **Use `save_files = FALSE`** in tests to avoid filesystem side effects
- **Piece count formulas**: Document expected counts in test names
  - Rectangular: `rows * cols`
  - Hexagonal/Concentric: `3 * rings * (rings - 1) + 1`

### 14. Batch Function Default Handling (2025-12-05)
- **Problem**: `generate_puzzle_batch()` passed NULL for missing variation parameters
- **Symptom**: `if (opacity < 1)` failed with "argument has length 0"
- **Root cause**: R function defaults don't apply when explicitly passing NULL
- **Solution**: Add explicit NULL checks with defaults in batch function:
  ```r
  if (is.null(var$opacity)) var$opacity <- 1.0
  if (is.null(var$do_warp)) var$do_warp <- FALSE
  ```
- **Lesson**: Batch/wrapper functions must handle NULL for all optional parameters

### 15. CLI Logging Environment Scoping (2025-12-05)
- **Problem**: `log_subheader("Generating puzzle {i}")` failed with "object 'i' not found"
- **Root cause**: `cli::cli_h2()` needs `.envir` parameter to find variables in caller's scope
- **Fix**: Pass `.envir = parent.frame()` to all cli wrapper functions
  ```r
  log_subheader <- function(text, .envir = parent.frame()) {
    cli::cli_h2(text, .envir = .envir)
  }
  ```
- **Pattern**: Any wrapper around cli functions with `{variable}` interpolation needs `.envir`

### 16. Edge Path Splitting for Fusion Features (2025-12-08)
- **Context**: Meta-piece fusion requires splitting a piece's SVG path into individual edges
- **Problem**: Original `split_concentric_path_into_edges()` used fixed segment counts (3-3-1-3)
- **Why it failed**:
  1. OUTER edges can have variable segment counts (3-6+) when connecting to multiple outer pieces
  2. Translated pieces (offset>0) have different coordinates than original vertices
  3. Vertex-based detection failed because piece_height calculation used translated center
- **Solution**: Segment pattern matching instead of vertex coordinate matching
  ```r
  # For boundary pieces: L segment marks the OUTER edge
  l_indices <- which(sapply(content_segments, function(s) s$type == "L"))
  if (length(l_indices) > 0) {
    # Split around the L segment
    l_idx <- l_indices[1]
    # INNER + RIGHT = segments before L, OUTER = L, LEFT = segments after L
  }

  # For internal pieces: Use fixed 3-3-X-3 pattern
  # INNER = first 3, RIGHT = next 3, OUTER = variable middle, LEFT = last 3
  ```
- **Key insight**: Edge splitting must be robust to:
  - Variable segment counts per edge
  - Coordinate translations from offset/separation
  - Different piece types (center hexagon vs ring trapezoids)
- **Verification**: Canonical edge keys (sorted "x1,y1|x2,y2") must match between adjacent pieces
- **Files**: `R/unified_renderer.R:split_concentric_path_into_edges()` (lines 571-736)
- **TODO**: Same fix needed for hexagonal puzzle type (see GitHub Issue #43)

### 17. Fused Edge Deduplication Strategy (2025-12-08)
- **Problem**: Fused edges between adjacent pieces would be drawn twice (once per piece)
- **Solution**: Create canonical edge keys from sorted endpoint coordinates
  ```r
  # Create canonical key - smaller coordinate string first
  p1 <- sprintf("%.1f,%.1f", start_x, start_y)
  p2 <- sprintf("%.1f,%.1f", end_x, end_y)
  edge_key <- if (p1 < p2) paste(p1, p2, sep = "|") else paste(p2, p1, sep = "|")

  # Track drawn edges
  if (edge_key %in% drawn_fused_edges) {
    # Skip - already drawn by adjacent piece
  } else {
    drawn_fused_edges <- c(drawn_fused_edges, edge_key)
    # Draw this edge
  }
  ```
- **Critical requirement**: Both pieces must produce identical edge paths for the shared edge
- **Debugging tip**: When deduplication fails, check:
  1. Edge splitting produces correct segments for each edge
  2. Endpoint coordinates match exactly (floating point precision)
  3. Edge direction doesn't affect key generation (canonical ordering handles this)

### 18. Hexagonal Topology vs Geometric Side Mapping (2025-12-09, Updated 2025-12-10)
- **Context**: Hexagonal fusion features require matching topology sides (from neighbor detection) to geometric sides (actual SVG path segments)
- **Problem**: Fused edges were rendered on wrong piece sides - e.g., piece 80's fused edge appeared on side 5 instead of the correct neighbor direction
- **Root cause (multi-layered)**:
  1. Topology sides (from `get_hex_neighbors_unified()`) differ from geometric sides (SVG path order)
  2. The conversion formula was **inverted**: `(dir - 30) / 60` instead of `(30 - dir) / 60`
  3. `apply_fusion_to_pieces()` was overwriting correctly-mapped data with raw topology keys

#### Pointed-Top Hexagon Geometry (SVG)
The geometric sides are numbered by the vertex angles in the SVG path:
```
     Side 2 (N, -90°)
        ____
 Side 1/    \Side 3  (-30°, -150°)
 (NE)  \____/  (NW)
 Side 0       Side 4  (30°, 150°)
 (E)   Side 5 (S, 90°)
```
**Formula derivation**:
- Side 0 → direction 30° (E, edge from V0 to V1)
- Side 1 → direction -30° (NE)
- Side N → direction `30 - N * 60`

Given direction `dir`, the geometric side is: `geo_side = round((30 - dir) / 60) %% 6`

**Verification examples**:
| Direction | Formula | Result | Expected Side |
|-----------|---------|--------|---------------|
| 30°       | (30-30)/60 = 0 | 0 | E ✓ |
| -30°      | (30-(-30))/60 = 1 | 1 | NE ✓ |
| -90°      | (30-(-90))/60 = 2 | 2 | N ✓ |
| 150°      | (30-150)/60 = -2 mod 6 = 4 | 4 | SW ✓ |

#### Bug Fix Details

**Bug 1**: Formula inversion in `generate_hex_pieces_with_fusion()` (line 326)
```r
# OLD (wrong):
geo_side <- round((dir_normalized - 30) / 60) %% 6

# NEW (correct):
geo_side <- round((30 - dir_to_neighbor) / 60) %% 6
```

**Bug 2**: Missing topo-to-geo mapping in `apply_fusion_to_pieces()` (lines 764-829)
- The function was using topology side keys directly as `fused_edges[["5"]]`
- For hexagonal puzzles, it now builds a mapping and converts: `fused_edges[[geo_dir]]`

**Test case**: For piece 52 with neighbor 113 at direction 150°:
- Expected geometric side: `(30 - 150) / 60 = -2 mod 6 = 4`
- Stored as: `fused_neighbor_ids[["4"]] = 113` ✓

- **Files modified**:
  - `R/unified_piece_generation.R:generate_hex_pieces_with_fusion()` (line 326)
  - `R/unified_piece_generation.R:apply_fusion_to_pieces()` (lines 764-829)
- **Related**: GitHub Issue #43 - Hexagonal edge path splitting for fusion features

### 19. Progress Feedback for Long-Running Operations (2025-12-09)

**Problem**: Large puzzles (7+ rings with fusion) take significant time to generate, leaving users without feedback.

**Solution**: Two-layer progress feedback:
1. **Console (cli package)**: `cli::cli_progress_step()` for step-by-step console progress
2. **UI (waiter package)**: Overlay spinner during puzzle generation

**Console Progress** (`R/unified_piece_generation.R`):
```r
cli::cli_progress_step("Generating {num_pieces} hexagonal pieces...")
cli::cli_progress_step("Computing fused edges for {n_groups} fusion group{?s}...")
cli::cli_progress_step("Processing piece geometry ({num_pieces} pieces)...")
```
Output shows timing: `✔ Generating 127 hexagonal pieces... [1.2s]`

**UI Progress** (`inst/shiny-app/app.R`):
```r
w <- Waiter$new(
  id = "puzzle_display",
  html = div(
    style = "display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100%;",
    spin_fading_circles(),
    h4("Generating puzzle...", style = "color: white; margin-top: 10px;")
  ),
  color = "rgba(0, 0, 0, 0.7)"
)
# In reactive: w$show() before generation, w$hide() after (including on error)
```

**Key insight**: Centering the waiter overlay requires flexbox CSS (`display: flex; align-items: center; justify-content: center; height: 100%;`) on the html content div.

**Files modified**:
- `R/unified_piece_generation.R` (hexagonal and concentric progress steps)
- `inst/shiny-app/app.R` (waiter integration)
- `DESCRIPTION` (waiter dependency)

### 20. Hash Set Optimization for Fusion Edge Computation (2025-12-09)

**Problem**: Large hexagonal puzzles (7+ rings) with fusion had multiple O(n²) bottlenecks:
1. `get_hex_neighbor()` used O(ring²) recursive reverse lookup loop
2. `%in%` membership checks are O(n) per call
3. Vector `c()` concatenation is O(n) per operation, leading to O(n²) total

**Solution**: R environments as hash maps for O(1) operations:

**Cached Adjacency Matrix** (`R/hexagonal_adjacency_cache.R`):
```r
# Pre-compute adjacency matrix using axial coordinate math
# O(n) construction, O(1) lookups thereafter
adj_matrix <- get_hex_adjacency_matrix(rings)
neighbor_id <- adj_matrix[piece_id, side + 1]  # O(1) instead of O(ring²)
```

**Hash Sets for Membership** (`compute_hex_fused_edges_fast()`):
```r
# Build group membership hash sets
group_set <- new.env(hash = TRUE, parent = emptyenv())
for (piece_id in group) {
  group_set[[as.character(piece_id)]] <- TRUE
}
# O(1) membership test instead of O(n) %in%
if (exists(as.character(neighbor_id), envir = group_set, inherits = FALSE))
```

**List Accumulation** (instead of vector `c()`):
```r
# O(1) append, O(n) final unlist
fused_edges_list <- list()
fused_edges_list[[length(fused_edges_list) + 1]] <- edge_key
# Instead of: fused_edges <- c(fused_edges, edge_key)  # O(n) per call!
```

**Benchmark Results** (5 rings, 61 pieces):
- Old method: 0.132s
- New method: 0.036s
- **Speedup: 3.7x** for fusion edge computation

**Key insight**: When maintaining compatibility with existing functions, use the SAME neighbor function (`get_hex_neighbors_for_fusion()`) for topology-side mapping but apply hash set optimizations for membership and deduplication checks. The adjacency matrix uses axial coordinate mapping which differs from the topology-side numbering.

**Files modified**:
- `R/hexagonal_adjacency_cache.R` (new file - adjacency matrix cache and fast functions)
- `R/unified_piece_generation.R` (updated to use `compute_hex_fused_edges_fast()`)

### 21. Concentric Puzzle Many-to-One Neighbor Relationships (2025-12-10)

**Problem**: In concentric puzzles with `ALL-18` fusion, the edge between pieces 4 and 13 was not rendering as dashed. Investigation revealed piece 4's OUTER edge was marked as neighbor=12 (solid), but piece 13's INNER edge was marked as neighbor=4 (dashed), causing overplotting.

**Root Cause**: Concentric puzzles have a **many-to-one neighbor relationship** between rings:
- Ring 1: 6 pieces, each spanning 60° angular width
- Ring 2: 12 pieces, each spanning 30° angular width

This means one inner ring piece's OUTER edge can touch **multiple** outer ring pieces' INNER edges:

```
Piece 4 (Ring 1, position 2):
  OUTER edge spans: 120° - 180°

  Touches:
    - Piece 12 (Ring 2, position 4): 120° - 150°
    - Piece 13 (Ring 2, position 5): 150° - 180°
```

The existing `get_concentric_neighbor()` function only returned ONE neighbor per edge (using `floor()` which gets the first), missing the second neighbor.

**Solution**: Added `get_all_concentric_outer_neighbors()` to return ALL outer neighbors:

```r
get_all_concentric_outer_neighbors <- function(piece_id, rings) {
  # Calculate angular span of this piece
  arc_angle <- 2 * pi / pieces_in_ring
  start_angle <- position * arc_angle
  end_angle <- (position + 1) * arc_angle

  # Find ALL outer pieces whose angular range overlaps
  neighbors <- integer(0)
  for (outer_pos in 0:(outer_pieces - 1)) {
    outer_start <- outer_pos * outer_arc_angle
    outer_end <- (outer_pos + 1) * outer_arc_angle
    if (outer_end > start_angle && outer_start < end_angle) {
      neighbors <- c(neighbors, outer_piece_start + outer_pos)
    }
  }
  return(neighbors)
}
```

**Integration Points**:
1. `compute_fused_edges()` now checks all outer neighbors for concentric puzzles
2. `apply_fusion_to_pieces()` has concentric-specific handling to mark OUTER edges as fused when ANY outer neighbor is in the fusion group

**Verification**:
```r
get_all_concentric_outer_neighbors(4, 3)  # Returns c(12, 13) ✓
result$pieces[[4]]$fused_edges$OUTER      # TRUE ✓
result$pieces[[13]]$fused_edges$INNER     # TRUE ✓
```

**Files modified**:
- `R/concentric_geometry.R` (added `get_all_concentric_outer_neighbors()`)
- `R/adjacency_api.R` (updated `compute_fused_edges()` for concentric)
- `R/unified_piece_generation.R` (updated `apply_fusion_to_pieces()` for concentric)

### 22. Edge-Segment-Level Fusion Rendering (2025-12-11)

**Problem (Issue #52)**: Insight #21 solved the *detection* of many-to-one neighbors, but the *rendering* was still incorrect. When one inner piece's OUTER edge spans multiple outer pieces, and only SOME of those outer pieces are in the fusion group, the entire OUTER edge was being rendered with a single style (either all dashed or all solid).

**Example**: With `ALL-18` fusion (all pieces except 18):
- Piece 7's OUTER edge spans 300°-360°
- Segment 300°-330° touches piece 18 (NOT in group) → should be **SOLID**
- Segment 330°-360° touches piece 19 (IN group) → should be **DASHED**
- Previous behavior: Entire edge rendered as DASHED

**Solution**: Implemented segment-level fusion rendering:

1. **New geometry function**: `get_outer_edge_segments(piece_id, rings)` returns angular segments of the OUTER edge with their neighbor IDs:
   ```r
   get_outer_edge_segments(7, 3)
   # Returns:
   # [[1]] list(start_angle=300°, end_angle=330°, neighbor_id=18)
   # [[2]] list(start_angle=330°, end_angle=360°, neighbor_id=19)
   ```

2. **Segment-level fusion data**: Each piece now stores `fused_edge_segments$OUTER` with per-segment fusion status:
   ```r
   piece$fused_edge_segments$OUTER[[1]]$fused  # FALSE (neighbor 18 not in group)
   piece$fused_edge_segments$OUTER[[2]]$fused  # TRUE (neighbor 19 in group)
   piece$outer_segments_mixed  # TRUE (mixed fusion status)
   ```

3. **Pass 3.5 rendering**: New rendering pass draws each segment as a separate SVG arc with appropriate styling:
   ```r
   generate_arc_segment_path(radius, start_angle, end_angle)
   # Returns: "M40.00,-69.28 A80.00,80.00 0 0,1 69.28,-40.00"
   ```

**Key implementation details**:
- `outer_radius` and `inner_radius` now stored in each piece for segment rendering
- Mixed segments skip Pass 2 (non-fused) and Pass 3 (fused) rendering
- Pass 3.5 handles segment-level arcs with deduplication by "innerPiece-outerPiece" keys
- Non-mixed pieces (all fused or all non-fused) use existing edge-level rendering

**Files modified**:
- `R/concentric_geometry.R` (added `get_outer_edge_segments()`)
- `R/unified_piece_generation.R` (segment-level fusion data, radius storage)
- `R/unified_renderer.R` (added `generate_arc_segment_path()`, Pass 3.5)
- `R/jigsawR_clean.R` (added `size` and `diameter` to temp_puzzle_result)
- `tests/testthat/test-segment-fusion.R` (27 new tests)

### 23. Circle Center Shape Implementation (2025-12-11)

**Problem (Issue #39)**: The `center_shape = "circle"` option for concentric puzzles was not producing different output from `center_shape = "hexagon"`. Both produced identical paths for the center piece.

**Root Cause**: The `build_concentric_piece_path()` function in `R/concentric_edge_generation.R` was collecting bezier edges and concatenating them directly for both circle and hexagon centers. Both used the same bezier tab edges calculated from ring 1 pieces' inner edge vertices.

**Expected Behavior**:
- **Hexagon center**: Straight lines between vertices, with bezier tabs to ring 1 pieces
- **Circle center**: Curved arcs between tab endpoints following a perfect circle

**Solution**: Updated `build_concentric_piece_path()` to use SVG arc commands (A) between bezier edge segments for circle centers:

```r
# Previous (incorrect): Just concatenate bezier edges
path_parts <- c(path_parts, edge$forward)

# New (correct): Add arc between edge endpoints
path_parts <- c(path_parts, sprintf("A %.2f %.2f 0 0 0 %.2f %.2f",
                                    r, r, bezier_start[1], bezier_start[2]))
path_parts <- c(path_parts, bezier_path)
```

**Key Implementation Details**:
1. Sort radial edges by angle (counterclockwise from 0)
2. For each edge: add SVG arc from previous edge endpoint to this edge start
3. Add the bezier tab edge
4. Final arc from last edge back to first edge start

**Verification**:
```r
hex_path <- result_hex$pieces[[1]]$path   # 725 chars, no arc commands
circle_path <- result_circle$pieces[[1]]$path  # 921 chars, has A commands
grepl(" A ", circle_path)  # TRUE ✓
```

**Files Modified**:
- `R/concentric_edge_generation.R` (updated `build_concentric_piece_path()`)
- `tests/testthat/test-core-integration.R` (added 4 center_shape tests)

### 24. Diagnostic Test Script Pattern (2025-12-11)

**Context**: When investigating why `center_shape = "circle"` wasn't working, a systematic diagnostic approach was essential.

**Pattern**: Create targeted test scripts that isolate and verify each layer of the system:

```r
# test_center_debug.R - Layer-by-layer diagnostic
cat("1. Testing calculate_concentric_vertices directly:\n")
hex_vertices <- calculate_concentric_vertices(1, 3, 200, "hexagon")
circle_vertices <- calculate_concentric_vertices(1, 3, 200, "circle")
cat("   Hexagon type:", hex_vertices$type, "\n")  # Verify geometry layer
cat("   Circle type:", circle_vertices$type, "\n")

cat("\n2. Testing generate_concentric_edge_map:\n")
edge_hex <- generate_concentric_edge_map(rings = 3, seed = 42, diameter = 200, center_shape = "hexagon")
cat("   Hexagon center piece type:", edge_hex$piece_vertices[[1]]$type, "\n")  # Verify edge generation

cat("\n3. Edge types for center piece:\n")
cat("   Edge types:", paste(sapply(edge_hex$piece_edges[[1]], function(e) e$type), collapse=", "), "\n")

cat("\n4. Building paths:\n")
path_hex <- build_concentric_piece_path(1, edge_hex)
cat("   Path contains arc:", grepl(" A ", path_hex), "\n")  # Verify path output
```

**Benefits**:
1. **Layer isolation**: Test geometry → edge generation → path building → rendering separately
2. **Quick feedback**: Script runs in seconds vs full test suite (60+ seconds)
3. **Clear output**: Human-readable diagnostics pinpoint exact failure layer
4. **Reusable**: Can be converted to formal tests once bug is fixed

**Insight**: The bug was found in layer 4 (path building) - the geometry and edge generation were correct (`type="circle"`, `edge_type="radial"`), but the path builder wasn't using SVG arc commands.

**Lesson**: When a parameter "doesn't work", test each processing layer independently rather than only checking input and output.

### 25. Segment-Level Fusion Data Preservation in Positioning (2025-12-11)

**Context**: In concentric puzzles with fusion, piece 7's OUTER edge touches both piece 18 and piece 19. When using `ALL-18` fusion (all pieces except 18), the segment toward piece 18 should be SOLID (external edge of meta-piece), but was being rendered DASHED.

**Problem (Issue #52 follow-up)**: The segment-level fusion data (`outer_segments_mixed`, `fused_edge_segments`, `inner_radius`, `outer_radius`) was being computed correctly in `apply_fusion_to_pieces()`, but **lost during positioning** when `apply_piece_positioning()` created new piece objects without preserving these fields.

**Root Cause Analysis**:
1. `apply_fusion_to_pieces()` correctly computes segment-level fusion data (lines 840-882 in `unified_piece_generation.R`)
2. `apply_piece_positioning()` is called AFTER fusion but creates NEW piece objects (lines 250-259 in `piece_positioning.R`)
3. The new piece objects only preserved `fused_edges` and `fused_neighbor_ids`, NOT the segment-level fields
4. Without `outer_segments_mixed = TRUE`, the renderer (Pass 3.5) skipped segment-level rendering entirely

**Fix Applied** (`R/piece_positioning.R` lines 250-264 for concentric, lines 451-465 for hexagonal):
```r
list(
  id = piece$id,
  path = new_path,
  center = new_center,
  ring_pos = piece$ring_pos,
  type = piece$type,
  fusion_group = if (!is.null(piece$fusion_group)) piece$fusion_group else NA,
  fused_edges = piece$fused_edges,
  fused_neighbor_ids = piece$fused_neighbor_ids,
  # Segment-level fusion data for many-to-one OUTER edges
  outer_segments_mixed = piece$outer_segments_mixed,
  fused_edge_segments = piece$fused_edge_segments,
  inner_radius = piece$inner_radius,
  outer_radius = piece$outer_radius
)
```

**Secondary Fix** (`R/adjacency_api.R` lines 853-860): Also added complementary INNER edge marking for each fused outer neighbor to ensure correct edge-level fusion status.

**Key Insight**: When transforming piece objects through pipeline stages, ALL metadata fields must be explicitly preserved - R does not automatically copy fields when creating new lists.

**Verification**:
- Segment 1 (toward piece 18): `fused=FALSE` → renders SOLID ✓
- Segment 2 (toward piece 19): `fused=TRUE` → renders DASHED ✓

**Tests Added**: 2 new tests in `tests/testthat/test-segment-fusion.R`:
1. `"excluded piece's INNER edge is NOT marked fused"` - verifies ALL-18 case
2. `"complementary edges marked correctly for many-to-one"` - verifies ALL-19 case

### 26. Hexagonal Topo-to-Geo Formula Must Match Piece Generation Offset (2025-12-11)

**Context**: Hexagonal fusion strokes were being applied to the wrong geometric edges. User reported "stroke mismatch" when comparing default vs ALL-18 fusion puzzles.

**Problem (Issue #53)**: The topology-to-geometry side mapping formula converts neighbor direction angles to geometric side numbers (0-5). The formula was:
```r
geo_side <- round((30 - dir_to_neighbor) / 60) %% 6
```

This formula assumed a **pointed-top hexagon** with vertices starting at 30° (base_offset = π/6). However, the actual hexagonal piece generation in `hexagonal_edge_generation_fixed.R` uses:
```r
base_offset <- 0  # Flat-top hexagon: vertices at 0°, 60°, 120°, 180°, 240°, 300°
```

**Root Cause Analysis**:

| Direction | Old Formula `(30-dir)/60` | Correct Formula `(dir-30)/60` |
|-----------|---------------------------|-------------------------------|
| 30° (NE)  | Side 0 ✓ | Side 0 ✓ |
| 90° (N)   | **Side 5** ✗ | **Side 1** ✓ |
| 150° (NW) | **Side 4** ✗ | **Side 2** ✓ |
| -150° (SW)| Side 3 ✓ | Side 3 ✓ |
| -90° (S)  | **Side 2** ✗ | **Side 4** ✓ |
| -30° (SE) | **Side 1** ✗ | **Side 5** ✓ |

The subtraction order was wrong: `(30 - dir)` vs `(dir - 30)`.

**Fix Applied** (`R/unified_piece_generation.R` lines 321-325 and 793-797):
```r
# OLD (wrong for flat-top hexagon):
geo_side <- round((30 - dir_to_neighbor) / 60) %% 6

# NEW (correct for flat-top hexagon with base_offset=0):
geo_side <- round((dir_to_neighbor - 30) / 60) %% 6
```

**Flat-Top Hexagon Geometry** (base_offset = 0):
```
Vertices at: 0°, 60°, 120°, 180°, 240°, 300°

        Side 1 (90°)
           ___
    Side 2/   \Side 0
   (150°) \___/ (30°)
    Side 3/   \Side 5
   (-150°)\___/(-30°)
        Side 4 (-90°)

Formula: Side N faces direction (30 + N*60)°
```

**Key Insight**: When implementing coordinate system mappings, **always verify the base offset** of the actual geometry generation code. Comments or formulas inherited from reference implementations may assume different coordinate systems.

**Verification**:
- Piece 7's edge toward piece 18 (excluded from fusion): `fused_edges[["3"]] = FALSE` ✓
- All other edges toward pieces in fusion group: `fused_edges = TRUE` ✓
- All 1374 tests pass

**Files Changed**:
- `R/unified_piece_generation.R`: Fixed formula in 2 locations
- `tests/testthat/test-fusion.R`: Updated expected values for flat-top hexagon geometry

### 27. SVG Inline HTML Embedding - XML Declaration Issue (2025-12-14, Issue #58)

**Context**: Noise fills (procedural noise textures) rendered correctly when SVG files were opened directly in a browser, but failed to render in the Shiny app preview.

**Problem**: The SVG header included `<?xml version="1.0" encoding="UTF-8"?>` at the start. This XML declaration is problematic when embedding SVG inline in HTML5:

```xml
<!-- WRONG for inline HTML -->
<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" ...>

<!-- CORRECT for inline HTML -->
<svg xmlns="http://www.w3.org/2000/svg" ...>
```

**Root Cause**: HTML5 parsers do not understand XML processing instructions. When Shiny's `HTML()` function embeds SVG inline, the `<?xml?>` declaration can cause:
- The browser to treat content as text rather than parsed markup
- Silent stripping of the declaration, potentially breaking pattern rendering
- Quirks mode behavior in some browsers

**Evidence**:
- W3C SVG in HTML spec: "XML processing instructions are not supported in HTML"
- Standalone SVG files (with XML declaration) rendered correctly
- Same SVG embedded via `HTML()` in Shiny failed to render noise patterns

**Solution**: Added `inline` parameter to control XML declaration:

```r
# build_svg_header() now accepts inline parameter
build_svg_header <- function(canvas_size, canvas_offset = NULL, inline = FALSE) {
  # XML declaration only for standalone files, not inline HTML
  xml_declaration <- if (inline) "" else '<?xml version="1.0" encoding="UTF-8"?>\n'
  # ...
}

# render_puzzle_svg() propagates inline parameter
render_puzzle_svg <- function(..., inline = FALSE) {
  svg_header <- build_svg_header(..., inline = inline)
}
```

**Implementation Details**:

| Context | `inline` Value | Has XML Declaration |
|---------|----------------|---------------------|
| Shiny preview | `TRUE` | No |
| File downloads | `FALSE` (default) | Yes |
| `generate_puzzle()` | `FALSE` (default) | Yes |

**Key Insight**: SVG has two valid rendering contexts with different requirements:
1. **Standalone files** (`.svg`): XML declaration is valid and recommended
2. **Inline HTML5**: XML declaration must be omitted

The same SVG content requires different headers depending on how it will be consumed. Design APIs with a parameter to handle both cases.

**Secondary Fix**: WYSIWYG download handler now prepends XML declaration:
```r
# SVG was generated with inline=TRUE for preview, add declaration for file
svg_standalone <- paste0('<?xml version="1.0" encoding="UTF-8"?>\n', svg_content())
```

**Namespaces Preserved**: Both modes retain required namespaces:
- `xmlns="http://www.w3.org/2000/svg"` - Required for SVG rendering
- `xmlns:xlink="http://www.w3.org/1999/xlink"` - Required for `xlink:href` in noise pattern images

**Files Changed**:
- `R/unified_renderer.R`: Added `inline` parameter to `build_svg_header()` and `render_puzzle_svg()`
- `inst/shiny-app/app.R`: Use `inline = TRUE` for preview, add XML declaration for downloads
- `tests/testthat/test-noise_fills.R`: Added 10 tests for inline parameter

**References**:
- [W3C SVG in HTML](https://www.w3.org/Graphics/SVG/WG/wiki/SVG_in_HTML)
- [CSS-Tricks: 6 Common SVG Fails](https://css-tricks.com/6-common-svg-fails-and-how-to-fix-them/)

### 28. SVG Pattern Content Units - Browser Rendering (2025-12-15)

**Context**: After fixing the XML declaration issue (Insight #27), noise backgrounds were still rendering as blank in browsers (Chrome and Edge), even though the SVG structure appeared correct.

**Problem**: The noise pattern used `patternUnits="objectBoundingBox"` with `width="1" height="1"`, which correctly means "100% of the bounding box". However, the `<image>` element inside the pattern also had `width="1" height="1"`, which was being interpreted as **1 pixel** because `patternContentUnits` defaults to `userSpaceOnUse`.

```xml
<!-- BROKEN: Image renders as 1x1 pixel -->
<pattern id="bgNoisePattern" patternUnits="objectBoundingBox" width="1" height="1">
  <image xlink:href="data:image/png;base64,..." width="1" height="1"/>
</pattern>

<!-- FIXED: Image fills entire pattern -->
<pattern id="bgNoisePattern" patternUnits="objectBoundingBox"
         patternContentUnits="objectBoundingBox" width="1" height="1">
  <image xlink:href="data:image/png;base64,..." width="1" height="1"/>
</pattern>
```

**Root Cause**: SVG patterns have TWO separate coordinate systems:

| Attribute | Default Value | Controls |
|-----------|---------------|----------|
| `patternUnits` | `objectBoundingBox` | Pattern tile dimensions (width/height on `<pattern>`) |
| `patternContentUnits` | `userSpaceOnUse` | Content dimensions (width/height on children) |

When `patternUnits="objectBoundingBox"`:
- Pattern's `width="1"` = 100% of bounding box ✓
- But image's `width="1"` = 1 pixel (userSpaceOnUse default) ✗

**Solution**: Explicitly set `patternContentUnits="objectBoundingBox"` so that the image's `width="1" height="1"` is also interpreted as 100% of the bounding box.

**Key Insight**: SVG pattern coordinate systems are independently configurable. When using relative coordinates (`objectBoundingBox`) for the pattern, you likely want relative coordinates for the content too. Always explicitly set both attributes to avoid browser defaults causing unexpected behavior.

**Debugging Approach**:
1. Tests passed (they checked SVG structure, not browser rendering)
2. User confirmed: "background is blank (tested in chrome and edge)"
3. Inspected browser DevTools - pattern existed but image was 1x1 pixel
4. Traced to `patternContentUnits` default behavior

**Files Changed**:
- `R/noise_fills.R`: Added `patternContentUnits="objectBoundingBox"` to pattern definition

**References**:
- [MDN: patternContentUnits](https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/patternContentUnits)
- [SVG 1.1 Spec: Patterns](https://www.w3.org/TR/SVG11/pservers.html#PatternElementPatternContentUnitsAttribute)

### 29. ggplot2 theme_void() Causes Blank PNG in Quarto/knitr (2025-12-19)

**Context**: Quarto documentation was rendering blank PNG images (exactly 3170 bytes) for all puzzle plots using `theme_void()` combined with `guide = "none"`.

**Problem**: The plots rendered correctly in RStudio but produced empty images in Quarto/knitr output.

**Root Cause Analysis**:
```r
# Inspecting theme_void() internals:
t <- theme_void()
t$plot.background$fill  # Returns "" (empty string, not "white")
t$plot.margin           # Returns margin(0, 0, 0, 0, "pt")
```

When `theme_void()` is combined with `guide = "none"`, there are **no visible theme elements** for knitr's figure capture mechanism to detect. The rendering system sees an "empty" plot and produces a minimal blank PNG.

**Solution**: Created `theme_puzzle()` that extends `theme_void()` with explicit visible elements:
```r
theme_puzzle <- function(base_size = 11, base_family = "",
                         background = "white",
                         margin = ggplot2::margin(2, 2, 2, 2, "pt")) {
  ggplot2::theme_void(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(
      plot.background = ggplot2::element_rect(
        fill = if (is.na(background)) "transparent" else background,
        colour = NA
      ),
      plot.margin = margin,
      complete = TRUE
    )
}
```

**Key Insight**: `theme_void()` is designed for pure data visualization where the plot background is irrelevant. For Quarto/knitr rendering, plots need at least one visible theme element (background or margin) to trigger proper figure capture.

**Additional Fix Required**: In development mode, Quarto setup scripts must use `devtools::load_all()` instead of `library()` to ensure newly created functions are available:
```r
# quarto/_setup.R
if (file.exists("DESCRIPTION") && requireNamespace("devtools", quietly = TRUE)) {
  suppressMessages(devtools::load_all(path = ".", quiet = TRUE))
} else if (requireNamespace("jigsawR", quietly = TRUE)) {
  suppressPackageStartupMessages(library(jigsawR))
}
```

**Files Changed**:
- Created: `R/theme_puzzle.R`
- Modified: `quarto/_setup.R` (dev mode detection)
- Modified: All 13 `.qmd` files (replaced `theme_void()` with `theme_puzzle()`)

**Verification**: 98 PNG files generated, 0 blank images (all 29KB-80KB vs previous 3170 bytes).

---

### Insight #22: Independent UI Controls for Related Options (2025-12-11)

**Problem**: The Shiny app had a shared `color_palette` dropdown that controlled both stroke and fill colors when using palette mode. This created confusion:
- Users couldn't have different palettes for stroke vs fill
- The "black" palette was redundant (same as solid black color)
- UI labels were inconsistent ("Use Palette" vs "Solid Color" vs "None")

**Solution**: Independent palette controls for each styling element:

1. **Consistent Labels**: Standardized across all radio button groups:
   - Fill: "None", "Solid", "Palette", "Gradient"
   - Stroke: "None", "Solid", "Palette"
   - Background: "None", "Solid", "Gradient"

2. **Independent Controls**: Each element has its OWN settings:
   ```
   Stroke Color:  [None] [Solid] [Palette]
                  └─ stroke_color (color picker, when Solid)
                  └─ stroke_palette + stroke_palette_invert (when Palette)

   Piece Fill:    [None] [Solid] [Palette] [Gradient]
                  └─ fill_color (color picker, when Solid)
                  └─ fill_palette + fill_palette_invert (when Palette)
                  └─ piece_gradient_center/middle/edge (when Gradient)
   ```

3. **Config Structure** (`inst/config.yml`):
   ```yaml
   styling:
     stroke_color_type: "solid"
     stroke_color: "#000000"
     stroke_palette: "viridis"
     stroke_palette_invert: false
     fill_type: "solid"
     fill_color: "#a1a1a1"
     fill_palette: "magma"
     fill_palette_invert: false
   ```

**Key Insight**: When UI has related but independent options, give each its own controls rather than sharing. This:
- Enables more flexible combinations (e.g., viridis stroke + magma fill)
- Makes the relationship between controls explicit
- Simplifies the code (no conditional logic to determine which context applies)

**Files Changed**:
- `inst/config.yml`: Added independent palette settings, removed "black" palette
- `inst/shiny-app/app.R`: UI restructuring, server logic updates

---

### Insight #23: Unified Single-Piece Rendering API (2025-12-11)

**Problem (Issue #51)**: The individual pieces download handler in the Shiny app used three separate legacy functions:
- `generate_individual_pieces()` for rectangular
- `generate_hexagonal_individual_pieces()` for hexagonal
- `generate_concentric_individual_pieces()` for concentric

Each function had different parameters, features, and output formats. Adding new styling features required updating three separate code paths.

**Solution**: Created `render_single_piece_svg()` function in `R/unified_renderer.R` that works with ANY puzzle type:

```r
render_single_piece_svg <- function(piece,
                                     fill = "none",
                                     stroke_color = "black",
                                     stroke_width = 1.5,
                                     opacity = 1.0,
                                     background = "none",
                                     padding = 0.15,
                                     show_label = FALSE,
                                     label_color = "black",
                                     label_size = NULL)
```

**Key Design Decisions**:

1. **Takes piece object, not puzzle type**: Works with `result$pieces[[i]]` from ANY puzzle type
2. **Computes viewBox from path bounds**: Uses `calculate_piece_bounds()` to extract coordinates from SVG path
3. **Consistent styling across types**: Fill, stroke, opacity, labels all work identically
4. **Self-contained output**: Each piece is a complete SVG with XML declaration, viewBox, and proper sizing

**ViewBox Calculation**:
```r
calculate_piece_bounds <- function(path) {
  segments <- parse_svg_path(path)
  # Extract all x,y coordinates from M, L, C, A segments
  x_coords <- c(...)
  y_coords <- c(...)
  list(
    min_x = min(x_coords), max_x = max(x_coords),
    min_y = min(y_coords), max_y = max(y_coords),
    width = max_x - min_x, height = max_y - min_y
  )
}
```

**Before/After Comparison**:
| Aspect | Before (Legacy) | After (Unified) |
|--------|-----------------|-----------------|
| Lines of code | ~300 (3 functions) | ~165 (1 function) |
| Feature parity | Inconsistent | Complete |
| Adding new style | 3 code changes | 1 code change |
| Test coverage | Minimal | 12 dedicated tests |

**Insight**: When you have type-specific functions that do similar things, look for the **common abstraction**. Here, all three functions ultimately needed to render a single piece with styling - the piece object itself already contains all type-specific information in its `path` field.

**Files Changed**:
- `R/unified_renderer.R`: Added `render_single_piece_svg()`, `calculate_piece_bounds()`
- `inst/shiny-app/app.R`: Refactored download handler (~300 → ~165 lines)
- `tests/testthat/test-individual-pieces.R`: Added 12 new tests

---

### Insight #24: Reuse Positioned Results Instead of Regenerating (2025-12-11)

**Context**: The individual pieces download handler was regenerating puzzles using legacy functions instead of using the already-generated `positioned_result()` from the reactive chain.

**Anti-pattern Identified**:
```r
# OLD (wasteful): Regenerate puzzle just to get pieces
individual_result <- generate_hexagonal_individual_pieces(
  seed = input$seed,
  rings = input$rings,
  ...
)
pieces <- individual_result$pieces
```

**Better Pattern**:
```r
# NEW (efficient): Reuse already-computed positioned pieces
pos <- positioned_result()  # Already computed by reactive chain
pieces <- pos$pieces        # Same pieces with all fusion/styling applied
```

**Benefits**:
1. **No redundant computation**: Puzzle already generated when user views it
2. **Consistent results**: Download matches exactly what user sees
3. **All features work**: Fusion, offset, styling all preserved
4. **Simpler code**: No need to recreate parameter lists

**Key Insight**: In Shiny apps, reactive values represent a **computation cache**. When multiple outputs need the same data, let them share the reactive result rather than each computing independently.

**Verification**: The download now produces pieces with:
- Fusion groups applied (if configured)
- Correct positioning (from offset slider)
- All styling options (fill/stroke palettes, gradients)
- Piece metadata intact (id, center, type, ring_pos)

---

### Insight #25: UI Separation by Parameter Scope (2025-12-11)

**Problem**: The Shiny app had fusion-related controls mixed together in the Styling panel:
- `fusion_groups` (PILES notation) - defines WHICH pieces to fuse
- `fusion_style` and `fusion_opacity` - defines HOW fused edges look

Users were confused because changing `fusion_groups` required clicking "Generate Puzzle", while style changes updated instantly.

**Root Cause**: Two different parameter scopes were conflated:
1. **Generation-time parameters**: Affect puzzle geometry, require regeneration
2. **Render-time parameters**: Affect SVG styling only, can update reactively

**Solution**: Separate UI controls by their scope:

| Control | Panel | Scope | Update Mechanism |
|---------|-------|-------|------------------|
| `fusion_groups` | Settings | Generation-time | Requires "Generate Puzzle" click |
| `fusion_style` | Styling | Render-time | Reactive (instant) |
| `fusion_opacity` | Styling | Render-time | Reactive (instant) |

**Implementation**:
```
Settings Panel:
├── Puzzle Type, Grid, Size...
├── Piece Fusion              ← NEW section
│   └── fusion_groups input
├── Seed
└── [Generate Puzzle] button

Styling Panel:
├── Piece Shape (tabsize, jitter)
├── Internal Edges (Fusion)   ← Renamed section
│   ├── fusion_style
│   └── fusion_opacity
├── Stroke, Fill, Background...
```

**Key Insight**: In reactive UIs, group controls by their **data flow**, not by their **conceptual domain**. Even though fusion_groups and fusion_style are both "fusion-related", they belong in different panels because they have different update semantics.

**General Rule for Shiny Apps**:
- **Settings panel**: Parameters that trigger expensive computation (puzzle generation)
- **Styling panel**: Parameters that only affect rendering (SVG attributes, colors, opacity)

**Files Changed**:
- `inst/shiny-app/app.R`: Moved `fusion_groups` to Settings, renamed section headers

---

### Insight #26: PNG Download with Graceful Fallback Chain (2025-12-11)

**Problem (Issue #25)**: Users needed PNG export for presentations, documents, and image editing, but the Shiny app only supported SVG downloads.

**Solution**: Implement PNG download with a fallback chain of conversion methods:

```r
# Priority order for SVG-to-PNG conversion
1. rsvg::rsvg_png()           # Fastest, most reliable
2. magick::image_read_svg()   # Good fallback, widely available
3. (Future: Inkscape CLI)     # System tool fallback
4. (Future: ImageMagick CLI)  # System tool fallback
```

**Implementation Pattern**:
```r
output$download_png <- downloadHandler(
  filename = function() { paste0(prefix, ".png") },
  content = function(file) {
    svg <- svg_content()

    # Show progress (PNG conversion can take a moment)
    progress_id <- showNotification("Converting...", duration = NULL)

    tryCatch({
      # Calculate dimensions maintaining aspect ratio
      canvas <- positioned_result()$canvas_size
      aspect_ratio <- canvas[1] / canvas[2]
      # ... calculate width_px, height_px (max 2000px)

      # Try rsvg first
      success <- FALSE
      if (requireNamespace("rsvg", quietly = TRUE)) {
        tryCatch({
          png_data <- rsvg::rsvg_png(temp_svg, width = width_px, height = height_px)
          writeBin(png_data, temp_png)
          success <- TRUE
        }, error = function(e) { })
      }

      # Fall back to magick
      if (!success && requireNamespace("magick", quietly = TRUE)) {
        tryCatch({
          img <- magick::image_read_svg(temp_svg, width = width_px, height = height_px)
          magick::image_write(img, temp_png, format = "png")
          success <- TRUE
        }, error = function(e) { })
      }

      # Handle result
      if (success) {
        file.copy(temp_png, file)
        showNotification("PNG ready!", type = "message")
      } else {
        showNotification("Install rsvg or magick package", type = "error")
      }
    }, finally = {
      removeNotification(progress_id)
      unlink(c(temp_svg, temp_png))
    })
  },
  contentType = "image/png"
)
```

**Key Design Decisions**:
1. **Maintain aspect ratio**: Calculate dimensions from canvas size, max 2000px
2. **Progress feedback**: Show notification during conversion (can take 1-2 seconds)
3. **Graceful degradation**: Clear error message if no conversion tool available
4. **Clean temp files**: Always clean up in `finally` block

**User Experience**:
- Button: "Current View (PNG)" with file-image icon
- Help text explains package requirements
- Works identically to SVG download (same content, different format)

**Files Changed**:
- `inst/shiny-app/app.R`: Added `download_png` handler and UI button

---

### Insight #27: Label Centering via Bounding Box Geometry (2025-12-11)

**Context**: Piece labels must appear centered within each piece regardless of:
- Puzzle type (rectangular, hexagonal, concentric)
- Layout mode (complete vs separated)
- Fusion groups (labels on all pieces, even fused ones)
- Transformations (warp, truncation, circular border)

**Problem**: Initial implementations used `piece$center` which is the **topological center** (center of grid cell), not the **geometric center** (center of actual rendered shape). This caused misalignment for:
- Pieces with asymmetric tabs
- Warped hexagonal pieces
- Concentric trapezoids (wider at outer edge)

**Solution**: Calculate label position from the **bounding box center** of the actual SVG path:

```r
calculate_path_bounding_box_center <- function(path) {
  segments <- parse_svg_path(path)

  xs <- c()
  ys <- c()

  for (seg in segments) {
    if (seg$type %in% c("M", "L")) {
      xs <- c(xs, seg$x)
      ys <- c(ys, seg$y)
    } else if (seg$type == "C") {
      # Include bezier control points for accurate bounds
      xs <- c(xs, seg$cp1x, seg$cp2x, seg$x)
      ys <- c(ys, seg$cp1y, seg$cp2y, seg$y)
    } else if (seg$type == "A") {
      xs <- c(xs, seg$x)
      ys <- c(ys, seg$y)
    }
  }

  # Bounding box center
  cx <- (min(xs) + max(xs)) / 2
  cy <- (min(ys) + max(ys)) / 2

  return(c(x = cx, y = cy))
}
```

**SVG Text Centering**:
```xml
<text x="123.45" y="67.89"
      text-anchor="middle"           <!-- Horizontal centering -->
      dominant-baseline="central"    <!-- Vertical centering -->
      font-family="sans-serif"
      font-size="12"
      font-weight="bold">
  5
</text>
```

**Why Bounding Box Center Works**:
1. **Matches visual perception**: The center of the bounding box is where humans expect the label
2. **Matches gradient center**: SVG `objectBoundingBox` gradients center at the same point
3. **Invariant to positioning**: Works for both complete and separated layouts
4. **Handles all piece shapes**: Rectangular, hexagonal, trapezoidal (concentric)

**Verification Results**:
| Configuration | Max Delta | Result |
|---------------|-----------|--------|
| All puzzle types | 0.007mm | ✅ Perfect |
| With fusion groups | 0.007mm | ✅ Perfect |
| With separation (offset>0) | 0.007mm | ✅ Perfect |
| With warp/trunc | 0.007mm | ✅ Perfect |
| render_single_piece_svg | 0.007mm | ✅ Perfect |

(Delta of 0.007mm is floating-point precision from `sprintf("%.2f")`)

**Key Insight**: For visual centering, always use the **rendered geometry** (bounding box), not the **logical position** (grid coordinates). This ensures labels appear centered regardless of piece shape irregularities.

**Files Involved**:
- `R/unified_renderer.R`: `calculate_path_bounding_box_center()`, `render_piece_label()`

---

### Insight #28: Canvas Bounds Must Use Actual Path Geometry (2025-12-12)

**Problem**: Rectangular puzzles with separation (offset > 0) and grid layout showed clipped pieces at the edges. The preview cut off bezier tabs and stroke widths.

**Root Cause**: `apply_rect_positioning()` used **formula-based canvas calculations** that computed theoretical bounds from grid positions:

```r
# OLD (broken) approach - theoretical calculation
canvas_width <- params$size[1] + (n_gaps_x + 2) * offset
canvas_height <- params$size[2] + (n_gaps_y + 2) * offset
```

This calculation assumes pieces are perfect rectangles and doesn't account for:
1. Bezier tab protrusions extending beyond piece boundaries
2. Stroke width adding visual size
3. Actual transformed path geometry

**Solution**: Use `calculate_pieces_bounds()` to measure the **actual transformed SVG paths**, just like hexagonal and concentric puzzles already did:

```r
# NEW (correct) approach - measure actual geometry
bounds <- calculate_pieces_bounds(transformed_pieces, fallback_fn = function() {
  # Fallback to theoretical if parsing fails
  list(min_x = ..., max_x = ..., min_y = ..., max_y = ...)
})

# Add margin for stroke and visual clarity
stroke_margin <- max(piece_width, piece_height) * 0.1 + offset
canvas_width <- (bounds$max_x + stroke_margin) - (bounds$min_x - stroke_margin)
canvas_height <- (bounds$max_y + stroke_margin) - (bounds$min_y - stroke_margin)
```

**Key Insight**: For SVG viewBox calculations, always measure the **rendered geometry** (actual paths with all their curves and protrusions), not the **logical geometry** (theoretical grid positions). This is the same principle as Insight #27 for label centering.

**Pattern**: All three puzzle types now use consistent bounds calculation:
- `apply_hex_positioning()`: Uses `calculate_pieces_bounds()`
- `apply_concentric_positioning()`: Uses `calculate_pieces_bounds()`
- `apply_rect_positioning()`: Now uses `calculate_pieces_bounds()` ✅ (was formula-based)

**Files Involved**:
- `R/piece_positioning.R`: `apply_rect_positioning()`, `calculate_pieces_bounds()`

---

### Insight #29: Shiny Parameter Scope - Reactive vs Generate-Click (2025-12-12)

**Problem**: When layout controls (grid/repel) were in the Styling panel, changing them triggered immediate reactive updates. The user requested that layout only be computed when "Generate" is clicked, not on every slider change.

**Pattern Identified**: Parameters fall into two categories by when they should take effect:

| Category | When Applied | Storage | Examples |
|----------|--------------|---------|----------|
| **Generation Parameters** | On Generate click only | `base_settings()` | puzzle type, grid size, seed, **layout** |
| **Rendering Parameters** | Immediately (reactive) | `input$*` directly | colors, stroke width, opacity, labels |

**Implementation Pattern**:

1. **Generation Parameters** (stored in `base_settings()` on Generate click):
```r
observeEvent(input$generate, {
  base_settings(list(
    type = input$puzzle_type,
    seed = input$seed,
    layout = input$layout,          # Captured at Generate time
    repel_margin = input$repel_margin,
    repel_max_iter = input$repel_max_iter
  ))
})
```

2. **Rendering Parameters** (read directly in reactive observer):
```r
observe({
  # Read generation params from stored settings
  settings <- base_settings()
  layout_val <- settings$layout

  # Read rendering params directly (reactive)
  stroke_color <- input$stroke_color
  fill_color <- input$fill_color

  # Generate puzzle...
})
```

**UI Organization**:
- **Settings Panel**: Generation parameters (puzzle type, grid, seed, layout)
- **Styling Panel**: Rendering parameters (colors, stroke, opacity, labels)

**Key Insight**: The distinction between "generation-time" and "render-time" parameters maps directly to whether users expect immediate visual feedback (colors → yes) or expect to control when computation happens (layout algorithm → no, too expensive).

**Files Involved**:
- `inst/shiny-app/app.R`: UI organization, `base_settings()` handling
- `inst/config.yml`: Default values for layout parameters

---

### Insight #30: Procedural Noise Fills via Embedded PNG Patterns (2025-12-12)

**Problem (Issue #57)**: Users wanted procedural noise textures for piece fills and backgrounds instead of just solid colors or predefined gradients. Noise fills provide organic, natural-looking textures that enhance visual interest.

**Solution**: Implemented noise fill support using the `ambient` package for noise generation, with textures embedded directly in the SVG as base64-encoded PNG images in SVG pattern elements.

**Architecture**:
```
noise_fill_spec()                    Create specification list
       ↓
generate_noise_texture()             Generate PNG texture via ambient package
       ↓                             (perlin, simplex, worley, cubic, value, white)
base64enc::base64encode()            Encode as base64 string
       ↓
render_noise_background()            Create SVG with pattern for backgrounds
render_noise_piece_fill_defs()       Create SVG defs for piece fills
       ↓
<pattern> + <image>                  Embedded in SVG
```

**Key Implementation Details**:

1. **Noise Specification Function** (`R/noise_fills.R`):
```r
noise_fill_spec <- function(
  noise_type = "simplex",     # perlin, simplex, worley, cubic, value, white
  frequency = 0.03,           # Controls noise scale
  color_low = "#2d2d44",      # Color at noise value 0
  color_high = "#8888aa",     # Color at noise value 1
  seed = NULL,                # For reproducibility
  octaves = 4,                # Fractal detail layers
  lacunarity = 2,             # Frequency multiplier per octave
  gain = 0.5                  # Amplitude multiplier per octave
)
# Returns: list(type = "noise", noise_type = ..., ...)
```

2. **Texture Generation** (`generate_noise_texture()`):
```r
# Generate noise grid using ambient package
noise_grid <- switch(noise_type,
  "perlin"  = ambient::noise_perlin(c(height, width), ...),
  "simplex" = ambient::noise_simplex(c(height, width), ...),
  "worley"  = ambient::noise_worley(c(height, width), ...),
  ...
)

# Normalize to [0,1] and map to colors
noise_normalized <- (noise_grid - min) / (max - min)
r <- col_low[1] + (col_high[1] - col_low[1]) * noise_normalized
g <- col_low[2] + (col_high[2] - col_low[2]) * noise_normalized
b <- col_low[3] + (col_high[3] - col_low[3]) * noise_normalized

# Create PNG and encode as base64
png::writePNG(array(c(r, g, b), dim = c(height, width, 3)), raw())
base64enc::base64encode(raw_bytes)
```

3. **SVG Pattern Structure** (backgrounds use `objectBoundingBox`, piece fills use `userSpaceOnUse`):
```xml
<defs>
  <pattern id="bgNoisePattern" width="1" height="1"
           patternUnits="objectBoundingBox">
    <image href="data:image/png;base64,iVBORw0K..."
           width="400" height="300"
           preserveAspectRatio="none"/>
  </pattern>
</defs>
<rect width="100%" height="100%" fill="url(#bgNoisePattern)"/>
```

**Pattern Units**:
- `objectBoundingBox`: Pattern scales to fill the bounding box (backgrounds)
- `userSpaceOnUse`: Pattern uses absolute coordinates (piece fills need this for seamless tiles)

**Type Detection** (`is_noise_fill_spec()`):
```r
is_noise_fill_spec <- function(spec) {
  is.list(spec) && !is.null(spec$type) && spec$type == "noise"
}
```

**Integration Points**:
- `render_puzzle_svg()`: Checks for noise specs, calls render functions
- `render_background()`: Routes noise specs to `render_noise_background()`
- Shiny app: radioButtons for "noise" fill type, conditionalPanel for noise parameters

**Dependencies**:
- `ambient`: Noise generation (perlin, simplex, worley, etc.)
- `png`: Write PNG image data
- `base64enc`: Encode PNG as base64 string
- All in Suggests (optional) - functions check availability at runtime

**Testing**:
- `tests/testthat/test-noise_fills.R`: 20+ tests covering all noise types and integration
- `inst/debug/user_noise_diagnostic.R`: User-facing diagnostic script

**Key Insight**: Embedding procedural textures as base64 PNGs in SVG patterns avoids browser compatibility issues with SVG filters while providing full control over noise parameters. The tradeoff is larger file sizes (base64 ~33% overhead), but this is acceptable for artistic puzzle outputs.

**Files Created/Modified**:
- `R/noise_fills.R` (new): All noise fill functions
- `R/unified_renderer.R`: Integration with rendering pipeline
- `inst/shiny-app/app.R`: UI controls for noise parameters
- `inst/config.yml`: Default noise settings
- `tests/testthat/test-noise_fills.R` (new): Test suite

---

### Insight #31: Tessellation-Based Puzzle Types Architecture (2025-12-15, Issues #41, #42)

**Context**: Added two new puzzle types based on computational geometry tessellations:
- **Voronoi Puzzles** (Issue #42): Fermat spiral point distribution → Voronoi tessellation
- **Random Shape Puzzles** (Issue #41): Constrained Delaunay triangulation with arbitrary boundary polygons

**Architecture Pattern**: Both types share a common tessellation edge generation layer:

```
┌─────────────────────────────────────────────────────────────────┐
│                    generate_puzzle(type="voronoi"|"random")     │
└───────────────────────────────┬─────────────────────────────────┘
                                │
┌───────────────────────────────▼─────────────────────────────────┐
│              generate_pieces_internal() dispatcher              │
│  ├─ type="voronoi" → generate_voronoi_pieces_internal()        │
│  └─ type="random"  → generate_random_pieces_internal()         │
└───────────────────────────────┬─────────────────────────────────┘
                                │
┌───────────────────────────────▼─────────────────────────────────┐
│            Shared: tessellation_edge_generation.R               │
│  - generate_bezier_edge_for_polygon(): 9-point Bezier tabs     │
│  - Edge map pattern for storing shared edges between cells     │
│  - Cell adjacency detection via shared vertices                 │
└─────────────────────────────────────────────────────────────────┘
```

**Key Implementation Details**:

1. **Voronoi Puzzle** (`R/voronoi_puzzle.R`):
   - Uses `deldir` package for Voronoi tessellation
   - Point distributions: `fermat` (spiral), `uniform` (random), `jittered` (grid + noise)
   - Fermat spiral formula: `r = c * sqrt(i)`, `θ = i * golden_angle`
   - Cells are arbitrary convex polygons with 3-8+ sides

2. **Random Shape Puzzle** (`R/random_puzzle.R`):
   - Uses `RCDT` package for constrained Delaunay triangulation
   - Base polygon with `n_corner` vertices (3=triangle, 4=rect, 5=pentagon, etc.)
   - Interior points randomly distributed
   - Piece count formula: `~2 * n_interior + n_corner - 2`

3. **RCDT API Details** (Critical for future maintenance):
   ```r
   tri <- RCDT::delaunay(
     vertices,           # n x 2 matrix of point coordinates
     edges = constraint_edges  # Note: parameter is "edges", NOT "constraints"
   )
   # Triangles are in tri$mesh$it (3 x n_triangles), need transposition:
   triangles <- t(tri$mesh$it)  # Now n_triangles x 3
   ```

4. **Edge Map Pattern** (shared between both types):
   ```r
   edge_map <- new.env(hash = TRUE)
   # Key: sorted vertex indices "min_idx,max_idx"
   # Value: list of cell IDs sharing this edge

   for (cell_id in seq_along(cells)) {
     for (edge in cell_edges) {
       key <- make_edge_key(edge$v1, edge$v2)
       if (exists(key, edge_map)) {
         # Shared edge - add Bezier tab
         edge_map[[key]]$cells <- c(edge_map[[key]]$cells, cell_id)
       } else {
         # New edge - boundary (no tab) or first occurrence
         edge_map[[key]] <- list(cells = cell_id, ...)
       }
     }
   }
   ```

**Bezier Tab Generation for Arbitrary Polygons**:
- 9 control points defining 3 cubic Bezier curves
- Tab direction alternates based on edge key hash (deterministic)
- Tab size scaled relative to edge length
- Works for any polygon edge, not just rectangular grid

**Shiny App Integration**:
- New radio button choices: "Voronoi", "Random"
- Conditional panels for type-specific parameters
- Value boxes display type-specific info (cells, distribution, base shape)
- Package availability checks with user-friendly warnings

**Dependencies** (in DESCRIPTION Suggests):
- `deldir`: Voronoi tessellation
- `RCDT`: Constrained Delaunay triangulation

**Files Created/Modified**:
- `R/voronoi_puzzle.R` (new): Voronoi puzzle implementation
- `R/random_puzzle.R` (new): Random shape puzzle implementation
- `R/tessellation_edge_generation.R` (new): Shared edge generation utilities
- `R/unified_piece_generation.R`: Dispatcher for new types
- `R/jigsawR_clean.R`: Added `point_distribution`, `n_corner` parameters
- `inst/shiny-app/app.R`: UI panels and server logic for new types
- `DESCRIPTION`: Added deldir, RCDT to Suggests

---

### Insight #32: Min/Max Tab Size Constraints for Variable-Length Edges (2025-12-15)

**Context**: Tessellation puzzles (Voronoi, Random) have edges of highly variable lengths, unlike rectangular/hexagonal puzzles with uniform edge lengths. This creates visual problems:
- Short edges: Tabs become proportionally too small to see/cut
- Long edges: Tabs become proportionally too large and may overlap

**Problem**: The tab size is calculated as a fraction of edge length (`tabsize / 100`). For a 10mm edge with `tabsize=20`, the tab height is ~6mm (appropriate). For a 100mm edge, it's ~60mm (potentially too large). For a 3mm edge, it's ~0.9mm (too small to cut).

**Solution**: Added `min_tab_size` and `max_tab_size` parameters that clamp absolute tab dimensions:

```r
generate_tessellation_edge <- function(v1, v2, seed, edge_id,
                                        tabsize = 20, jitter = 4,
                                        tab_direction = 1,
                                        min_tab_size = NULL,   # NEW
                                        max_tab_size = NULL) { # NEW
  # Tab height is approximately 3 * t * edge_length
  tab_height <- 3.0 * t * edge_length

  # Clamp up to minimum
  if (!is.null(min_tab_size) && tab_height < min_tab_size) {
    t <- min_tab_size / (3.0 * edge_length)

    # Check if this would make tab too wide for edge
    if (4.0 * t > 0.7) {
      # Edge too short - fall back to straight line
      return(list(forward = sprintf("L %.4f %.4f", v2[1], v2[2]),
                  reverse = sprintf("L %.4f %.4f", v1[1], v1[2]),
                  type = "straight_constrained"))
    }
  }

  # Clamp down to maximum
  if (!is.null(max_tab_size) && tab_height > max_tab_size) {
    t <- max_tab_size / (3.0 * edge_length)
  }
  # ... bezier generation continues
}
```

**Key Design Decisions**:

1. **Proportional fallback**: When a short edge can't fit even a minimum tab (would make tab width > 70% of edge), use a straight line instead of a deformed tab

2. **Parameter propagation**: The constraints flow through the entire pipeline:
   ```
   generate_puzzle(min_tab_size, max_tab_size)
     → generate_pieces_internal()
       → generate_voronoi_pieces_internal() / generate_random_pieces_internal()
         → build_voronoi_edge_map() / build_random_edge_map()
           → generate_tessellation_edge()
   ```

3. **Type-specific application**: Only Voronoi and Random types support these parameters (rectangular/hexagonal have uniform edges)

**Tab Geometry Reference**:
```
Tab fraction t = tabsize / 100 (e.g., 0.2 for tabsize=20)
Tab height ≈ 3 * t * edge_length
Tab width  ≈ 4 * t * edge_length (from 0.5-2t to 0.5+2t on normalized edge)
```

**Recommended Values**:
- Physical puzzles: `min_tab_size = 5` (5mm minimum for cuttability)
- Large puzzles: `max_tab_size = 30` (30mm maximum to prevent overlap)
- Digital only: Leave as NULL (no constraints)

**Files Modified**:
- `R/tessellation_edge_generation.R`: Core constraint logic
- `R/voronoi_puzzle.R`: Parameter propagation
- `R/random_puzzle.R`: Parameter propagation
- `R/unified_piece_generation.R`: Dispatcher
- `R/jigsawR_clean.R`: Main API

---

### Insight #33: Defensive NULL Checks in Shiny Reactive Chains (2025-12-15)

**Problem**: The Shiny app crashed when generating Voronoi or Random puzzles. Debug showed the app sourced correctly, but failed during reactive execution.

**Root Cause**: Settings stored in `base_settings()` reactive value might not contain all fields when:
1. User generates a puzzle before new parameters were added to the code
2. Certain fields are only relevant for specific puzzle types
3. Settings object is partially populated due to reactive timing

**Symptom**: Accessing `settings$point_distribution` when settings exists but the field is NULL causes:
```r
# This can evaluate to NULL even when type is "voronoi"
if (settings$type == "voronoi") settings$point_distribution else "fermat"
# Because: TRUE && NULL → NULL, not "fermat"
```

**Solution**: Use compound NULL checks:
```r
# WRONG: May return NULL if point_distribution is NULL
point_distribution = if (settings$type == "voronoi") settings$point_distribution else "fermat"

# CORRECT: Explicit NULL check
point_distribution = if (settings$type == "voronoi" && !is.null(settings$point_distribution)) {
  settings$point_distribution
} else {
  "fermat"
}
```

**Apply this pattern to**:
- `generate_puzzle()` calls in reactive observers
- Download handlers that regenerate puzzles
- Value boxes that display type-specific data

**Key Insight**: In Shiny apps with multiple puzzle types, every type-specific parameter access needs defensive NULL checking. The pattern `if (type == X) value else default` is NOT safe when `value` might be NULL.

**Files Modified**:
- `inst/shiny-app/app.R`: Added NULL checks in 4 locations (reactive observer, download handlers, value boxes)

---

### Insight #34: ggplot2 Extension Patterns for Custom Geoms (2025-12-16, Issue #54)

**Context**: Implemented `geom_puzzle_rect()` as a ggplot2 extension to enable puzzle pieces as data visualization layers. This required understanding ggplot2's ggproto system for creating custom Stat and Geom objects.

**Architecture**:
```
ggplot(data, aes(fill = value)) +
  geom_puzzle_rect(rows = 3, cols = 3, seed = 42)
        │
        └─→ layer(stat = StatPuzzle, geom = GeomPuzzle, ...)
                    │                       │
                    ▼                       ▼
            compute_panel()          draw_panel()
            - generate_puzzle()      - coord$transform()
            - svg_path_to_polygon()  - grid::polygonGrob()
            - data-to-piece mapping  - split by piece_id
```

**Key Implementation Details**:

1. **StatPuzzle** (`R/stat_puzzle.R`):
   - CRITICAL: Use `required_aes = character()` NOT `required_aes = c()`
   - The latter causes `strsplit()` error: "non-character argument"
   ```r
   StatPuzzle <- ggplot2::ggproto("StatPuzzle", ggplot2::Stat,
     required_aes = character(),  # NOT c()!
     compute_panel = function(data, scales, puzzle_type, rows, cols, ...) {
       # Generate puzzle and convert paths to polygons
       result <- generate_puzzle(type = puzzle_type, grid = c(rows, cols), ...)
       pieces_df <- do.call(rbind, lapply(seq_along(result$pieces), function(i) {
         poly <- svg_path_to_polygon(result$pieces[[i]]$path)
         poly$piece_id <- i
         poly
       }))
       # Map data to pieces with recycling
       data_idx <- rep_len(seq_len(nrow(data)), n_pieces)
       # ... merge data columns
     }
   )
   ```

2. **GeomPuzzle** (`R/geom_puzzle.R`):
   ```r
   GeomPuzzle <- ggplot2::ggproto("GeomPuzzle", ggplot2::Geom,
     required_aes = c("x", "y"),
     default_aes = ggplot2::aes(fill = "grey80", colour = "black", ...),
     draw_key = ggplot2::draw_key_polygon,
     draw_panel = function(data, panel_params, coord) {
       coords <- coord$transform(data, panel_params)
       pieces <- split(coords, coords$piece_id)
       grobs <- lapply(pieces, function(piece) {
         grid::polygonGrob(x = piece$x, y = piece$y, ...)
       })
       do.call(grid::grobTree, grobs)
     }
   )
   ```

3. **Bezier-to-Polygon Conversion** (`R/bezier_utils.R`):
   ```r
   # De Casteljau's algorithm for Bezier curve approximation
   bezier_to_points <- function(p0, cp1, cp2, p1, n_points = 20) {
     t <- seq(0, 1, length.out = n_points)
     # B(t) = (1-t)³P₀ + 3(1-t)²tCP₁ + 3(1-t)t²CP₂ + t³P₁
     x <- (1-t)^3 * p0[1] + 3*(1-t)^2*t * cp1[1] + ...
     y <- (1-t)^3 * p0[2] + 3*(1-t)^2*t * cp1[2] + ...
     data.frame(x = x, y = y)
   }
   ```

**Data-to-Piece Mapping**:
- When `nrow(data) < n_pieces`: Recycle data rows with `rep_len()`
- When `nrow(data) >= n_pieces`: Each row maps to one piece
- Data values are replicated to all polygon points in each piece

**Key Insight**: ggplot2 extensions work by:
1. **Stat** transforms data (our case: generates puzzle geometry from parameters)
2. **Geom** renders transformed data (our case: draws polygons from coordinates)
3. `coord$transform()` converts data coordinates to panel coordinates (0-1 range)
4. grid graphics (`polygonGrob`) does actual rendering

**Common Pitfalls**:
- `required_aes = c()` vs `required_aes = character()` - the former breaks strsplit
- Data recycling must replicate values for ALL polygon points, not just piece_id
- Single-piece puzzles (1x1) may not work - use minimum 2x2

**Files Created**:
- `R/stat_puzzle.R`: StatPuzzle ggproto object
- `R/geom_puzzle.R`: GeomPuzzle and `geom_puzzle_rect()`
- `R/bezier_utils.R`: Added `bezier_to_points()`, `svg_path_to_polygon()`
- `tests/testthat/test-ggpuzzle.R`: 32 comprehensive tests

**PR**: #61 - https://github.com/pjt222/jigsawR/pull/61

---

### Insight #35: Fallback Config Must Mirror Primary Config (2025-12-16)

**Problem**: Shiny app crashed on shinyapps.io with error:
```
Error in if (!is.na(min)) inputTag$attribs$min = min :
  argument is of length zero
```

**Root Cause**: The shinyapps.io environment couldn't find `inst/config.yml`, triggering the fallback config in `get_fallback_config()`. The fallback was **missing constraints** that the Shiny UI expected:

```r
# app.R references these:
numericInput("min_tab_size", ...,
  min = cfg_const$min_tab_size$min,  # Returns NULL from fallback!
  max = cfg_const$min_tab_size$max)
```

When `cfg_const$min_tab_size$min` returns `NULL`, Shiny's `numericInput()` passes it to the HTML builder, which expects a numeric value for the `if (!is.na(min))` check.

**Diagnostic Clue**: The shinyapps.io log showed:
```
! Config file not found in any location, using hardcoded defaults
```

This indicated the fallback path was being used.

**Solution**: Added missing constraints to `get_fallback_config()`:
```r
constraints = list(
  # ... existing constraints ...
  repel_margin = list(min = 0, max = 20),      # Was missing
  repel_max_iter = list(min = 10, max = 500),  # Was missing
  min_tab_size = list(min = 0, max = 50),      # Was missing (Issue #41, #42)
  max_tab_size = list(min = 5, max = 100)      # Was missing (Issue #41, #42)
)
```

**Key Insight**: When adding new configuration values to `config.yml`, **always update `get_fallback_config()` simultaneously**. The fallback serves as:
1. Deployment failsafe (config file not found)
2. Testing default values
3. Documentation of expected structure

**Pattern**: After adding ANY config value:
1. Add to `inst/config.yml`
2. Add SAME structure to `get_fallback_config()`
3. Test with `jigsawR:::get_fallback_config()` to verify

**Files Modified**:
- `R/config_utils.R`: Added missing constraints to fallback

---

### Insight #36: Edge Segments Must Be Complete SVG Paths (2025-12-18)

**Problem**: Voronoi and random puzzle piece strokes were completely invisible in the Shiny app preview, despite debug logging showing that:
- `edge_segments` was correctly populated
- `has_fusion = TRUE` was detected
- SVG contained stroke-dasharray for fused edges

**Root Cause**: The `edge_segments` stored in piece data contained **invalid SVG paths**. They stored only the curve/line commands (e.g., `L 0 44` or `C ...`) **without** the required starting `M` (moveto) command.

Before fix (invalid SVG):
```xml
<path d="L 0.0000 44.8405" fill="none" stroke="#000000".../>
```

SVG paths **must** start with "M" (moveto) to establish the drawing position. Browsers silently fail to render paths that start with "L" or "C".

**Solution**: Modified edge segment storage to prepend the starting point:

```r
# voronoi_puzzle.R and random_puzzle.R
# Before (invalid):
edge_segments[[neighbor_key]] <- list(
  path = edge_result$path,  # Just "C ..." or "L ..."
  ...
)

# After (valid):
edge_segments[[neighbor_key]] <- list(
  path = sprintf("M %.4f %.4f %s", v1[1], v1[2], edge_result$path),
  ...
)
```

**Key Insight**: When storing SVG path fragments for later rendering, always include the complete path specification including the starting "M" command. Path fragments without "M" are invalid SVG and will silently fail to render.

**Diagnostic Approach**: Created debug script that:
1. Generated puzzle through same code path as Shiny
2. Inspected actual SVG output character-by-character
3. Found edge paths starting with "L" instead of "M"

**Files Modified**:
- `R/voronoi_puzzle.R`: Lines 376-397 - edge segment storage with M command
- `R/random_puzzle.R`: Lines 383-447 - edge segment storage with M command

**Related**: This is similar to Insight #28 (Canvas Bounds Must Use Actual Path Geometry) - both involve ensuring SVG path data is complete and valid.

---

### Insight #37: Fusion Positioning Requires Effective Centers (2025-12-18)

**Problem**: Voronoi and random puzzles with fusion groups did not move together when offset > 0. Fused pieces would separate because each piece calculated its own displacement independently.

**Root Cause**: The `apply_voronoi_positioning()` and `apply_random_positioning()` functions were missing fusion-aware positioning logic. Each piece calculated `dx`/`dy` based on its own center:

```r
# WRONG: Each piece uses its own center
transformed_pieces <- lapply(pieces, function(piece) {
  piece_center <- piece$center  # Individual piece center
  dir <- piece_center - canvas_center
  # ... each fused piece gets DIFFERENT dx/dy
})
```

Meanwhile, rectangular, hexagonal, and concentric puzzles correctly use `build_effective_centers_radial()` to ensure fused pieces share the same displacement.

**Solution**: Apply the same pattern to voronoi/random positioning:

```r
# CORRECT: Use effective centers for fusion groups
effective_centers <- build_effective_centers_radial(
  pieces,
  piece_result$fusion_data
)

transformed_pieces <- lapply(seq_along(pieces), function(i) {
  piece <- pieces[[i]]
  eff_center <- effective_centers[[i]]  # Group centroid for fused pieces
  dir <- eff_center - canvas_center
  # ... all fused pieces get SAME dx/dy
})
```

**Key Insight**: For fusion positioning to work correctly:
1. Compute **effective centers** for all pieces (fused pieces share group centroid)
2. Calculate displacement using **effective center**, not individual piece center
3. All pieces in a fusion group receive **identical translation** (dx/dy)
4. Relative positions within the group are preserved

**Verification Test**:
```
Distance between pieces 1-2: 104.60
Distance without offset: 104.60
Distance change: 0.0000 (should be ~0 for fused pieces)
PASS: Fused pieces maintained relative position
```

**Files Modified**:
- `R/voronoi_puzzle.R`: `apply_voronoi_positioning()` - added effective center calculation
- `R/random_puzzle.R`: `apply_random_positioning()` - added effective center calculation

**Architectural Pattern**: The positioning architecture correctly separates:
- **Central dispatcher** (`piece_positioning.R:apply_piece_positioning()`) - routes to type-specific handlers
- **Type-specific handlers** - implement positioning strategy appropriate for puzzle geometry
- **Shared helper** (`build_effective_centers_radial()`) - reused across radial puzzle types

When adding new puzzle types, always ensure the positioning function handles fusion groups by using effective centers.

### Insight #38: Boundary Edge Keys Must Be Unique (2025-12-18)

**Problem**: Some boundary pieces in voronoi and random puzzles were missing border strokes (outlines). This was visible when pieces were separated (offset > 0) - corner pieces with multiple boundary edges would only show some of their borders.

**Root Cause**: In `assemble_voronoi_pieces()` and `assemble_random_pieces()`, boundary edges were being stored in `edge_segments` using the neighbor_id as the key:

```r
# WRONG: All boundary edges get key "-1" (neighbor_id < 0 for boundaries)
neighbor_key <- as.character(edge_result$neighbor_id)  # Always "-1" for boundaries
edge_segments[[neighbor_key]] <- list(...)  # OVERWRITES previous boundary edge!
```

When a piece has multiple boundary edges (e.g., corner pieces have 2 boundaries), they all get key `"-1"` and overwrite each other. Only the **last** boundary edge survives.

**Example**: Piece 1 (corner) has 2 boundary edges:
- Bottom edge: (41.37, 245.95) → (-44.57, 245.95) **OVERWRITTEN**
- Left edge: (-44.57, 245.95) → (-44.57, 200.76) **KEPT**

Result: Bottom boundary stroke is missing from the SVG.

**Solution**: Use unique keys for boundary edges by including the vertex index:

```r
# CORRECT: Each boundary edge gets a unique key
if (edge_result$neighbor_id < 0) {
  neighbor_key <- sprintf("boundary_%d", j)  # j = vertex index
} else {
  neighbor_key <- as.character(edge_result$neighbor_id)
}
edge_segments[[neighbor_key]] <- list(...)
```

**Key Insight**: When storing edge segments in a map/list:
1. Internal edges can use neighbor_id as key (each neighbor is unique)
2. Boundary edges CANNOT use neighbor_id (all are -1)
3. Use unique keys like `"boundary_1"`, `"boundary_2"`, etc. for boundary edges
4. The fallback path (lines 391-396) already used this pattern correctly

**Verification**:
```
BEFORE: Piece 1: 4 segments (1 boundary, 3 internal) - wrong!
AFTER:  Piece 1: 5 segments (2 boundary, 3 internal) - correct!

Total stroke elements increased from 19 to 23 (the missing boundary edges)
```

**Files Modified**:
- `R/voronoi_puzzle.R`: `assemble_voronoi_pieces()` - use `"boundary_%d"` keys for boundary edges
- `R/random_puzzle.R`: `assemble_random_pieces()` - use `"boundary_N"` keys for boundary edges

**Testing**: All 82 tessellation tests + 462 fusion/positioning tests pass.

### Insight #39: Voronoi/Random Fusion Requires Neighbor-ID Keyed Edge Segments (2025-12-18)

**Problem**: Fusion styling (dashed lines, opacity) was not rendering for voronoi and random puzzles, even though the fusion data was correctly computed and stored.

**Root Cause**: A key mismatch between how edges are named in different parts of the system:

1. **Rectangular/Hex/Concentric puzzles**: Edges are named by direction (`"TOP"`, `"RIGHT"`, `"BOTTOM"`, `"LEFT"`, `"OUTER"`, etc.)
2. **Voronoi/Random puzzles**: Edges are keyed by `neighbor_id` (e.g., `"2"`, `"5"`, `"boundary_1"`)

The fusion system was storing `fused_edges` with neighbor_id keys, but the renderer's `get_piece_edge_names()` was returning directional names. This mismatch caused the renderer to never find matching fused edges.

**Diagnostic Pattern** (from debug scripts):
```r
# Check if fused_edges keys match edge_segment keys
edge_names <- get_piece_edge_names(piece)
fused_keys <- names(piece$fused_edges)
cat("Edge names:", paste(edge_names, collapse = ", "), "\n")
cat("Fused keys:", paste(fused_keys, collapse = ", "), "\n")
# If these don't match, fusion rendering fails silently!
```

**Solution**: Extended `get_piece_edge_paths()` and `get_piece_edge_names()` in `unified_renderer.R` to handle voronoi/random pieces differently:

```r
get_piece_edge_names <- function(piece) {
  piece_type <- piece$type %||% "rectangular"

  if (piece_type %in% c("voronoi", "random")) {
    # Use neighbor_id keys from edge_segments
    if (is.null(piece$edge_segments)) return(character())
    return(names(piece$edge_segments))
  }
  # ... directional names for other types
}
```

**Key Insights from Debug Scripts**:

1. **Edge Key Consistency**: For fusion to work, `fused_edges` keys MUST match `edge_segments` keys exactly
2. **SVG Verification**: Check for `stroke-dasharray` and `opacity` attributes to verify fusion styling
3. **Validation UX**: When fusion fails validation, showing actual neighbors helps users fix their PILES notation
4. **Adjacency Data**: The puzzle result must include `adjacency` data for fusion validation to provide helpful hints

**Files Modified**:
- `R/unified_renderer.R`: Type-aware edge path/name retrieval
- `R/unified_piece_generation.R`: Neighbor-ID keyed fusion application for voronoi/random
- `R/adjacency_api.R`: Enhanced validation with neighbor hints

**Debugging Checklist for Fusion Issues**:
1. ✓ Is `fused_edges` populated on the pieces?
2. ✓ Do `fused_edges` keys match `edge_segments` keys?
3. ✓ Does SVG contain `stroke-dasharray` for dashed style?
4. ✓ Does SVG contain `opacity` for fused edges?
5. ✓ Is `has_fusion` correctly detected in renderer?

### Insight #40: RNG Batch Optimization via Pre-Generated Values (2025-12-18, Issue #65)

**Problem**: The rectangular and hexagonal puzzle generators used per-call RNG (random number generation), calling `sin(seed) * 10000` for each random value needed. For large puzzles, this meant thousands of individual function calls with environment variable updates.

**Background**: Issue #65 identified that the `uniform_batch()` C++ function existed (added in PR #64) but wasn't integrated into the puzzle generators. Benchmarks showed ~27x speedup potential:
- C++ batch (10K values): 0.003s
- R per-call (10K values): 0.069s

**Solution**: Created an RNG Iterator pattern that pre-generates all random values needed:

1. **RNG Count Calculation** (`R/rng_iterator.R`):
   ```r
   # Rectangular: count exact RNG calls needed
   calc_rect_rng_count <- function(xn, yn) {
     horizontal <- (yn - 1) * (6 + xn * 5)  # first() + next_tab() per edge
     vertical <- (xn - 1) * (6 + yn * 5)
     horizontal + vertical
   }
   ```

2. **Iterator Interface** (`R/rng_iterator.R`):
   ```r
   create_rng_iterator <- function(seed, count) {
     values <- uniform_batch(seed, count)  # Single batch call
     idx <- 0L
     list(
       next_val = function() { idx <<- idx + 1L; values[idx] },
       uniform = function(min, max) { idx <<- idx + 1L; min + values[idx] * (max - min) },
       rbool = function() { idx <<- idx + 1L; values[idx] > 0.5 }
     )
   }
   ```

3. **Integration** (`R/rectangular_puzzle.R`, `R/hexagonal_puzzle.R`):
   - `init_jigsaw()` / `init_hex_jigsaw()` now creates the iterator
   - `random()` / `hex_random()` delegates to `rng$next_val()`
   - All other code unchanged - same interface, same results

**Key Design Decisions**:
- **Same interface**: Existing `random()`, `uniform()`, `rbool()` functions work identically
- **Deterministic**: Same seed produces identical output (verified by 1780+ tests)
- **Graceful fallback**: Uses R implementation when C++ unavailable
- **Pre-calculation**: RNG count calculated from grid/ring parameters before generation

**RNG Call Patterns**:
- `first()`: 1 call (e) + 5 calls (next_tab) = 6 calls
- `next_tab()`: 5 calls (flip, b, c, d, e)
- `hex_next()`: 6 calls (flip, a, b, c, d, e)

**Files Modified**:
- `R/rng_iterator.R` (NEW): Iterator and count calculation functions
- `R/rectangular_puzzle.R`: Use iterator in `init_jigsaw()`
- `R/hexagonal_puzzle.R`: Use iterator in `init_hex_jigsaw()`
- `tests/testthat/test-rng-batch.R` (NEW): 32 tests for batch optimization

**Scope Notes**:
- Only affects rectangular and hexagonal puzzles (sine-based RNG)
- Voronoi/random puzzles use R's built-in `set.seed()` / `runif()` - not affected
- Concentric puzzles delegate to hexagonal - inherits optimization

### Insight #41: Repel Layout and Parameter Naming Edge Cases (2025-12-18)

**Problem 1 - Parameter Naming Mismatch**: The `apply_hex_positioning()` and `apply_concentric_positioning()` functions looked for `params$rings` and `params$diameter`, but `generate_puzzle()` stored these as `params$grid` and `params$size`. This caused `piece_size` to be `NA`, leading to `NaN` values when calculating separation factors:

```r
# BEFORE (broken):
rings <- params$rings        # Returns NULL
diameter <- params$diameter  # Returns NULL
piece_size <- diameter / (4 * rings - 2)  # NaN!
separation_factor <- 1.0 + (offset / piece_size)  # NaN!
```

**Problem 2 - Repel Zero Vector**: When two bounding boxes have centers at exactly the same position (e.g., center piece at origin and ring 1's centroid at origin), the repulsion direction fallback sets `dx=1, dy=0`. But when `overlap_x >= overlap_y`, the code tried to push vertically using `sign(dy)`, which returned 0:

```r
# BEFORE (broken):
if (dist < 0.001) { dx <- 1; dy <- 0 }  # Fallback direction
# ...
if (overlap_x < overlap_y) {
  c(sign(dx) * push_dist, 0)  # Horizontal push
} else {
  c(0, sign(dy) * push_dist)  # sign(0) = 0 → zero vector!
}
```

**Solutions**:

1. **Handle both parameter naming conventions** (`R/piece_positioning.R:441-452`):
   ```r
   rings <- params$rings %||% params$grid[1]
   diameter <- params$diameter %||% params$size[1]
   ```

2. **Prefer horizontal push when dy is zero** (`R/piece_positioning.R:922-928`):
   ```r
   if (overlap$overlap_x < overlap$overlap_y || abs(dy) < 0.001) {
     dir_x <- if (abs(dx) < 0.001) 1 else sign(dx)
     c(dir_x * push_dist, 0)  # Always non-zero
   }
   ```

**Key Insight**: When a fused group (like ring 1) is radially symmetric around the origin, its centroid is at `(0, 0)` - the same as the center piece. This means:
- **Grid layout**: Neither center nor ring 1 move (0 * factor = 0)
- **Repel layout**: Must push them apart using bbox collision detection

The repel algorithm correctly detects the overlap, but the repulsion vector must be non-zero to actually move them apart. The fix ensures that when the fallback horizontal direction `(1, 0)` is used, we always push horizontally rather than trying to push vertically with `sign(0)`.

**Files Modified**:
- `R/piece_positioning.R`: Parameter naming fix in `apply_hex_positioning()` and `apply_concentric_positioning()`, repulsion vector fix in `compute_repulsion_vector()`

---

### Insight #42: Geometry Processing Optimization via Cached Parsed Segments (2025-12-18, Issue #67)

**Problem**: Geometry processing for a 61-piece hexagonal puzzle took ~963ms, with `calculate_pieces_bounds()` accounting for ~428ms due to redundant SVG path re-parsing.

**Root Cause Analysis**:

The piece generation flow was:
1. **Geometry loop** (line 397): `parse_svg_path(hp$path)` → stored in `piece$parsed_segments`
2. **Bounds calculation** (line 413): `calculate_pieces_bounds(pieces)` → calls `extract_path_coords(piece$path)` → **re-parses ALL paths**

Each `parse_svg_path()` call takes ~7ms. For 61 pieces:
- First parse (cached): 61 × 7ms = 427ms
- Second parse (bounds): 61 × 7ms = 428ms (redundant!)

**Solution**: Modified `extract_all_piece_coords()` to use cached `parsed_segments` if available:

```r
# R/bezier_utils.R - extract_all_piece_coords()
coords_list <- lapply(pieces, function(piece) {
  if (!is.null(piece$parsed_segments)) {
    extract_coords_from_segments(piece$parsed_segments)  # ~40x faster
  } else {
    extract_path_coords(piece$path)  # Fallback
  }
})
```

Added new helper function `extract_coords_from_segments()` that extracts coordinates from pre-parsed segment objects without re-parsing.

**Performance Impact**:

| Metric | Before | After | Improvement |
|--------|--------|-------|-------------|
| `calculate_pieces_bounds()` | 428ms | 6ms | **71x faster** |
| Processing piece geometry | 963ms | 335ms | **65% faster** |
| Total puzzle generation | 2.36s | 1.45s | **38.6% faster** |

**Key Insight**: When caching expensive parse results, ensure ALL consumers of that data use the cache. It's easy to miss secondary call sites that re-compute the same information.

**Files Modified**:
- `R/bezier_utils.R`: Added `extract_coords_from_segments()`, modified `extract_all_piece_coords()` to use cached segments

---

### Insight #43: Failed Optimization - Geometry vs Topology Side Mismatch (2025-12-18, Issue #66)

**Problem**: Attempted to optimize edge generation by replacing O(n²) brute-force neighbor search with O(1) adjacency matrix lookups. The optimization broke piece rendering due to incorrect geo-topo mapping.

**The Failed Optimization**:

```r
# WRONG: Assumed topology sides = geometry sides
adj_matrix <- get_hex_adjacency_matrix(rings)
neighbor_id <- adj_matrix[piece_id, side + 1]  # Uses side as topology
neighbor_side <- (side + 3) %% 6              # Wrong assumption!
```

**Why It Failed**:

The adjacency matrix uses **topology sides** (based on axial coordinate directions):
- Side 0 = E (0°), Side 1 = NE (60°), Side 2 = NW (120°), etc.

Edge generation iterates over **geometry sides** (based on vertex indices):
- Side 0 faces 30°, Side 1 faces 90°, Side 2 faces 150°, etc.

**These are NOT aligned!** The relationship requires calculating the actual geometric direction using `atan2()`:

```r
# CORRECT: Calculate actual direction to neighbor
dir_to_neighbor <- atan2(neighbor_cy - piece_cy, neighbor_cx - piece_cx) * 180 / pi
geo_side <- round((dir_to_neighbor - 30) / 60) %% 6  # Convert to geometry side
```

**Previous Fix History**:
- Commit `83e9317`: fix(hexagonal): Correct topo-to-geo formula for flat-top hexagon (Issue #53)
- Commit `277c45f`: fix: Correct hexagonal topology-to-geometry side mapping formula

**Resolution**: Reverted to original brute-force vertex matching. The 520ms → 217ms speedup wasn't worth the geo-topo complexity.

**Key Lessons**:
1. **Topology ≠ Geometry**: Axial coordinate directions don't match vertex-based edge ordering
2. **The adjacency matrix is for topology-based operations** (like fusion group detection)
3. **Edge generation needs geometry-based matching** because vertex positions are transformed (warp/trunc)
4. **Previous fixes exist for a reason** - always check git history before "optimizing"

**Files**: No changes (optimization reverted)

### Insight #44: ggplot2 Layer Parameter Passthrough Pattern (2025-12-19, Issue #68)

**Problem**: The ggplot2 geom functions (`geom_puzzle_rect()`, etc.) did not support the `fusion_groups` parameter, even though `generate_puzzle()` did. Users could not fuse pieces when using the ggplot2 API.

**Solution**: Implement proper ggplot2 layer parameter passthrough following the established pattern:

**1. StatPuzzle setup_params() - Set Defaults**:
```r
setup_params = function(data, params) {
  # ... existing params ...
  params$fusion_groups <- params$fusion_groups %||% NULL
  params$fusion_style <- params$fusion_style %||% "none"
  params$fusion_opacity <- params$fusion_opacity %||% 0.3
  params
}
```

**2. StatPuzzle compute_panel() - Accept Parameters**:
```r
compute_panel = function(data, scales, ...,
                         fusion_groups = NULL,
                         fusion_style = "none",
                         fusion_opacity = 0.3) {
  # Pass to generate_puzzle()
  result <- generate_puzzle(
    ...,
    fusion_groups = fusion_groups,
    fusion_style = fusion_style,
    fusion_opacity = fusion_opacity
  )
}
```

**3. geom_puzzle_*() - Expose to Users**:
```r
geom_puzzle_rect <- function(...,
                              fusion_groups = NULL,
                              fusion_style = "none",
                              fusion_opacity = 0.3,
                              ...) {
  ggplot2::layer(
    params = list(
      ...,
      fusion_groups = fusion_groups,
      fusion_style = fusion_style,
      fusion_opacity = fusion_opacity
    )
  )
}
```

**Key Insights**:
1. **Three-layer passthrough**: Parameters flow from geom → layer params → setup_params → compute_panel → generate_puzzle
2. **Null coalescing**: Use `%||%` operator for clean default handling in setup_params
3. **Consistent defaults**: Same defaults must appear in setup_params, compute_panel signature, and geom function signature
4. **Documentation**: roxygen2 `@param` tags needed in geom functions for user-facing API docs

**Result**: All 5 geom functions now accept fusion parameters:
```r
ggplot(df, aes(fill = value)) +
  geom_puzzle_rect(rows = 3, cols = 3, seed = 42,
                   fusion_groups = "1-2-3,7-8-9",
                   fusion_style = "dashed",
                   fusion_opacity = 0.5)
```

**Files Changed**: `R/stat_puzzle.R`, `R/geom_puzzle.R`
**Tests Added**: 12 new tests in `tests/testthat/test-ggpuzzle.R`

---

### Insight #45: Unified RNG Strategy with Rcpp Fallback (2025-12-22)

**Problem**: The rectangular puzzle failed on shinyapps.io with error `object '_jigsawR_random_batch_cpp' not found`, while hexagonal and concentric puzzles worked fine.

**Root Cause Analysis**:
1. **Rectangular puzzle**: Used `uniform_batch()` → Rcpp C++ implementation
2. **Hexagonal/Concentric puzzles**: Used `set.seed()` + `runif()` → R's built-in RNG
3. **Fallback detection bug**: `.rcpp_available()` only checked if the R wrapper function existed (it always does in `RcppExports.R`), not whether the C++ symbol was actually callable

**The Bug in Detail**:
```r
# BEFORE: Only checked if wrapper exists (always TRUE)
.rcpp_available <- function() {
  exists("random_batch_cpp", mode = "function")  # Always TRUE!
}

# The R wrapper exists, but calling it fails:
random_batch_cpp <- function(...) {
  .Call(`_jigsawR_random_batch_cpp`, ...)  # Fails if C++ not compiled
}
```

**Solution - Two Parts**:

**1. Fix Rcpp Detection** (`R/rcpp_wrappers.R`):
```r
# AFTER: Actually TEST the C++ function
.rcpp_available <- function() {
  tryCatch({
    result <- random_batch_cpp(seed = 1, count = 1, min_val = 0, max_val = 1)
    is.numeric(result) && length(result) == 1
  }, error = function(e) FALSE)
}
```

**2. Unify RNG Strategy** (`R/hexagonal_bezier_generation.R`, `R/tessellation_edge_generation.R`):
```r
# BEFORE: R's standard RNG
set.seed(seed + edge_id)
t <- tabsize * (0.8 + 0.4 * runif(1))
a <- jitter * (runif(1) - 0.5)
# ...

# AFTER: Unified batch RNG (Rcpp with R fallback)
rng_vals <- uniform_batch(seed + edge_id, 6)
t <- tabsize * (0.8 + 0.4 * rng_vals[1])
a <- jitter * (rng_vals[2] - 0.5)
# ...
```

**Key Insight - Three-Layer RNG Architecture**:
```
┌─────────────────────────────────────────────────┐
│ uniform_batch(seed, count)  [R/rcpp_wrappers.R] │
│   └─> if .rcpp_status() → random_batch_cpp()    │
│   └─> else → .random_batch_r() [pure R]         │
├─────────────────────────────────────────────────┤
│ random_batch_cpp()          [R/RcppExports.R]   │
│   └─> .Call('_jigsawR_random_batch_cpp', ...)   │
├─────────────────────────────────────────────────┤
│ _jigsawR_random_batch_cpp   [src/jigsawR_core.cpp] │
│   └─> Sine-based deterministic RNG (~27x faster)│
└─────────────────────────────────────────────────┘
```

**Result**:
| Puzzle Type | Before | After |
|-------------|--------|-------|
| Rectangular | Rcpp only (failed on shinyapps.io) | Rcpp with R fallback |
| Hexagonal | `set.seed()`/`runif()` only | Unified `uniform_batch()` |
| Concentric | `set.seed()`/`runif()` only | Unified `uniform_batch()` |
| Tessellation | `set.seed()`/`runif()` only | Unified `uniform_batch()` |

**Deployment Optimization**:
To get C++ on shinyapps.io, updated `inst/shiny-app/deploy.R` to:
1. Create a DESCRIPTION listing `jigsawR` as a GitHub dependency
2. Use `Remotes: pjt222/jigsawR` so shinyapps.io installs from GitHub
3. App now tries `library(jigsawR)` first, falls back to sourcing R files

**Files Changed**:
- `R/rcpp_wrappers.R` - Fixed fallback detection
- `R/hexagonal_bezier_generation.R` - Unified RNG
- `R/tessellation_edge_generation.R` - Unified RNG
- `inst/shiny-app/app.R` - Package-first loading strategy
- `inst/shiny-app/deploy.R` - GitHub package installation

**Lesson**: When implementing fallback logic, always TEST the actual functionality, not just the existence of wrapper functions. The wrapper may exist but fail when called if underlying dependencies aren't available.

---

### Insight #46: Shinyapps.io Deployment Dependencies for Fallback Mode (2025-12-22)

**Problem**: After deploying to shinyapps.io, puzzles appeared to generate (logs showed "Updated positioned_result", "rendered_svg() triggered") but the preview stayed empty. Additionally, a warning appeared: "Config file not found in any location, using hardcoded defaults".

**Root Cause Analysis**:
When the jigsawR package fails to install from GitHub (the Remotes dependency), the app falls back to sourcing R files directly. However, the deployment DESCRIPTION only listed packages needed for the Shiny UI, not for the sourced R code.

**Missing Dependencies**:
| Package | Used By | Purpose |
|---------|---------|---------|
| viridis | config_utils.R:81 | Stroke color palettes |
| scales | (viridis dependency) | Color utilities |
| config | config_utils.R:43 | YAML config parsing |

Without `viridis`, calls to `get_puzzle_colors()` fail silently, resulting in pieces with no visible stroke colors (hence "empty" preview).

**Solution - Three Parts**:

**1. Add Missing Packages to Deployment DESCRIPTION** (`inst/shiny-app/deploy.R`):
```r
app_desc <- c(
  # ... existing packages ...
  "    viridis,",
  "    scales,",
  "    config",
  # ...
)
```

**2. Copy config.yml to Deployment** (`inst/shiny-app/deploy.R`):
```r
# Copy config.yml for configuration
config_source <- "inst/config.yml"
if (file.exists(config_source)) {
  file.copy(config_source, file.path(app_dir, "config.yml"), overwrite = TRUE)
}

# Updated appFiles to include config.yml
appFiles = c("app.R", "www/", "R/", "DESCRIPTION", "config.yml")
```

**3. Update Config Search Paths** (`R/config_utils.R`):
```r
possible_paths <- c(
  system.file("config.yml", package = "jigsawR"),
  "config.yml",  # Current directory (for deployed Shiny app) <-- ADDED
  "inst/config.yml",
  # ...
)
```

**Debugging Added** (`inst/shiny-app/app.R`):
```r
# In rendered_svg() reactive:
svg_len <- nchar(svg)
log_info("SVG generated: {svg_len} characters")
has_svg_tag <- grepl("<svg", svg)
has_path <- grepl("<path", svg)
log_info("Has <svg> tag: {has_svg_tag}, Has <path>: {has_path}")
```

**Key Insight**: When deploying a Shiny app that has both a "package mode" and a "fallback source mode":
1. The deployment DESCRIPTION must include ALL packages used by sourced R files
2. Configuration files need to be copied AND their search paths updated
3. Add debugging for SVG content to diagnose "empty" renders

**Files Changed**:
- `inst/shiny-app/deploy.R` - Added viridis, scales, config; copy config.yml
- `R/config_utils.R` - Added "config.yml" to search paths
- `inst/shiny-app/app.R` - Added SVG debugging logs

---

### 30. Cross-Type Feature Extension: Min/Max Tab Size (2025-12-21, Issue #76)

**Context**: The `min_tab_size` and `max_tab_size` parameters only applied to voronoi/random puzzle types. Issue #76 requested extending these constraints to all puzzle types (rectangular, hexagonal, concentric).

**Universal Tab Height Formula**:
All puzzle types share the same fundamental relationship:
```r
tab_height = 3.0 * t * edge_length
```
Where:
- `t` = normalized tab fraction (typically 0.08-0.12, derived from tabsize percentage)
- `edge_length` = physical length of the puzzle edge in mm

**Constraint Logic Pattern** (applied identically across all types):
```r
# MINIMUM constraint: scale UP if tab too small
if (!is.null(min_tab_size) && tab_height < min_tab_size) {
  t <- min_tab_size / (3.0 * edge_length)
  # Safety: if tab would be too wide (>70% of edge), use straight line or cap
  if (4.0 * t > 0.7) {
    t <- 0.175  # Cap at safe maximum, or return straight line
  }
}

# MAXIMUM constraint: scale DOWN if tab too large
if (!is.null(max_tab_size) && tab_height > max_tab_size) {
  t <- max_tab_size / (3.0 * edge_length)
}
```

**Key Implementation Differences by Type**:

1. **Hexagonal/Concentric** (Edge-by-edge generation):
   - Added params to `generate_hex_bezier_edge()` signature
   - Constraint logic applied directly after tab size calculation
   - Returns straight line if constraints impossible

2. **Rectangular** (Global environment state):
   - Uses `.jigsaw_env` for global state, but edges vary in length
   - Added `t_base` to store original tab size
   - Created `apply_tab_constraints()` helper called in `next_tab()`
   - Constraints recalculated per-edge based on current `sl()` (side length)

**Files Changed**:
- `R/hexagonal_bezier_generation.R`: Core constraint implementation
- `R/rectangular_puzzle.R`: Added `apply_tab_constraints()` helper
- `R/concentric_edge_generation.R`: Parameter pass-through (5 call sites)
- `R/hexagonal_edge_generation_fixed.R`: Parameter pass-through
- `R/unified_piece_generation.R`: Updated all type dispatches
- `R/puzzle_core_clean.R`: Pass-through to `init_jigsaw()`
- `R/jigsawR_clean.R`: Removed "voronoi/random only" restriction
- `inst/shiny-app/app.R`: Removed conditional panel

**Tests Updated**: Changed expectations from "constraints don't apply" to "constraints apply" for rectangular/hexagonal types.

**Key Insight**: When extending a feature across puzzle types with different architectures:
1. Identify the universal formula/logic (tab height calculation)
2. Find the appropriate injection point for each type
3. Respect each type's architectural patterns (edge-by-edge vs global state)
4. Update tests to expect the new behavior

---

### 31. Documentation Consolidation Pattern (2025-12-21, Issue #73)

**Context**: Found 3 nearly-identical "Aspect Ratios" sections across gallery documentation files (rectangular.qmd, voronoi.qmd, random.qmd), each ~45 lines showing the same concept with type-specific code.

**Problem**: Duplicate documentation creates:
- Maintenance burden (update 3 places for one concept)
- Inconsistency risk (different seeds, palettes, explanations)
- Poor user experience (same information repeated)

**Solution Pattern**: Consolidate → Cross-reference

1. **Create single authoritative source**: `quarto/tutorials/aspect-ratios.qmd`
   - Shows all puzzle types side-by-side for comparison
   - Single place to update when concept evolves
   - Better educational value through cross-type comparison

2. **Replace duplicates with callout links**:
   ```markdown
   ::: {.callout-tip}
   ## Aspect Ratios
   Rectangular puzzles handle any aspect ratio. See the
   [Aspect Ratios Tutorial](../tutorials/aspect-ratios.qmd) for examples.
   :::
   ```

3. **Update navigation**: Add new page to `_quarto.yml` menu structure

**Files Changed**:
- Created: `quarto/tutorials/aspect-ratios.qmd`
- Modified: `quarto/gallery/rectangular.qmd`, `voronoi.qmd`, `random.qmd`
- Modified: `quarto/_quarto.yml` (navigation)

**Key Insight**: When you find duplicate content across documentation files:
1. Identify what's truly shared vs type-specific
2. Create consolidated guide showing cross-type comparison
3. Replace duplicates with concise callout boxes linking to the guide
4. Ensure navigation structure exposes the new consolidated content

**Additional Finding**: The same pattern applies to other duplicated sections:
- Color Palettes (5 duplicates across gallery files)
- Offset/Separation (5 duplicates)
- These are candidates for future consolidation

---

## Development History

### Completed Work (Archive)

✅ **Unified RNG Strategy with Rcpp Fallback** (2025-12-22)
  - Fixed Rcpp fallback detection to actually TEST the C++ function, not just check wrapper existence
  - Unified all puzzle types to use `uniform_batch()` (Rcpp with R fallback)
  - Updated Shiny app to install jigsawR from GitHub for C++ compilation on shinyapps.io
  - Files: `R/rcpp_wrappers.R`, `R/hexagonal_bezier_generation.R`, `R/tessellation_edge_generation.R`
  - Files: `inst/shiny-app/app.R`, `inst/shiny-app/deploy.R`

✅ **ggpuzzle Fusion Groups Support** (2025-12-19, Issue #68)
  - Added `fusion_groups`, `fusion_style`, `fusion_opacity` parameters to all 5 geom functions
  - Proper ggplot2 layer parameter passthrough pattern
  - 12 new tests covering all puzzle types with fusion
  - Files: `R/stat_puzzle.R`, `R/geom_puzzle.R`, `tests/testthat/test-ggpuzzle.R`

✅ **RNG Batch Optimization** (2025-12-18, Issue #65)
  - Integrated `uniform_batch()` C++ function into puzzle generators
  - Created RNG iterator pattern for pre-generated batch values
  - ~27x speedup for RNG operations (C++ vs R per-call)
  - Files: `R/rng_iterator.R`, `R/rectangular_puzzle.R`, `R/hexagonal_puzzle.R`
  - Tests: `tests/testthat/test-rng-batch.R` (32 tests)

✅ **Voronoi and Random Shape Puzzle Types** (2025-12-15, Issues #41, #42)
  - Added two new tessellation-based puzzle types
  - Voronoi: Fermat spiral point distribution, deldir package
  - Random: Constrained Delaunay triangulation, RCDT package
  - Shared edge generation utilities for arbitrary polygons
  - Shiny app integration with type-specific UI panels
  - Files: `R/voronoi_puzzle.R`, `R/random_puzzle.R`, `R/tessellation_edge_generation.R`

✅ **Concentric as Top-Level Type** (2025-12-04)
  - Elevated "concentric" from a sub-mode of hexagonal to a proper top-level puzzle type
  - API changed from `generate_puzzle(type="hexagonal", concentric_mode=TRUE)` to `generate_puzzle(type="concentric")`
  - Shiny app now has three puzzle types: Rectangular, Hexagonal, Concentric
  - `concentric_mode` parameter removed entirely (hard break, not deprecated)
  - `center_shape` parameter only applies when `type="concentric"`

✅ **Concentric Ring Puzzles** (2025-12-04)
  - New puzzle type with constant radial height for all pieces
  - Trapezoidal pieces that get wider toward the outside (dartboard pattern)
  - Center piece can be hexagon or circle (configurable via `center_shape`)
  - All inner edges have bezier tabs; boundary OUTER edges are straight lines
  - Files: `R/concentric_geometry.R`, `R/concentric_edge_generation.R`
  - Test suite: `tests/test_concentric_mode.R` - all passing

✅ **Warp/Trunc transformation fix** (2025-12-01)
  - Fixed `apply_hex_warp()` to use DIVISION instead of multiplication (matching original JS)
  - Warp now applies to ALL vertices, not just boundary vertices
  - Boundary vertices projected to exact circle radius when both warp+trunc enabled
  - Arc radius uses consistent `diameter/2` for all border edges

✅ **Epic #32 - Unified Puzzle Generation Pipeline** (2025-12-01)
  - Issue #33: Unified piece generation module (`R/unified_piece_generation.R`)
  - Issue #34: Positioning engine (`R/piece_positioning.R`)
  - Issue #35: Unified SVG renderer (`R/unified_renderer.R`)
  - Issue #36: Updated `generate_puzzle()` to use unified pipeline
  - Issue #37: Updated Shiny app with offset slider (replaces output mode dropdown)
  - Issue #38: Deprecated legacy functions with `.Deprecated()` warnings
  - **Key change**: `offset` parameter replaces `output` mode (0=complete, >0=separated)

✅ **Hexagonal individual piece extraction** (Issue #10, 2025-12-01)
  - Hybrid Direct Generation approach approved and implemented
  - 42 unique edges for 3-ring puzzle, all with complementary forward/reverse paths
  - Test suite: `tests/test_hexagonal_individual.R` - all passing

✅ **Warp/Trunc for separated hexagonal mode** (Issues #29, #30, #31)
  - Circular warp, hexagonal truncation, and combined modes all working
  - Semantics matched between separated and complete modes

✅ **Hexagonal topology-based separation** (2025-11-26)
  - Ring-based hexagonal topology utilities
  - Center-at-(0,0) direction-based positioning
