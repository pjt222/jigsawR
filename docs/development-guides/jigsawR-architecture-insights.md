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

### 18. Hexagonal Topology vs Geometric Side Mapping (2025-12-09)
- **Context**: Hexagonal fusion features require matching topology sides (from neighbor mapping) to geometric sides (in SVG paths)
- **Problem**: `get_hex_neighbor(piece_id, side, rings)` uses a "topology side" convention that differs from the "geometric side" in the SVG path
- **Root cause**: Topology sides are defined by the honeycomb grid iteration order during piece generation, while geometric sides are defined by vertex angles (V0 at 0°, V1 at 60°, etc.)
- **Example mismatch for center piece**:
  - Topology side 0 → piece 2 (at direction 150° from center)
  - Geometric side 0 → edge at direction 30° (from V0 to V1)
  - These don't match! Topology side 0 should map to geometric side 2.
- **Solution**: Dynamically compute the topology-to-geometry mapping for each piece based on actual neighbor directions
  ```r
  # For each topology side with a neighbor:
  dir_to_neighbor <- atan2(neighbor_cy - piece_cy, neighbor_cx - piece_cx) * 180 / pi

  # Find matching geometric side (direction 30 + geo_side * 60)
  for (geo_side in 0:5) {
    geo_dir <- 30 + geo_side * 60
    if (geo_dir > 180) geo_dir <- geo_dir - 360
    if (abs(dir_to_neighbor - geo_dir) < 30) {
      topo_to_geo_map[[topo_side]] <- geo_side
      break
    }
  }
  ```
- **Key insight**: The mapping varies by piece because each piece is rotated differently in the honeycomb grid. Only the center piece (piece 1) has a consistent formula `(2 - topo + 6) % 6`.
- **Files modified**: `R/unified_piece_generation.R:generate_hex_pieces_internal()` (lines 289-345)
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

---

## Development History

### Completed Work (Archive)

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
