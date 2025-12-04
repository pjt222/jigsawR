# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repository contains R translations of Draradech's JavaScript jigsaw puzzle generators. The codebase implements mathematical algorithms to generate SVG-based jigsaw puzzle patterns with configurable parameters for piece shapes, tabs, and layouts.

### Core Components

- **jigsaw.R**: Direct R translation of rectangular jigsaw puzzle generator
  - Uses environment-based state management (`.jigsaw_env`) 
  - Implements custom random number generator for reproducible puzzles
  - Generates SVG paths for horizontal cuts, vertical cuts, and borders
  - Main function: `generate_jigsaw_svg()` returns structured data with SVG content

- **jigsaw-hex.R**: Direct R translation of hexagonal jigsaw puzzle generator
  - Uses separate environment (`.hex_jigsaw_env`) for hexagonal puzzles
  - Supports circular warping and edge truncation for different shapes
  - Complex coordinate transformations for hexagonal grid layout
  - Main function: `generate_hex_jigsaw_svg()` supports both hexagonal and circular puzzles

### R Package Structure

**Package Organization:**
```
jigsawR/
├── DESCRIPTION              # Package metadata and dependencies
├── R/                      # Package source code
│   ├── jigsawR_clean.R          # ⭐ Main entry point: generate_puzzle()
│   ├── unified_piece_generation.R  # Unified piece generation (rectangular + hexagonal)
│   ├── piece_positioning.R      # Positioning engine with offset support
│   ├── unified_renderer.R       # SVG rendering with backgrounds/colors
│   ├── rectangular_puzzle.R     # Core rectangular puzzle generator
│   ├── hexagonal_puzzle.R       # Core hexagonal/circular puzzle generator
│   ├── puzzle_core_clean.R      # Clean edge generation implementation
│   ├── individual_pieces.R      # (DEPRECATED) Individual piece generation
│   ├── puzzle_separation.R      # (DEPRECATED) Puzzle separation functions
│   ├── hexagonal_separation.R   # (DEPRECATED) Hexagonal separation
│   ├── gradient_background.R    # Circular gradient generation functions
│   ├── svg_utils.R             # SVG enhancement utilities
│   ├── image_processing.R      # PNG conversion and layer combination
│   ├── main_generator.R        # Main orchestration functions
│   ├── bezier_utils.R          # Bezier curve manipulation utilities
│   ├── hexagonal_individual_pieces.R  # Hexagonal piece generation
│   └── scripts_archive/        # Archived/deprecated implementations
│       └── deprecated/         # Old implementations (for reference)
├── man/                    # Documentation (auto-generated)
├── inst/                   # Installed files
│   ├── examples/           # Example scripts
│   └── shiny-app/          # Shiny web application
├── tests/                  # Test scripts
│   ├── test_generate_puzzle.R       # Main API tests
│   ├── test_unified_renderer.R      # Renderer tests
│   └── test_individual_pieces.R     # Individual pieces test suite
├── output/                 # Generated puzzle files directory
└── svg_to_png_overlay.R    # Backward-compatible entry point
```

**Unified Pipeline Architecture (Epic #32):**
```
generate_puzzle()  →  generate_pieces_internal()  →  apply_piece_positioning()  →  render_puzzle_svg()
     ↓                       ↓                              ↓                          ↓
   Main API            Piece generation              Offset/separation           SVG output
                    (rectangular/hexagonal)          (offset parameter)       (colors, background)
```

**Core Module Functions:**
- **R/jigsawR_clean.R**: `generate_puzzle()` - **THE main entry point** for all puzzle generation
- **R/unified_piece_generation.R**: `generate_pieces_internal()` - handles both rectangular and hexagonal
- **R/piece_positioning.R**: `apply_piece_positioning()` - applies offset/separation to pieces
- **R/unified_renderer.R**: `render_puzzle_svg()`, `render_piece()`, `build_svg_header()`
- **R/rectangular_puzzle.R**: `init_jigsaw()`, `generate_jigsaw_svg()`, coordinate functions (`l()`, `w()`, etc.)
- **R/hexagonal_puzzle.R**: `init_hex_jigsaw()`, `generate_hex_jigsaw_svg()`
- **R/puzzle_core_clean.R**: `generate_puzzle_core()`, `generate_all_edges()`, `generate_edge_segment()`, `generate_single_piece()`
- **R/gradient_background.R**: `create_gradient_circle_png()`, `save_gradient_background()`
- **R/bezier_utils.R**: `parse_svg_path()`, `reverse_path_segments()`, `flip_path_segments()`, `create_complementary_edge()`
- **R/hexagonal_individual_pieces.R**: `generate_hexagonal_individual_pieces()`

**Output Directory:**
- All generated files (SVG, PNG) are automatically placed in `output/` directory
- Functions automatically create the directory if it doesn't exist
- Keeps project root clean and organized

## Code Style Guidelines

### Console Output

**ALWAYS use `cli` package for console output, NOT `cat()` or `print()`**

The project uses the `cli` package for all console output to provide:
- Consistent, colored, and formatted output
- Proper handling of paths, files, and code elements
- Better error and warning messages
- Progress indicators

**Logging Wrapper Functions** (defined in `R/logging.R`):
```r
log_info("Found {n_files} files in {.path {dir_path}}")
log_success("Operation completed!")
log_warn("Missing optional parameter: {param_name}")
log_error("Failed to process {.file {filename}}")
log_header("Section Title")
log_subheader("Subsection Title")
```

**Important Notes:**
- Use variables directly in log messages - they are automatically captured from the calling environment
- Extract values from expressions into variables first (e.g., `n_files <- length(files)`)
- Use cli inline markup: `{.path}`, `{.file}`, `{.field}`, `{.val}`, etc.
- The `.envir = parent.frame()` parameter ensures proper variable scoping

**Examples:**
```r
# GOOD - Extract values first
path <- "data/output"
n_files <- length(list.files(path))
log_info("Found {n_files} files in {.path {path}}")

# BAD - Don't use cat()
cat("Found", length(files), "files in", path, "\n")

# BAD - Don't nest function calls in cli markup
log_info("Found {length(files)} files")  # Won't work!
```

## WSL R Execution

This project is developed in WSL with R installed on Windows. Always use the full Windows R path:

```bash
# Define R path for convenience (or use directly)
R_EXE="/mnt/c/Program Files/R/R-4.5.0/bin/Rscript.exe"

# Run script files (PREFERRED - always reliable)
"$R_EXE" inst/examples/generate_puzzles.R
"$R_EXE" tests/test_hexagonal_individual.R

# Simple inline R code (OK for trivial commands)
"$R_EXE" -e "cat('test\n')"
"$R_EXE" -e "print(1 + 1)"
```

**Important Notes:**
- The `.Rprofile` automatically activates renv - dependencies are managed
- Don't use `--vanilla` flag as it skips `.Rprofile` and renv activation

### WSL R Execution Limitations (Exit Code 5)

When running Windows `Rscript.exe` from WSL, **complex inline `-e` commands often fail with Exit code 5**. This is due to shell escaping issues at the WSL→Windows boundary.

**What causes Exit code 5:**
- `$` in R code (e.g., `e$message`, `list$item`) conflicts with bash variable expansion
- Quote nesting (single quotes inside double quotes) gets mangled
- Multi-line R code passed via `-e` can break
- Character encoding issues crossing the WSL→Windows boundary

**Examples that FAIL:**
```bash
# FAILS - $ causes bash variable expansion issues
"$R_EXE" -e "tryCatch({ x() }, error = function(e) e$message)"

# FAILS - Complex multi-line with special characters
"$R_EXE" -e "
source('R/file.R')
result <- list$item
"
```

**Examples that WORK:**
```bash
# WORKS - Simple commands without $
"$R_EXE" -e "cat('hello\n')"

# WORKS - Script files (ALWAYS preferred)
"$R_EXE" tests/test_hexagonal_individual.R

# WORKS - Use [[ ]] instead of $ for extraction
"$R_EXE" -e "x <- list(a=1); cat(x[['a']])"
```

**Best Practices:**
1. **Always prefer script files** for anything beyond trivial R commands
2. **Create test files** in `tests/` rather than using inline `-e` commands
3. **Avoid `$` notation** in inline commands - use `[["key"]]` instead
4. **Keep inline commands simple** - single statements, no complex escaping
5. **When Exit code 5 occurs**, move the code to a `.R` file and run that instead

## Development Commands

### Running the Scripts
```bash
# Main entry point (backward compatible)
"/mnt/c/Program Files/R/R-4.5.0/bin/Rscript.exe" svg_to_png_overlay.R

# Package development example
"/mnt/c/Program Files/R/R-4.5.0/bin/Rscript.exe" inst/examples/generate_puzzles.R

# Individual puzzle pieces example
"/mnt/c/Program Files/R/R-4.5.0/bin/Rscript.exe" inst/examples/individual_pieces_demo.R

# Individual puzzle generators (located in R/ directory)
"/mnt/c/Program Files/R/R-4.5.0/bin/Rscript.exe" R/rectangular_puzzle.R    # Rectangular puzzles
"/mnt/c/Program Files/R/R-4.5.0/bin/Rscript.exe" R/hexagonal_puzzle.R      # Hexagonal puzzles
```

### Package Development with renv
```r
# The project uses renv for reproducible environments
# Dependencies are automatically managed via renv.lock

# Restore package environment (first time setup)
renv::restore()

# Development workflow
devtools::load_all()          # Load package functions
devtools::document()          # Update documentation
devtools::check()             # Check package
devtools::test()              # Run tests (when available)

# ⭐ RECOMMENDED: Use generate_puzzle() for all puzzle generation
# Rectangular puzzle (complete)
result <- generate_puzzle(
  type = "rectangular",
  seed = 42,
  grid = c(3, 4),      # 3 rows, 4 columns
  size = c(400, 300),  # width x height in mm
  offset = 0           # 0 = complete, >0 = separated pieces
)

# Rectangular puzzle (separated by 15mm)
result <- generate_puzzle(
  type = "rectangular",
  seed = 42,
  grid = c(3, 4),
  size = c(400, 300),
  offset = 15,
  palette = "viridis",
  background = "white"
)

# Hexagonal puzzle (complete)
result <- generate_puzzle(
  type = "hexagonal",
  seed = 42,
  grid = c(3),         # 3 rings = 19 pieces
  size = c(200),       # diameter in mm
  offset = 0,
  do_warp = TRUE,      # circular boundary
  do_trunc = TRUE      # truncate edge pieces
)

# Access results
svg_content <- result$svg_content   # SVG string
pieces <- result$pieces             # List of piece data with paths
canvas_size <- result$canvas_size   # c(width, height)

# Save to file
result <- generate_puzzle(..., save_files = TRUE, output_dir = "output")
result$files$svg  # Path to saved SVG

# Test with comprehensive test suites
source("tests/test_generate_puzzle.R")
source("tests/test_unified_renderer.R")

# Update dependencies if needed
renv::snapshot()              # Save current package state
```

### Testing Individual Functions
```r
# Load functions interactively
source("jigsaw.R")

# Generate custom puzzle
puzzle <- generate_jigsaw_svg(seed = 1234, xn = 10, yn = 8)
save_jigsaw_svg(puzzle, "test_puzzle.svg")

# Test hexagonal puzzles
source("jigsaw-hex.R")
hex_puzzle <- generate_hex_jigsaw_svg(rings = 4, diameter = 200)

# Package development mode
devtools::load_all()  # Load all package functions
result <- generate_svg_puzzle_layers(seed = 1234, rings = 3, tabsize = 25, base_filename = "test_puzzle")

# Test individual modules (development)
source("R/gradient_background.R")
gradient <- create_gradient_circle_png(2000)

source("R/svg_utils.R")
svg_result <- create_enhanced_puzzle_svg(seed = 5678, rings = 4)

# Check output directory
list.files("output/", pattern = "test_puzzle")
```

## Algorithm Architecture

### State Management Pattern
Both puzzle generators use R environments to maintain global state:
- Seed-based deterministic random number generation (matching original JS)
- Parameter storage for puzzle dimensions and generation settings  
- Tab generation variables shared across coordinate calculation functions

### Mathematical Components
1. **Random Number Generation**: Custom deterministic RNG using sine functions
2. **Bézier Curve Generation**: Control point calculations for smooth puzzle tab shapes
3. **Coordinate Transformations**: Complex transformations for hexagonal grids with rotation/scaling
4. **SVG Path Construction**: String concatenation to build valid SVG path data

### Hexagonal Warp/Trunc Architecture

The hexagonal puzzle supports four boundary modes controlled by `do_warp` and `do_trunc`:

| Mode | do_warp | do_trunc | Boundary Shape | Border Edges |
|------|---------|----------|----------------|--------------|
| neither | FALSE | FALSE | Zigzag hexagon | Straight lines (L) |
| trunc_only | FALSE | TRUE | Clean hexagon | Straight lines (L) |
| warp_only | TRUE | FALSE | Warped zigzag | Straight lines (L) |
| both | TRUE | TRUE | Perfect circle | Arc commands (A) |

**Key Implementation Details:**

1. **Warp Formula** (`R/hexagonal_topology.R:apply_hex_warp`):
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

2. **Warp Application Scope** (`R/hexagonal_edge_generation_fixed.R`):
   - Warp must be applied to ALL vertices (internal + boundary), not just boundary
   - This matches the deprecated complete mode behavior

3. **Piece Radius Formula** (`R/hexagonal_edge_generation_fixed.R`):
   ```r
   # CORRECT formula: diameter / (4 * rings - 2)
   piece_radius <- diameter / (4 * rings - 2)
   ```
   - This ensures boundary vertices reach exactly `diameter/2` after warp transformation
   - The old formula `diameter / (rings * 4)` produced coordinates ~73-80% of target size
   - Derived by solving: find piece_radius where max(warped_boundary_dist) = diameter/2
   - The factor `4*rings - 2` is exact for all ring counts (verified for rings 2-12)

4. **Boundary Projection** (when both warp+trunc):
   - After warping, boundary vertices are at different distances from origin
   - Project them onto the target circle: `projected = (v / |v|) * circle_radius`
   - This ensures all border arcs connect smoothly on a perfect circle

5. **Arc Radius** (when both warp+trunc):
   - Use consistent `circle_radius = diameter / 2` for ALL border arcs
   - Don't calculate per-edge radii (causes gaps between arcs)

6. **Transformation Order** (verified 2025-12-03):
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

7. **Floating Point Precision in Vertex Keys** (fixed 2025-12-04):
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

### Key Functions by Purpose

**⭐ Unified Pipeline (Recommended - Epic #32):**
- `generate_puzzle()`: **THE main entry point** - handles all puzzle types, complete/separated modes
  - Parameters: type, grid, size, seed, offset, fill_color, colors, palette, background, opacity, etc.
  - Returns: `$svg_content`, `$pieces`, `$canvas_size`, `$files`
- `generate_pieces_internal()`: Internal piece generation for both rectangular and hexagonal
- `apply_piece_positioning()`: Applies offset/separation to pieces
- `render_puzzle_svg()`: Renders positioned pieces to SVG string

**Core Puzzle Generation (jigsaw.R, jigsaw-hex.R):**
- `init_*()`: Environment setup and parameter parsing
- `gen_d*()`: Main path generation (horizontal, vertical, border)
- `*_process()`: Coordinate transformation pipeline (hex only)
- `save_*_svg()`: File output utilities

**Legacy Functions (DEPRECATED - use generate_puzzle() instead):**
- `generate_individual_pieces()`: ⚠️ DEPRECATED - use `generate_puzzle()` and access `result$pieces`
- `generate_separated_puzzle_svg()`: ⚠️ DEPRECATED - use `generate_puzzle(offset = X)`
- `generate_separated_hexagonal_svg()`: ⚠️ DEPRECATED - use `generate_puzzle(type = "hexagonal", offset = X)`

**Supporting Functions:**
- `create_gradient_circle_png()`: Circular gradient background generation (gradient_background.R)
- `combine_image_layers()`: Layer composition using magick (image_processing.R)
- `parse_svg_path()`, `reverse_path_segments()`: Bezier curve utilities (bezier_utils.R)

## Important Implementation Details

- **Exact JS Translation**: Both generators are direct ports maintaining mathematical precision
- **Environment Isolation**: Separate environments prevent conflicts between puzzle types
- **SVG Output Format**: Generates complete SVG with embedded styling and viewBox
- **Reproducible Results**: Seed-based generation ensures identical output for same parameters
- **R Package Structure**: Professional package organization with proper DESCRIPTION and exports
- **renv Integration**: Reproducible development environment with locked dependencies
- **Dedicated Output Directory**: All generated files automatically organized in `output/`
- **Individual Pieces - Clean Approach**:
  - ✅ Unified implementation in `individual_pieces.R` (replaces 4 duplicate implementations)
  - ✅ Uses generation functions directly (no hardcoded paths or SVG manipulation)
  - ✅ Works for any puzzle size, not just 2x2
  - ✅ Properly generates complementary edges using forward/reverse paths
  - ❌ Deprecated files archived in `R/scripts_archive/deprecated/`

## Dependencies

### Package Management
- **renv**: Reproducible environment management (configured)
- Dependencies are locked in `renv.lock` and automatically managed

### Core Dependencies (via DESCRIPTION)
- **ggplot2, ggforce, ggfx, viridis**: Gradient background visualization (required)

### Suggested Dependencies  
- **rsvg**: High-quality SVG to PNG conversion (recommended)
- **magick**: Layer combination and image processing (recommended)
- **devtools**: Package development tools
- **testthat**: Testing framework (for future tests)

### Setup
```r
# First time setup - restore renv environment
renv::restore()

# The .Rprofile automatically activates renv and provides development guidance
```

## Development Philosophy

### Code Over Output Files
**IMPORTANT**: We focus on refining scripts rather than tinkering with output files. Reproducible code ensures data quality. Even if we achieve data quality through manual adjustments, this will not provide reliable code.

### Recent Work
✅ **COMPLETED**: Warp/Trunc transformation fix (2025-12-01)
  - Fixed `apply_hex_warp()` to use DIVISION instead of multiplication (matching original JS)
  - Warp now applies to ALL vertices, not just boundary vertices
  - Boundary vertices projected to exact circle radius when both warp+trunc enabled
  - Arc radius uses consistent `diameter/2` for all border edges
  - See "Hexagonal Warp/Trunc Architecture" section below for details
✅ **COMPLETED**: Epic #32 - Unified Puzzle Generation Pipeline (2025-12-01)
  - Issue #33: Unified piece generation module (`R/unified_piece_generation.R`)
  - Issue #34: Positioning engine (`R/piece_positioning.R`)
  - Issue #35: Unified SVG renderer (`R/unified_renderer.R`)
  - Issue #36: Updated `generate_puzzle()` to use unified pipeline
  - Issue #37: Updated Shiny app with offset slider (replaces output mode dropdown)
  - Issue #38: Deprecated legacy functions with `.Deprecated()` warnings
  - **Key change**: `offset` parameter replaces `output` mode (0=complete, >0=separated)
✅ **COMPLETED**: Hexagonal individual piece extraction (Issue #10, 2025-12-01)
  - Hybrid Direct Generation approach approved and implemented
  - 42 unique edges for 3-ring puzzle, all with complementary forward/reverse paths
  - Test suite: `tests/test_hexagonal_individual.R` - all passing
✅ **COMPLETED**: Warp/Trunc for separated hexagonal mode (Issues #29, #30, #31)
  - Circular warp, hexagonal truncation, and combined modes all working
  - Semantics matched between separated and complete modes
✅ **COMPLETED**: Hexagonal topology-based separation (2025-11-26)
  - Ring-based hexagonal topology utilities
  - Center-at-(0,0) direction-based positioning

### Current Status
- **Main API**: ✅ `generate_puzzle()` is the single entry point for all puzzle generation
  - Unified pipeline: `generate_pieces_internal()` → `apply_piece_positioning()` → `render_puzzle_svg()`
  - `offset` parameter controls complete vs separated output
  - Always returns `result$pieces` for programmatic access
- **Shiny App**: ✅ Updated with offset slider (0-50mm) and three download buttons
- **Individual Pieces (Rectangular)**: ✅ Fully functional for any puzzle size
- **Individual Pieces (Hexagonal)**: ✅ Complete (Issue #10)
- **Hexagonal Separation**: ✅ Ring-based topology positioning with real piece extraction
- **Warp/Trunc for Separated Mode**: ✅ Complete (Issues #29, #30, #31)
- **Test Suites**: `tests/test_generate_puzzle.R` (10 tests), `tests/test_unified_renderer.R` (10 tests)

### Next Phase
📋 **Enhancement #25**: Add PNG download capability to Shiny app (independent)

### Principles
1. **Reproducibility First**: All outputs must be reproducible from scripts with the same seed
2. **Script Refinement**: Fix issues in the code, not in the output files
3. **Mathematical Correctness**: Ensure bezier curves and transformations are mathematically sound
4. **Incremental Development**: Start with 2x2 puzzles, then expand to larger sizes
5. **Test-Driven**: Verify outputs programmatically, not just visually

# important-instruction-reminders
Do what has been asked; nothing more, nothing less.
NEVER create files unless they're absolutely necessary for achieving your goal.
ALWAYS prefer editing an existing file to creating a new one.
NEVER proactively create documentation files (*.md) or README files. Only create documentation files if explicitly requested by the User.