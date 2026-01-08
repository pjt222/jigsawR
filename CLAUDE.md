# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repository contains R translations of Draradech's JavaScript jigsaw puzzle generators. The codebase implements mathematical algorithms to generate SVG-based jigsaw puzzle patterns with configurable parameters for piece shapes, tabs, and layouts.

### Core Components

- **jigsaw.R**: Rectangular jigsaw puzzle generator (environment-based state, custom RNG)
- **jigsaw-hex.R**: Hexagonal jigsaw puzzle generator (circular warping, edge truncation)

### Package Structure

```
jigsawR/
├── R/                      # Package source code
│   ├── jigsawR_clean.R          # ⭐ Main entry point: generate_puzzle()
│   ├── unified_piece_generation.R  # Piece generation (all types)
│   ├── piece_positioning.R      # Positioning engine
│   ├── unified_renderer.R       # SVG rendering
│   ├── rectangular_puzzle.R     # Rectangular puzzle core
│   ├── hexagonal_puzzle.R       # Hexagonal puzzle core
│   ├── concentric_geometry.R    # Concentric puzzle geometry
│   ├── bezier_utils.R          # Bezier curve utilities
│   └── piles_notation.R        # PILES notation parser/serializer
├── inst/shiny-app/         # Shiny web application
├── tests/testthat/         # Test suite (1986 tests)
├── output/                 # Generated puzzle files
└── docs/development-guides/  # Detailed architecture docs
```

**Unified Pipeline:**
```
generate_puzzle() → generate_pieces_internal() → apply_piece_positioning() → render_puzzle_svg()
```

**Key Functions:**
- `generate_puzzle()`: **THE main entry point** - handles all puzzle types
  - Types: `"rectangular"`, `"hexagonal"`, `"concentric"`
  - Returns: `$svg_content`, `$pieces`, `$canvas_size`, `$files`

### PILES Notation (Fusion Groups)

PILES (Puzzle Input Line Entry System) is a SMILES-inspired notation for specifying piece fusion groups.
See [`docs/PILES-notation.md`](docs/PILES-notation.md) for full documentation.

**Quick Reference:**
```r
# Basic syntax
"1-2"           # Fuse pieces 1 and 2
"1-2-3,4-5"     # Two groups: (1,2,3) and (4,5)
"1:6"           # Range: pieces 1 through 6

# Keywords (require puzzle_result)
"center"        # Center piece (hex/concentric)
"ring1"         # All pieces in ring 1
"R1"            # Row 1 (rectangular)
"boundary"      # All boundary pieces

# Functions
parse_piles("1-2-3,4-5")                    # Parse PILES string
parse_fusion("1-2-3", puzzle)               # Auto-detect format
to_piles(list(c(1,2), c(3,4)))              # Convert to PILES
validate_piles_syntax("1-2(-3)-4")          # Validate syntax
```

## Code Style Guidelines

### Console Output

**ALWAYS use `cli` package for console output, NOT `cat()` or `print()`**

**Logging Wrapper Functions** (defined in `R/logging.R`):
```r
log_info("Found {n_files} files in {.path {dir_path}}")
log_success("Operation completed!")
log_warn("Missing optional parameter: {param_name}")
log_error("Failed to process {.file {filename}}")
```

**Important**: Extract values into variables first; don't nest function calls in cli markup.

## WSL R Execution

This project is developed in WSL with R installed on Windows:

```bash
R_EXE="/mnt/c/Program Files/R/R-4.5.0/bin/Rscript.exe"

# Run script files (PREFERRED)
"$R_EXE" inst/examples/generate_puzzles.R
"$R_EXE" tests/testthat.R

# Simple inline (OK for trivial commands)
"$R_EXE" -e "cat('test\n')"
```

**Key Notes:**
- `.Rprofile` auto-activates renv - don't use `--vanilla`
- Complex `-e` commands may fail with Exit code 5 due to shell escaping
- **Best practice**: Use script files for anything beyond trivial commands

## Quarto Documentation

The `quarto/` directory contains the project documentation site (GitHub Pages).

### Render Script (Recommended)

```bash
# Fresh render (default) - clears cache, renders all pages
bash inst/scripts/render_quarto.sh

# Cached render - uses existing _freeze files (faster)
bash inst/scripts/render_quarto.sh --cached
```

### Manual Rendering

```bash
QUARTO_EXE="/mnt/c/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe"

# Render entire site
"$QUARTO_EXE" render quarto

# Render single file
"$QUARTO_EXE" render quarto/getting-started.qmd

# Clear cache manually
rm -rf quarto/_freeze quarto/_site
```

**Key Notes:**
- Quarto is bundled with RStudio (no separate installation needed)
- Output goes to `quarto/_site/`
- Freeze cache in `quarto/_freeze/` - delete to force re-render
- All R code chunks should have `#| label:` for clean rendering output
- **Fresh render takes ~5-7 minutes** (14 pages with R code execution)

## Development Commands

### Package Development
```r
renv::restore()               # First time setup
renv::install("package_name") # Install missing packages into renv
devtools::load_all()          # Load package
devtools::document()          # Update docs
devtools::test()              # Run tests
devtools::check()             # Full check
```

### Quick Development Entry Point
```r
# Start Shiny app with clean workspace (from RStudio):
source("inst/dev.R")
```
**IMPORTANT**: `inst/dev.R` is a critical development file - DO NOT DELETE.

### Generate Puzzles
```r
# Rectangular
result <- generate_puzzle(
  type = "rectangular", seed = 42,
  grid = c(3, 4), size = c(400, 300),
  offset = 0,  # 0=complete, >0=separated
  layout = "grid"  # "grid" (default) or "repel"
)

# Hexagonal
result <- generate_puzzle(
  type = "hexagonal", seed = 42,
  grid = c(3), size = c(200),  # 3 rings, 200mm diameter
  do_warp = TRUE, do_trunc = TRUE
)

# Concentric
result <- generate_puzzle(
  type = "concentric", seed = 42,
  grid = c(3), size = c(240),
  center_shape = "hexagon"
)
```

## Algorithm Architecture

### State Management
Both generators use R environments for global state with seed-based deterministic RNG.

### Mathematical Components
1. **Random Number Generation**: Custom deterministic RNG using sine functions
2. **Bézier Curve Generation**: Control point calculations for puzzle tabs
3. **Coordinate Transformations**: Complex transforms for hexagonal grids
4. **SVG Path Construction**: String concatenation for valid SVG path data

### Detailed Architecture Documentation

For in-depth technical details, see:
**`docs/development-guides/jigsawR-architecture-insights.md`**

This includes:
- Hexagonal Warp/Trunc transformation details (5 boundary modes)
- Concentric Ring Mode geometry calculations
- 66 Key Development Insights (lessons learned)
- Complete development history

## Important Implementation Details

- **Exact JS Translation**: Direct ports maintaining mathematical precision
- **Reproducible Results**: Seed-based generation ensures identical output
- **R Package Structure**: Professional organization with DESCRIPTION and exports
- **renv Integration**: Reproducible environment with locked dependencies
- **Output Directory**: All generated files in `output/`

## Dependencies

### Core Dependencies (DESCRIPTION)
- **ggplot2, ggforce, ggfx, viridis**: Gradient visualization

### Suggested
- **rsvg**: SVG to PNG conversion
- **magick**: Image processing
- **devtools, testthat**: Development tools

## Development Philosophy

### Code Over Output Files
**IMPORTANT**: Focus on refining scripts, not output files. Reproducible code ensures data quality.

### Subagents for Complex Debugging
For complex debugging, spawn multiple subagents in parallel to investigate different aspects:
> "Please investigate [issue]. Go step by step, slow but correct, and create subagents as quality assurance and pair programming specialists."

### Principles
1. **Reproducibility First**: Same seed → same output
2. **Script Refinement**: Fix code, not output files
3. **Mathematical Correctness**: Sound bezier curves and transformations
4. **Test-Driven**: Verify programmatically, not just visually

## Current Status

- **Main API**: ✅ `generate_puzzle()` - single entry point for all types
- **Puzzle Types**: Rectangular, Hexagonal, Concentric
- **Shiny App**: ✅ Three puzzle types, offset slider, PNG/SVG downloads
- **Test Suites**: 1986 tests

### Open Issues
- **#62**: Epic: Create GitHub Pages Documentation Site (Quarto) - in progress
- **#78**: feat(ggpuzzle): Add repel layout support to geom_puzzle_rect

# important-instruction-reminders
Do what has been asked; nothing more, nothing less.
NEVER create files unless they're absolutely necessary for achieving your goal.
ALWAYS prefer editing an existing file to creating a new one.
NEVER proactively create documentation files (*.md) or README files. Only create documentation files if explicitly requested by the User.
