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
│   ├── rectangular_puzzle.R     # Core rectangular puzzle generator
│   ├── hexagonal_puzzle.R       # Core hexagonal/circular puzzle generator
│   ├── puzzle_core_clean.R      # Clean edge generation implementation
│   ├── individual_pieces.R      # Unified individual piece generation
│   ├── individual_pieces_final.R # Additional piece utilities
│   ├── gradient_background.R    # Circular gradient generation functions
│   ├── svg_utils.R             # SVG enhancement utilities
│   ├── image_processing.R      # PNG conversion and layer combination
│   ├── main_generator.R        # Main orchestration functions
│   ├── bezier_utils.R          # Bezier curve manipulation utilities
│   ├── hexagonal_individual_pieces.R  # Hexagonal piece generation
│   └── scripts_archive/        # Archived/deprecated implementations
│       └── deprecated/         # Old implementations (for reference)
├── man/                    # Documentation (auto-generated)
├── inst/examples/          # Example scripts
│   ├── generate_puzzles.R       # Package usage examples
│   └── individual_pieces_demo.R # Individual pieces demonstration
├── tests/                  # Test scripts
│   └── test_individual_pieces.R # Individual pieces test suite
├── output/                 # Generated puzzle files directory
└── svg_to_png_overlay.R    # Backward-compatible entry point
```

**Core Module Functions:**
- **R/rectangular_puzzle.R**: `init_jigsaw()`, `generate_jigsaw_svg()`, coordinate functions (`l()`, `w()`, etc.)
- **R/hexagonal_puzzle.R**: `init_hex_jigsaw()`, `generate_hex_jigsaw_svg()`
- **R/puzzle_core_clean.R**: `generate_puzzle_core()`, `generate_all_edges()`, `generate_edge_segment()`, `generate_single_piece()`
- **R/individual_pieces.R**: `generate_individual_pieces()`, `extract_tab_data()`, `build_piece_path()`
- **R/gradient_background.R**: `create_gradient_circle_png()`, `save_gradient_background()`
- **R/svg_utils.R**: `create_enhanced_puzzle_svg()`, `save_enhanced_svg()`
- **R/image_processing.R**: `convert_svg_to_png()`, `combine_image_layers()`, `check_conversion_tools()`
- **R/main_generator.R**: `generate_svg_puzzle_layers()`, `generate_puzzle_variations()`
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

# Run inline R code
"$R_EXE" -e "source('R/logging.R'); cat('test\n')"

# Run script files
"$R_EXE" inst/examples/generate_puzzles.R

# Test package functions
"$R_EXE" -e "
source('R/logging.R')
source('R/config_utils.R')
source('R/rectangular_puzzle.R')
source('R/puzzle_core_clean.R')
source('R/individual_pieces.R')

result <- generate_individual_pieces(seed = 42, xn = 2, yn = 2)
cat('Generated', length(result\$pieces), 'pieces\n')
"
```

**Important Notes:**
- The `.Rprofile` automatically activates renv - dependencies are managed
- Don't use `--vanilla` flag as it skips `.Rprofile` and renv activation
- Multi-line R code works best in script files or with proper escaping

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

# Generate puzzles using package functions
puzzle_variations <- list(
  list(seed = 1234, rings = 4, diameter = 200, base_filename = "my_puzzle")
)
results <- generate_puzzle_variations(puzzle_variations)

# Generate individual pieces (works for any size!)
result <- generate_individual_pieces(seed = 42, xn = 3, yn = 3, width = 300, height = 300)

# Test with comprehensive test suite
source("tests/test_individual_pieces.R")

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

### Key Functions by Purpose

**Core Puzzle Generation (jigsaw.R, jigsaw-hex.R):**
- `init_*()`: Environment setup and parameter parsing
- `gen_d*()`: Main path generation (horizontal, vertical, border) 
- `*_process()`: Coordinate transformation pipeline (hex only)
- `save_*_svg()`: File output utilities

**Modular Pipeline Functions:**
- `create_enhanced_puzzle_svg()`: Enhanced SVG generation with styling (svg_utils.R)
- `convert_svg_to_png()`: Multi-method SVG to PNG conversion (image_processing.R)
- `create_gradient_circle_png()`: Circular gradient background generation (gradient_background.R)
- `combine_image_layers()`: Layer composition using magick (image_processing.R)
- `generate_svg_puzzle_layers()`: Main orchestration function (main_generator.R)
- `generate_puzzle_variations()`: Batch processing workflow (main_generator.R)

**Individual Piece Generation (Clean Implementation):**
- `generate_individual_pieces()`: **Unified implementation** for any puzzle size (2x2, 3x3, 5x4, etc.)
  - Uses core generation functions directly (no SVG manipulation)
  - Generates both forward and reverse edge paths
  - Ensures perfect complementary edges between adjacent pieces
- `generate_all_edges()`: Pre-generates all shared edges with forward/reverse paths
- `build_piece_path()`: Assembles individual piece from 4 edges (top, right, bottom, left)
- `extract_tab_data()`: Extracts tab parameters for advanced manipulation
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
✅ **COMPLETED**: Code refactoring - consolidated duplicate implementations
✅ **COMPLETED**: Individual puzzle piece generation (rectangular) with complementary edges

### Current Status
- **Individual Pieces (Rectangular)**: ✅ Fully functional for any puzzle size (2x2, 3x3, 5x4, etc.)
- **Individual Pieces (Hexagonal)**: ✅ **COMPLETE** (2025-12-01, Issue #10)
  - Implemented using Hybrid Direct Generation approach (following rectangular pattern)
  - Key files: `R/hexagonal_individual_pieces.R`, `R/hexagonal_edge_generation_fixed.R`
  - Functions: `generate_hexagonal_individual_pieces()`, `generate_hex_edge_map()`
  - Features:
    - Unique edge mapping with deterministic seed-based tabs
    - Forward/reverse paths for complementary edges
    - Individual SVG files + combined view
    - Works for 2-6 rings (7 to 91 pieces)
  - Test suite: `tests/test_hexagonal_individual.R`
- **Hexagonal Separation**: ✅ Ring-based topology positioning with real piece extraction
- **Warp/Trunc for Separated Mode**: ✅ Complete (Issues #29, #30, #31)
- **Code Quality**: Clean, maintainable implementations following best practices
- **Test Suite**: Comprehensive testing framework in place

### Next Phase
📋 **Epic #32**: Unified Puzzle Generation Pipeline
  - Prerequisites complete: #10 (hexagonal individual pieces) ✅
  - Remaining sub-issues: #33-38 (unified pipeline implementation)
  - See GitHub issues for detailed implementation plan

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