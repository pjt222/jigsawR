# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repository contains R translations of Draradech's JavaScript jigsaw puzzle generators. The codebase implements mathematical algorithms to generate SVG-based jigsaw puzzle patterns with configurable parameters for piece shapes, tabs, and layouts.

### Core Components

- **jigswa.R**: Direct R translation of rectangular jigsaw puzzle generator
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
│   ├── rectangular_puzzle.R     # Original rectangular puzzle generator
│   ├── hexagonal_puzzle.R       # Original hexagonal/circular puzzle generator
│   ├── gradient_background.R    # Circular gradient generation functions
│   ├── svg_utils.R             # SVG enhancement utilities
│   ├── image_processing.R      # PNG conversion and layer combination
│   └── main_generator.R        # Main orchestration functions
├── man/                    # Documentation (auto-generated)
├── inst/examples/          # Example scripts
│   └── generate_puzzles.R       # Package usage examples
├── output/                 # Generated puzzle files directory
└── svg_to_png_overlay.R    # Backward-compatible entry point
```

**Core Module Functions:**
- **R/gradient_background.R**: `create_gradient_circle_png()`, `save_gradient_background()`
- **R/svg_utils.R**: `create_enhanced_puzzle_svg()`, `save_enhanced_svg()`
- **R/image_processing.R**: `convert_svg_to_png()`, `combine_image_layers()`, `check_conversion_tools()`
- **R/main_generator.R**: `generate_svg_puzzle_layers()`, `generate_puzzle_variations()`

**Output Directory:**
- All generated files (SVG, PNG) are automatically placed in `output/` directory
- Functions automatically create the directory if it doesn't exist
- Keeps project root clean and organized

## Development Commands

### Running the Scripts
```bash
# Main entry point (backward compatible)
Rscript svg_to_png_overlay.R

# Package development example
Rscript inst/examples/generate_puzzles.R

# Individual puzzle generators (located in R/ directory)
Rscript R/rectangular_puzzle.R    # Rectangular puzzles
Rscript R/hexagonal_puzzle.R      # Hexagonal puzzles
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

# Update dependencies if needed
renv::snapshot()              # Save current package state
```

### Testing Individual Functions
```r
# Load functions interactively
source("jigswa.R")

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

## Important Implementation Details

- **Exact JS Translation**: Both generators are direct ports maintaining mathematical precision
- **Environment Isolation**: Separate environments prevent conflicts between puzzle types  
- **SVG Output Format**: Generates complete SVG with embedded styling and viewBox
- **Reproducible Results**: Seed-based generation ensures identical output for same parameters
- **R Package Structure**: Professional package organization with proper DESCRIPTION and exports
- **renv Integration**: Reproducible development environment with locked dependencies
- **Dedicated Output Directory**: All generated files automatically organized in `output/`

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