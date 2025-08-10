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

- **svg_to_png_overlay.R**: High-quality circular puzzle with gradient background generator
  - Uses rsvg package for reliable SVG to PNG conversion
  - Creates enhanced SVGs with proper styling and transparency  
  - Generates circular gradient backgrounds using ggplot2 and ggfx masking
  - Produces high-resolution PNG overlays with precise puzzle outlines
  - Automatically combines background and overlay layers using magick
  - Outputs multiple puzzle variations (3, 4, 6 rings) with customizable parameters

## Development Commands

### Running the Scripts
```bash
# Generate default rectangular puzzle
Rscript jigswa.R

# Generate hexagonal puzzles  
Rscript jigsaw-hex.R

# Generate combined circular puzzle with gradient background (MAIN SCRIPT)
Rscript svg_to_png_overlay.R
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

# Test SVG to PNG circular puzzle generation
source("svg_to_png_overlay.R")
result <- generate_svg_puzzle_layers(seed = 1234, rings = 3, tabsize = 25, base_filename = "test_puzzle")
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
- `init_*()`: Environment setup and parameter parsing
- `gen_d*()`: Main path generation (horizontal, vertical, border) 
- `*_process()`: Coordinate transformation pipeline (hex only)
- `save_*_svg()`: File output utilities
- `create_enhanced_puzzle_svg()`: Enhanced SVG generation with styling
- `convert_svg_to_png()`: SVG to PNG conversion with multiple fallback methods
- `create_gradient_circle_png()`: Circular gradient background generation
- `generate_svg_puzzle_layers()`: Main function combining all steps

## Important Implementation Details

- **Exact JS Translation**: Both generators are direct ports maintaining mathematical precision
- **Environment Isolation**: Separate environments prevent conflicts between puzzle types
- **SVG Output Format**: Generates complete SVG with embedded styling and viewBox
- **Reproducible Results**: Seed-based generation ensures identical output for same parameters

## Dependencies

### Core Puzzle Generation
- Base R (no external dependencies for basic puzzle generation)

### Visualization and Combination
- ggplot2, ggforce, ggfx, viridis (for gradient background visualization)
- **rsvg** (RECOMMENDED - for high-quality SVG to PNG conversion)
- **magick** (for layer combination and image processing)

### Installation
```r
# Essential packages for best results
install.packages(c("ggplot2", "ggforce", "ggfx", "viridis", "rsvg", "magick"))
```