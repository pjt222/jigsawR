# jigsawR

R translations of [Draradech's JavaScript jigsaw puzzle generators](https://github.com/Draradech/jigsaw) with enhanced circular puzzle visualization capabilities.

## Features

- **Rectangular Puzzles**: Direct R translation of the original JavaScript rectangular jigsaw generator
- **Hexagonal/Circular Puzzles**: Support for both hexagonal grid and circular warped puzzles
- **High-Quality Visualization**: SVG to PNG conversion with gradient backgrounds
- **Customizable Parameters**: Adjustable puzzle size, piece count, tab size, and jitter
- **Professional Output**: High-resolution PNG files suitable for printing or digital use

## Quick Start

```r
# Generate circular puzzle with gradient background
source("svg_to_png_overlay.R")

# Or run directly
Rscript svg_to_png_overlay.R
```

## Requirements

```r
# Install required packages
install.packages(c("ggplot2", "ggforce", "ggfx", "viridis", "rsvg", "magick"))
```

## Example Output

The main script generates:
- `svg_puzzle_3rings_combined.png` - Final circular puzzle with gradient background
- Individual SVG, background, and overlay layers for custom editing

## Original Work

This is a port of the excellent JavaScript jigsaw puzzle generators by [Draradech](https://github.com/Draradech/jigsaw), maintaining mathematical precision while adding R-based visualization capabilities.

## License

CC0 (Public Domain) - Same as original JavaScript implementation