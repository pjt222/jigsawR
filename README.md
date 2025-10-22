# jigsawR

<p align="center">
  <strong>Generate beautiful jigsaw puzzles in R with mathematical precision</strong>
</p>

R translations of [Draradech's JavaScript jigsaw puzzle generators](https://github.com/Draradech/jigsaw) with enhanced features for laser cutting, printing, and digital applications.

---

## 🎯 Features

- **📐 Rectangular Puzzles**: Classic jigsaw puzzles with any dimensions (2×2, 5×4, 10×8, etc.)
- **⬡ Hexagonal Puzzles**: Honeycomb-pattern puzzles with ring-based layouts
- **🔄 Circular Puzzles**: Hexagonal puzzles warped into circular shapes
- **✂️ Individual Pieces**: Export each piece as a separate SVG file
- **🎨 Customizable**: Adjust tab size, jitter, colors, and dimensions
- **🔁 Reproducible**: Same seed = same puzzle every time
- **📏 Manufacturing-Ready**: All coordinates in millimeters for accurate cutting/printing

---

## 🚀 Quick Start

### Installation

```r
# Clone the repository
git clone https://github.com/pjt222/jigsawR.git
cd jigsawR

# Install dependencies
install.packages(c("ggplot2", "ggforce", "ggfx", "viridis"))

# Optional: for SVG to PNG conversion
install.packages(c("rsvg", "magick"))

# Set up reproducible environment
renv::restore()
```

### Your First Puzzle (30 seconds)

```r
# Load required functions
source("R/rectangular_puzzle.R")
source("R/puzzle_core_clean.R")
source("R/individual_pieces.R")

# Generate a 3×3 puzzle with individual pieces
result <- generate_individual_pieces(
  seed = 42,
  xn = 3,        # 3 columns
  yn = 3,        # 3 rows
  width = 300,   # 300mm
  height = 300   # 300mm
)

# Find your puzzles in: output/piece_*.svg
```

**That's it!** You now have 9 individual puzzle pieces ready for use.

---

## 📚 Documentation

### Table of Contents

1. [Basic Usage](#basic-usage)
2. [Rectangular Puzzles](#rectangular-puzzles)
3. [Hexagonal Puzzles](#hexagonal-puzzles)
4. [Individual Pieces](#individual-pieces)
5. [Parameters Guide](#parameters-guide)
6. [Example Scripts](#example-scripts)
7. [Use Cases](#use-cases)
8. [Troubleshooting](#troubleshooting)

---

## 📖 Basic Usage

### Generate a Complete Puzzle

```r
source("R/rectangular_puzzle.R")

# Generate a complete 5×4 puzzle (20 pieces)
puzzle <- generate_jigsaw_svg(
  seed = 1234,
  xn = 5,        # 5 columns
  yn = 4,        # 4 rows
  width = 250,   # 250mm width
  height = 200   # 200mm height
)

# Save to file
save_jigsaw_svg(puzzle, "output/my_puzzle.svg")
```

### Generate Individual Pieces

```r
source("R/rectangular_puzzle.R")
source("R/puzzle_core_clean.R")
source("R/individual_pieces.R")

# Generate individual pieces
result <- generate_individual_pieces(
  seed = 1234,
  xn = 5,
  yn = 4,
  width = 250,
  height = 200,
  output_dir = "output/my_puzzle"
)

# Each piece is saved as: piece_0_0.svg, piece_0_1.svg, etc.
# Plus a combined view: combined_pieces.svg
```

---

## 🔲 Rectangular Puzzles

### Simple Examples

```r
# Small puzzle (4 pieces)
generate_individual_pieces(seed = 42, xn = 2, yn = 2, width = 200, height = 200)

# Standard puzzle (20 pieces)
generate_individual_pieces(seed = 42, xn = 5, yn = 4, width = 300, height = 240)

# Large puzzle (100 pieces)
generate_individual_pieces(seed = 42, xn = 10, yn = 10, width = 500, height = 500)
```

### Customizing Appearance

```r
# Larger tabs (more pronounced)
generate_individual_pieces(
  seed = 42, xn = 3, yn = 3,
  width = 300, height = 300,
  tabsize = 25    # Default is 20
)

# More variation in piece shapes
generate_individual_pieces(
  seed = 42, xn = 3, yn = 3,
  width = 300, height = 300,
  jitter = 8      # Default is 4
)

# Both combined
generate_individual_pieces(
  seed = 42, xn = 4, yn = 4,
  width = 400, height = 400,
  tabsize = 25,
  jitter = 8
)
```

---

## ⬡ Hexagonal Puzzles

Hexagonal puzzles use a ring-based system instead of rows/columns.

### Basic Hexagonal Puzzle

```r
source("R/hexagonal_puzzle.R")

# Generate 3-ring hexagonal puzzle (19 pieces)
hex_puzzle <- generate_hex_jigsaw_svg(
  rings = 3,
  diameter = 240,
  seed = 1234
)

save_hex_jigsaw_svg(hex_puzzle, "output/hex_puzzle.svg")
```

### Circular Puzzles

```r
# Hexagonal puzzle warped into a circle
circular_puzzle <- generate_hex_jigsaw_svg(
  rings = 4,
  diameter = 300,
  seed = 5678,
  do_warp = TRUE,    # Apply circular warping
  do_trunc = TRUE    # Truncate edge pieces
)

save_hex_jigsaw_svg(circular_puzzle, "output/circular_puzzle.svg")
```

### Hexagonal Individual Pieces

```r
source("R/hexagonal_individual_pieces.R")

# Generate individual pieces for hexagonal puzzle
result <- generate_hexagonal_individual_pieces(
  rings = 3,
  seed = 1234,
  diameter = 240,
  output_dir = "output/hex_pieces"
)
```

---

## ✂️ Individual Pieces

### Why Individual Pieces?

Individual piece generation is perfect for:
- **Laser Cutting**: Import each piece into your laser cutter software
- **3D Printing**: Extrude pieces to different depths
- **Custom Assembly**: Create partial puzzles or teaching materials
- **Manufacturing**: Send individual pieces for production

### Understanding Piece Naming

```
piece_X_Y.svg
      │ │
      │ └─ Row index (0-based, top to bottom)
      └─── Column index (0-based, left to right)
```

For a 2×2 puzzle:
```
piece_0_0.svg    piece_1_0.svg
(top-left)       (top-right)

piece_0_1.svg    piece_1_1.svg
(bottom-left)    (bottom-right)
```

### Piece Types

The generator creates three types of pieces:

1. **Corner Pieces** (4 total): Two straight edges with rounded corners
2. **Edge Pieces** (4×(n-2) total): One straight edge, three curved
3. **Interior Pieces** (remainder): All curved edges

### Quality Guarantees

✓ **Perfect Fit**: Adjacent pieces share identical edge curves
✓ **No Gaps**: Pieces fit together with mathematical precision
✓ **Smooth Curves**: Professional Bézier curve implementation
✓ **Manufacturing Ready**: Coordinates in millimeters

---

## ⚙️ Parameters Guide

### Common Parameters

| Parameter | Type | Default | Range | Description |
|-----------|------|---------|-------|-------------|
| `seed` | integer | random | any | Random seed for reproducibility |
| `xn` | integer | 2 | 1+ | Number of columns (rectangular) |
| `yn` | integer | 2 | 1+ | Number of rows (rectangular) |
| `rings` | integer | 3 | 1+ | Number of rings (hexagonal) |
| `width` | numeric | 200 | > 0 | Puzzle width in mm |
| `height` | numeric | 200 | > 0 | Puzzle height in mm |
| `diameter` | numeric | 240 | > 0 | Puzzle diameter in mm (hexagonal) |
| `tabsize` | numeric | 20 | 10-30 | Tab size as percentage |
| `jitter` | numeric | 4 | 0-10 | Random variation in piece shapes |
| `output_dir` | string | "output" | path | Directory for output files |

### Tab Size Guide

```r
tabsize = 15  # Small, subtle tabs
tabsize = 20  # Default, balanced
tabsize = 25  # Larger, more pronounced
tabsize = 30  # Very large, easy to grasp
```

### Jitter Guide

```r
jitter = 0    # Uniform, symmetrical pieces (less interesting)
jitter = 4    # Default, good variation
jitter = 7    # High variation, more challenging
jitter = 10   # Maximum variation, very irregular
```

### Dimension Guidelines

**For Printing:**
- Small: 150-250mm (hand-sized)
- Medium: 300-400mm (standard)
- Large: 500-800mm (poster-sized)

**For Laser Cutting:**
- Consider your material thickness
- Add 0.1-0.2mm tolerance if needed
- Test with a 2×2 puzzle first

---

## 📋 Example Scripts

The repository includes comprehensive examples in `inst/examples/`:

### Run the Demo

```bash
# Comprehensive demo with 5 different puzzles
Rscript inst/examples/individual_pieces_demo.R
```

This generates:
- 2×2 puzzle (4 pieces)
- 3×3 puzzle (9 pieces)
- 5×4 puzzle (20 pieces)
- Custom parameters puzzle (9 pieces)
- Large 10×8 puzzle (80 pieces)

### Other Examples

```bash
# Rectangular puzzles
Rscript inst/examples/rectangular_puzzle_example.R

# Hexagonal puzzles
Rscript inst/examples/hexagonal_puzzle_example.R

# Clean API usage
Rscript inst/examples/clean_usage_example.R

# Separation for laser cutting
Rscript inst/examples/separation_example.R
```

---

## 🎨 Use Cases

### 1. Laser Cutting

```r
# Generate puzzle optimized for laser cutting
result <- generate_individual_pieces(
  seed = 42,
  xn = 6, yn = 6,
  width = 300, height = 300,
  tabsize = 22,  # Slightly larger for stability
  output_dir = "output/laser_cutting"
)

# Import piece_*.svg files into your laser cutter software
# Recommended materials: 3mm plywood, acrylic, cardboard
```

### 2. Educational Materials

```r
# Create numbered pieces for teaching
for (i in 1:4) {
  generate_individual_pieces(
    seed = 1000 + i,
    xn = 2, yn = 2,
    width = 150, height = 150,
    output_dir = paste0("output/lesson_", i)
  )
}
```

### 3. Custom Art Puzzles

```r
# Generate puzzle pattern, then overlay your artwork
result <- generate_individual_pieces(
  seed = 42,
  xn = 8, yn = 6,
  width = 400, height = 300,
  output_dir = "output/art_puzzle"
)

# Open combined_pieces.svg in Illustrator/Inkscape
# Import your artwork as a layer below the puzzle
# Use puzzle lines for cutting paths
```

### 4. 3D Printing

```r
# Generate base pattern for 3D extrusion
result <- generate_individual_pieces(
  seed = 42,
  xn = 4, yn = 4,
  width = 200, height = 200,
  tabsize = 25,  # Larger tabs for 3D stability
  output_dir = "output/3d_puzzle"
)

# Import SVG into Fusion 360, Blender, or OpenSCAD
# Extrude to desired thickness (3-5mm typical)
```

---

## 🐛 Troubleshooting

### Issue: "Error in init_jigsaw(): could not find function"

**Solution**: Make sure you source the required files in order:
```r
source("R/rectangular_puzzle.R")
source("R/puzzle_core_clean.R")
source("R/individual_pieces.R")
```

### Issue: Pieces don't fit together perfectly

**Cause**: You may be using an outdated implementation.

**Solution**: Make sure you're using `generate_individual_pieces()` from the consolidated implementation, not older functions.

### Issue: SVG files are huge

**Solution**: SVG files are text-based and compress well. For smaller files:
```r
# Use gzip compression
system("gzip output/*.svg")
```

### Issue: Want to convert SVG to PNG

```r
# Install rsvg package
install.packages("rsvg")

# Convert
library(rsvg)
rsvg::rsvg_png("output/piece_0_0.svg",
               "output/piece_0_0.png",
               width = 1000)  # pixels
```

---

## 🔧 Advanced Usage

### Batch Generation

```r
# Generate multiple puzzles with different seeds
seeds <- c(42, 1234, 5678, 9999)

for (s in seeds) {
  generate_individual_pieces(
    seed = s,
    xn = 4, yn = 4,
    width = 300, height = 300,
    output_dir = paste0("output/puzzle_", s)
  )
}
```

### Custom Processing

```r
# Generate and get piece data for custom processing
result <- generate_individual_pieces(
  seed = 42, xn = 3, yn = 3,
  width = 300, height = 300
)

# Access piece paths programmatically
for (piece_id in names(result$pieces)) {
  piece <- result$pieces[[piece_id]]
  cat("Piece", piece$xi, ",", piece$yi, "\n")
  cat("Path:", substr(piece$path, 1, 50), "...\n\n")
}
```

---

## 🤝 Contributing

Contributions are welcome! Areas for improvement:

- Additional puzzle types (triangular, irregular shapes)
- Export formats (DXF, PDF, PNG)
- GUI/Shiny interface enhancements
- Documentation and examples
- Performance optimizations

---

## 📄 License

CC0 (Public Domain) - Same as the original JavaScript implementation by [Draradech](https://github.com/Draradech/jigsaw).

You are free to use, modify, and distribute this code for any purpose, including commercial applications.

---

## 🙏 Acknowledgments

- **Original Algorithm**: [Draradech](https://github.com/Draradech/jigsaw) - JavaScript jigsaw puzzle generators
- **R Translation**: Philipp Thoss - Port to R with enhancements
- **Mathematics**: Based on Bézier curve theory for smooth piece edges

---

## 📞 Support

- **Issues**: [GitHub Issues](https://github.com/pjt222/jigsawR/issues)
- **Documentation**: See `CLAUDE.md` for developer documentation
- **Examples**: Check `inst/examples/` for more use cases

---

## 🗺️ Roadmap

- [x] Rectangular puzzle generation
- [x] Hexagonal puzzle generation
- [x] Individual piece extraction
- [x] Circular puzzle warping
- [ ] Triangular puzzle patterns
- [ ] DXF export for CAD software
- [ ] Interactive web interface
- [ ] Piece separation with configurable gaps
- [ ] Custom piece shapes and patterns

---

<p align="center">
  <strong>Happy Puzzling! 🧩</strong>
</p>
