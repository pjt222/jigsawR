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

## 🎨 Interactive Shiny App

**Prefer a graphical interface?** Launch the interactive Shiny app for real-time puzzle design:

```r
# Install Shiny dependencies (first time only)
install.packages(c("shiny", "shinyjs"))

# Launch the app
source("R/launch_app.R")
launch_jigsaw_app()
```

The app will open in your browser with:
- 🎛️ **Interactive controls** for all parameters
- 👁️ **Live preview** of your puzzle
- 🎨 **5 color schemes** (Black, Rainbow, Blues, Warm, Cool)
- 📥 **Download button** for instant SVG export
- 💡 **Built-in help** with usage tips

Perfect for exploration, teaching, and rapid prototyping!

---

## 📚 Documentation

### Table of Contents

1. [Interactive Shiny App](#interactive-shiny-app)
2. [Basic Usage](#basic-usage)
3. [Rectangular Puzzles](#rectangular-puzzles)
4. [Hexagonal Puzzles](#hexagonal-puzzles)
5. [Individual Pieces](#individual-pieces)
6. [Parameters Guide](#parameters-guide)
7. [Example Scripts](#example-scripts)
8. [Use Cases](#use-cases)
9. [Troubleshooting](#troubleshooting)

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

## 🎨 Interactive Shiny App (Detailed)

### Launch the App

The jigsawR package includes a full-featured Shiny application for interactive puzzle design:

```r
# Method 1: Simple launch
source("R/launch_app.R")
launch_jigsaw_app()

# Method 2: Use the alias
jigsawR_app()

# Method 3: Custom port
launch_jigsaw_app(port = 3838)

# Method 4: Don't open browser (for remote servers)
launch_jigsaw_app(launch.browser = FALSE)

# Check if dependencies are installed
check_app_dependencies()
```

### App Features

**🎛️ Control Panel**
- **Puzzle Type Selection**: Rectangular or Hexagonal
- **Grid Configuration**: Rows/columns or rings
- **Dimensions**: Width/height or diameter (in mm)
- **Random Seed**: Reproducible designs with randomize button
- **Tab Size**: 10-30% adjustment slider
- **Jitter**: 0-10% variation control
- **Output Modes**:
  - Complete puzzle (all connected)
  - Individual pieces (colored)
  - Separated pieces (with gaps for laser cutting)

**🎨 Styling Options**
- **5 Color Schemes**:
  - Black (classic)
  - Rainbow (9 vibrant colors)
  - Blues (cool palette)
  - Warm (reds, oranges, yellows)
  - Cool (teals, blues, greens)
- **Line Width**: 0.5-3mm adjustable
- **Backgrounds**: None, White, Gradient, or Light Blue

**📊 Live Features**
- Real-time SVG preview (updates instantly)
- Puzzle information display (piece count, dimensions, area)
- Progress indicator during generation
- Download with smart filename (includes parameters)

**💡 Built-in Help Tab**
- How to use the app
- Parameter explanations
- Laser cutting tips
- Output mode descriptions

### App Use Cases

**1. Exploratory Design**
```r
launch_jigsaw_app()
# Experiment with parameters visually
# Try different seeds until you find the perfect pattern
# Download immediately when satisfied
```

**2. Teaching & Demonstrations**
```r
# Project the app during a workshop
launch_jigsaw_app()
# Show how different parameters affect puzzle design
# Generate examples for students in real-time
```

**3. Client Consultations**
```r
# Meet with clients to design custom puzzles
launch_jigsaw_app(port = 3838)
# Adjust parameters based on their feedback
# Download final design immediately
```

**4. Rapid Prototyping**
```r
# Quickly test multiple designs
launch_jigsaw_app()
# Use different seeds to generate variations
# Compare separated vs. individual modes
```

### App Tips

**Performance:**
- Puzzles up to 10×10 generate instantly
- Larger puzzles (12×12+) may take 2-3 seconds
- Hexagonal puzzles with 5+ rings are slower

**Workflow:**
1. Set basic dimensions and piece count
2. Adjust tab size and jitter for desired difficulty
3. Try different seeds (use randomize button)
4. Switch output modes to compare
5. Download when satisfied

**Troubleshooting:**
- If preview is blank: Click "Generate Puzzle"
- If download fails: Regenerate the puzzle first
- If app won't launch: Run `check_app_dependencies()`

### App vs. Command Line

| Feature | Shiny App | Command Line |
|---------|-----------|--------------|
| Learning curve | Immediate | Requires R knowledge |
| Speed | Point & click | Scriptable/automated |
| Exploration | Excellent | Manual |
| Batch generation | No | Yes |
| Reproducibility | Must record parameters | Built into script |
| Best for | Exploration, teaching | Production, automation |

**Use Both!**
- Start with the app to explore and find parameters you like
- Note the seed and parameters from a design you love
- Use command line scripts for batch generation or automation

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
