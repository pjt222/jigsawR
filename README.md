# jigsawR

> Generate customizable jigsaw puzzles in R for laser cutting, printing, or digital use

R implementation of [Draradech's jigsaw puzzle generators](https://github.com/Draradech/jigsaw) with enhancements for manufacturing and reproducibility.

[![R](https://img.shields.io/badge/R-4.0%2B-blue)](https://www.r-project.org/)
[![License](https://img.shields.io/badge/license-CC0-green)](LICENSE)

---

## Quick Start

**Choose your path:**

### 🎨 Interactive App (Easiest)

```r
# Install and launch
install.packages(c("shiny", "shinyjs", "ggplot2", "ggforce", "ggfx", "viridis"))
source("R/launch_app.R")
launch_jigsaw_app()
```

Opens a browser interface with live preview, controls, and instant download.

### 💻 Code (3 lines)

```r
# Generate 9-piece puzzle
devtools::load_all()  # or source the required R files
result <- generate_individual_pieces(seed = 42, xn = 3, yn = 3, width = 300, height = 300)
# Files saved to: output/piece_*.svg
```

### 📦 Package Installation

```bash
# Clone and set up
git clone https://github.com/pjt222/jigsawR.git
cd jigsawR
Rscript -e "renv::restore()"  # Install dependencies
```

Then use `devtools::load_all()` or source individual files.

---

## What Can You Make?

| Type | Command | Output |
|------|---------|--------|
| **Rectangular** | `generate_individual_pieces(xn=5, yn=4, ...)` | 20 pieces (5×4 grid) |
| **Hexagonal** | `generate_hex_jigsaw_svg(rings=3, ...)` | 19 pieces (3 rings) |
| **Circular** | `generate_hex_jigsaw_svg(rings=3, do_warp=TRUE, ...)` | Warped hexagonal |
| **Separated** | `generate_separated_puzzle_svg(...)` | Offset pieces for laser cutting |

**All puzzles are:**
- ✅ Reproducible (same seed = same puzzle)
- ✅ Manufacturing-ready (millimeter coordinates)
- ✅ Customizable (tab size, jitter, dimensions)
- ✅ Individual pieces exported as separate SVG files

---

## Core Functions

### Rectangular Puzzles

```r
# Generate individual pieces (most common use case)
result <- generate_individual_pieces(
  seed = 1234,    # Random seed for reproducibility
  xn = 5,         # 5 columns
  yn = 4,         # 4 rows
  width = 300,    # 300mm width
  height = 240    # 240mm height
)
# Output: output/piece_0_0.svg, piece_0_1.svg, ..., combined_seed1234.svg
```

### Hexagonal Puzzles

```r
# Generate hexagonal puzzle
puzzle <- generate_hex_jigsaw_svg(
  rings = 3,         # 3 rings = 19 pieces
  diameter = 240,    # 240mm diameter
  seed = 1234
)
save_hex_jigsaw_svg(puzzle, "output/hex_puzzle.svg")
```

### Advanced: Separated Layout (Laser Cutting)

```r
# Generate with spacing between pieces
separated <- generate_separated_puzzle_svg(
  puzzle_structure = generate_puzzle_core(grid = c(4,5), size = c(300,240)),
  offset = 10,           # 10mm gap between pieces
  colors = "black",      # Black lines for laser
  stroke_width = 0.5
)
```

---

## Key Parameters

| Parameter | Description | Default | Range |
|-----------|-------------|---------|-------|
| `seed` | Random seed (reproducibility) | random | 1-99999 |
| `xn`, `yn` | Columns and rows (rectangular) | 15, 10 | 1-20 |
| `rings` | Number of rings (hexagonal) | 3 | 2-6 |
| `width`, `height` | Dimensions in mm | 300, 200 | 50-1000 |
| `diameter` | Diameter in mm (hexagonal) | 240 | 100-500 |
| `tabsize` | Tab size (%) | 20 | 10-30 |
| `jitter` | Shape variation (%) | 4 | 0-10 |

---

## Use Cases

**🔧 Laser Cutting**
```r
# Use separated layout with offset
generate_separated_puzzle_svg(..., offset = 10)
```

**🎨 Custom Designs**
```r
# Generate pieces, then add colors/text in graphic software
generate_individual_pieces(..., output_dir = "my_design")
```

**🖨️ 3D Printing**
```r
# Export individual pieces, convert to STL with extrusion
# (SVG → vector → extrude in CAD software)
```

**📚 Education**
```r
# Demonstrate geometry, tessellation, or algorithms
# Use Shiny app for interactive exploration
```

---

## Examples

See [`inst/examples/`](inst/examples/) for complete scripts:
- `rectangular_puzzle_2.R` - Advanced rectangular with text overlay
- `individual_pieces_demo.R` - Generate and validate individual pieces
- `generate_puzzles.R` - Batch generation examples

---

## Documentation

- **Interactive Shiny App**: Launch `launch_jigsaw_app()` and click "Help"
- **Function Reference**: See `man/` directory (roxygen2 docs)
- **Development Guide**: See [`CLAUDE.md`](CLAUDE.md) for architecture and development notes
- **Issues & Roadmap**: [GitHub Issues](https://github.com/pjt222/jigsawR/issues)

---

## Features In Development

- ⏳ **Hexagonal individual pieces** ([#10](https://github.com/pjt222/jigsawR/issues/10)) - Extract individual pieces from hexagonal puzzles
- ⏳ **Unit system** - Support for both mm and pixels with DPI conversion (partially implemented)

---

## Project Structure

```
jigsawR/
├── R/
│   ├── rectangular_puzzle.R      # Core rectangular puzzle generator
│   ├── hexagonal_puzzle.R        # Core hexagonal puzzle generator
│   ├── individual_pieces.R       # Individual piece extraction
│   ├── puzzle_core_clean.R       # Shared edge generation
│   ├── launch_app.R              # Shiny app launcher
│   └── ...                       # Additional utilities
├── inst/
│   ├── shiny-app/                # Interactive Shiny application
│   └── examples/                 # Example scripts
├── man/                          # Function documentation
├── tests/                        # Test suite
└── output/                       # Generated puzzles (auto-created)
```

---

## Requirements

**R Packages** (auto-installed with `renv::restore()`):
- **Core**: ggplot2, ggforce, ggfx, viridis
- **Optional**: rsvg, magick (for PNG conversion)
- **Shiny**: shiny, shinyjs (for interactive app)

**R Version**: 4.0 or higher

---

## Contributing

1. Check [open issues](https://github.com/pjt222/jigsawR/issues) for planned features
2. For hexagonal development, see issues [#6-10](https://github.com/pjt222/jigsawR/issues/10)
3. Development artifacts archived in `R/scripts_archive/development/`

---

## Credits

- **Original Algorithm**: [Draradech's JavaScript implementation](https://github.com/Draradech/jigsaw)
- **R Translation**: Philipp Thoss ([@pjt222](https://github.com/pjt222))
- **License**: CC0 (Public Domain)

---

## Troubleshooting

<details>
<summary><strong>Package won't load / missing functions</strong></summary>

```r
# If using as package
devtools::load_all()

# If sourcing directly
source("R/rectangular_puzzle.R")
source("R/puzzle_core_clean.R")
source("R/individual_pieces.R")
```
</details>

<details>
<summary><strong>Pandoc error when building vignettes</strong></summary>

Set environment variable in `.Renviron`:
```
RSTUDIO_PANDOC="C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools"
```
</details>

<details>
<summary><strong>SVG files won't open</strong></summary>

- Check file exists: `list.files("output/")`
- Try different viewer: Inkscape, Chrome, Firefox
- Validate SVG: Look for `<svg>` tag and closing `</svg>`
</details>

<details>
<summary><strong>Pieces don't fit together</strong></summary>

This shouldn't happen with proper generation. If it does:
1. Use same seed for both pieces
2. Check that complementary edges are used
3. Report as bug with seed and parameters
</details>

<details>
<summary><strong>Shinyapps.io deployment fails with package errors</strong></summary>

**Problem**: Deployment fails with errors like `unable to satisfy package: digest (0.6.39)`

**Cause**: Packages released today may not be synced to shinyapps.io's CRAN mirror yet

**Solution**: The deployment workflow automatically uses yesterday's CRAN snapshot via Posit Package Manager:
```yaml
# In .github/workflows/deploy-shiny.yml
- name: Set CRAN snapshot date (yesterday)
  run: |
    echo "CRAN_SNAPSHOT_DATE=$(date -d 'yesterday' '+%Y-%m-%d')" >> $GITHUB_ENV
```

This ensures package versions are stable and available on shinyapps.io's mirror.

**Manual override**: Set `CRAN_SNAPSHOT_DATE` to a specific date: `2025-11-18`
</details>

---

## Quick Reference

```r
# Most common workflow
devtools::load_all()

# 1. Generate individual pieces
result <- generate_individual_pieces(seed = 42, xn = 5, yn = 4,
                                     width = 300, height = 240)

# 2. Check output
list.files("output/", pattern = "piece_.*\\.svg")

# 3. For laser cutting, add separation
separated <- generate_separated_puzzle_svg(
  puzzle_structure = generate_puzzle_core(grid = c(4,5), size = c(300,240)),
  offset = 10
)
```

---

**Ready to create puzzles?** Start with the [Interactive App](#-interactive-app-easiest) or jump to [Core Functions](#core-functions).
