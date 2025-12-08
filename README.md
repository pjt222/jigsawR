# jigsawR

> Generate customizable jigsaw puzzles in R for laser cutting, printing, or digital use

R implementation of [Draradech's jigsaw puzzle generators](https://github.com/Draradech/jigsaw) with enhancements for manufacturing and reproducibility.

[![R](https://img.shields.io/badge/R-4.0%2B-blue)](https://www.r-project.org/)
[![License](https://img.shields.io/badge/license-GPL--3-blue)](LICENSE.md)

---

![Puzzle Banner](https://i.imgur.com/L5g0rR4.png)

## Why jigsawR?

*   **Reproducible:** Create the same puzzle every time with the same seed.
*   **Customizable:** Control the number of pieces, tab size, jitter, and dimensions.
*   **Manufacturing-Ready:** Generate individual SVG files for each piece, perfect for laser cutting.
*   **Multiple Formats:** Output puzzles as SVG or PNG, with or without a background.
*   **Interactive:** Use the built-in Shiny app for a live preview and easy downloads.

## Getting Started

The easiest way to get started with `jigsawR` is to use the interactive Shiny app.

```r
# Install dependencies
install.packages(c("shiny", "shinyjs", "ggplot2", "ggforce", "ggfx", "viridis"))

# Clone the repository
git clone https://github.com/pjt222/jigsawR.git
cd jigsawR

# Launch the app
source("R/launch_app.R")
launch_jigsaw_app()
```

This will open a browser window with a live preview of your puzzle, controls to customize it, and an instant download button.

## Usage

You can also generate puzzles directly from your R code.

### Rectangular Puzzles

```r
# Load the package functions
devtools::load_all()

# Generate individual pieces
result <- generate_individual_pieces(
  seed = 1234,    # Random seed for reproducibility
  xn = 5,         # 5 columns
  yn = 4,         # 4 rows
  width = 300,    # 300mm width
  height = 240    # 240mm height
)

# The output files will be saved in the "output/" directory.
```

### Hexagonal Puzzles

```r
# Generate a hexagonal puzzle
puzzle <- generate_hex_jigsaw_svg(
  rings = 3,         # 3 rings = 19 pieces
  diameter = 240,    # 240mm diameter
  seed = 1234
)

# Save the puzzle to a file
save_hex_jigsaw_svg(puzzle, "output/hex_puzzle.svg")
```

## Features

| Type | Command | Output |
|------|---------|--------|
| **Rectangular** | `generate_individual_pieces(xn=5, yn=4, ...)` | 20 pieces (5×4 grid) |
| **Hexagonal** | `generate_hex_jigsaw_svg(rings=3, ...)` | 19 pieces (3 rings) |
| **Circular** | `generate_hex_jigsaw_svg(rings=3, do_warp=TRUE, ...)` | Warped hexagonal |
| **Separated** | `generate_separated_puzzle_svg(...)` | Offset pieces for laser cutting |

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

## Requirements

**R Packages** (auto-installed with `renv::restore()`):
- **Core**: ggplot2, ggforce, ggfx, viridis
- **Optional**: rsvg, magick (for PNG conversion)
- **Shiny**: shiny, shinyjs (for interactive app)

**R Version**: 4.0 or higher

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

## Documentation

*   **Interactive Shiny App:** Launch `launch_jigsaw_app()` and click "Help" for an interactive guide.
*   **Function Reference:** The `man/` directory contains detailed documentation for each function.
*   **Developer's Guide:** See `CLAUDE.md` for a deep dive into the project's architecture and development conventions.
*   **Project Overview:** For a higher-level overview, see `GEMINI.md`.

## Contributing

We welcome contributions! Please check the [open issues](https://github.com/pjt222/jigsawR/issues) for planned features.

## Credits

*   **Original Algorithm:** [Draradech's JavaScript implementation](https://github.com/Draradech/jigsaw) (CC0)
*   **R Translation:** Philipp Thoss ([@pjt222](https://github.com/pjt222))

## License

jigsawR is dual-licensed:

*   **Open Source (GPL-3):** Free for personal, academic, and open-source use. Derivative works must also be GPL-3. See [LICENSE.md](LICENSE.md) for details.
*   **Commercial License:** For proprietary/closed-source use without GPL-3 requirements, contact ph.thoss@gmx.de or [open an issue](https://github.com/pjt222/jigsawR/issues).
