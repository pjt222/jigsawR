# GEMINI.md - Project Overview: jigsawR

## Project Overview

`jigsawR` is an R package for generating customizable jigsaw puzzles. It is a direct port of the JavaScript jigsaw puzzle generators created by Draradech. The package is capable of producing both rectangular and hexagonal (including circular warped) puzzles.

The key features of `jigsawR` include:

*   **Reproducible Puzzles:** Puzzles are generated based on a seed, so the same seed will always produce the same puzzle.
*   **Customizable Parameters:** Users can control the number of pieces, tab size, jitter (randomness in shape), and dimensions of the puzzle.
*   **Multiple Output Formats:** The primary output is SVG, but the package also includes functions to convert SVGs to PNGs and to create composite images with gradient backgrounds.
*   **Individual Piece Generation:** The package can export each puzzle piece as a separate SVG file, which is useful for laser cutting or other manufacturing processes.
*   **Interactive Application:** An interactive Shiny application is included for live previews, parameter adjustments, and instant downloads.

The codebase is a direct translation from JavaScript, and it retains the original's algorithmic structure, including the use of an R environment to mimic global variables. This is an unusual but effective approach for a direct port.

## Building and Running

There are three primary ways to use this project:

### 1. Interactive Shiny App (Easiest)

For an interactive experience with live previews and controls, you can run the included Shiny app.

```r
# Install dependencies
install.packages(c("shiny", "shinyjs", "ggplot2", "ggforce", "ggfx", "viridis"))

# Launch the app
source("R/launch_app.R")
launch_jigsaw_app()
```

### 2. Running the Code Directly (3 lines)

To generate a puzzle from the R console, you can load the necessary files and call the generation functions.

```r
# Load the package functions
devtools::load_all() # Or source the required R files

# Generate a 9-piece puzzle
result <- generate_individual_pieces(seed = 42, xn = 3, yn = 3, width = 300, height = 300)

# The output files will be saved in the "output/" directory.
```

### 3. Package Installation with `renv`

To install the package and its dependencies for development, use the `renv` package.

```bash
# Clone the repository
git clone https://github.com/pjt222/jigsawR.git
cd jigsawR

# Install dependencies from the renv.lock file
Rscript -e "renv::restore()"
```

After installation, you can load the package in your R session using `devtools::load_all()`.

## Development Conventions

*   **State Management (`.*_env`):** The code is a direct port of a JavaScript project. To maintain consistency with the original, it uses R environments (`.jigsaw_env` for rectangular puzzles and `.hex_jigsaw_env` for hexagonal puzzles) to manage state. This is not a typical R practice, but it is a key part of this project's architecture.

    *   **How it works:** These environments are created using `new.env()` and are populated by the `init_jigsaw()` and `init_hex_jigsaw()` functions. They store all the parameters and state required for puzzle generation, such as the random seed, tab size, jitter, and puzzle dimensions.
    *   **Usage:** Functions throughout the puzzle generation process access these "global" variables using the `$` operator (e.g., `.jigsaw_env$seed`). This allows for a direct, line-by-line translation of the original JavaScript code, which likely used global variables in a similar manner.
    *   **What this means for developers:** When working with the core puzzle generation logic, you will need to interact with these environments to access and modify parameters. Any new functions that are part of the core generation algorithm should follow this pattern.
*   **Documentation:** The package uses `roxygen2` for inline documentation. All exported functions have corresponding `.Rd` files in the `man/` directory.
*   **Testing:** Tests are located in the `tests/` directory and use the `testthat` framework. To run the tests, use `devtools::test()`.
*   **File Structure:**
    *   `R/`: Contains the core R source code.
        *   `rectangular_puzzle.R`: Core logic for rectangular puzzles.
        *   `hexagonal_puzzle.R`: Core logic for hexagonal puzzles.
        *   `main_generator.R`: Orchestrates the puzzle generation, including SVG to PNG conversion.
        *   `individual_pieces.R`: Extracts individual puzzle pieces.
        *   `launch_app.R`: Launches the Shiny app.
    *   `inst/`: Contains supporting files.
        *   `shiny-app/`: The source code for the interactive Shiny application.
        *   `examples/`: Example scripts demonstrating how to use the package.
    *   `man/`: `roxygen2`-generated documentation files.
    *   `tests/`: `testthat` unit tests.
*   **Dependencies:** Project dependencies are managed with `renv` and are listed in the `DESCRIPTION` file.

## Comparison with CLAUDE.md

This repository also contains a `CLAUDE.md` file. While both files provide an overview of the project, they have different purposes and target audiences.

*   **GEMINI.md (This file):** Provides a high-level overview of the project, its features, and how to get started. It is intended for a general audience of users and developers.
*   **CLAUDE.md:** A more detailed, technical document intended as a set of instructions for an AI pair programmer (Claude Code). It includes in-depth information about the project's architecture, development philosophy, and current status.

In short, `GEMINI.md` is the "user manual," while `CLAUDE.md` is the "developer's guide."
