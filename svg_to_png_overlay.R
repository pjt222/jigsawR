# Entry Point Script for jigsawR Package
#
# This script provides backward compatibility with the original monolithic version
# while using the new package structure

# Load the package functions
# Note: In a proper installation, you would use library(jigsawR)
# For development, use devtools::load_all() or source the R files

# Check if running in package development mode
if (file.exists("R/main_generator.R")) {
  # Development mode - source files directly
  source("R/rectangular_puzzle.R")
  source("R/hexagonal_puzzle.R")
  source("R/gradient_background.R")
  source("R/svg_utils.R")
  source("R/image_processing.R")
  source("R/main_generator.R")
  cat("Loaded jigsawR functions from R/ directory\n")
} else {
  # Installed package mode
  library(jigsawR)
  cat("Loaded jigsawR package\n")
}

# All output files (SVG, PNG) will be created in the output/ directory
# Define puzzle variations to generate (same as before for compatibility)
puzzle_variations <- list(
  list(
    seed = 9999,
    rings = 3,
    diameter = 200,
    tabsize = 20,
    jitter = 3,
    base_filename = "svg_puzzle_3rings",
    size_px = 2000,
    line_color = "black",
    line_width = 2.0
  )
)

# Generate the puzzle variations
results <- generate_puzzle_variations(puzzle_variations)
