# Legacy Compatibility Script for jigsawR Package
#
# This script provides backward compatibility with the original monolithic version
# while using the new package structure
#
# Previously located at: svg_to_png_overlay.R

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
    diameter = 180,
    tabsize = 35,
    jitter = 3,
    base_filename = "puzzle_3_opaque",
    size_px = 2000,
    line_color = "black",
    line_width = 1.5,
    transparent_background = FALSE  # Set to FALSE to compare with white background
  )
)

# Generate the puzzle variations
results <- generate_puzzle_variations(puzzle_variations)

cat("\n=== Legacy Compatibility Mode ===\n")
cat("This script maintains backward compatibility with svg_to_png_overlay.R\n")
cat("For new projects, use the clean API in inst/examples/clean_usage_example.R\n")