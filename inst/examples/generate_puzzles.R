# Example script for generating jigsaw puzzles
# This script demonstrates how to use the jigsawR package

# Load the package (assumes installed or using devtools::load_all())
library(jigsawR)

# Define puzzle variations to generate
puzzle_variations <- list(
  list(
    seed = 9999,
    rings = 3,
    diameter = 200,
    tabsize = 20,
    jitter = 3,
    base_filename = "example_puzzle_3rings",
    size_px = 2000,
    line_color = "black",
    line_width = 2.0
  ),
  list(
    seed = 1234,
    rings = 4,
    diameter = 220,
    tabsize = 25,
    jitter = 4,
    base_filename = "example_puzzle_4rings", 
    size_px = 2200,
    line_color = "black",
    line_width = 2.0
  )
)

# Generate the puzzle variations
cat("Generating example puzzles...\n")
results <- generate_puzzle_variations(puzzle_variations)

# Display results
if (!is.null(results)) {
  cat("\nSuccessfully generated", length(results), "puzzle variations!\n")
  cat("Check the 'output/' directory for generated files.\n")
} else {
  cat("\nFailed to generate puzzles. Please check your setup.\n")
}