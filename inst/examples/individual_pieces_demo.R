# Demonstration of individual puzzle piece generation
# This example shows how to generate individual pieces with proper complementary edges

# Load required functions
source("R/rectangular_puzzle.R")
source("R/individual_pieces_correct.R")

# Create output directory for demo
demo_dir <- "output/individual_pieces_demo"
if (!dir.exists(demo_dir)) {
  dir.create(demo_dir, recursive = TRUE)
}

cat("=== Individual Puzzle Pieces Demo ===\n\n")

# Example 1: Generate a 2x2 puzzle with individual pieces
cat("Example 1: 2x2 Puzzle\n")
cat("---------------------\n")

result <- generate_2x2_individual_pieces(
  seed = 12345,
  width = 200,
  height = 200,
  output_dir = demo_dir
)

cat("Generated files:\n")
for (f in result$files$individual) {
  cat("  - ", f, "\n")
}
cat("  - ", result$files$combined, " (combined view with colors)\n\n")

# Example 2: Generate with different parameters
cat("Example 2: Different seed for unique puzzle\n")
cat("------------------------------------------\n")

result2 <- generate_2x2_individual_pieces(
  seed = 99999,
  width = 300,
  height = 300,
  output_dir = demo_dir
)

cat("Generated larger puzzle (300x300mm)\n\n")

# Example 3: Understanding the piece structure
cat("Example 3: Piece Structure\n")
cat("-------------------------\n")

cat("Each piece has:\n")
cat("- Proper complementary edges with neighbors\n")
cat("- Corner radius on border pieces\n")
cat("- Unique tab/blank patterns based on seed\n\n")

cat("Piece naming convention:\n")
cat("- piece_0_0: Top-left\n")
cat("- piece_1_0: Top-right\n")
cat("- piece_0_1: Bottom-left\n")
cat("- piece_1_1: Bottom-right\n\n")

# Future enhancement placeholder
cat("Future Enhancements:\n")
cat("-------------------\n")
cat("- Support for larger puzzles (3x3, 4x4, etc.)\n")
cat("- Customizable tab styles\n")
cat("- Export to different formats\n")
cat("- Piece grouping for partial assembly\n\n")

cat("Demo complete! Check", demo_dir, "for generated files.\n")