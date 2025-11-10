# Demonstration of Individual Puzzle Piece Generation
# This example shows how to generate individual pieces with proper complementary edges
# using the consolidated, clean implementation

# Load required functions
source("R/rectangular_puzzle.R")
source("R/puzzle_core_clean.R")
source("R/individual_pieces.R")

# Create output directory for demo
demo_dir <- "output/individual_pieces_demo"
if (!dir.exists(demo_dir)) {
  dir.create(demo_dir, recursive = TRUE)
}

cat("========================================\n")
cat("Individual Puzzle Pieces Demo\n")
cat("========================================\n\n")

# Example 1: Simple 2x2 puzzle
cat("Example 1: 2x2 Puzzle (4 pieces)\n")
cat("----------------------------------\n")
cat("Perfect for beginners - generates a simple 2x2 puzzle\n")
cat("with proper complementary edges.\n\n")

result_2x2 <- generate_individual_pieces(
  seed = 42,
  xn = 2,          # 2 columns
  yn = 2,          # 2 rows
  width = 200,     # 200mm width
  height = 200,    # 200mm height
  output_dir = file.path(demo_dir, "2x2_puzzle")
)

cat("Generated", result_2x2$parameters$total_pieces, "pieces\n")
cat("Files saved to:", file.path(demo_dir, "2x2_puzzle"), "\n")
cat("  - Individual pieces: piece_0_0.svg, piece_0_1.svg, etc.\n")
cat("  - Combined view: combined_pieces.svg\n\n")

# Example 2: Standard 3x3 puzzle
cat("Example 2: 3x3 Puzzle (9 pieces)\n")
cat("----------------------------------\n")
cat("A classic puzzle size - demonstrates scaling\n")
cat("beyond the simple 2x2 case.\n\n")

result_3x3 <- generate_individual_pieces(
  seed = 1234,
  xn = 3,          # 3 columns
  yn = 3,          # 3 rows
  width = 300,     # 300mm width
  height = 300,    # 300mm height
  output_dir = file.path(demo_dir, "3x3_puzzle")
)

cat("Generated", result_3x3$parameters$total_pieces, "pieces\n")
cat("Files saved to:", file.path(demo_dir, "3x3_puzzle"), "\n\n")

# Example 3: Asymmetric puzzle
cat("Example 3: 5x4 Puzzle (20 pieces)\n")
cat("----------------------------------\n")
cat("Demonstrates that our implementation works for\n")
cat("ANY puzzle dimensions, not just square puzzles.\n\n")

result_5x4 <- generate_individual_pieces(
  seed = 5678,
  xn = 5,          # 5 columns
  yn = 4,          # 4 rows
  width = 250,     # 250mm width
  height = 200,    # 200mm height
  output_dir = file.path(demo_dir, "5x4_puzzle")
)

cat("Generated", result_5x4$parameters$total_pieces, "pieces\n")
cat("Files saved to:", file.path(demo_dir, "5x4_puzzle"), "\n\n")

# Example 4: Custom parameters
cat("Example 4: Customized Puzzle\n")
cat("----------------------------------\n")
cat("Demonstrates adjusting tab size and jitter\n")
cat("for different puzzle characteristics.\n\n")

result_custom <- generate_individual_pieces(
  seed = 9999,
  xn = 3,
  yn = 3,
  width = 300,
  height = 300,
  tabsize = 25,    # Larger tabs (default is 20)
  jitter = 8,      # More variation (default is 4)
  output_dir = file.path(demo_dir, "custom_puzzle")
)

cat("Generated with:\n")
cat("  - Tab size: 25% (larger tabs)\n")
cat("  - Jitter: 8% (more variation)\n")
cat("Files saved to:", file.path(demo_dir, "custom_puzzle"), "\n\n")

# Example 5: Large puzzle for printing
cat("Example 5: Large Puzzle (10x8 = 80 pieces)\n")
cat("----------------------------------\n")
cat("Demonstrates scalability - generates many pieces\n")
cat("suitable for printing and cutting.\n\n")

result_large <- generate_individual_pieces(
  seed = 2024,
  xn = 10,
  yn = 8,
  width = 500,     # 500mm = ~19.7 inches
  height = 400,    # 400mm = ~15.7 inches
  output_dir = file.path(demo_dir, "large_puzzle"),
  save_combined = TRUE  # Also save combined view
)

cat("Generated", result_large$parameters$total_pieces, "pieces\n")
cat("Puzzle dimensions:", result_large$parameters$size[1], "x",
    result_large$parameters$size[2], "mm\n")
cat("Files saved to:", file.path(demo_dir, "large_puzzle"), "\n\n")

# Summary
cat("========================================\n")
cat("Demo Summary\n")
cat("========================================\n\n")

cat("Key Features Demonstrated:\n")
cat("✓ Generate puzzles of ANY size (2x2, 3x3, 5x4, 10x8, etc.)\n")
cat("✓ Proper complementary edges between adjacent pieces\n")
cat("✓ Customizable parameters (tab size, jitter)\n")
cat("✓ Deterministic output (same seed = same puzzle)\n")
cat("✓ Individual SVG files for each piece\n")
cat("✓ Combined view for visualization\n\n")

cat("Understanding the Output:\n")
cat("--------------------------\n")
cat("Piece naming: piece_X_Y.svg where:\n")
cat("  - X = column index (0-based, left to right)\n")
cat("  - Y = row index (0-based, top to bottom)\n\n")

cat("Example for 2x2 puzzle:\n")
cat("  piece_0_0.svg = Top-left corner\n")
cat("  piece_1_0.svg = Top-right corner\n")
cat("  piece_0_1.svg = Bottom-left corner\n")
cat("  piece_1_1.svg = Bottom-right corner\n\n")

cat("Piece Types:\n")
cat("  - Corner pieces: 2 straight edges (rounded)\n")
cat("  - Edge pieces: 1 straight edge\n")
cat("  - Interior pieces: All curved edges\n\n")

cat("Parameters Explained:\n")
cat("---------------------\n")
cat("seed      : Random seed for reproducibility (same seed = same puzzle)\n")
cat("xn        : Number of columns (pieces across)\n")
cat("yn        : Number of rows (pieces down)\n")
cat("width     : Puzzle width in millimeters\n")
cat("height    : Puzzle height in millimeters\n")
cat("tabsize   : Tab size as percentage (10-30, default: 20)\n")
cat("            Larger = more pronounced tabs\n")
cat("jitter    : Variation in tab placement (0-10, default: 4)\n")
cat("            Larger = more irregular pieces\n\n")

cat("Next Steps:\n")
cat("-----------\n")
cat("1. Open the generated SVG files in a vector editor (Inkscape, Illustrator)\n")
cat("2. Use for laser cutting, printing, or digital applications\n")
cat("3. Experiment with different seeds to find your favorite puzzle pattern\n")
cat("4. Try larger puzzles for more challenging designs\n\n")

cat("Technical Notes:\n")
cat("----------------\n")
cat("- All pieces use proper Bézier curves for smooth edges\n")
cat("- Edge complementarity ensures perfect fit between adjacent pieces\n")
cat("- Border pieces include rounded corners (2mm radius by default)\n")
cat("- All coordinates are in millimeters for accurate manufacturing\n\n")

cat("Demo complete! Check '", demo_dir, "' for all generated files.\n", sep = "")
cat("Total files generated: ",
    result_2x2$parameters$total_pieces +
    result_3x3$parameters$total_pieces +
    result_5x4$parameters$total_pieces +
    result_custom$parameters$total_pieces +
    result_large$parameters$total_pieces + 5, " SVG files\n", sep = "")
