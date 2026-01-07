# Test Script for Individual Pieces Generation
# Run this manually to verify the consolidated implementation

# Source required files
source("R/rectangular_puzzle.R")
source("R/puzzle_core_clean.R")
source("R/individual_pieces.R")

cat("="  , rep("=", 70), "\n", sep = "")
cat("Testing Individual Pieces Generation\n")
cat("=" , rep("=", 70), "\n\n", sep = "")

# Test 1: 2x2 puzzle (baseline test)
cat("Test 1: 2x2 Puzzle (seed = 42)\n")
cat("-" , rep("-", 70), "\n", sep = "")

result_2x2 <- generate_individual_pieces(
  seed = 42,
  xn = 2,
  yn = 2,
  width = 200,
  height = 200,
  output_dir = "output/test_2x2"
)

cat("\nGenerated files:\n")
files_2x2 <- list.files("output/test_2x2", pattern = "*.svg", full.names = FALSE)
print(files_2x2)

cat("\nPiece count:", length(result_2x2$pieces), "\n")
cat("Expected: 4 pieces\n")
cat("Status:", if(length(result_2x2$pieces) == 4) "✓ PASS" else "✗ FAIL", "\n\n")

# Test 2: 3x3 puzzle
cat("Test 2: 3x3 Puzzle (seed = 1234)\n")
cat("-", rep("-", 70), "\n", sep = "")

result_3x3 <- generate_individual_pieces(
  seed = 1234,
  xn = 3,
  yn = 3,
  width = 300,
  height = 300,
  output_dir = "output/test_3x3"
)

cat("\nPiece count:", length(result_3x3$pieces), "\n")
cat("Expected: 9 pieces\n")
cat("Status:", if(length(result_3x3$pieces) == 9) "✓ PASS" else "✗ FAIL", "\n\n")

# Test 3: 5x4 puzzle (asymmetric)
cat("Test 3: 5x4 Puzzle - Asymmetric (seed = 5678)\n")
cat("-", rep("-", 70), "\n", sep = "")

result_5x4 <- generate_individual_pieces(
  seed = 5678,
  xn = 5,
  yn = 4,
  width = 250,
  height = 200,
  output_dir = "output/test_5x4"
)

cat("\nPiece count:", length(result_5x4$pieces), "\n")
cat("Expected: 20 pieces\n")
cat("Status:", if(length(result_5x4$pieces) == 20) "✓ PASS" else "✗ FAIL", "\n\n")

# Verify edge structure
cat("="  , rep("=", 70), "\n", sep = "")
cat("Edge Structure Verification\n")
cat("="  , rep("=", 70), "\n\n", sep = "")

# Generate edges for 2x2 puzzle
init_jigsaw(seed = 42, xn = 2, yn = 2, width = 200, height = 200)
edges_2x2 <- generate_all_edges(2, 2)

cat("2x2 Puzzle Edge Count:\n")
cat("  Horizontal edges:", length(edges_2x2$horizontal), "rows\n")
cat("  Vertical edges:", length(edges_2x2$vertical), "columns\n")

# Check that edges have both forward and reverse paths
if (length(edges_2x2$horizontal) > 0 && length(edges_2x2$horizontal[[1]]) > 0) {
  sample_h_edge <- edges_2x2$horizontal[[1]][[1]]
  cat("\nSample horizontal edge has:\n")
  cat("  - forward path:", !is.null(sample_h_edge$forward), "\n")
  cat("  - reverse path:", !is.null(sample_h_edge$reverse), "\n")
  cat("  - start point:", paste(sample_h_edge$start, collapse = ", "), "\n")
  cat("  - end point:", paste(sample_h_edge$end, collapse = ", "), "\n")
}

if (length(edges_2x2$vertical) > 0 && length(edges_2x2$vertical[[1]]) > 0) {
  sample_v_edge <- edges_2x2$vertical[[1]][[1]]
  cat("\nSample vertical edge has:\n")
  cat("  - forward path:", !is.null(sample_v_edge$forward), "\n")
  cat("  - reverse path:", !is.null(sample_v_edge$reverse), "\n")
}

cat("\n", "=", rep("=", 70), "\n", sep = "")
cat("All tests completed!\n")
cat("Check output/test_* directories for generated SVG files\n")
cat("="  , rep("=", 70), "\n", sep = "")
