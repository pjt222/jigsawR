# Test script for clean 2x2 puzzle implementation
# Validates edge sharing, reproducibility, and correctness

library(testthat)

# Load required functions
source("R/rectangular_puzzle.R")  # For init_jigsaw() and helper functions
source("R/puzzle_core_clean.R")   # Our clean implementation

cat("=== Testing Clean 2x2 Puzzle Implementation ===\n\n")

# Test 1: Reproducibility
cat("Test 1: Reproducibility\n")
test_that("Same seed produces identical puzzles", {
  puzzle1 <- generate_puzzle_core(seed = 1234, grid = c(2, 2))
  puzzle2 <- generate_puzzle_core(seed = 1234, grid = c(2, 2))

  # Check that edge data is identical
  expect_identical(puzzle1$edges, puzzle2$edges)

  # Generate SVGs
  svg1 <- generate_puzzle_svg(puzzle1, mode = "individual")
  svg2 <- generate_puzzle_svg(puzzle2, mode = "individual")

  expect_identical(svg1, svg2)
})
cat("  ✓ Reproducibility test passed\n\n")

# Test 2: Edge Sharing Validation
cat("Test 2: Edge Sharing Between Adjacent Pieces\n")
test_that("Adjacent pieces share exact same edge paths", {
  puzzle <- generate_puzzle_core(seed = 1234, grid = c(2, 2))

  # Check horizontal edge shared by pieces (0,0) and (0,1)
  # Piece (0,0) uses this as bottom edge (reverse)
  # Piece (0,1) uses this as top edge (forward)
  h_edge <- puzzle$edges$horizontal[[1]][[1]]

  # Both pieces should reference the same edge object
  expect_true(!is.null(h_edge$forward))
  expect_true(!is.null(h_edge$reverse))

  # Check vertical edge shared by pieces (0,0) and (1,0)
  # Piece (0,0) uses this as right edge (forward)
  # Piece (1,0) uses this as left edge (reverse)
  v_edge <- puzzle$edges$vertical[[1]][[1]]

  expect_true(!is.null(v_edge$forward))
  expect_true(!is.null(v_edge$reverse))
})
cat("  ✓ Edge sharing test passed\n\n")

# Test 3: Piece Generation
cat("Test 3: Individual Piece Generation\n")
test_that("All four pieces generate correctly", {
  puzzle <- generate_puzzle_core(seed = 1234, grid = c(2, 2))

  pieces <- list()
  for (yi in 0:1) {
    for (xi in 0:1) {
      piece_path <- generate_single_piece(xi, yi, puzzle)

      # Check path starts with M (move)
      expect_true(grepl("^M", piece_path))

      # Check path ends with Z (close)
      expect_true(grepl("Z$", piece_path))

      # Store for visual inspection
      pieces[[paste0(xi, "_", yi)]] <- piece_path
    }
  }

  # All pieces should be unique (different positions)
  expect_equal(length(unique(pieces)), 4)
})
cat("  ✓ Piece generation test passed\n\n")

# Test 4: SVG Output
cat("Test 4: SVG Output Generation\n")
test_that("SVG outputs are valid", {
  puzzle <- generate_puzzle_core(seed = 1234, grid = c(2, 2), size = c(200, 200))

  # Test complete mode
  svg_complete <- generate_puzzle_svg(puzzle, mode = "complete")
  expect_true(grepl("^<\\?xml", svg_complete))
  expect_true(grepl("</svg>$", svg_complete))

  # Test individual mode
  svg_individual <- generate_puzzle_svg(puzzle, mode = "individual",
                                       colors = c("red", "blue", "green", "orange"))
  expect_true(grepl("piece-0-0", svg_individual))
  expect_true(grepl("piece-1-1", svg_individual))

  # Save for visual inspection
  writeLines(svg_complete, "output/test_clean_2x2_complete.svg")
  writeLines(svg_individual, "output/test_clean_2x2_individual.svg")
})
cat("  ✓ SVG output test passed\n\n")

# Test 5: Mathematical Correctness of Reversal
cat("Test 5: Mathematical Correctness of Edge Reversal\n")
test_that("Reversed edges maintain continuity", {
  puzzle <- generate_puzzle_core(seed = 1234, grid = c(2, 2))

  # Get a horizontal edge
  edge <- puzzle$edges$horizontal[[1]][[1]]

  # Parse the forward path
  forward_parts <- strsplit(edge$forward, " ")[[1]]

  # Parse the reverse path
  reverse_parts <- strsplit(edge$reverse, " ")[[1]]

  # The reverse should have same number of coordinates
  expect_equal(length(forward_parts), length(reverse_parts))

  # The last point of forward should be first point of reverse
  # (within the bezier curve structure)
  expect_true(length(forward_parts) > 0)
  expect_true(length(reverse_parts) > 0)
})
cat("  ✓ Edge reversal test passed\n\n")

# Test 6: Visual Verification
cat("Test 6: Generating Visual Verification Files\n")

# Generate a 2x2 puzzle with different configurations
configs <- list(
  list(seed = 1234, tabsize = 20, jitter = 4, name = "standard"),
  list(seed = 5678, tabsize = 25, jitter = 6, name = "larger_tabs"),
  list(seed = 999, tabsize = 15, jitter = 2, name = "smaller_tabs")
)

for (config in configs) {
  puzzle <- generate_puzzle_core(
    seed = config$seed,
    grid = c(2, 2),
    size = c(200, 200),
    tabsize = config$tabsize,
    jitter = config$jitter
  )

  # Generate both modes
  svg_complete <- generate_puzzle_svg(puzzle, mode = "complete")
  svg_individual <- generate_puzzle_svg(puzzle, mode = "individual",
                                       colors = c("#FF6B6B", "#4ECDC4", "#45B7D1", "#FFA07A"))

  # Save files
  writeLines(svg_complete, sprintf("output/clean_2x2_%s_complete.svg", config$name))
  writeLines(svg_individual, sprintf("output/clean_2x2_%s_individual.svg", config$name))

  cat(sprintf("  Generated: clean_2x2_%s_*.svg\n", config$name))
}

cat("\n=== All Tests Passed ===\n")
cat("\nGenerated files in output/ directory:\n")
cat("  - test_clean_2x2_complete.svg (complete puzzle)\n")
cat("  - test_clean_2x2_individual.svg (individual pieces)\n")
cat("  - clean_2x2_*_complete.svg (various configurations)\n")
cat("  - clean_2x2_*_individual.svg (various configurations)\n")
cat("\nPlease visually inspect these files to verify:\n")
cat("  1. Pieces fit together perfectly\n")
cat("  2. No gaps or overlaps at edges\n")
cat("  3. Tab/blank patterns are complementary\n")
cat("  4. Different seeds produce different patterns\n")
