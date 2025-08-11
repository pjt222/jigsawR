# Clean jigsawR Usage Examples
# Demonstrates the clean, reproducible puzzle generation API

# Load required functions
source("R/rectangular_puzzle.R")  # Core functions
source("R/puzzle_core_clean.R")   # Clean implementation
source("R/jigsawR_clean.R")       # Main API

cat("=== jigsawR Clean Implementation Examples ===\n\n")
cat("This implementation ensures:\n")
cat("  - Reproducible output (same seed = same puzzle)\n")
cat("  - No hard-coded adjustments\n")
cat("  - Adjacent pieces share exact same edge paths\n")
cat("  - Clean separation of concerns\n\n")

# Example 1: Simple 2x2 puzzle
cat("Example 1: Simple 2x2 Puzzle\n")
cat("-" , rep("-", 40), "\n", sep="")

puzzle1 <- generate_puzzle(
  type = "rectangular",
  grid = c(2, 2),
  size = c(200, 200),
  seed = 1234,
  output = "both",
  filename_prefix = "example1_simple"
)

cat("Generated with seed:", puzzle1$seed, "\n\n")

# Example 2: Individual pieces with colors
cat("Example 2: Colored Individual Pieces\n")
cat("-" , rep("-", 40), "\n", sep="")

puzzle2 <- generate_puzzle(
  grid = c(3, 3),
  size = c(300, 300),
  seed = 5678,
  output = "individual",
  colors = c("#FF6B6B", "#4ECDC4", "#45B7D1", "#FFA07A", "#98D8C8", 
            "#F7DC6F", "#BB8FCE", "#85C1E2", "#F8B739"),
  filename_prefix = "example2_colored"
)

cat("\n")

# Example 3: Puzzle with gradient background
cat("Example 3: Puzzle with Gradient Background\n")
cat("-" , rep("-", 40), "\n", sep="")

puzzle3 <- generate_puzzle(
  grid = c(2, 2),
  size = c(250, 250),
  seed = 999,
  output = "both",
  background = "gradient",
  filename_prefix = "example3_gradient"
)

cat("\n")

# Example 4: Batch generation with variations
cat("Example 4: Batch Generation\n")
cat("-" , rep("-", 40), "\n", sep="")

variations <- list(
  list(name = "easy_4piece", seed = 100, grid = c(2, 2), tabsize = 25),
  list(name = "medium_9piece", seed = 200, grid = c(3, 3), tabsize = 20),
  list(name = "harder_16piece", seed = 300, grid = c(4, 4), tabsize = 18),
  list(name = "small_tabs", seed = 400, grid = c(2, 2), tabsize = 15, jitter = 2),
  list(name = "large_tabs", seed = 500, grid = c(2, 2), tabsize = 30, jitter = 6)
)

batch_results <- generate_puzzle_batch(variations, base_dir = "output/batch_example")

cat("\n")

# Example 5: Validation and reproducibility test
cat("Example 5: Validation and Reproducibility\n")
cat("-" , rep("-", 40), "\n", sep="")

# Generate puzzle structure for validation
puzzle_struct <- generate_puzzle_core(seed = 777, grid = c(2, 2))

# Validate that all edges are properly defined
validate_puzzle_fit(puzzle_struct)

# Test reproducibility
cat("\nReproducibility test:\n")
test1 <- generate_puzzle(seed = 12345, grid = c(2, 2), save_files = FALSE)
test2 <- generate_puzzle(seed = 12345, grid = c(2, 2), save_files = FALSE)

if (identical(test1$svg_complete, test2$svg_complete)) {
  cat("✓ Same seed produces identical output\n")
} else {
  cat("✗ Reproducibility check failed!\n")
}

# Example 6: Custom configuration
cat("\nExample 6: Custom Configuration\n")
cat("-" , rep("-", 40), "\n", sep="")

custom_puzzle <- generate_puzzle(
  type = "rectangular",
  grid = c(5, 4),      # 5 rows, 4 columns = 20 pieces
  size = c(400, 500),  # 400mm x 500mm
  seed = 2024,
  tabsize = 22,        # Medium-sized tabs
  jitter = 3,          # Low jitter for cleaner look
  output = "complete", # Just the complete puzzle
  background = "#E8F5E9",  # Light green background
  filename_prefix = "example6_custom"
)

cat("\n")

# Example 7: Programmatic piece access
cat("Example 7: Programmatic Piece Access\n")
cat("-" , rep("-", 40), "\n", sep="")

# Generate puzzle structure
puzzle <- generate_puzzle_core(seed = 888, grid = c(2, 2), size = c(200, 200))

# Access individual piece paths programmatically
cat("Generating piece paths programmatically:\n")
for (yi in 0:1) {
  for (xi in 0:1) {
    piece_path <- generate_single_piece(xi, yi, puzzle)
    
    # Count path commands
    n_moves <- length(gregexpr("M", piece_path)[[1]])
    n_curves <- length(gregexpr("C", piece_path)[[1]]) 
    n_lines <- length(gregexpr("L", piece_path)[[1]])
    
    cat(sprintf("  Piece [%d,%d]: %d moves, %d curves, %d lines\n", 
                xi, yi, n_moves, n_curves, n_lines))
  }
}

# Summary
cat("\n=== Summary ===\n")
cat("Generated examples demonstrate:\n")
cat("  1. Simple 2x2 puzzle (both modes)\n")
cat("  2. Colored 3x3 individual pieces\n")
cat("  3. Puzzle with gradient background\n")
cat("  4. Batch generation with variations\n")
cat("  5. Validation and reproducibility\n")
cat("  6. Custom configuration (5x4 puzzle)\n")
cat("  7. Programmatic piece access\n")
cat("\nAll files saved to output/ directory\n")
cat("\nKey principles maintained:\n")
cat("  ✓ Deterministic generation (seed-based)\n")
cat("  ✓ No hard-coded adjustments\n")
cat("  ✓ Clean, modular architecture\n")
cat("  ✓ Adjacent pieces share exact edge paths\n")