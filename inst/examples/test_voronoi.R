# Test Voronoi Puzzle Generation
# Quick verification that the Voronoi puzzle type works correctly

library(jigsawR)

# Check if deldir is available
if (!requireNamespace("deldir", quietly = TRUE)) {
  cat("Installing deldir package...\n")
  install.packages("deldir")
}

# Test 1: Basic Voronoi puzzle with Fermat spiral
cat("\n=== Test 1: Basic Voronoi puzzle (Fermat spiral) ===\n")
result <- tryCatch({
  generate_puzzle(
    type = "voronoi",
    seed = 42,
    grid = c(20),        # 20 cells
    size = c(200, 200),  # 200x200mm
    tabsize = 20,
    jitter = 4,
    offset = 0,          # Complete puzzle
    fill_color = "none",
    stroke_width = 1.5,
    save_files = TRUE,
    output_dir = "output",
    filename_prefix = "voronoi_test1"
  )
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
  NULL
})

if (!is.null(result)) {
  cat("Success!\n")
  cat("  - Pieces generated:", length(result$pieces), "\n")
  cat("  - Canvas size:", paste(round(result$canvas_size, 1), collapse = " x "), "\n")
  cat("  - Type:", result$type, "\n")
  if (length(result$files) > 0) {
    cat("  - Files saved:", paste(unlist(result$files), collapse = ", "), "\n")
  }
}

# Test 2: Separated Voronoi puzzle
cat("\n=== Test 2: Separated Voronoi puzzle ===\n")
result2 <- tryCatch({
  generate_puzzle(
    type = "voronoi",
    seed = 42,
    grid = c(15),
    size = c(200, 200),
    offset = 10,         # Separated pieces
    save_files = TRUE,
    output_dir = "output",
    filename_prefix = "voronoi_test2_separated"
  )
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
  NULL
})

if (!is.null(result2)) {
  cat("Success!\n")
  cat("  - Pieces generated:", length(result2$pieces), "\n")
  cat("  - Canvas size:", paste(round(result2$canvas_size, 1), collapse = " x "), "\n")
}

# Test 3: Uniform point distribution
cat("\n=== Test 3: Uniform point distribution ===\n")
# Note: This requires passing point_distribution parameter
# For now, test that the default (Fermat) works

cat("\nVoronoi puzzle tests complete!\n")
