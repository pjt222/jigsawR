# Test Random Shape Puzzle Generation
# Quick verification that the Random Shape puzzle type works correctly

library(jigsawR)

# Check if RCDT is available
if (!requireNamespace("RCDT", quietly = TRUE)) {
  cat("Installing RCDT package...\n")
  install.packages("RCDT")
}

# Test 1: Basic Random Shape puzzle (rectangle boundary)
cat("\n=== Test 1: Basic Random Shape puzzle (rectangle) ===\n")
result <- tryCatch({
  generate_puzzle(
    type = "random",
    seed = 42,
    grid = c(10),        # 10 interior points
    size = c(200, 200),  # 200x200mm
    tabsize = 20,
    jitter = 4,
    offset = 0,          # Complete puzzle
    fill_color = "none",
    stroke_width = 1.5,
    save_files = TRUE,
    output_dir = "output",
    filename_prefix = "random_test1"
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

# Test 2: Separated Random Shape puzzle
cat("\n=== Test 2: Separated Random Shape puzzle ===\n")
result2 <- tryCatch({
  generate_puzzle(
    type = "random",
    seed = 42,
    grid = c(8),
    size = c(200, 200),
    offset = 10,         # Separated pieces
    save_files = TRUE,
    output_dir = "output",
    filename_prefix = "random_test2_separated"
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

# Test 3: Hexagonal boundary
cat("\n=== Test 3: Hexagonal boundary (6 corners) ===\n")
result3 <- tryCatch({
  generate_puzzle(
    type = "random",
    seed = 123,
    grid = c(12),
    size = c(200, 200),
    n_corner = 6,        # Hexagonal boundary
    offset = 0,
    save_files = TRUE,
    output_dir = "output",
    filename_prefix = "random_test3_hex"
  )
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
  NULL
})

if (!is.null(result3)) {
  cat("Success!\n")
  cat("  - Pieces generated:", length(result3$pieces), "\n")
  cat("  - Canvas size:", paste(round(result3$canvas_size, 1), collapse = " x "), "\n")
}

# Test 4: Triangular boundary
cat("\n=== Test 4: Triangular boundary (3 corners) ===\n")
result4 <- tryCatch({
  generate_puzzle(
    type = "random",
    seed = 456,
    grid = c(6),
    size = c(200, 200),
    n_corner = 3,        # Triangular boundary
    offset = 0,
    save_files = TRUE,
    output_dir = "output",
    filename_prefix = "random_test4_tri"
  )
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
  NULL
})

if (!is.null(result4)) {
  cat("Success!\n")
  cat("  - Pieces generated:", length(result4$pieces), "\n")
  cat("  - Canvas size:", paste(round(result4$canvas_size, 1), collapse = " x "), "\n")
}

# Test 5: Palette coloring
cat("\n=== Test 5: Random puzzle with palette coloring ===\n")
result5 <- tryCatch({
  generate_puzzle(
    type = "random",
    seed = 42,
    grid = c(15),
    size = c(250, 200),
    offset = 0,
    fill_color = "palette",
    palette = "viridis",
    save_files = TRUE,
    output_dir = "output",
    filename_prefix = "random_colored"
  )
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
  NULL
})

if (!is.null(result5)) {
  cat("Success!\n")
  cat("  - Pieces generated:", length(result5$pieces), "\n")
  if (length(result5$files) > 0) {
    cat("  - Files saved:", paste(unlist(result5$files), collapse = ", "), "\n")
  }
}

cat("\nRandom Shape puzzle tests complete!\n")
