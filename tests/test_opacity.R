# Test opacity parameter across all puzzle generation modes
# Run with: Rscript tests/test_opacity.R

cat("=== Testing Opacity Parameter ===\n\n")

# Source all required files
source("R/logging.R")
source("R/config_utils.R")
source("R/rectangular_puzzle.R")
source("R/puzzle_core_clean.R")
source("R/individual_pieces.R")
source("R/puzzle_separation.R")
source("R/hexagonal_puzzle.R")
source("R/hexagonal_separation.R")
source("R/jigsawR_clean.R")

# Helper function to check if SVG contains opacity attribute
check_opacity_in_svg <- function(svg, expected_opacity, mode_name) {
  # Look for opacity="0.50" pattern (with some tolerance for formatting)
  pattern <- sprintf('opacity="%.2f"', expected_opacity)
  has_opacity <- grepl(pattern, svg, fixed = TRUE)

  if (has_opacity) {
    cat(sprintf("  [PASS] %s: Found opacity=%.2f\n", mode_name, expected_opacity))
  } else {
    # Check for any opacity attribute
    if (grepl('opacity="', svg)) {
      cat(sprintf("  [INFO] %s: Found opacity attribute but not %.2f\n", mode_name, expected_opacity))
      # Extract actual opacity values
      matches <- regmatches(svg, gregexpr('opacity="[0-9.]+"', svg))[[1]]
      cat(sprintf("         Actual values: %s\n", paste(unique(matches), collapse=", ")))
    } else {
      cat(sprintf("  [FAIL] %s: No opacity attribute found\n", mode_name))
    }
  }
  return(has_opacity)
}

test_opacity <- 0.5
results <- list()

cat("Testing with opacity = 0.5 (50%)\n\n")

# Test 1: Rectangular Complete
cat("1. Rectangular Complete:\n")
tryCatch({
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    size = c(200, 200),
    seed = 42,
    output = "complete",
    save_files = FALSE,
    opacity = test_opacity
  )
  results$rect_complete <- check_opacity_in_svg(result$svg_complete, test_opacity, "rectangular complete")
}, error = function(e) {
  cat(sprintf("  [ERROR] %s\n", e$message))
  results$rect_complete <<- FALSE
})

# Test 2: Rectangular Individual
cat("\n2. Rectangular Individual:\n")
tryCatch({
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    size = c(200, 200),
    seed = 42,
    output = "individual",
    save_files = FALSE,
    opacity = test_opacity
  )
  results$rect_individual <- check_opacity_in_svg(result$svg_individual, test_opacity, "rectangular individual")
}, error = function(e) {
  cat(sprintf("  [ERROR] %s\n", e$message))
  results$rect_individual <<- FALSE
})

# Test 3: Rectangular Separated
cat("\n3. Rectangular Separated:\n")
tryCatch({
  puzzle_struct <- generate_puzzle_core(
    seed = 42,
    grid = c(2, 2),
    size = c(200, 200),
    tabsize = 20,
    jitter = 4
  )
  svg <- generate_separated_puzzle_svg(
    puzzle_structure = puzzle_struct,
    offset = 10,
    opacity = test_opacity
  )
  results$rect_separated <- check_opacity_in_svg(svg, test_opacity, "rectangular separated")
}, error = function(e) {
  cat(sprintf("  [ERROR] %s\n", e$message))
  results$rect_separated <<- FALSE
})

# Test 4: Hexagonal Complete
cat("\n4. Hexagonal Complete:\n")
tryCatch({
  hex_result <- generate_hex_jigsaw_svg(
    rings = 3,
    diameter = 240,
    seed = 42,
    opacity = test_opacity
  )
  results$hex_complete <- check_opacity_in_svg(hex_result$svg, test_opacity, "hexagonal complete")
}, error = function(e) {
  cat(sprintf("  [ERROR] %s\n", e$message))
  results$hex_complete <<- FALSE
})

# Test 5: Hexagonal Separated
cat("\n5. Hexagonal Separated:\n")
tryCatch({
  svg <- generate_separated_hexagonal_svg(
    rings = 3,
    seed = 42,
    diameter = 240,
    offset = 10,
    opacity = test_opacity
  )
  results$hex_separated <- check_opacity_in_svg(svg, test_opacity, "hexagonal separated")
}, error = function(e) {
  cat(sprintf("  [ERROR] %s\n", e$message))
  results$hex_separated <<- FALSE
})

# Summary
cat("\n=== Summary ===\n")
passed <- sum(unlist(results))
total <- length(results)
cat(sprintf("Passed: %d/%d\n", passed, total))

if (passed == total) {
  cat("\nAll opacity tests passed!\n")
} else {
  cat("\nSome tests failed. Please review.\n")
}
