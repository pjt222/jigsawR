# Test gradient background in separated modes
# Run with: Rscript tests/test_gradient_background.R

cat("=== Testing Gradient Background in Separated Modes ===\n\n")

# Source all required files
source("R/logging.R")
source("R/config_utils.R")
source("R/rectangular_puzzle.R")
source("R/puzzle_core_clean.R")
source("R/puzzle_separation.R")
source("R/hexagonal_puzzle.R")
source("R/hexagonal_separation.R")

# Test gradient background configuration
gradient_bg <- list(
  type = "gradient",
  center = "#ff0000",
  middle = "#00ff00",
  edge = "#0000ff"
)

results <- list()

# Test 1: Rectangular Separated with Custom Gradient
cat("1. Rectangular Separated with Custom Gradient:\n")
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
    background = gradient_bg
  )

  # Check for gradient elements
  has_gradient_def <- grepl('radialGradient id="bg-gradient"', svg)
  has_center_color <- grepl('#ff0000', svg)
  has_middle_color <- grepl('#00ff00', svg)
  has_edge_color <- grepl('#0000ff', svg)
  has_gradient_fill <- grepl('fill="url\\(#bg-gradient\\)"', svg)

  if (has_gradient_def && has_center_color && has_middle_color && has_edge_color && has_gradient_fill) {
    cat("  [PASS] Custom gradient background found\n")
    results$rect_custom <- TRUE
  } else {
    cat("  [FAIL] Missing gradient elements\n")
    cat(sprintf("    gradient def: %s, colors: %s/%s/%s, fill: %s\n",
                has_gradient_def, has_center_color, has_middle_color, has_edge_color, has_gradient_fill))
    results$rect_custom <- FALSE
  }
}, error = function(e) {
  cat(sprintf("  [ERROR] %s\n", e$message))
  results$rect_custom <<- FALSE
})

# Test 2: Rectangular Separated with Legacy Gradient
cat("\n2. Rectangular Separated with Legacy Gradient:\n")
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
    background = "gradient"
  )

  # Check for default gradient colors
  has_gradient_def <- grepl('radialGradient id="bg-gradient"', svg)
  has_default_colors <- grepl('#e3f2fd', svg) && grepl('#bbdefb', svg) && grepl('#90caf9', svg)

  if (has_gradient_def && has_default_colors) {
    cat("  [PASS] Legacy gradient background found\n")
    results$rect_legacy <- TRUE
  } else {
    cat("  [FAIL] Missing gradient elements\n")
    results$rect_legacy <- FALSE
  }
}, error = function(e) {
  cat(sprintf("  [ERROR] %s\n", e$message))
  results$rect_legacy <<- FALSE
})

# Test 3: Hexagonal Separated with Custom Gradient
cat("\n3. Hexagonal Separated with Custom Gradient:\n")
tryCatch({
  svg <- generate_separated_hexagonal_svg(
    rings = 3,
    seed = 42,
    diameter = 240,
    offset = 10,
    background = gradient_bg
  )

  # Check for gradient elements
  has_gradient_def <- grepl('radialGradient id="bg-gradient"', svg)
  has_center_color <- grepl('#ff0000', svg)
  has_middle_color <- grepl('#00ff00', svg)
  has_edge_color <- grepl('#0000ff', svg)
  has_gradient_fill <- grepl('fill="url\\(#bg-gradient\\)"', svg)

  if (has_gradient_def && has_center_color && has_middle_color && has_edge_color && has_gradient_fill) {
    cat("  [PASS] Custom gradient background found\n")
    results$hex_custom <- TRUE
  } else {
    cat("  [FAIL] Missing gradient elements\n")
    cat(sprintf("    gradient def: %s, colors: %s/%s/%s, fill: %s\n",
                has_gradient_def, has_center_color, has_middle_color, has_edge_color, has_gradient_fill))
    results$hex_custom <- FALSE
  }
}, error = function(e) {
  cat(sprintf("  [ERROR] %s\n", e$message))
  results$hex_custom <<- FALSE
})

# Test 4: Hexagonal Separated with Legacy Gradient
cat("\n4. Hexagonal Separated with Legacy Gradient:\n")
tryCatch({
  svg <- generate_separated_hexagonal_svg(
    rings = 3,
    seed = 42,
    diameter = 240,
    offset = 10,
    background = "gradient"
  )

  # Check for default gradient colors
  has_gradient_def <- grepl('radialGradient id="bg-gradient"', svg)
  has_default_colors <- grepl('#e3f2fd', svg) && grepl('#bbdefb', svg) && grepl('#90caf9', svg)

  if (has_gradient_def && has_default_colors) {
    cat("  [PASS] Legacy gradient background found\n")
    results$hex_legacy <- TRUE
  } else {
    cat("  [FAIL] Missing gradient elements\n")
    results$hex_legacy <- FALSE
  }
}, error = function(e) {
  cat(sprintf("  [ERROR] %s\n", e$message))
  results$hex_legacy <<- FALSE
})

# Test 5: Solid color background still works
cat("\n5. Solid Color Background (regression test):\n")
tryCatch({
  svg <- generate_separated_hexagonal_svg(
    rings = 3,
    seed = 42,
    diameter = 240,
    offset = 10,
    background = "#abcdef"
  )

  has_solid_bg <- grepl('fill="#abcdef"', svg)
  no_gradient <- !grepl('radialGradient', svg)

  if (has_solid_bg && no_gradient) {
    cat("  [PASS] Solid color background works correctly\n")
    results$solid <- TRUE
  } else {
    cat("  [FAIL] Solid color background issue\n")
    results$solid <- FALSE
  }
}, error = function(e) {
  cat(sprintf("  [ERROR] %s\n", e$message))
  results$solid <<- FALSE
})

# Test 6: No background works
cat("\n6. No Background (regression test):\n")
tryCatch({
  svg <- generate_separated_hexagonal_svg(
    rings = 3,
    seed = 42,
    diameter = 240,
    offset = 10,
    background = "none"
  )

  # Should have no rect with fill (except piece fills)
  no_bg_rect <- !grepl('<rect x="[^"]*" y="[^"]*" width="[^"]*" height="[^"]*" fill="[^"]*"/>', svg)

  if (no_bg_rect) {
    cat("  [PASS] No background rect added\n")
    results$none <- TRUE
  } else {
    cat("  [FAIL] Unexpected background rect found\n")
    results$none <- FALSE
  }
}, error = function(e) {
  cat(sprintf("  [ERROR] %s\n", e$message))
  results$none <<- FALSE
})

# Summary
cat("\n=== Summary ===\n")
passed <- sum(unlist(results))
total <- length(results)
cat(sprintf("Passed: %d/%d\n", passed, total))

if (passed == total) {
  cat("\nAll gradient background tests passed!\n")
} else {
  cat("\nSome tests failed. Please review.\n")
}
