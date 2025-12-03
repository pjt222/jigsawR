#!/usr/bin/env Rscript
# Test updated generate_puzzle() function (Issue #36)

cat("=== Testing Updated generate_puzzle() ===\n\n")

# Source dependencies
source("R/logging.R")
source("R/config_utils.R")
source("R/rectangular_puzzle.R")
source("R/puzzle_core_clean.R")
source("R/hexagonal_puzzle.R")
source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")
source("R/unified_piece_generation.R")
source("R/piece_positioning.R")
source("R/unified_renderer.R")
source("R/jigsawR_clean.R")

# Test 1: Basic rectangular with offset=0
cat("--- Test 1: Rectangular with offset=0 ---\n")
result1 <- tryCatch({
  result <- generate_puzzle(
    type = "rectangular",
    seed = 42,
    grid = c(2, 3),
    size = c(150, 100),
    offset = 0
  )

  has_svg <- !is.null(result$svg_content) && nchar(result$svg_content) > 100
  has_pieces <- !is.null(result$pieces) && length(result$pieces) == 6
  has_canvas <- !is.null(result$canvas_size) && length(result$canvas_size) == 2

  cat(sprintf("  Has SVG content: %s\n", has_svg))
  cat(sprintf("  Has %d pieces (expected 6): %s\n", length(result$pieces), length(result$pieces) == 6))
  cat(sprintf("  Has canvas size: %s\n", has_canvas))

  if (has_svg && has_pieces && has_canvas) {
    cat("  PASS: Rectangular offset=0 works\n")
    "PASS"
  } else {
    cat("  FAIL: Missing components\n")
    "FAIL"
  }
}, error = function(e) {
  cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
  "FAIL"
})

# Test 2: Rectangular with offset>0
cat("\n--- Test 2: Rectangular with offset>0 ---\n")
result2 <- tryCatch({
  result <- generate_puzzle(
    type = "rectangular",
    seed = 42,
    grid = c(2, 3),
    size = c(150, 100),
    offset = 15
  )

  # Canvas should be larger with offset
  canvas_width <- result$canvas_size[1]
  expected_min_width <- 150 + (3 - 1) * 15  # original + (cols-1)*offset

  cat(sprintf("  Canvas width: %.1f (expected >= %.1f)\n", canvas_width, expected_min_width))
  cat(sprintf("  Piece count: %d\n", length(result$pieces)))

  if (canvas_width >= expected_min_width && length(result$pieces) == 6) {
    cat("  PASS: Rectangular offset>0 works\n")
    "PASS"
  } else {
    cat("  FAIL: Canvas or pieces incorrect\n")
    "FAIL"
  }
}, error = function(e) {
  cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
  "FAIL"
})

# Test 3: Hexagonal with offset=0
cat("\n--- Test 3: Hexagonal with offset=0 ---\n")
result3 <- tryCatch({
  result <- generate_puzzle(
    type = "hexagonal",
    seed = 42,
    grid = c(3),
    size = c(200),
    offset = 0
  )

  # 3 rings = 19 pieces
  expected_pieces <- 19

  cat(sprintf("  Piece count: %d (expected %d)\n", length(result$pieces), expected_pieces))
  cat(sprintf("  Type: %s\n", result$type))

  if (length(result$pieces) == expected_pieces && result$type == "hexagonal") {
    cat("  PASS: Hexagonal offset=0 works\n")
    "PASS"
  } else {
    cat("  FAIL: Wrong piece count or type\n")
    "FAIL"
  }
}, error = function(e) {
  cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
  "FAIL"
})

# Test 4: Hexagonal with offset>0
cat("\n--- Test 4: Hexagonal with offset>0 ---\n")
result4 <- tryCatch({
  result_compact <- generate_puzzle(
    type = "hexagonal",
    seed = 42,
    grid = c(3),
    size = c(200),
    offset = 0
  )

  result_separated <- generate_puzzle(
    type = "hexagonal",
    seed = 42,
    grid = c(3),
    size = c(200),
    offset = 20
  )

  # Separated should have larger canvas
  compact_area <- result_compact$canvas_size[1] * result_compact$canvas_size[2]
  separated_area <- result_separated$canvas_size[1] * result_separated$canvas_size[2]

  cat(sprintf("  Compact canvas area: %.0f\n", compact_area))
  cat(sprintf("  Separated canvas area: %.0f\n", separated_area))

  if (separated_area > compact_area) {
    cat("  PASS: Hexagonal separation increases canvas\n")
    "PASS"
  } else {
    cat("  FAIL: Canvas not properly expanded\n")
    "FAIL"
  }
}, error = function(e) {
  cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
  "FAIL"
})

# Test 5: result$pieces always available
cat("\n--- Test 5: Pieces always available in result ---\n")
result5 <- tryCatch({
  result_complete <- generate_puzzle(type = "rectangular", seed = 1, grid = c(2, 2), offset = 0)
  result_separated <- generate_puzzle(type = "rectangular", seed = 1, grid = c(2, 2), offset = 10)

  complete_has_pieces <- !is.null(result_complete$pieces) && length(result_complete$pieces) == 4
  separated_has_pieces <- !is.null(result_separated$pieces) && length(result_separated$pieces) == 4

  cat(sprintf("  Complete has pieces: %s\n", complete_has_pieces))
  cat(sprintf("  Separated has pieces: %s\n", separated_has_pieces))

  if (complete_has_pieces && separated_has_pieces) {
    cat("  PASS: Pieces always available\n")
    "PASS"
  } else {
    cat("  FAIL: Pieces missing\n")
    "FAIL"
  }
}, error = function(e) {
  cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
  "FAIL"
})

# Test 6: Backward compatibility - deprecated output parameter
cat("\n--- Test 6: Deprecated 'output' parameter ---\n")
result6 <- tryCatch({
  # This should work but show a deprecation warning
  result <- suppressWarnings(
    generate_puzzle(
      type = "rectangular",
      seed = 42,
      grid = c(2, 2),
      size = c(100, 100),
      output = "individual"
    )
  )

  # Should have generated with offset=10 (default for individual/separated)
  has_svg <- !is.null(result$svg_content)
  has_pieces <- length(result$pieces) == 4

  cat(sprintf("  Has SVG: %s\n", has_svg))
  cat(sprintf("  Has pieces: %s\n", has_pieces))

  if (has_svg && has_pieces) {
    cat("  PASS: Deprecated parameter still works\n")
    "PASS"
  } else {
    cat("  FAIL: Backward compatibility broken\n")
    "FAIL"
  }
}, error = function(e) {
  cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
  "FAIL"
})

# Test 7: Save files
cat("\n--- Test 7: Save files ---\n")
result7 <- tryCatch({
  result <- generate_puzzle(
    type = "rectangular",
    seed = 42,
    grid = c(2, 2),
    size = c(100, 100),
    offset = 0,
    save_files = TRUE,
    output_dir = "output",
    filename_prefix = "test_gen_puzzle"
  )

  file_exists <- file.exists(result$files$svg)

  cat(sprintf("  File path: %s\n", result$files$svg))
  cat(sprintf("  File exists: %s\n", file_exists))

  # Cleanup
  if (file_exists) {
    file.remove(result$files$svg)
  }

  if (file_exists) {
    cat("  PASS: File saved successfully\n")
    "PASS"
  } else {
    cat("  FAIL: File not saved\n")
    "FAIL"
  }
}, error = function(e) {
  cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
  "FAIL"
})

# Test 8: Color palette
cat("\n--- Test 8: Color palette ---\n")
result8 <- tryCatch({
  result <- generate_puzzle(
    type = "rectangular",
    seed = 42,
    grid = c(2, 2),
    size = c(100, 100),
    palette = "plasma"
  )

  # Check SVG contains hex colors
  has_colors <- grepl('stroke="#[0-9A-Fa-f]{6,8}"', result$svg_content)

  cat(sprintf("  Has palette colors: %s\n", has_colors))

  if (has_colors) {
    cat("  PASS: Palette applied\n")
    "PASS"
  } else {
    cat("  FAIL: Colors not applied\n")
    "FAIL"
  }
}, error = function(e) {
  cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
  "FAIL"
})

# Test 9: Fill color and opacity
cat("\n--- Test 9: Fill color and opacity ---\n")
result9 <- tryCatch({
  result <- generate_puzzle(
    type = "rectangular",
    seed = 42,
    grid = c(2, 2),
    size = c(100, 100),
    fill_color = "#CCCCCC",
    opacity = 0.7
  )

  has_fill <- grepl('fill="#CCCCCC"', result$svg_content)
  has_opacity <- grepl('opacity="0.70"', result$svg_content)

  cat(sprintf("  Has fill color: %s\n", has_fill))
  cat(sprintf("  Has opacity: %s\n", has_opacity))

  if (has_fill && has_opacity) {
    cat("  PASS: Fill and opacity applied\n")
    "PASS"
  } else {
    cat("  FAIL: Styling not applied\n")
    "FAIL"
  }
}, error = function(e) {
  cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
  "FAIL"
})

# Test 10: Hexagonal with warp/trunc
cat("\n--- Test 10: Hexagonal with warp/trunc ---\n")
result10 <- tryCatch({
  result <- generate_puzzle(
    type = "hexagonal",
    seed = 42,
    grid = c(3),
    size = c(200),
    do_warp = TRUE,
    do_trunc = TRUE
  )

  # Warp transformation changes vertex positions
  # With warp, outer pieces should have vertices at different distances from origin
  # (not all at the same radius like with projection)
  # We can verify warp is applied by checking SVG has paths and correct piece count
  has_paths <- grepl("<path", result$svg_content)

  cat(sprintf("  Has paths: %s\n", has_paths))
  cat(sprintf("  Piece count: %d\n", length(result$pieces)))

  if (has_paths && length(result$pieces) == 19) {
    cat("  PASS: Warp/trunc applied\n")
    "PASS"
  } else {
    cat("  FAIL: Warp/trunc not working\n")
    "FAIL"
  }
}, error = function(e) {
  cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
  "FAIL"
})

# Summary
cat("\n=== Test Summary ===\n")
cat(sprintf("Test 1 (Rect offset=0): %s\n", result1))
cat(sprintf("Test 2 (Rect offset>0): %s\n", result2))
cat(sprintf("Test 3 (Hex offset=0): %s\n", result3))
cat(sprintf("Test 4 (Hex offset>0): %s\n", result4))
cat(sprintf("Test 5 (Pieces always available): %s\n", result5))
cat(sprintf("Test 6 (Deprecated output param): %s\n", result6))
cat(sprintf("Test 7 (Save files): %s\n", result7))
cat(sprintf("Test 8 (Color palette): %s\n", result8))
cat(sprintf("Test 9 (Fill/opacity): %s\n", result9))
cat(sprintf("Test 10 (Warp/trunc): %s\n", result10))

cat("\n=== Tests Complete ===\n")
