# Tests for main API functions in jigsawR package
# Updated to use current generate_puzzle() API (2025-12)

# =============================================================================
# RECTANGULAR PUZZLE TESTS
# =============================================================================

test_that("generate_puzzle produces valid output with default parameters", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    seed = 1234,
    save_files = FALSE
  )

  # Check return structure
  expect_type(result, "list")
  expect_true("svg_content" %in% names(result))
  expect_true("pieces" %in% names(result))
  expect_true("canvas_size" %in% names(result))
  expect_true("parameters" %in% names(result))

  # Check SVG content is valid
  expect_match(result$svg_content, "^<\\?xml")
  expect_match(result$svg_content, "</svg>$")

  # Check piece count (2x2 = 4 pieces)
  expect_equal(length(result$pieces), 4)
})

test_that("generate_puzzle is reproducible with same seed", {
  puzzle1 <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    seed = 5678,
    save_files = FALSE
  )

  puzzle2 <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    seed = 5678,
    save_files = FALSE
  )

  # Same seed should produce identical SVG content

  expect_identical(puzzle1$svg_content, puzzle2$svg_content)
  expect_identical(puzzle1$pieces[[1]]$path, puzzle2$pieces[[1]]$path)
})

test_that("generate_puzzle validates input parameters", {
  # Test invalid grid size
  expect_error(
    generate_puzzle(type = "rectangular", grid = c(0, 2), seed = 1234, save_files = FALSE)
  )

  # Test invalid type
  expect_error(
    generate_puzzle(type = "invalid_type", grid = c(2, 2), seed = 1234, save_files = FALSE),
    "Invalid type"
  )
})

test_that("generate_puzzle offset parameter works correctly", {
  # Test complete mode (offset = 0)
  result_complete <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    seed = 1234,
    offset = 0,
    save_files = FALSE
  )

  expect_type(result_complete$svg_content, "character")
  expect_equal(length(result_complete$pieces), 4)

  # Test separated mode (offset > 0)
  result_separated <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    seed = 1234,
    offset = 15,
    save_files = FALSE
  )

  expect_type(result_separated$svg_content, "character")
  expect_equal(length(result_separated$pieces), 4)

  # Separated canvas should be larger than complete
  expect_gt(result_separated$canvas_size[1], result_complete$canvas_size[1])
  expect_gt(result_separated$canvas_size[2], result_complete$canvas_size[2])
})

test_that("generate_puzzle handles colors correctly", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    seed = 1234,
    offset = 0,
    colors = c("red", "blue", "green", "orange"),
    save_files = FALSE
  )

  # Check that colors appear in SVG content
  svg_content <- result$svg_content
  expect_match(svg_content, "red")
  expect_match(svg_content, "blue")
  expect_match(svg_content, "green")
  expect_match(svg_content, "orange")
})

test_that("generate_puzzle returns correct piece count for different grids", {
  # 3x3 rectangular = 9 pieces
  result_3x3 <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    seed = 1234,
    save_files = FALSE
  )
  expect_equal(length(result_3x3$pieces), 9)

  # 2x4 rectangular = 8 pieces
  result_2x4 <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 4),
    seed = 1234,
    save_files = FALSE
  )
  expect_equal(length(result_2x4$pieces), 8)
})

# =============================================================================
# HEXAGONAL PUZZLE TESTS
# =============================================================================

test_that("generate_puzzle works with hexagonal type", {
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(3),
    size = c(200),
    seed = 1234,
    save_files = FALSE
  )

  expect_type(result, "list")
  expect_match(result$svg_content, "<svg")

  # 3 rings = 3*3*(3-1)+1 = 19 pieces
  expect_equal(length(result$pieces), 19)
  expect_equal(result$parameters$type, "hexagonal")
})

test_that("generate_puzzle hexagonal with warp and trunc", {
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(3),
    size = c(200),
    seed = 1234,
    do_warp = TRUE,
    do_trunc = TRUE,
    save_files = FALSE
  )

  expect_equal(length(result$pieces), 19)
  expect_true(result$parameters$do_warp)
  expect_true(result$parameters$do_trunc)
})

test_that("generate_puzzle hexagonal with circular border", {
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(3),
    size = c(200),
    seed = 1234,
    do_warp = TRUE,
    do_trunc = TRUE,
    do_circular_border = TRUE,
    save_files = FALSE
  )

  expect_equal(length(result$pieces), 19)
  expect_true(result$parameters$do_circular_border)

  # Circular border should use arc commands (A) in boundary paths
  # At least some pieces should have arc commands
  has_arc <- any(sapply(result$pieces, function(p) grepl(" A ", p$path)))
  expect_true(has_arc)
})

# =============================================================================
# CONCENTRIC PUZZLE TESTS
# =============================================================================

test_that("generate_puzzle works with concentric type", {
  result <- generate_puzzle(
    type = "concentric",
    grid = c(3),
    size = c(240),
    seed = 1234,
    center_shape = "hexagon",
    save_files = FALSE
  )

  expect_type(result, "list")
  expect_match(result$svg_content, "<svg")

  # 3 rings = 3*3*(3-1)+1 = 19 pieces
  expect_equal(length(result$pieces), 19)
  expect_equal(result$parameters$type, "concentric")
  expect_equal(result$parameters$center_shape, "hexagon")
})

test_that("generate_puzzle concentric with circle center", {
  result <- generate_puzzle(
    type = "concentric",
    grid = c(3),
    size = c(240),
    seed = 1234,
    center_shape = "circle",
    save_files = FALSE
  )

  expect_equal(length(result$pieces), 19)
  expect_equal(result$parameters$center_shape, "circle")
})

test_that("generate_puzzle concentric with offset", {
  result_complete <- generate_puzzle(
    type = "concentric",
    grid = c(3),
    size = c(240),
    seed = 1234,
    offset = 0,
    save_files = FALSE
  )

  result_separated <- generate_puzzle(
    type = "concentric",
    grid = c(3),
    size = c(240),
    seed = 1234,
    offset = 15,
    save_files = FALSE
  )

  # Both should have same number of pieces
  expect_equal(length(result_complete$pieces), 19)
  expect_equal(length(result_separated$pieces), 19)

  # Separated canvas should be larger
  expect_gt(result_separated$canvas_size[1], result_complete$canvas_size[1])
})

# =============================================================================
# STYLING TESTS
# =============================================================================

test_that("generate_puzzle respects palette parameter", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    seed = 1234,
    palette = "viridis",
    save_files = FALSE
  )

  expect_equal(result$parameters$palette, "viridis")
  # SVG should contain fill colors
  expect_match(result$svg_content, "fill=")
})

test_that("generate_puzzle respects opacity parameter", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    seed = 1234,
    opacity = 0.5,
    save_files = FALSE
  )

  expect_equal(result$parameters$opacity, 0.5)
  # SVG should contain opacity attribute
  expect_match(result$svg_content, "opacity")
})

test_that("generate_puzzle respects show_labels parameter", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    seed = 1234,
    show_labels = TRUE,
    save_files = FALSE
  )

  expect_true(result$parameters$show_labels)
  # SVG should contain text elements for labels
  expect_match(result$svg_content, "<text")
})

# =============================================================================
# PIECE STRUCTURE TESTS
# =============================================================================

test_that("pieces have correct structure", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    seed = 1234,
    save_files = FALSE
  )

  for (piece in result$pieces) {
    expect_true("id" %in% names(piece))
    expect_true("path" %in% names(piece))
    expect_true("center" %in% names(piece))

    # Path should be valid SVG path (starts with M, ends with Z)
    expect_match(piece$path, "^M ")
    expect_match(piece$path, "Z$")
  }
})

# =============================================================================
# BATCH GENERATION TESTS
# =============================================================================

test_that("generate_puzzle_batch works with multiple configurations", {
  skip_if_not(exists("generate_puzzle_batch"), "generate_puzzle_batch not available")

  configs <- list(
    list(type = "rectangular", grid = c(2, 2), seed = 100, tabsize = 20),
    list(type = "hexagonal", grid = c(3), size = c(200), seed = 200),
    list(type = "concentric", grid = c(3), size = c(240), seed = 300)
  )

  results <- generate_puzzle_batch(
    variations = configs,
    base_dir = tempdir()
  )

  expect_equal(length(results), length(configs))

  for (i in seq_along(results)) {
    expect_type(results[[i]], "list")
    expect_true("svg_content" %in% names(results[[i]]))
  }
})

# =============================================================================
# BACKWARD COMPATIBILITY TESTS
# =============================================================================

test_that("deprecated output parameter still works with warning", {
  # This should work but emit a deprecation warning
  expect_warning(
    result <- generate_puzzle(
      type = "rectangular",
      grid = c(2, 2),
      seed = 1234,
      output = "complete",
      save_files = FALSE
    ),
    "deprecated"
  )

  expect_type(result$svg_content, "character")
})
