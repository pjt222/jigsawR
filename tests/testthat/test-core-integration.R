# Core integration tests - end-to-end workflow validation
# Updated to use current API (2025-12)

# =============================================================================
# FULL PIPELINE INTEGRATION
# =============================================================================

test_that("rectangular puzzle full pipeline works", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    size = c(300, 200),
    seed = 1234,
    offset = 0,
    palette = "viridis",
    save_files = FALSE
  )

  # Verify complete result structure

  expect_type(result, "list")
  expect_true("svg_content" %in% names(result))
  expect_true("pieces" %in% names(result))
  expect_true("canvas_size" %in% names(result))
  expect_true("parameters" %in% names(result))

  # Verify SVG is valid
  expect_match(result$svg_content, "^<\\?xml")
  expect_match(result$svg_content, "</svg>$")

  # Verify piece count
  expect_equal(length(result$pieces), 9)

  # Verify each piece has required fields
  for (piece in result$pieces) {
    expect_true("id" %in% names(piece))
    expect_true("path" %in% names(piece))
    expect_true("center" %in% names(piece))
  }
})

test_that("hexagonal puzzle full pipeline works", {
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(3),
    size = c(200),
    seed = 1234,
    offset = 0,
    do_warp = TRUE,
    do_trunc = TRUE,
    palette = "magma",
    save_files = FALSE
  )

  expect_type(result, "list")
  expect_match(result$svg_content, "<svg")
  expect_equal(length(result$pieces), 19)

  # Verify transformation parameters are stored
  expect_true(result$parameters$do_warp)
  expect_true(result$parameters$do_trunc)
})

test_that("concentric puzzle full pipeline works", {
  result <- generate_puzzle(
    type = "concentric",
    grid = c(3),
    size = c(240),
    seed = 1234,
    center_shape = "hexagon",
    palette = "plasma",
    save_files = FALSE
  )

  expect_type(result, "list")
  expect_match(result$svg_content, "<svg")
  expect_equal(length(result$pieces), 19)
  expect_equal(result$parameters$center_shape, "hexagon")
})

# =============================================================================
# CROSS-TYPE CONSISTENCY
# =============================================================================

test_that("all three types produce valid SVG with same structure", {
  result_rect <- generate_puzzle(
    type = "rectangular", grid = c(2, 2), size = c(100, 100),
    seed = 42, save_files = FALSE
  )

  result_hex <- generate_puzzle(
    type = "hexagonal", grid = c(2), size = c(100),
    seed = 42, save_files = FALSE
  )

  result_conc <- generate_puzzle(
    type = "concentric", grid = c(2), size = c(100),
    seed = 42, save_files = FALSE
  )

  # All should have same top-level structure
  for (result in list(result_rect, result_hex, result_conc)) {
    expect_true("svg_content" %in% names(result))
    expect_true("pieces" %in% names(result))
    expect_true("canvas_size" %in% names(result))
    expect_true("parameters" %in% names(result))
    expect_match(result$svg_content, "^<\\?xml")
  }
})

test_that("different types have correct type in parameters", {
  result_rect <- generate_puzzle(
    type = "rectangular", grid = c(2, 2), seed = 1, save_files = FALSE
  )
  expect_equal(result_rect$parameters$type, "rectangular")

  result_hex <- generate_puzzle(
    type = "hexagonal", grid = c(2), size = c(100), seed = 1, save_files = FALSE
  )
  expect_equal(result_hex$parameters$type, "hexagonal")

  result_conc <- generate_puzzle(
    type = "concentric", grid = c(2), size = c(100), seed = 1, save_files = FALSE
  )
  expect_equal(result_conc$parameters$type, "concentric")
})

# =============================================================================
# SEPARATION/OFFSET INTEGRATION
# =============================================================================

test_that("offset changes piece positions consistently", {
  # Generate same puzzle with different offsets
  result_compact <- generate_puzzle(
    type = "rectangular", grid = c(2, 2), seed = 42, offset = 0, save_files = FALSE
  )

  result_sep_10 <- generate_puzzle(
    type = "rectangular", grid = c(2, 2), seed = 42, offset = 10, save_files = FALSE
  )

  result_sep_20 <- generate_puzzle(
    type = "rectangular", grid = c(2, 2), seed = 42, offset = 20, save_files = FALSE
  )

  # Piece count should be same
  expect_equal(length(result_compact$pieces), length(result_sep_10$pieces))
  expect_equal(length(result_sep_10$pieces), length(result_sep_20$pieces))

  # Canvas should grow with offset
  expect_lt(result_compact$canvas_size[1], result_sep_10$canvas_size[1])
  expect_lt(result_sep_10$canvas_size[1], result_sep_20$canvas_size[1])
})

test_that("hexagonal offset works correctly", {
  result_compact <- generate_puzzle(
    type = "hexagonal", grid = c(3), size = c(200), seed = 42,
    offset = 0, save_files = FALSE
  )

  result_separated <- generate_puzzle(
    type = "hexagonal", grid = c(3), size = c(200), seed = 42,
    offset = 15, save_files = FALSE
  )

  expect_equal(length(result_compact$pieces), 19)
  expect_equal(length(result_separated$pieces), 19)
  expect_gt(result_separated$canvas_size[1], result_compact$canvas_size[1])
})

# =============================================================================
# REPRODUCIBILITY
# =============================================================================

test_that("same seed produces identical results", {
  result1 <- generate_puzzle(
    type = "rectangular", grid = c(3, 3), seed = 12345, save_files = FALSE
  )

  result2 <- generate_puzzle(
    type = "rectangular", grid = c(3, 3), seed = 12345, save_files = FALSE
  )

  expect_identical(result1$svg_content, result2$svg_content)
  expect_identical(result1$pieces[[1]]$path, result2$pieces[[1]]$path)
})

test_that("different seeds produce different results", {
  result1 <- generate_puzzle(
    type = "rectangular", grid = c(3, 3), seed = 11111, save_files = FALSE
  )

  result2 <- generate_puzzle(
    type = "rectangular", grid = c(3, 3), seed = 22222, save_files = FALSE
  )

  expect_false(identical(result1$svg_content, result2$svg_content))
})

# =============================================================================
# STYLING INTEGRATION
# =============================================================================

test_that("styling parameters are reflected in SVG output", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    seed = 1234,
    fill_color = "lightblue",
    stroke_width = 2.5,
    show_labels = TRUE,
    opacity = 0.7,
    save_files = FALSE
  )

  svg <- result$svg_content

  # Check fill color
  expect_match(svg, "lightblue")

  # Check stroke width
  expect_match(svg, "stroke-width")

  # Check labels (text elements)
  expect_match(svg, "<text")

  # Check opacity
  expect_match(svg, "opacity")
})

test_that("background parameter affects SVG output", {
  result_white <- generate_puzzle(
    type = "rectangular", grid = c(2, 2), seed = 1,
    background = "white", save_files = FALSE
  )

  result_none <- generate_puzzle(
    type = "rectangular", grid = c(2, 2), seed = 1,
    background = "none", save_files = FALSE
  )

  # Results should be different
  expect_false(identical(result_white$svg_content, result_none$svg_content))
})

# =============================================================================
# ERROR HANDLING
# =============================================================================

test_that("invalid type produces clear error", {
  expect_error(
    generate_puzzle(type = "invalid", grid = c(2, 2), seed = 1, save_files = FALSE),
    "Invalid type"
  )
})

test_that("invalid grid produces error", {
  expect_error(
    generate_puzzle(type = "rectangular", grid = c(-1, 2), seed = 1, save_files = FALSE)
  )

  expect_error(
    generate_puzzle(type = "rectangular", grid = c(0, 0), seed = 1, save_files = FALSE)
  )
})

# =============================================================================
# PACKAGE EXPORTS
# =============================================================================

test_that("main exported functions exist", {
  expect_true(exists("generate_puzzle"))
  expect_true(is.function(generate_puzzle))
})

test_that("Shiny app functions exist", {
  expect_true(is.function(launch_jigsaw_app))
})

# =============================================================================
# CENTER_SHAPE OPTION (Issue #39)
# =============================================================================

test_that("concentric center_shape='hexagon' produces straight-line edges", {
  result <- generate_puzzle(
    type = "concentric",
    grid = c(3),
    size = c(200),
    seed = 42,
    center_shape = "hexagon",
    save_files = FALSE
  )

  expect_equal(length(result$pieces), 19)
  expect_equal(result$parameters$center_shape, "hexagon")

  # Center piece (piece 1) should NOT have arc commands
  center_path <- result$pieces[[1]]$path
  expect_false(grepl(" A ", center_path))
})

test_that("concentric center_shape='circle' produces arc commands", {
  result <- generate_puzzle(
    type = "concentric",
    grid = c(3),
    size = c(200),
    seed = 42,
    center_shape = "circle",
    save_files = FALSE
  )

  expect_equal(length(result$pieces), 19)
  expect_equal(result$parameters$center_shape, "circle")

  # Center piece (piece 1) should have arc commands (A)
  center_path <- result$pieces[[1]]$path
  expect_true(grepl(" A ", center_path))
})

test_that("center_shape produces different paths for hexagon vs circle", {
  result_hex <- generate_puzzle(
    type = "concentric", grid = c(3), size = c(200), seed = 42,
    center_shape = "hexagon", save_files = FALSE
  )

  result_circle <- generate_puzzle(
    type = "concentric", grid = c(3), size = c(200), seed = 42,
    center_shape = "circle", save_files = FALSE
  )

  # Paths should be different
  hex_path <- result_hex$pieces[[1]]$path
  circle_path <- result_circle$pieces[[1]]$path

  expect_false(identical(hex_path, circle_path))

  # Circle path should be longer (has arc commands)
  expect_gt(nchar(circle_path), nchar(hex_path))
})

test_that("center_shape defaults to hexagon", {
  result <- generate_puzzle(
    type = "concentric", grid = c(3), size = c(200), seed = 42,
    save_files = FALSE
  )

  # Default should be hexagon (no arc commands in center piece)
  center_path <- result$pieces[[1]]$path
  expect_false(grepl(" A ", center_path))
})
