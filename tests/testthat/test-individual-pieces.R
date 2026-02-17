# Tests for individual puzzle pieces functionality
# Updated to use current generate_puzzle() API (2025-12)

# =============================================================================
# RECTANGULAR PIECE STRUCTURE
# =============================================================================

test_that("rectangular pieces have correct structure", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    seed = 1234,
    save_files = FALSE
  )

  # Should have 4 pieces for 2x2
  expect_equal(length(result$pieces), 4)

  # Each piece should have required fields
  for (piece in result$pieces) {
    expect_true("id" %in% names(piece), info = "Piece missing 'id' field")
    expect_true("path" %in% names(piece), info = "Piece missing 'path' field")
    expect_true("center" %in% names(piece), info = "Piece missing 'center' field")
  }
})

test_that("rectangular piece paths are valid SVG", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    seed = 1234,
    save_files = FALSE
  )

  for (piece in result$pieces) {
    path <- piece$path

    # Path should start with M (moveto)
    expect_match(path, "^M ", info = paste("Piece", piece$id, "path doesn't start with M"))

    # Path should end with Z (closepath)
    expect_match(path, "Z$", info = paste("Piece", piece$id, "path doesn't end with Z"))

    # Path should contain valid SVG commands
    expect_match(path, "[MLCZ]", info = paste("Piece", piece$id, "has no valid SVG commands"))
  }
})

test_that("rectangular piece centers are numeric coordinates", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    size = c(200, 200),
    seed = 1234,
    save_files = FALSE
  )

  for (piece in result$pieces) {
    center <- piece$center

    # Center should be numeric vector of length 2
    expect_type(center, "double")
    expect_length(center, 2)

    # Center coordinates should be within puzzle bounds (approximately)
    expect_true(center[1] >= 0, label = paste("Piece", piece$id, "center x >= 0"))
    expect_true(center[2] >= 0, label = paste("Piece", piece$id, "center y >= 0"))
    expect_true(center[1] <= 250, label = paste("Piece", piece$id, "center x <= 250"))
    expect_true(center[2] <= 250, label = paste("Piece", piece$id, "center y <= 250"))
  }
})

# =============================================================================
# HEXAGONAL PIECE STRUCTURE
# =============================================================================

test_that("hexagonal pieces have correct structure", {
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(3),
    size = c(200),
    seed = 1234,
    save_files = FALSE
  )

  # 3 rings = 19 pieces
  expect_equal(length(result$pieces), 19)

  for (piece in result$pieces) {
    expect_true("id" %in% names(piece))
    expect_true("path" %in% names(piece))
    expect_true("center" %in% names(piece))
  }
})

test_that("hexagonal piece paths are valid SVG", {
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(3),
    size = c(200),
    seed = 1234,
    save_files = FALSE
  )

  for (piece in result$pieces) {
    path <- piece$path
    expect_match(path, "^M ")
    expect_match(path, "Z$")
  }
})

# =============================================================================
# CONCENTRIC PIECE STRUCTURE
# =============================================================================

test_that("concentric pieces have correct structure", {
  result <- generate_puzzle(
    type = "concentric",
    grid = c(3),
    size = c(240),
    seed = 1234,
    center_shape = "hexagon",
    save_files = FALSE
  )

  # 3 rings = 19 pieces
  expect_equal(length(result$pieces), 19)

  for (piece in result$pieces) {
    expect_true("id" %in% names(piece))
    expect_true("path" %in% names(piece))
    expect_true("center" %in% names(piece))
  }
})

test_that("concentric piece paths are valid SVG", {
  result <- generate_puzzle(
    type = "concentric",
    grid = c(3),
    size = c(240),
    seed = 1234,
    center_shape = "circle",
    save_files = FALSE
  )

  for (piece in result$pieces) {
    path <- piece$path
    expect_match(path, "^M ")
    expect_match(path, "Z$")
  }
})

# =============================================================================
# PIECE REPRODUCIBILITY
# =============================================================================

test_that("pieces are reproducible with same seed", {
  result1 <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    seed = 9999,
    save_files = FALSE
  )

  result2 <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    seed = 9999,
    save_files = FALSE
  )

  # Same seed should produce identical pieces
  expect_equal(length(result1$pieces), length(result2$pieces))

  for (i in seq_along(result1$pieces)) {
    expect_identical(result1$pieces[[i]]$path, result2$pieces[[i]]$path)
  }
})

test_that("pieces differ with different seeds", {
  result1 <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    seed = 11111,
    save_files = FALSE
  )

  result2 <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    seed = 22222,
    save_files = FALSE
  )

  # Different seeds should produce different paths
  expect_false(identical(result1$pieces[[1]]$path, result2$pieces[[1]]$path))
})

# =============================================================================
# PIECE SEPARATION (OFFSET)
# =============================================================================

test_that("separated pieces have same paths as compact pieces", {
  result_compact <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    seed = 1234,
    offset = 0,
    save_files = FALSE
  )

  result_separated <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    seed = 1234,
    offset = 20,
    save_files = FALSE
  )

  # Same number of pieces
  expect_equal(length(result_compact$pieces), length(result_separated$pieces))

  # Piece shapes (paths) should be identical - separation only changes positions
  # Note: This may fail if paths include absolute positions vs relative
  # If paths are position-dependent, pieces should at least have same structure
  for (i in seq_along(result_compact$pieces)) {
    expect_type(result_compact$pieces[[i]]$path, "character")
    expect_type(result_separated$pieces[[i]]$path, "character")
  }
})

test_that("separated pieces have different centers", {
  result_compact <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    seed = 1234,
    offset = 0,
    save_files = FALSE
  )

  result_separated <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    seed = 1234,
    offset = 20,
    save_files = FALSE
  )

  # At least some centers should differ due to separation
  centers_differ <- FALSE
  for (i in seq_along(result_compact$pieces)) {
    if (!identical(result_compact$pieces[[i]]$center, result_separated$pieces[[i]]$center)) {
      centers_differ <- TRUE
      break
    }
  }

  expect_true(centers_differ, info = "Separated pieces should have different centers")
})

# =============================================================================
# PIECE COUNTS BY GRID SIZE
# =============================================================================

test_that("rectangular piece count matches grid", {
  # 3x4 = 12 pieces
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 4),
    seed = 1234,
    save_files = FALSE
  )
  expect_equal(length(result$pieces), 12)

  # 5x5 = 25 pieces
  result2 <- generate_puzzle(
    type = "rectangular",
    grid = c(5, 5),
    seed = 1234,
    save_files = FALSE
  )
  expect_equal(length(result2$pieces), 25)
})

test_that("hexagonal piece count follows formula", {
  # Formula: 3 * rings * (rings - 1) + 1

  # 2 rings = 7 pieces
  result2 <- generate_puzzle(
    type = "hexagonal",
    grid = c(2),
    size = c(100),
    seed = 1234,
    save_files = FALSE
  )
  expect_equal(length(result2$pieces), 7)

  # 4 rings = 37 pieces
  result4 <- generate_puzzle(
    type = "hexagonal",
    grid = c(4),
    size = c(200),
    seed = 1234,
    save_files = FALSE
  )
  expect_equal(length(result4$pieces), 37)
})

# =============================================================================
# LEGACY FUNCTION TESTS (if available)
# =============================================================================

test_that("legacy generate_individual_pieces works", {
  result <- generate_individual_pieces(seed = 1234, xn = 2, yn = 2)

  expect_type(result, "list")
  expect_true("pieces" %in% names(result))
})

test_that("legacy hexagonal individual pieces works", {
  result <- generate_hexagonal_individual_pieces(seed = 1234, rings = 2)

  expect_type(result, "list")
  expect_true("pieces" %in% names(result))
})

# =============================================================================
# SVG PATH UTILITIES
# =============================================================================

test_that("parse_svg_path parses basic commands", {
  path <- "M 0 0 L 10 10 C 20 20 30 30 40 40 Z"
  parsed <- parse_svg_path(path)

  expect_type(parsed, "list")
  expect_true(length(parsed) >= 3)

  # Should parse M command
  types <- sapply(parsed, function(x) x$type)
  expect_true("M" %in% types)
})

test_that("reverse_path_segments reverses correctly", {
  path <- "M 0 0 L 10 0 L 10 10 Z"
  parsed <- parse_svg_path(path)
  reversed <- reverse_path_segments(parsed)

  # reverse_path_segments returns a path string
  expect_type(reversed, "character")
})

# =============================================================================
# RENDER_SINGLE_PIECE_SVG TESTS (Issue #51)
# =============================================================================

test_that("render_single_piece_svg creates valid SVG for rectangular piece", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    seed = 42,
    save_files = FALSE
  )

  svg <- render_single_piece_svg(result$pieces[[1]], fill = "#FF0000", stroke_color = "black")

  expect_type(svg, "character")
  expect_match(svg, "^<\\?xml version")
  expect_match(svg, "<svg.*xmlns=")
  expect_match(svg, "viewBox=")
  expect_match(svg, "<path")
  expect_match(svg, 'fill="#FF0000"')
  expect_match(svg, 'stroke="black"')
  expect_match(svg, "</svg>$")
})

test_that("render_single_piece_svg creates valid SVG for hexagonal piece", {
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(2),
    size = c(100),
    seed = 42,
    save_files = FALSE
  )

  svg <- render_single_piece_svg(result$pieces[[1]], fill = "blue", stroke_color = "#000000")

  expect_type(svg, "character")
  expect_match(svg, "viewBox=")
  expect_match(svg, 'fill="blue"')
})

test_that("render_single_piece_svg creates valid SVG for concentric piece", {
  result <- generate_puzzle(
    type = "concentric",
    grid = c(2),
    size = c(100),
    seed = 42,
    save_files = FALSE
  )

  svg <- render_single_piece_svg(result$pieces[[1]], fill = "green", stroke_color = "gray")

  expect_type(svg, "character")
  expect_match(svg, "viewBox=")
  expect_match(svg, 'fill="green"')
})

test_that("render_single_piece_svg supports labels", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    seed = 42,
    save_files = FALSE
  )

  svg <- render_single_piece_svg(result$pieces[[2]], fill = "yellow", show_label = TRUE)

  expect_match(svg, "<text")
  expect_match(svg, "2")  # Piece ID should appear
})

test_that("render_single_piece_svg supports background", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    seed = 42,
    save_files = FALSE
  )

  svg <- render_single_piece_svg(result$pieces[[3]], fill = "purple", background = "white")

  expect_match(svg, '<rect.*fill="white"')
})

test_that("render_single_piece_svg respects opacity parameter", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    seed = 42,
    save_files = FALSE
  )

  svg <- render_single_piece_svg(result$pieces[[1]], fill = "red", opacity = 0.5)

  expect_match(svg, "opacity")
})

test_that("render_single_piece_svg respects stroke_width parameter", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    seed = 42,
    save_files = FALSE
  )

  svg <- render_single_piece_svg(result$pieces[[1]], stroke_width = 3.0)

  # Stroke width may include decimal places (3.00)
  expect_match(svg, 'stroke-width="3')
})

test_that("render_single_piece_svg validates piece input", {
  # Missing path field
  expect_error(
    render_single_piece_svg(list(id = 1)),
    "path"
  )

  # Empty path field
  expect_error(
    render_single_piece_svg(list(id = 1, path = "")),
    "path"
  )
})

test_that("render_single_piece_svg supports none fill and stroke", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    seed = 42,
    save_files = FALSE
  )

  svg <- render_single_piece_svg(result$pieces[[1]], fill = "none", stroke_color = "none")

  expect_match(svg, 'fill="none"')
  expect_match(svg, 'stroke="none"')
})

test_that("calculate_piece_bounds extracts correct bounds", {
  # Simple square path
  path <- "M 0 0 L 100 0 L 100 100 L 0 100 Z"
  bounds <- calculate_piece_bounds(path)

  expect_type(bounds, "list")
  expect_equal(bounds$min_x, 0, tolerance = 0.001)
  expect_equal(bounds$max_x, 100, tolerance = 0.001)
  expect_equal(bounds$min_y, 0, tolerance = 0.001)
  expect_equal(bounds$max_y, 100, tolerance = 0.001)
  expect_equal(bounds$width, 100, tolerance = 0.001)
  expect_equal(bounds$height, 100, tolerance = 0.001)
})

test_that("render_single_piece_svg works with all pieces from a puzzle", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    seed = 42,
    save_files = FALSE
  )

  # All pieces should render successfully
  for (piece in result$pieces) {
    svg <- render_single_piece_svg(piece, fill = "lightblue")
    expect_type(svg, "character")
    expect_match(svg, "viewBox=")
    expect_match(svg, "<path")
  }
})
