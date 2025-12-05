# Tests for hexagonal puzzle functionality
# Updated to use current API (2025-12)

# =============================================================================
# BASIC HEXAGONAL GENERATION (via generate_puzzle)
# =============================================================================

test_that("hexagonal puzzle generates correctly via generate_puzzle", {
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(3),
    size = c(200),
    seed = 1234,
    save_files = FALSE
  )

  expect_type(result, "list")
  expect_true("svg_content" %in% names(result))
  expect_true("pieces" %in% names(result))

  # Check SVG is valid
  expect_match(result$svg_content, "^<\\?xml")
  expect_match(result$svg_content, "</svg>$")

  # 3 rings = 19 pieces
  expect_equal(length(result$pieces), 19)
  expect_equal(result$parameters$type, "hexagonal")
})

test_that("hexagonal puzzles are reproducible", {
  puzzle1 <- generate_puzzle(
    type = "hexagonal", grid = c(3), size = c(200), seed = 5678, save_files = FALSE
  )
  puzzle2 <- generate_puzzle(
    type = "hexagonal", grid = c(3), size = c(200), seed = 5678, save_files = FALSE
  )

  expect_identical(puzzle1$svg_content, puzzle2$svg_content)
  expect_identical(puzzle1$pieces[[1]]$path, puzzle2$pieces[[1]]$path)
})

# =============================================================================
# WARP TRANSFORMATION
# =============================================================================

test_that("circular warping can be applied to hexagonal puzzles", {
  puzzle_normal <- generate_puzzle(
    type = "hexagonal", grid = c(3), size = c(200), seed = 1234,
    do_warp = FALSE, save_files = FALSE
  )

  puzzle_warped <- generate_puzzle(
    type = "hexagonal", grid = c(3), size = c(200), seed = 1234,
    do_warp = TRUE, save_files = FALSE
  )

  # Warped and normal should be different
  expect_false(identical(puzzle_normal$svg_content, puzzle_warped$svg_content))

  # Both should be valid SVGs
  expect_match(puzzle_normal$svg_content, "<svg")
  expect_match(puzzle_warped$svg_content, "<svg")

  # Parameters should be stored
  expect_false(puzzle_normal$parameters$do_warp)
  expect_true(puzzle_warped$parameters$do_warp)
})

# =============================================================================
# TRUNCATION
# =============================================================================

test_that("edge truncation works for hexagonal puzzles", {
  puzzle_truncated <- generate_puzzle(
    type = "hexagonal", grid = c(3), size = c(200), seed = 1234,
    do_trunc = TRUE, save_files = FALSE
  )

  expect_match(puzzle_truncated$svg_content, "<svg")
  expect_true(puzzle_truncated$parameters$do_trunc)
})

# =============================================================================
# CIRCULAR BORDER (Perfect Circle Mode)
# =============================================================================

test_that("circular border uses arc commands", {
  puzzle_circle <- generate_puzzle(
    type = "hexagonal", grid = c(3), size = c(200), seed = 1234,
    do_warp = TRUE, do_trunc = TRUE, do_circular_border = TRUE,
    save_files = FALSE
  )

  expect_match(puzzle_circle$svg_content, "<svg")
  expect_true(puzzle_circle$parameters$do_circular_border)

  # Circular border should use arc commands (A) in boundary piece paths
  has_arc <- any(sapply(puzzle_circle$pieces, function(p) grepl(" A ", p$path)))
  expect_true(has_arc, info = "Circular border should produce arc commands")
})

test_that("circular border differs from straight border", {
  puzzle_straight <- generate_puzzle(
    type = "hexagonal", grid = c(3), size = c(200), seed = 1234,
    do_warp = TRUE, do_trunc = TRUE, do_circular_border = FALSE,
    save_files = FALSE
  )

  puzzle_circle <- generate_puzzle(
    type = "hexagonal", grid = c(3), size = c(200), seed = 1234,
    do_warp = TRUE, do_trunc = TRUE, do_circular_border = TRUE,
    save_files = FALSE
  )

  # SVG should be different due to arc vs line commands
  expect_false(identical(puzzle_straight$svg_content, puzzle_circle$svg_content))

  # Both should have same number of pieces
  expect_equal(length(puzzle_straight$pieces), length(puzzle_circle$pieces))
})

# =============================================================================
# COMBINED TRANSFORMATIONS
# =============================================================================

test_that("warp + trunc combination works", {
  result <- generate_puzzle(
    type = "hexagonal", grid = c(3), size = c(200), seed = 1234,
    do_warp = TRUE, do_trunc = TRUE, save_files = FALSE
  )

  expect_equal(length(result$pieces), 19)
  expect_true(result$parameters$do_warp)
  expect_true(result$parameters$do_trunc)
})

test_that("all five boundary modes produce different outputs", {
  # Mode 1: Zigzag (no warp, no trunc)
  mode1 <- generate_puzzle(
    type = "hexagonal", grid = c(3), size = c(200), seed = 1234,
    do_warp = FALSE, do_trunc = FALSE, do_circular_border = FALSE,
    save_files = FALSE
  )

  # Mode 2: Clean Hexagon (no warp, trunc)
  mode2 <- generate_puzzle(
    type = "hexagonal", grid = c(3), size = c(200), seed = 1234,
    do_warp = FALSE, do_trunc = TRUE, do_circular_border = FALSE,
    save_files = FALSE
  )

  # Mode 3: Warped Zigzag (warp, no trunc)
  mode3 <- generate_puzzle(
    type = "hexagonal", grid = c(3), size = c(200), seed = 1234,
    do_warp = TRUE, do_trunc = FALSE, do_circular_border = FALSE,
    save_files = FALSE
  )

  # Mode 4: Warped Hexagon (warp, trunc, no circular)
  mode4 <- generate_puzzle(
    type = "hexagonal", grid = c(3), size = c(200), seed = 1234,
    do_warp = TRUE, do_trunc = TRUE, do_circular_border = FALSE,
    save_files = FALSE
  )

  # Mode 5: Perfect Circle (warp, trunc, circular)
  mode5 <- generate_puzzle(
    type = "hexagonal", grid = c(3), size = c(200), seed = 1234,
    do_warp = TRUE, do_trunc = TRUE, do_circular_border = TRUE,
    save_files = FALSE
  )

  # All five should be different
  svgs <- list(mode1$svg_content, mode2$svg_content, mode3$svg_content,
               mode4$svg_content, mode5$svg_content)

  # Verify each pair is different
  for (i in 1:4) {
    for (j in (i+1):5) {
      expect_false(identical(svgs[[i]], svgs[[j]]),
                   info = paste("Mode", i, "should differ from mode", j))
    }
  }
})

# =============================================================================
# SEPARATION (OFFSET)
# =============================================================================

test_that("hexagonal separation works with offset parameter", {
  result_compact <- generate_puzzle(
    type = "hexagonal", grid = c(3), size = c(200), seed = 1234,
    offset = 0, save_files = FALSE
  )

  result_separated <- generate_puzzle(
    type = "hexagonal", grid = c(3), size = c(200), seed = 1234,
    offset = 15, save_files = FALSE
  )

  # Both should have same number of pieces
  expect_equal(length(result_compact$pieces), 19)
  expect_equal(length(result_separated$pieces), 19)

  # Separated canvas should be larger
  expect_gt(result_separated$canvas_size[1], result_compact$canvas_size[1])
})

test_that("hexagonal separation preserves transformations", {
  result <- generate_puzzle(
    type = "hexagonal", grid = c(3), size = c(200), seed = 1234,
    do_warp = TRUE, do_trunc = TRUE, do_circular_border = TRUE,
    offset = 20, save_files = FALSE
  )

  # Should have arc commands even with offset
  has_arc <- any(sapply(result$pieces, function(p) grepl(" A ", p$path)))
  expect_true(has_arc, info = "Arcs should be preserved after separation")
})

# =============================================================================
# EDGE CASES
# =============================================================================

test_that("hexagonal puzzles handle edge cases", {
  # Minimum ring count (1 ring = 1 piece)
  result_1 <- generate_puzzle(
    type = "hexagonal", grid = c(1), size = c(100), seed = 1234, save_files = FALSE
  )
  expect_equal(length(result_1$pieces), 1)

  # 2 rings = 7 pieces
  result_2 <- generate_puzzle(
    type = "hexagonal", grid = c(2), size = c(100), seed = 1234, save_files = FALSE
  )
  expect_equal(length(result_2$pieces), 7)

  # 5 rings = 61 pieces
  result_5 <- generate_puzzle(
    type = "hexagonal", grid = c(5), size = c(300), seed = 1234, save_files = FALSE
  )
  expect_equal(length(result_5$pieces), 61)
})

# =============================================================================
# LEGACY FUNCTION TESTS (generate_hex_jigsaw_svg)
# =============================================================================

test_that("legacy generate_hex_jigsaw_svg produces valid output", {
  skip_if_not(exists("generate_hex_jigsaw_svg"), "Legacy function not available")

  result <- generate_hex_jigsaw_svg(seed = 1234, rings = 3, diameter = 200)

  expect_type(result, "list")
  expect_true("svg" %in% names(result))
  # Legacy function returns SVG without XML declaration
  expect_match(result$svg, "<svg")
  expect_match(result$svg, "</svg>$")
})

# =============================================================================
# INDIVIDUAL PIECES (HEXAGONAL)
# =============================================================================

test_that("hexagonal individual pieces via generate_puzzle", {
  result <- generate_puzzle(
    type = "hexagonal", grid = c(3), size = c(200), seed = 1234,
    offset = 0, save_files = FALSE
  )

  # Each piece should have required fields
  for (piece in result$pieces) {
    expect_true("id" %in% names(piece))
    expect_true("path" %in% names(piece))
    expect_true("center" %in% names(piece))

    # Path should be valid (starts with M, ends with Z)
    expect_match(piece$path, "^M ")
    expect_match(piece$path, "Z$")
  }
})

test_that("generate_hexagonal_individual_pieces works", {
  skip_if_not(exists("generate_hexagonal_individual_pieces"),
              "generate_hexagonal_individual_pieces not available")

  result <- generate_hexagonal_individual_pieces(seed = 1234, rings = 2)

  expect_type(result, "list")
  expect_true(length(result$pieces) > 1)
})

# =============================================================================
# SVG STRUCTURE
# =============================================================================

test_that("hexagonal puzzle SVG contains expected elements", {
  result <- generate_puzzle(
    type = "hexagonal", grid = c(3), size = c(200), seed = 1234, save_files = FALSE
  )

  svg <- result$svg_content

  # Should contain SVG namespace
  expect_match(svg, 'xmlns="http://www.w3.org/2000/svg"')

  # Should contain path elements
  expect_match(svg, "<path")

  # Should have viewBox attribute
  expect_match(svg, 'viewBox=')
})
