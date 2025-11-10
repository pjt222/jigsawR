# Tests for hexagonal puzzle functionality

test_that("generate_hex_jigsaw_svg produces valid hexagonal puzzles", {
  result <- generate_hex_jigsaw_svg(
    seed = 1234,
    rings = 3,
    diameter = 200
  )
  
  # Check return structure
  expect_type(result, "list")
  expect_true("svg" %in% names(result))
  expect_true("paths" %in% names(result))
  expect_true("info" %in% names(result))
  
  # Check SVG is valid
  expect_true(grepl("^<\\?xml", result$svg))
  expect_true(grepl("</svg>$", result$svg))
  
  # Check puzzle info
  info <- result$info
  expect_equal(info$rings, 3)
  expect_equal(info$diameter, 200)
  expect_equal(info$seed, 1234)
})

test_that("hexagonal puzzles are reproducible", {
  puzzle1 <- generate_hex_jigsaw_svg(seed = 5678, rings = 2)
  puzzle2 <- generate_hex_jigsaw_svg(seed = 5678, rings = 2)
  
  # Same parameters should produce identical results
  expect_identical(puzzle1$svg, puzzle2$svg)
  expect_identical(puzzle1$paths, puzzle2$paths)
})

test_that("circular warping can be applied to hexagonal puzzles", {
  # Test without warping
  puzzle_normal <- generate_hex_jigsaw_svg(
    seed = 1234,
    rings = 2,
    do_warp = FALSE
  )
  
  # Test with warping
  puzzle_warped <- generate_hex_jigsaw_svg(
    seed = 1234,
    rings = 2,
    do_warp = TRUE
  )
  
  # Warped and normal should be different
  expect_false(identical(puzzle_normal$svg, puzzle_warped$svg))
  
  # Both should be valid SVGs
  expect_true(grepl("^<\\?xml", puzzle_normal$svg))
  expect_true(grepl("^<\\?xml", puzzle_warped$svg))
})

test_that("edge truncation works for circular puzzles", {
  # Test with edge truncation
  puzzle_truncated <- generate_hex_jigsaw_svg(
    seed = 1234,
    rings = 2,
    do_trunc = TRUE
  )
  
  # Should produce valid SVG
  expect_true(grepl("^<\\?xml", puzzle_truncated$svg))
  
  # Info should reflect truncation setting
  expect_equal(puzzle_truncated$info$do_trunc, TRUE)
})

test_that("hexagonal individual pieces can be generated", {
  result <- generate_hexagonal_individual_pieces(
    seed = 1234,
    rings = 2
  )
  
  expect_type(result, "list")
  expect_true("svg" %in% names(result))
  expect_true("pieces" %in% names(result))
  
  # Should have valid SVG
  expect_true(grepl("^<\\?xml", result$svg))
  expect_true(grepl("</svg>$", result$svg))
  
  # Should have multiple pieces
  expect_true(length(result$pieces) > 1)
})

test_that("hexagonal separation can be applied", {
  # First extract puzzle structure
  structure <- extract_hexagonal_puzzle_structure(
    seed = 1234,
    rings = 2
  )
  
  # Apply separation
  separated_structure <- apply_hexagonal_separation(
    structure,
    offset = 10,
    arrangement = "hexagonal"
  )
  
  expect_type(separated_structure, "list")
  expect_true("separation" %in% names(separated_structure))
  
  # Check separation parameters
  separation <- separated_structure$separation
  expect_equal(separation$offset, 10)
  expect_equal(separation$arrangement, "hexagonal")
  expect_true("positions" %in% names(separation))
})

test_that("hex grid position calculation works correctly", {
  # Test center piece (ring 0)
  pos_center <- calculate_hex_piece_position(0, 0, rings = 2)
  expect_type(pos_center, "list")
  expect_true("x" %in% names(pos_center))
  expect_true("y" %in% names(pos_center))
  
  # Center should be at origin (approximately)
  expect_true(abs(pos_center$x) < 1e-10)
  expect_true(abs(pos_center$y) < 1e-10)
  
  # Test outer ring positions
  pos_outer <- calculate_hex_piece_position(1, 0, rings = 2)
  expect_true(pos_outer$x > 0)  # Should be offset from center
})

test_that("optimal hex separation can be calculated", {
  optimal_offset <- calculate_optimal_hex_separation(
    rings = 3,
    piece_size = 50,
    min_gap = 5
  )
  
  expect_type(optimal_offset, "numeric")
  expect_true(optimal_offset >= 5)  # Should respect minimum gap
  expect_length(optimal_offset, 1)
})

test_that("hexagonal puzzles handle edge cases", {
  # Test minimum ring count (1 ring)
  puzzle_min <- generate_hex_jigsaw_svg(rings = 1, seed = 1234)
  expect_true(grepl("^<\\?xml", puzzle_min$svg))
  
  # Test larger ring count
  puzzle_large <- generate_hex_jigsaw_svg(rings = 5, seed = 1234)
  expect_true(grepl("^<\\?xml", puzzle_large$svg))
  
  # Larger puzzle should have more complex path data
  expect_true(nchar(puzzle_large$svg) > nchar(puzzle_min$svg))
})

test_that("hex puzzle info is correctly formatted", {
  puzzle <- generate_hex_jigsaw_svg(
    seed = 9999,
    rings = 4,
    diameter = 250,
    tabsize = 25,
    jitter = 6,
    do_warp = TRUE,
    do_trunc = FALSE
  )
  
  info <- puzzle$info
  
  # All parameters should be preserved
  expect_equal(info$seed, 9999)
  expect_equal(info$rings, 4)
  expect_equal(info$diameter, 250)
  expect_equal(info$tabsize, 25)
  expect_equal(info$jitter, 6)
  expect_equal(info$do_warp, TRUE)
  expect_equal(info$do_trunc, FALSE)
})

test_that("hex puzzle SVG contains expected elements", {
  puzzle <- generate_hex_jigsaw_svg(seed = 1234, rings = 2)
  svg <- puzzle$svg
  
  # Should contain SVG namespace
  expect_true(grepl('xmlns="http://www.w3.org/2000/svg"', svg))
  
  # Should contain path elements
  expect_true(grepl("<path", svg))
  
  # Should have viewBox attribute
  expect_true(grepl('viewBox=', svg))
  
  # Should be properly closed
  expect_true(grepl("</svg>$", svg))
})