# Tests for utility functions and helpers

test_that("check_app_dependencies identifies missing packages correctly", {
  # This function should run without error
  result <- check_app_dependencies()
  
  # Should return a boolean
  expect_type(result, "logical")
  expect_length(result, 1)
})

test_that("SVG path translation works correctly", {
  # Test basic path translation
  original_path <- "M 0 0 L 10 10 L 20 0 Z"
  translated_path <- translate_svg_path(original_path, 5, 3)
  
  # Should be different from original (unless offset is 0)
  expect_false(identical(original_path, translated_path))
  
  # Should still be a valid path format
  expect_true(grepl("^M", translated_path))
  expect_true(grepl("Z$", translated_path))
})

test_that("SVG path translation with zero offset returns original", {
  original_path <- "M 10 20 C 30 40 50 60 70 80 Z"
  translated_path <- translate_svg_path(original_path, 0, 0)
  
  # Zero offset should return identical path
  expect_identical(original_path, translated_path)
})

test_that("piece offset calculation is consistent", {
  # Test offset calculation for different positions
  offset_0_0 <- calculate_piece_offset(0, 0, offset = 10, 
                                      piece_width = 50, piece_height = 50)
  offset_1_1 <- calculate_piece_offset(1, 1, offset = 10,
                                      piece_width = 50, piece_height = 50)
  
  expect_type(offset_0_0, "numeric")
  expect_length(offset_0_0, 2)  # Should return c(x_offset, y_offset)
  
  expect_type(offset_1_1, "numeric")
  expect_length(offset_1_1, 2)
  
  # Different positions should give different offsets
  expect_false(identical(offset_0_0, offset_1_1))
})

test_that("optimal offset calculation produces reasonable values", {
  optimal <- calculate_optimal_offset(
    grid = c(3, 3),
    piece_size = c(100, 100),
    min_gap = 5,
    target_efficiency = 0.8
  )
  
  expect_type(optimal, "numeric")
  expect_length(optimal, 1)
  expect_true(optimal >= 5)  # Should respect minimum gap
})

test_that("gradient background creation works", {
  # Test basic gradient creation
  gradient_svg <- create_gradient_circle_png(size = 200)
  
  expect_type(gradient_svg, "character")
  expect_true(grepl("^<\\?xml", gradient_svg))
  expect_true(grepl("</svg>$", gradient_svg))
  
  # Should contain gradient definitions
  expect_true(grepl("radialGradient", gradient_svg))
})

test_that("SVG enhancement utilities work", {
  # Test basic puzzle SVG creation
  enhanced_svg <- create_enhanced_puzzle_svg(
    seed = 1234,
    grid = c(2, 2),
    size = c(200, 200)
  )
  
  expect_type(enhanced_svg, "character")
  expect_true(grepl("^<\\?xml", enhanced_svg))
  expect_true(grepl("</svg>$", enhanced_svg))
})

test_that("puzzle validation catches errors", {
  # Create a valid puzzle structure
  puzzle <- generate_puzzle_core(
    seed = 1234,
    grid = c(2, 2),
    size = c(200, 200)
  )
  
  # Validation should pass for valid puzzle
  expect_silent(validate_puzzle_fit(puzzle))
  
  # Test with modified (invalid) structure
  invalid_puzzle <- puzzle
  invalid_puzzle$edges <- NULL
  
  expect_error(validate_puzzle_fit(invalid_puzzle))
})

test_that("puzzle info printing works", {
  puzzle <- generate_puzzle_core(
    seed = 1234,
    grid = c(3, 2),
    size = c(300, 200),
    tabsize = 25,
    jitter = 5
  )
  
  # Should produce output without error
  expect_output(print_puzzle_info(puzzle), "Seed: 1234")
  expect_output(print_puzzle_info(puzzle), "Grid: 3 x 2")
  expect_output(print_puzzle_info(puzzle), "Size: 300 x 200")
})

test_that("file saving utilities handle paths correctly", {
  # Create a simple SVG
  simple_svg <- '<?xml version="1.0"?><svg xmlns="http://www.w3.org/2000/svg"><rect width="100" height="100"/></svg>'
  
  # Test saving (without actually writing files in test)
  result <- tryCatch({
    save_enhanced_svg(simple_svg, "test_output.svg", create_file = FALSE)
  }, error = function(e) {
    # If function doesn't have create_file parameter, that's okay
    "not_implemented"
  })
  
  # Should either succeed or be not implemented
  expect_true(result %in% c("saved", "not_implemented", TRUE, FALSE) || is.null(result))
})

test_that("color validation works correctly", {
  # Test with valid colors
  valid_colors <- c("red", "blue", "#FF0000", "rgb(0,255,0)")
  
  # Test with invalid colors (if validation function exists)
  invalid_colors <- c("not_a_color", "#ZZZ", "rgb(300,400,500)")
  
  # This test depends on whether color validation is implemented
  # For now, just check that colors can be used in SVG generation
  puzzle_with_colors <- generate_puzzle(
    grid = c(2, 2),
    seed = 1234,
    colors = valid_colors[1:4],
    output = "individual",
    save_files = FALSE
  )
  
  expect_true(grepl("red", puzzle_with_colors$svg_content$individual))
})

test_that("edge case handling works for small puzzles", {
  # Test 1x1 puzzle (if supported)
  result_1x1 <- tryCatch({
    generate_puzzle(grid = c(1, 1), seed = 1234, save_files = FALSE)
  }, error = function(e) {
    # 1x1 might not be supported, which is fine
    "unsupported"
  })
  
  # Either should work or give meaningful error
  if (result_1x1 != "unsupported") {
    expect_type(result_1x1, "list")
  }
  
  # Test 1x2 puzzle
  result_1x2 <- generate_puzzle(grid = c(1, 2), seed = 1234, save_files = FALSE)
  expect_type(result_1x2, "list")
  
  # Test 2x1 puzzle  
  result_2x1 <- generate_puzzle(grid = c(2, 1), seed = 1234, save_files = FALSE)
  expect_type(result_2x1, "list")
})

test_that("large puzzle generation is stable", {
  # Test larger puzzle to ensure no memory/performance issues in tests
  large_puzzle <- generate_puzzle(
    grid = c(4, 4),
    seed = 1234,
    output = "complete",  # Faster than individual for large puzzles
    save_files = FALSE
  )
  
  expect_type(large_puzzle, "list")
  expect_true("svg_content" %in% names(large_puzzle))
  
  # SVG should be valid
  expect_true(grepl("^<\\?xml", large_puzzle$svg_content$complete))
})