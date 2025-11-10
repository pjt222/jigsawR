# Tests for main API functions in jigsawR package

test_that("generate_puzzle produces valid output with default parameters", {
  # Test basic rectangular puzzle generation
  result <- generate_puzzle(
    grid = c(2, 2),
    seed = 1234,
    save_files = FALSE
  )
  
  # Check return structure
  expect_type(result, "list")
  expect_true("svg_content" %in% names(result))
  expect_true("puzzle_data" %in% names(result))
  
  # Check SVG content is valid
  expect_true(grepl("^<\\?xml", result$svg_content$complete))
  expect_true(grepl("</svg>$", result$svg_content$complete))
})

test_that("generate_puzzle is reproducible with same seed", {
  puzzle1 <- generate_puzzle(
    grid = c(2, 2),
    seed = 5678,
    save_files = FALSE
  )
  
  puzzle2 <- generate_puzzle(
    grid = c(2, 2),
    seed = 5678,
    save_files = FALSE
  )
  
  # Same seed should produce identical SVG content
  expect_identical(puzzle1$svg_content$complete, puzzle2$svg_content$complete)
  expect_identical(puzzle1$svg_content$individual, puzzle2$svg_content$individual)
})

test_that("generate_puzzle validates input parameters", {
  # Test invalid grid size
  expect_error(
    generate_puzzle(grid = c(0, 2), seed = 1234, save_files = FALSE),
    "must be positive"
  )
  
  # Test invalid tabsize
  expect_error(
    generate_puzzle(grid = c(2, 2), tabsize = 50, seed = 1234, save_files = FALSE),
    "must be between"
  )
})

test_that("generate_puzzle works with different output modes", {
  # Test complete mode only
  result_complete <- generate_puzzle(
    grid = c(2, 2),
    seed = 1234,
    output = "complete",
    save_files = FALSE
  )
  
  expect_true("complete" %in% names(result_complete$svg_content))
  expect_false("individual" %in% names(result_complete$svg_content))
  
  # Test individual mode only
  result_individual <- generate_puzzle(
    grid = c(2, 2),
    seed = 1234,
    output = "individual",
    save_files = FALSE
  )
  
  expect_false("complete" %in% names(result_individual$svg_content))
  expect_true("individual" %in% names(result_individual$svg_content))
  
  # Test both modes
  result_both <- generate_puzzle(
    grid = c(2, 2),
    seed = 1234,
    output = "both",
    save_files = FALSE
  )
  
  expect_true("complete" %in% names(result_both$svg_content))
  expect_true("individual" %in% names(result_both$svg_content))
})

test_that("generate_puzzle handles colors correctly", {
  result <- generate_puzzle(
    grid = c(2, 2),
    seed = 1234,
    colors = c("red", "blue", "green", "orange"),
    output = "individual",
    save_files = FALSE
  )
  
  # Check that colors appear in SVG content
  svg_content <- result$svg_content$individual
  expect_true(grepl("red", svg_content))
  expect_true(grepl("blue", svg_content))
  expect_true(grepl("green", svg_content))
  expect_true(grepl("orange", svg_content))
})

test_that("individual piece IDs are correctly generated", {
  result <- generate_puzzle(
    grid = c(3, 3),
    seed = 1234,
    output = "individual",
    save_files = FALSE
  )
  
  svg_content <- result$svg_content$individual
  
  # Check for expected piece IDs in a 3x3 puzzle
  expected_ids <- c(
    "piece-0-0", "piece-1-0", "piece-2-0",
    "piece-0-1", "piece-1-1", "piece-2-1", 
    "piece-0-2", "piece-1-2", "piece-2-2"
  )
  
  for (id in expected_ids) {
    expect_true(grepl(id, svg_content), 
                info = paste("Missing piece ID:", id))
  }
})

test_that("generate_puzzle_batch works with multiple configurations", {
  configs <- list(
    list(grid = c(2, 2), seed = 100, tabsize = 20),
    list(grid = c(2, 2), seed = 200, tabsize = 25),
    list(grid = c(3, 3), seed = 300, tabsize = 15)
  )
  
  results <- generate_puzzle_batch(
    configurations = configs,
    save_files = FALSE
  )
  
  # Should return same number of results as configs
  expect_equal(length(results), length(configs))
  
  # Each result should have the expected structure
  for (i in seq_along(results)) {
    expect_type(results[[i]], "list")
    expect_true("svg_content" %in% names(results[[i]]))
    expect_true("puzzle_data" %in% names(results[[i]]))
  }
})

test_that("convert_svg_to_png handles missing dependencies gracefully", {
  # Test with a simple SVG string
  simple_svg <- '<?xml version="1.0"?><svg xmlns="http://www.w3.org/2000/svg"><circle cx="50" cy="50" r="40"/></svg>'
  
  # This should either succeed or give a meaningful error about missing dependencies
  result <- tryCatch({
    convert_svg_to_png(simple_svg, width = 100, height = 100, method = "auto")
  }, error = function(e) {
    # Should be a meaningful error about missing tools
    expect_true(grepl("conversion tool|not available|not found", e$message, ignore.case = TRUE))
    "error_handled"
  })
  
  # If it succeeded, result should be binary data
  if (result != "error_handled") {
    expect_type(result, "raw")
  }
})

test_that("check_conversion_tools reports available tools correctly", {
  # This should run without error and return information
  expect_output(check_conversion_tools(), "Available")
  
  # Should not crash even if no tools are available
  tools_info <- capture.output(check_conversion_tools())
  expect_true(length(tools_info) > 0)
})