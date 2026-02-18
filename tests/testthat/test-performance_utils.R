# Tests for performance utility functions (R/performance_utils.R)

# =============================================================================
# 1. Package detection
# =============================================================================

test_that("has_data_table returns logical", {
  result <- has_data_table()
  expect_type(result, "logical")
  expect_length(result, 1)
})

test_that("has_parallel returns logical", {
  result <- has_parallel()
  expect_type(result, "logical")
  expect_length(result, 1)
})

# =============================================================================
# 2. configure_data_table
# =============================================================================

test_that("configure_data_table returns invisible integer", {
  result <- configure_data_table(verbose = FALSE)
  expect_type(result, "integer")
  expect_true(result >= 1L)
})

test_that("configure_data_table with explicit thread count", {
  result <- configure_data_table(threads = 1L, verbose = FALSE)
  expect_equal(result, 1L)
})

test_that("configure_data_table verbose mode doesn't error", {
  expect_no_error(
    capture.output(configure_data_table(threads = 1L, verbose = TRUE))
  )
})

# =============================================================================
# 3. Vectorized string operations
# =============================================================================

test_that("format_coords returns formatted coordinate pairs", {
  result <- format_coords(c(1.234, 5.678), c(9.012, 3.456), digits = 2)
  expect_type(result, "character")
  expect_length(result, 2)
  expect_equal(result[1], "1.23,9.01")
  expect_equal(result[2], "5.68,3.46")
})

test_that("format_coords handles single values", {
  result <- format_coords(10, 20, digits = 0)
  expect_equal(result, "10,20")
})

test_that("build_svg_path concatenates commands", {
  commands <- c("M 0 0", "L 10 10", "L 20 0", "Z")
  result <- build_svg_path(commands)
  expect_type(result, "character")
  expect_length(result, 1)
  expect_equal(result, "M 0 0 L 10 10 L 20 0 Z")
})

test_that("build_svg_path handles empty input", {
  result <- build_svg_path(character(0))
  expect_equal(result, "")
})

# =============================================================================
# 4. get_performance_config
# =============================================================================

test_that("get_performance_config returns expected structure", {
  config <- get_performance_config()
  expect_type(config, "list")
  expect_true("data_table" %in% names(config))
  expect_true("parallel" %in% names(config))
  expect_true("r_version" %in% names(config))
  expect_true("platform" %in% names(config))
})

test_that("get_performance_config data_table section has correct fields", {
  config <- get_performance_config()
  expect_true("available" %in% names(config$data_table))
  expect_true("threads" %in% names(config$data_table))
  expect_type(config$data_table$available, "logical")
})

test_that("get_performance_config parallel section has correct fields", {
  config <- get_performance_config()
  expect_true("available" %in% names(config$parallel))
  expect_true("cores" %in% names(config$parallel))
  expect_type(config$parallel$available, "logical")
  expect_true(is.numeric(config$parallel$cores))
})

test_that("get_performance_config r_version is a string", {
  config <- get_performance_config()
  expect_type(config$r_version, "character")
  expect_true(grepl("^R (version |Under development)", config$r_version))
})

test_that("get_performance_config platform is valid", {
  config <- get_performance_config()
  expect_true(config$platform %in% c("unix", "windows"))
})

# =============================================================================
# 5. print_performance_config
# =============================================================================

test_that("print_performance_config runs without error and returns config invisibly", {
  result <- capture.output(config <- print_performance_config())
  expect_type(config, "list")
  expect_true("data_table" %in% names(config))
})

# =============================================================================
# 6. generate_puzzles_parallel (sequential fallback)
# =============================================================================

test_that("generate_puzzles_parallel validates seed length", {
  expect_error(
    generate_puzzles_parallel(
      n_puzzles = 2, type = "rectangular",
      grid = c(2, 2), size = c(100, 100),
      seeds = 1  # Wrong length
    ),
    "Length of seeds must equal n_puzzles"
  )
})
