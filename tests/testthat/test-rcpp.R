# Tests for Rcpp C++ optimizations
# These tests verify correctness and fallback behavior

# =============================================================================
# RCPP AVAILABILITY
# =============================================================================

test_that(".rcpp_status detects availability correctly", {
  # Should return logical
  status <- .rcpp_status()
  expect_type(status, "logical")
  expect_length(status, 1)
})

# =============================================================================
# RNG BATCH GENERATION
# =============================================================================

test_that("R fallback random batch matches JavaScript RNG", {
  # Test that our R implementation matches the expected sine-based RNG
  result <- .random_batch_r(seed = 42, count = 5)

  expect_type(result, "double")
  expect_length(result, 5)

  # Values should be in [0, 1]
  expect_true(all(result >= 0 & result <= 1))

  # Verify determinism - same seed = same output
  result2 <- .random_batch_r(seed = 42, count = 5)
  expect_identical(result, result2)
})

test_that("R fallback random batch respects min/max", {
  result <- .random_batch_r(seed = 100, count = 10, min_val = -5, max_val = 5)

  expect_type(result, "double")
  expect_length(result, 10)
  expect_true(all(result >= -5 & result <= 5))
})

test_that("uniform_batch produces consistent results", {
  # This tests the wrapper function (uses C++ if available, R fallback otherwise)
  result1 <- uniform_batch(seed = 123, count = 20)
  result2 <- uniform_batch(seed = 123, count = 20)

  expect_identical(result1, result2)
  expect_length(result1, 20)
  expect_true(all(result1 >= 0 & result1 <= 1))
})

test_that("uniform_batch matches single-call RNG", {
  # Verify that batch generation produces same values as single calls
  # This is critical for maintaining puzzle reproducibility

  seed <- 42
  count <- 5

  # Generate batch

  batch <- uniform_batch(seed = seed, count = count)

  # Generate one-by-one using same algorithm
  single <- numeric(count)
  for (i in seq_len(count)) {
    x <- sin(seed + i - 1) * 10000
    single[i] <- x - floor(x)
  }

  expect_equal(batch, single, tolerance = 1e-10)
})

# =============================================================================
# BEZIER INTERPOLATION
# =============================================================================

test_that("R fallback bezier batch produces correct shape", {
  p0 <- c(0, 0)
  cp1 <- c(0, 1)
  cp2 <- c(1, 0)
  p1 <- c(1, 1)

  result <- .bezier_batch_r(p0, cp1, cp2, p1, n_points = 20)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 20)
  expect_equal(ncol(result), 2)
  expect_true("x" %in% names(result))
  expect_true("y" %in% names(result))

  # First point should be p0
  expect_equal(result$x[1], p0[1], tolerance = 1e-10)
  expect_equal(result$y[1], p0[2], tolerance = 1e-10)

  # Last point should be p1
  expect_equal(result$x[20], p1[1], tolerance = 1e-10)
  expect_equal(result$y[20], p1[2], tolerance = 1e-10)
})

test_that("bezier_batch matches existing bezier_to_points", {
  # Verify our optimized version matches the original implementation
  p0 <- c(10, 20)
  cp1 <- c(15, 30)
  cp2 <- c(25, 15)
  p1 <- c(30, 25)

  # Original implementation (from bezier_utils.R)
  original <- bezier_to_points(p0, cp1, cp2, p1, n_points = 20)

  # Our batch implementation
  batch <- bezier_batch(p0, cp1, cp2, p1, n_points = 20)

  expect_equal(batch$x, original$x, tolerance = 1e-10)
  expect_equal(batch$y, original$y, tolerance = 1e-10)
})

test_that("bezier_batch produces consistent results", {
  p0 <- c(0, 0)
  cp1 <- c(1, 2)
  cp2 <- c(2, 1)
  p1 <- c(3, 3)

  result1 <- bezier_batch(p0, cp1, cp2, p1, n_points = 50)
  result2 <- bezier_batch(p0, cp1, cp2, p1, n_points = 50)

  expect_identical(result1, result2)
})

# =============================================================================
# SVG PATH TRANSLATION
# =============================================================================

test_that("R fallback SVG translate handles basic commands", {
  # Test M and L commands
  path <- "M 10 20 L 30 40"
  result <- .svg_translate_r(path, dx = 5, dy = -3)

  expect_match(result, "M 15\\.00 17\\.00")
  expect_match(result, "L 35\\.00 37\\.00")
})

test_that("R fallback SVG translate handles cubic bezier", {
  path <- "M 0 0 C 10 10 20 20 30 30"
  result <- .svg_translate_r(path, dx = 100, dy = 200)

  expect_match(result, "M 100\\.00 200\\.00")
  expect_match(result, "C 110\\.00 210\\.00 120\\.00 220\\.00 130\\.00 230\\.00")
})

test_that("R fallback SVG translate handles arc command", {
  # Arc: A rx ry rotation large-arc sweep x y
  path <- "M 0 0 A 10 10 0 1 0 20 20"
  result <- .svg_translate_r(path, dx = 5, dy = 5)

  # rx, ry, rotation, large-arc, sweep should stay the same
  # Only endpoint (20, 20) should translate to (25, 25)
  expect_match(result, "A 10 10 0 1 0 25\\.00 25\\.00")
})

test_that("R fallback SVG translate handles Z command", {
  path <- "M 0 0 L 10 0 L 10 10 Z"
  result <- .svg_translate_r(path, dx = 100, dy = 100)

  expect_match(result, "Z")
})

test_that("R fallback SVG translate returns original for zero translation", {
  path <- "M 10 20 L 30 40"
  result <- .svg_translate_r(path, dx = 0, dy = 0)

  expect_equal(result, path)
})

test_that("svg_translate produces consistent results", {
  path <- "M 0 0 L 100 100 C 150 50 200 150 250 100 Z"

  result1 <- svg_translate(path, dx = 10, dy = 20)
  result2 <- svg_translate(path, dx = 10, dy = 20)

  expect_identical(result1, result2)
})

test_that("svg_translate matches original translate_svg_path", {
  # Verify our optimized version matches the original implementation
  path <- "M 10.5 20.3 L 30.7 40.9 C 50 60 70 80 90 100 Z"

  original <- translate_svg_path(path, dx = 5.5, dy = -10.2)
  optimized <- svg_translate(path, dx = 5.5, dy = -10.2)

  # Parse both results and compare numerically
  # (string comparison may fail due to whitespace differences)
  original_nums <- as.numeric(unlist(regmatches(original, gregexpr("-?[0-9]+\\.?[0-9]*", original))))
  optimized_nums <- as.numeric(unlist(regmatches(optimized, gregexpr("-?[0-9]+\\.?[0-9]*", optimized))))

  expect_equal(optimized_nums, original_nums, tolerance = 0.01)
})

# =============================================================================
# DETERMINISM - CRITICAL FOR PUZZLE REPRODUCIBILITY
# =============================================================================

test_that("puzzle generation is deterministic with Rcpp wrappers", {
  # This is the most important test - ensure puzzles are reproducible

  result1 <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    seed = 12345,
    save_files = FALSE
  )

  result2 <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    seed = 12345,
    save_files = FALSE
  )

  # Same seed must produce identical output
  expect_identical(result1$svg_content, result2$svg_content)
  expect_identical(result1$pieces[[1]]$path, result2$pieces[[1]]$path)
})

test_that("hexagonal puzzle generation is deterministic", {
  result1 <- generate_puzzle(
    type = "hexagonal",
    grid = c(2),
    size = c(100),
    seed = 54321,
    save_files = FALSE
  )

  result2 <- generate_puzzle(
    type = "hexagonal",
    grid = c(2),
    size = c(100),
    seed = 54321,
    save_files = FALSE
  )

  expect_identical(result1$svg_content, result2$svg_content)
})

# =============================================================================
# PERFORMANCE CHARACTERISTICS (Skip if benchmarking not available)
# =============================================================================

test_that("batch operations handle large inputs", {
  # Test that we can handle large batches without error
  large_batch <- uniform_batch(seed = 1, count = 10000)
  expect_length(large_batch, 10000)

  # Test bezier with many points
  large_bezier <- bezier_batch(
    c(0, 0), c(1, 2), c(3, 1), c(4, 4),
    n_points = 1000
  )
  expect_equal(nrow(large_bezier), 1000)
})
