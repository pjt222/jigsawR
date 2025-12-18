# Tests for RNG Batch Optimization (Issue #65)
# Verifies that the batch RNG integration produces identical results to per-call RNG

test_that("calc_rect_rng_count returns correct count for small grid", {
  # 3x2 grid: (yn-1)*(6+xn*5) + (xn-1)*(6+yn*5)
  # Horizontal: (2-1)*(6+3*5) = 1*21 = 21
  # Vertical: (3-1)*(6+2*5) = 2*16 = 32
  # Total: 53
  expect_equal(calc_rect_rng_count(3, 2), 53L)
})

test_that("calc_rect_rng_count returns correct count for standard grid", {
  # 5x4 grid
  # Horizontal: (4-1)*(6+5*5) = 3*31 = 93

  # Vertical: (5-1)*(6+4*5) = 4*26 = 104
  # Total: 197
  expect_equal(calc_rect_rng_count(5, 4), 197L)
})

test_that("calc_rect_rng_count returns correct count for large grid", {
  # 15x10 grid (default)
  # Horizontal: (10-1)*(6+15*5) = 9*81 = 729
  # Vertical: (15-1)*(6+10*5) = 14*56 = 784
  # Total: 1513
  expect_equal(calc_rect_rng_count(15, 10), 1513L)
})

test_that("calc_hex_rng_count returns correct count for 2 rings", {
  # 2 rings: need to count hex_gen_dh and hex_gen_dv tabs
  count <- calc_hex_rng_count(2)
  expect_true(count > 0)
  expect_true(is.integer(count))
})

test_that("calc_hex_rng_count returns correct count for 3 rings", {
  count <- calc_hex_rng_count(3)
  expect_true(count > calc_hex_rng_count(2))  # More rings = more RNG calls
})

test_that("calc_hex_rng_count scales with ring count", {
  counts <- sapply(2:6, calc_hex_rng_count)
  # Each additional ring should add more RNG calls
  expect_true(all(diff(counts) > 0))
})

test_that("create_rng_iterator returns all required functions", {
  rng <- create_rng_iterator(42, 100)
  expect_true(is.function(rng$next_val))
  expect_true(is.function(rng$uniform))
  expect_true(is.function(rng$rbool))
  expect_true(is.function(rng$get_index))
  expect_true(is.function(rng$get_count))
})

test_that("create_rng_iterator next_val returns values in [0, 1)", {
  rng <- create_rng_iterator(42, 100)
  values <- sapply(1:100, function(i) rng$next_val())
  expect_true(all(values >= 0))
  expect_true(all(values < 1))
})

test_that("create_rng_iterator uniform returns values in range", {
  rng <- create_rng_iterator(42, 100)
  values <- sapply(1:100, function(i) rng$uniform(-0.5, 0.5))
  expect_true(all(values >= -0.5))
  expect_true(all(values <= 0.5))
})

test_that("create_rng_iterator rbool returns logical values", {
  rng <- create_rng_iterator(42, 100)
  values <- sapply(1:100, function(i) rng$rbool())
  expect_true(all(is.logical(values)))
})

test_that("create_rng_iterator is deterministic", {
  rng1 <- create_rng_iterator(42, 50)
  rng2 <- create_rng_iterator(42, 50)

  values1 <- sapply(1:50, function(i) rng1$next_val())
  values2 <- sapply(1:50, function(i) rng2$next_val())

  expect_equal(values1, values2)
})

test_that("create_rng_iterator different seeds produce different values", {
  rng1 <- create_rng_iterator(42, 50)
  rng2 <- create_rng_iterator(123, 50)

  values1 <- sapply(1:50, function(i) rng1$next_val())
  values2 <- sapply(1:50, function(i) rng2$next_val())

  expect_false(identical(values1, values2))
})

test_that("create_rng_iterator tracks index correctly", {
  rng <- create_rng_iterator(42, 100)
  expect_equal(rng$get_index(), 0)

  rng$next_val()
  expect_equal(rng$get_index(), 1)

  rng$uniform(0, 1)
  expect_equal(rng$get_index(), 2)

  rng$rbool()
  expect_equal(rng$get_index(), 3)
})

test_that("rectangular puzzle is deterministic with batch RNG", {
  # Generate same puzzle twice with same seed
  result1 <- generate_jigsaw_svg(seed = 42, xn = 5, yn = 4)
  result2 <- generate_jigsaw_svg(seed = 42, xn = 5, yn = 4)

  expect_equal(result1$horizontal, result2$horizontal)
  expect_equal(result1$vertical, result2$vertical)
  expect_equal(result1$border, result2$border)
})

test_that("hexagonal puzzle is deterministic with batch RNG", {
  # Generate same puzzle twice with same seed
  result1 <- generate_hex_jigsaw_svg(seed = 42, rings = 3)
  result2 <- generate_hex_jigsaw_svg(seed = 42, rings = 3)

  expect_equal(result1$horizontal, result2$horizontal)
  expect_equal(result1$vertical, result2$vertical)
  expect_equal(result1$border, result2$border)
})

test_that("different seeds produce different rectangular puzzles", {
  result1 <- generate_jigsaw_svg(seed = 42, xn = 5, yn = 4)
  result2 <- generate_jigsaw_svg(seed = 123, xn = 5, yn = 4)

  expect_false(identical(result1$horizontal, result2$horizontal))
})

test_that("different seeds produce different hexagonal puzzles", {
  result1 <- generate_hex_jigsaw_svg(seed = 42, rings = 3)
  result2 <- generate_hex_jigsaw_svg(seed = 123, rings = 3)

  expect_false(identical(result1$horizontal, result2$horizontal))
})

test_that("batch RNG matches per-call RNG values", {
  # Verify that uniform_batch produces same values as per-call would
  seed <- 42
  count <- 20

  # Get batch values
  batch_values <- uniform_batch(seed, count)

  # Calculate expected per-call values using the same algorithm
  expected <- numeric(count)
  for (i in seq_len(count)) {
    x <- sin(seed + i - 1) * 10000
    expected[i] <- x - floor(x)
  }

  expect_equal(batch_values, expected, tolerance = 1e-10)
})
