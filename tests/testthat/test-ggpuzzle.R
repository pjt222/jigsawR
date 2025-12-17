# Tests for ggpuzzle functionality (ggplot2 extension)

# =============================================================================
# Phase 1: Core utility tests
# =============================================================================

test_that("bezier_to_points produces correct number of points", {
  pts <- bezier_to_points(c(0, 0), c(0, 1), c(1, 1), c(1, 0), n_points = 5)

  expect_equal(nrow(pts), 5)
  expect_true(all(c("x", "y") %in% names(pts)))
})

test_that("bezier_to_points handles start and end points correctly", {
  pts <- bezier_to_points(c(0, 0), c(0.5, 1), c(0.5, 1), c(1, 0), n_points = 10)

  # First point should be at start
  expect_equal(pts$x[1], 0)
  expect_equal(pts$y[1], 0)

  # Last point should be at end
  expect_equal(pts$x[10], 1)
  expect_equal(pts$y[10], 0)
})

test_that("bezier_to_points handles default n_points",
{
  pts <- bezier_to_points(c(0, 0), c(1, 0), c(0, 1), c(1, 1))

  expect_equal(nrow(pts), 20)  # Default is 20 points
})

test_that("svg_path_to_polygon converts simple rectangle", {
  path <- "M 0 0 L 100 0 L 100 100 L 0 100 Z"
  poly <- svg_path_to_polygon(path)

  expect_equal(nrow(poly), 4)
  expect_equal(poly$x, c(0, 100, 100, 0))
  expect_equal(poly$y, c(0, 0, 100, 100))
})

test_that("svg_path_to_polygon handles paths with Bezier curves", {
  # Simple path with one Bezier curve
  path <- "M 0 0 L 50 0 C 60 0 70 10 70 20 L 70 50 Z"
  poly <- svg_path_to_polygon(path, bezier_resolution = 10)

  # Should have: M(1) + L(1) + C(9, first skipped) + L(1) = 12 points
  expect_true(nrow(poly) >= 4)  # At least the corner points
  expect_true("x" %in% names(poly))
  expect_true("y" %in% names(poly))
})

test_that("svg_path_to_polygon handles empty path", {
  poly <- svg_path_to_polygon("")

  expect_equal(nrow(poly), 0)
})

test_that("svg_path_to_polygon converts real puzzle piece path", {
  # Generate a simple puzzle and test one piece's path
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    size = c(100, 100),
    seed = 42,
    offset = 0,
    save_files = FALSE
  )

  piece_path <- result$pieces[[1]]$path
  poly <- svg_path_to_polygon(piece_path)

  # Should produce non-empty polygon
  expect_true(nrow(poly) > 0)

  # Coordinates should be within puzzle bounds (with some margin for tabs)
  expect_true(all(poly$x >= -20 & poly$x <= 120))
  expect_true(all(poly$y >= -20 & poly$y <= 120))
})

# =============================================================================
# Phase 2 & 3: Stat and Geom tests
# =============================================================================

test_that("StatPuzzle computes piece geometry", {
  df <- data.frame(value = 1:4)

  # Build the stat layer manually
  stat <- StatPuzzle$compute_panel(
    data = df,
    scales = list(),
    puzzle_type = "rectangular",
    rows = 2,
    cols = 2,
    seed = 42
  )

  # Should have geometry columns
  expect_true("x" %in% names(stat))
  expect_true("y" %in% names(stat))
  expect_true("piece_id" %in% names(stat))

  # Should have 4 unique pieces
  expect_equal(length(unique(stat$piece_id)), 4)
})

test_that("geom_puzzle_rect creates valid ggplot layer", {
  df <- data.frame(value = 1:9)

  p <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_rect(rows = 3, cols = 3, seed = 42)

  # Should be a valid ggplot
  expect_s3_class(p, "ggplot")

  # Should have one layer
  expect_equal(length(p$layers), 1)
})

test_that("geom_puzzle_rect builds without error", {
  df <- data.frame(value = 1:4)

  p <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_rect(rows = 2, cols = 2, seed = 42)

  # Should build without error
  expect_error(ggplot2::ggplot_build(p), NA)
})

test_that("geom_puzzle_rect respects seed for reproducibility", {
  df <- data.frame(value = 1:4)

  p1 <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_rect(rows = 2, cols = 2, seed = 42)

  p2 <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_rect(rows = 2, cols = 2, seed = 42)

  built1 <- ggplot2::ggplot_build(p1)
  built2 <- ggplot2::ggplot_build(p2)

  # Same seed should produce same coordinates
  expect_equal(built1$data[[1]]$x, built2$data[[1]]$x)
  expect_equal(built1$data[[1]]$y, built2$data[[1]]$y)
})

test_that("geom_puzzle_rect maps fill aesthetic correctly", {
  df <- data.frame(value = c(1, 2, 3, 4))

  p <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_rect(rows = 2, cols = 2, seed = 42)

  built <- ggplot2::ggplot_build(p)

  # Fill values should vary (not all the same)
  fills <- unique(built$data[[1]]$fill)
  expect_true(length(fills) > 1)
})

test_that("geom_puzzle_rect handles data recycling", {
  # Only 2 data points but 4 pieces
  df <- data.frame(value = c(1, 2))

  p <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_rect(rows = 2, cols = 2, seed = 42)

  # Should build without error (recycling the 2 values)
  expect_error(ggplot2::ggplot_build(p), NA)
})

test_that("geom_puzzle_rect works with theme_void", {
  df <- data.frame(value = 1:9)

  p <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_rect(rows = 3, cols = 3, seed = 42) +
    ggplot2::theme_void()

  # Should build and render without error
  expect_error(ggplot2::ggplot_build(p), NA)
})

test_that("geom_puzzle_rect works with viridis scale", {
  skip_if_not_installed("viridis")

  df <- data.frame(value = 1:9)

  p <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_rect(rows = 3, cols = 3, seed = 42) +
    ggplot2::scale_fill_viridis_c()

  expect_error(ggplot2::ggplot_build(p), NA)
})

# =============================================================================
# Edge cases
# =============================================================================

test_that("geom_puzzle_rect handles minimum 2x2 puzzle", {
  # Note: 1x1 puzzles are not supported (no edges to create)
  # Minimum supported is 2x2
  df <- data.frame(value = 1:4)

  p <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_rect(rows = 2, cols = 2, seed = 42)

  expect_error(ggplot2::ggplot_build(p), NA)
})

test_that("geom_puzzle_rect handles larger puzzles", {
  df <- data.frame(value = 1:25)

  p <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_rect(rows = 5, cols = 5, seed = 42)

  expect_error(ggplot2::ggplot_build(p), NA)
})
