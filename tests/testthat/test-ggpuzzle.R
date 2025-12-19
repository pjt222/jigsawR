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

# =============================================================================
# Hexagonal puzzle tests (geom_puzzle_hex)
# =============================================================================

test_that("StatPuzzle computes hexagonal piece geometry", {
  # 3 rings = 19 pieces
  df <- data.frame(value = 1:19)

  stat <- StatPuzzle$compute_panel(
    data = df,
    scales = list(),
    puzzle_type = "hexagonal",
    rings = 3,
    seed = 42
  )

  expect_true("x" %in% names(stat))
  expect_true("y" %in% names(stat))
  expect_true("piece_id" %in% names(stat))

  # Should have 19 unique pieces for 3 rings
  expect_equal(length(unique(stat$piece_id)), 19)
})

test_that("geom_puzzle_hex creates valid ggplot layer", {
  df <- data.frame(value = 1:19)

  p <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_hex(rings = 3, seed = 42)

  expect_s3_class(p, "ggplot")
  expect_equal(length(p$layers), 1)
})

test_that("geom_puzzle_hex builds without error", {
  df <- data.frame(value = 1:7)

  p <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_hex(rings = 2, seed = 42)

  expect_error(ggplot2::ggplot_build(p), NA)
})

test_that("geom_puzzle_hex respects seed for reproducibility", {
  df <- data.frame(value = 1:7)

  p1 <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_hex(rings = 2, seed = 42)

  p2 <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_hex(rings = 2, seed = 42)

  built1 <- ggplot2::ggplot_build(p1)
  built2 <- ggplot2::ggplot_build(p2)

  expect_equal(built1$data[[1]]$x, built2$data[[1]]$x)
  expect_equal(built1$data[[1]]$y, built2$data[[1]]$y)
})

test_that("geom_puzzle_hex handles warp parameters", {
  df <- data.frame(value = 1:7)

  # With warp disabled
  p <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_hex(rings = 2, seed = 42, do_warp = FALSE, do_trunc = FALSE)

  expect_error(ggplot2::ggplot_build(p), NA)
})

test_that("geom_puzzle_hex maps fill aesthetic correctly", {
  df <- data.frame(value = 1:7)

  p <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_hex(rings = 2, seed = 42)

  built <- ggplot2::ggplot_build(p)
  fills <- unique(built$data[[1]]$fill)
  expect_true(length(fills) > 1)
})

# =============================================================================
# Concentric puzzle tests (geom_puzzle_conc)
# =============================================================================

test_that("StatPuzzle computes concentric piece geometry", {
  # 3 rings = 19 pieces (1 center + 6 + 6 + 6)
  df <- data.frame(value = 1:19)

  stat <- StatPuzzle$compute_panel(
    data = df,
    scales = list(),
    puzzle_type = "concentric",
    rings = 3,
    seed = 42
  )

  expect_true("x" %in% names(stat))
  expect_true("y" %in% names(stat))
  expect_true("piece_id" %in% names(stat))

  # Should have 19 unique pieces
  expect_equal(length(unique(stat$piece_id)), 19)
})

test_that("geom_puzzle_conc creates valid ggplot layer", {
  # 2 rings = 13 pieces (1 center + 6 + 6)
  df <- data.frame(value = 1:13)

  p <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_conc(rings = 2, seed = 42)

  expect_s3_class(p, "ggplot")
  expect_equal(length(p$layers), 1)
})

test_that("geom_puzzle_conc builds without error", {
  df <- data.frame(value = 1:13)

  p <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_conc(rings = 2, seed = 42)

  expect_error(ggplot2::ggplot_build(p), NA)
})

test_that("geom_puzzle_conc respects seed for reproducibility", {
  df <- data.frame(value = 1:13)

  p1 <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_conc(rings = 2, seed = 42)

  p2 <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_conc(rings = 2, seed = 42)

  built1 <- ggplot2::ggplot_build(p1)
  built2 <- ggplot2::ggplot_build(p2)

  expect_equal(built1$data[[1]]$x, built2$data[[1]]$x)
  expect_equal(built1$data[[1]]$y, built2$data[[1]]$y)
})

test_that("geom_puzzle_conc handles center_shape parameter", {
  df <- data.frame(value = 1:13)

  # With circle center shape
  p <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_conc(rings = 2, seed = 42, center_shape = "circle")

  expect_error(ggplot2::ggplot_build(p), NA)
})

test_that("geom_puzzle_conc maps fill aesthetic correctly", {
  df <- data.frame(value = 1:13)

  p <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_conc(rings = 2, seed = 42)

  built <- ggplot2::ggplot_build(p)
  fills <- unique(built$data[[1]]$fill)
  expect_true(length(fills) > 1)
})

# =============================================================================
# Voronoi puzzle tests (geom_puzzle_voronoi)
# =============================================================================

test_that("StatPuzzle computes voronoi piece geometry", {
  skip_if_not_installed("deldir")

  df <- data.frame(value = 1:12)

  stat <- StatPuzzle$compute_panel(
    data = df,
    scales = list(),
    puzzle_type = "voronoi",
    n_cells = 12,
    seed = 42
  )

  expect_true("x" %in% names(stat))
  expect_true("y" %in% names(stat))
  expect_true("piece_id" %in% names(stat))

  # Should have 12 unique pieces
  expect_equal(length(unique(stat$piece_id)), 12)
})

test_that("geom_puzzle_voronoi creates valid ggplot layer", {
  skip_if_not_installed("deldir")

  df <- data.frame(value = 1:8)

  p <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_voronoi(n_cells = 8, seed = 42)

  expect_s3_class(p, "ggplot")
  expect_equal(length(p$layers), 1)
})

test_that("geom_puzzle_voronoi builds without error", {
  skip_if_not_installed("deldir")

  df <- data.frame(value = 1:10)

  p <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_voronoi(n_cells = 10, seed = 42)

  expect_error(ggplot2::ggplot_build(p), NA)
})

test_that("geom_puzzle_voronoi respects seed for reproducibility", {
  skip_if_not_installed("deldir")

  df <- data.frame(value = 1:8)

  p1 <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_voronoi(n_cells = 8, seed = 42)

  p2 <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_voronoi(n_cells = 8, seed = 42)

  built1 <- ggplot2::ggplot_build(p1)
  built2 <- ggplot2::ggplot_build(p2)

  expect_equal(built1$data[[1]]$x, built2$data[[1]]$x)
  expect_equal(built1$data[[1]]$y, built2$data[[1]]$y)
})

test_that("geom_puzzle_voronoi handles point_distribution parameter", {
  skip_if_not_installed("deldir")

  df <- data.frame(value = 1:8)

  # With uniform distribution
  p <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_voronoi(n_cells = 8, seed = 42, point_distribution = "uniform")

  expect_error(ggplot2::ggplot_build(p), NA)
})

test_that("geom_puzzle_voronoi maps fill aesthetic correctly", {
  skip_if_not_installed("deldir")

  df <- data.frame(value = 1:8)

  p <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_voronoi(n_cells = 8, seed = 42)

  built <- ggplot2::ggplot_build(p)
  fills <- unique(built$data[[1]]$fill)
  expect_true(length(fills) > 1)
})

# =============================================================================
# Random puzzle tests (geom_puzzle_random)
# =============================================================================

test_that("StatPuzzle computes random piece geometry", {
  skip_if_not_installed("RCDT")

  df <- data.frame(value = 1:10)

  stat <- StatPuzzle$compute_panel(
    data = df,
    scales = list(),
    puzzle_type = "random",
    n_pieces = 10,
    seed = 42
  )

  expect_true("x" %in% names(stat))
  expect_true("y" %in% names(stat))
  expect_true("piece_id" %in% names(stat))

  # Should have at least some pieces (exact count depends on triangulation)
  expect_true(length(unique(stat$piece_id)) > 0)
})

test_that("geom_puzzle_random creates valid ggplot layer", {
  skip_if_not_installed("RCDT")

  df <- data.frame(value = 1:8)

  p <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_random(n_pieces = 8, seed = 42)

  expect_s3_class(p, "ggplot")
  expect_equal(length(p$layers), 1)
})

test_that("geom_puzzle_random builds without error", {
  skip_if_not_installed("RCDT")

  df <- data.frame(value = 1:10)

  p <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_random(n_pieces = 10, seed = 42)

  expect_error(ggplot2::ggplot_build(p), NA)
})

test_that("geom_puzzle_random respects seed for reproducibility", {
  skip_if_not_installed("RCDT")

  df <- data.frame(value = 1:8)

  p1 <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_random(n_pieces = 8, seed = 42)

  p2 <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_random(n_pieces = 8, seed = 42)

  built1 <- ggplot2::ggplot_build(p1)
  built2 <- ggplot2::ggplot_build(p2)

  expect_equal(built1$data[[1]]$x, built2$data[[1]]$x)
  expect_equal(built1$data[[1]]$y, built2$data[[1]]$y)
})

test_that("geom_puzzle_random handles n_corner parameter", {
  skip_if_not_installed("RCDT")

  df <- data.frame(value = 1:8)

  # With hexagonal base (6 corners)
  p <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_random(n_pieces = 8, seed = 42, n_corner = 6)

  expect_error(ggplot2::ggplot_build(p), NA)
})

test_that("geom_puzzle_random maps fill aesthetic correctly", {
  skip_if_not_installed("RCDT")

  df <- data.frame(value = 1:8)

  p <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_random(n_pieces = 8, seed = 42)

  built <- ggplot2::ggplot_build(p)
  fills <- unique(built$data[[1]]$fill)
  expect_true(length(fills) > 1)
})

# =============================================================================
# Integration tests - all types with common patterns
# =============================================================================

test_that("all puzzle types work with theme_void", {
  skip_if_not_installed("deldir")
  skip_if_not_installed("RCDT")

  df <- data.frame(value = 1:8)

  # Rectangular
  p_rect <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_rect(rows = 2, cols = 4, seed = 42) +
    ggplot2::theme_void()
  expect_error(ggplot2::ggplot_build(p_rect), NA)

  # Hexagonal (2 rings = 7 pieces)
  df_hex <- data.frame(value = 1:7)
  p_hex <- ggplot2::ggplot(df_hex, ggplot2::aes(fill = value)) +
    geom_puzzle_hex(rings = 2, seed = 42) +
    ggplot2::theme_void()
  expect_error(ggplot2::ggplot_build(p_hex), NA)

  # Concentric (2 rings = 13 pieces)
  df_conc <- data.frame(value = 1:13)
  p_conc <- ggplot2::ggplot(df_conc, ggplot2::aes(fill = value)) +
    geom_puzzle_conc(rings = 2, seed = 42) +
    ggplot2::theme_void()
  expect_error(ggplot2::ggplot_build(p_conc), NA)

  # Voronoi
  p_vor <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_voronoi(n_cells = 8, seed = 42) +
    ggplot2::theme_void()
  expect_error(ggplot2::ggplot_build(p_vor), NA)

  # Random
  p_rnd <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_random(n_pieces = 8, seed = 42) +
    ggplot2::theme_void()
  expect_error(ggplot2::ggplot_build(p_rnd), NA)
})

test_that("all puzzle types work with viridis scale", {
  skip_if_not_installed("viridis")
  skip_if_not_installed("deldir")
  skip_if_not_installed("RCDT")

  df <- data.frame(value = 1:8)

  # Test each type with viridis
  types <- list(
    rect = geom_puzzle_rect(rows = 2, cols = 4, seed = 42),
    vor = geom_puzzle_voronoi(n_cells = 8, seed = 42),
    rnd = geom_puzzle_random(n_pieces = 8, seed = 42)
  )

  for (geom in types) {
    p <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
      geom +
      ggplot2::scale_fill_viridis_c()
    expect_error(ggplot2::ggplot_build(p), NA)
  }
})

test_that("all puzzle types handle data recycling", {
  skip_if_not_installed("deldir")
  skip_if_not_installed("RCDT")

  # Only 2 data points for puzzles with more pieces
  df <- data.frame(value = c(1, 2))

  # Rectangular (4 pieces, 2 data points)
  p_rect <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_rect(rows = 2, cols = 2, seed = 42)
  expect_error(ggplot2::ggplot_build(p_rect), NA)

  # Hexagonal (7 pieces for 2 rings)
  p_hex <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_hex(rings = 2, seed = 42)
  expect_error(ggplot2::ggplot_build(p_hex), NA)

  # Concentric (13 pieces for 2 rings)
  p_conc <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_conc(rings = 2, seed = 42)
  expect_error(ggplot2::ggplot_build(p_conc), NA)

  # Voronoi (8 pieces)
  p_vor <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_voronoi(n_cells = 8, seed = 42)
  expect_error(ggplot2::ggplot_build(p_vor), NA)

  # Random (variable pieces)
  p_rnd <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_random(n_pieces = 8, seed = 42)
  expect_error(ggplot2::ggplot_build(p_rnd), NA)
})

# =============================================================================
# Fusion groups tests (Issue #68)
# =============================================================================

test_that("geom_puzzle_rect accepts fusion_groups parameter", {
  df <- data.frame(value = 1:9)

  # PILES notation string
  p <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_rect(rows = 3, cols = 3, seed = 42,
                     fusion_groups = "1-2-3,7-8-9")

  expect_s3_class(p, "ggplot")
  expect_error(ggplot2::ggplot_build(p), NA)
})

test_that("geom_puzzle_rect accepts fusion_groups as list", {
  df <- data.frame(value = 1:9)

  # List of integer vectors
  p <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_rect(rows = 3, cols = 3, seed = 42,
                     fusion_groups = list(c(1, 2, 3), c(7, 8, 9)))

  expect_s3_class(p, "ggplot")
  expect_error(ggplot2::ggplot_build(p), NA)
})

test_that("geom_puzzle_rect accepts fusion_style parameter", {
  df <- data.frame(value = 1:9)

  # Test each fusion style
  for (style in c("none", "dashed", "solid")) {
    p <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
      geom_puzzle_rect(rows = 3, cols = 3, seed = 42,
                       fusion_groups = "1-2-3",
                       fusion_style = style)

    expect_error(ggplot2::ggplot_build(p), NA)
  }
})

test_that("geom_puzzle_rect accepts fusion_opacity parameter", {
  df <- data.frame(value = 1:9)

  p <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_rect(rows = 3, cols = 3, seed = 42,
                     fusion_groups = "1-2-3",
                     fusion_style = "dashed",
                     fusion_opacity = 0.5)

  expect_s3_class(p, "ggplot")
  expect_error(ggplot2::ggplot_build(p), NA)
})

test_that("geom_puzzle_hex accepts fusion_groups parameter", {
  df <- data.frame(value = 1:19)

  p <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_hex(rings = 3, seed = 42,
                    fusion_groups = "1-2-3")

  expect_s3_class(p, "ggplot")
  expect_error(ggplot2::ggplot_build(p), NA)
})

test_that("geom_puzzle_conc accepts fusion_groups parameter", {
  df <- data.frame(value = 1:19)

  p <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_conc(rings = 3, seed = 42,
                     fusion_groups = "1-2-3")

  expect_s3_class(p, "ggplot")
  expect_error(ggplot2::ggplot_build(p), NA)
})

test_that("geom_puzzle_voronoi accepts fusion_groups parameter", {
  skip_if_not_installed("deldir")

  df <- data.frame(value = 1:12)

  p <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_voronoi(n_cells = 12, seed = 42,
                        fusion_groups = "1-2-3")

  expect_s3_class(p, "ggplot")
  expect_error(ggplot2::ggplot_build(p), NA)
})

test_that("geom_puzzle_random accepts fusion_groups parameter", {
  skip_if_not_installed("RCDT")

  df <- data.frame(value = 1:12)

  p <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
    geom_puzzle_random(n_pieces = 12, seed = 42,
                       fusion_groups = "1-2-3")

  expect_s3_class(p, "ggplot")
  expect_error(ggplot2::ggplot_build(p), NA)
})

test_that("StatPuzzle setup_params sets fusion defaults", {
  # Test that setup_params correctly initializes fusion parameters
  params <- StatPuzzle$setup_params(data.frame(), list())

  expect_null(params$fusion_groups)
  expect_equal(params$fusion_style, "none")
  expect_equal(params$fusion_opacity, 0.3)
})

test_that("StatPuzzle compute_panel accepts fusion parameters", {
  df <- data.frame(value = 1:9)

  # Should not error with fusion parameters
  stat <- StatPuzzle$compute_panel(
    data = df,
    scales = list(),
    puzzle_type = "rectangular",
    rows = 3,
    cols = 3,
    seed = 42,
    fusion_groups = "1-2-3",
    fusion_style = "dashed",
    fusion_opacity = 0.5
  )

  expect_true("x" %in% names(stat))
  expect_true("y" %in% names(stat))
  expect_equal(length(unique(stat$piece_id)), 9)
})

test_that("all puzzle geoms support full fusion parameter set", {
  skip_if_not_installed("deldir")
  skip_if_not_installed("RCDT")

  # Define all geom constructors with their required data
  test_cases <- list(
    list(
      geom = function() geom_puzzle_rect(
        rows = 3, cols = 3, seed = 42,
        fusion_groups = "1-2,4-5",
        fusion_style = "solid",
        fusion_opacity = 0.7
      ),
      n_pieces = 9
    ),
    list(
      geom = function() geom_puzzle_hex(
        rings = 2, seed = 42,
        fusion_groups = "1-2,4-5",
        fusion_style = "dashed",
        fusion_opacity = 0.5
      ),
      n_pieces = 7
    ),
    list(
      geom = function() geom_puzzle_conc(
        rings = 2, seed = 42,
        fusion_groups = "1-2,4-5",
        fusion_style = "none",
        fusion_opacity = 0.3
      ),
      n_pieces = 13
    ),
    list(
      geom = function() geom_puzzle_voronoi(
        n_cells = 8, seed = 42,
        fusion_groups = "1-2,4-5",
        fusion_style = "solid",
        fusion_opacity = 0.8
      ),
      n_pieces = 8
    ),
    list(
      geom = function() geom_puzzle_random(
        n_pieces = 8, seed = 42,
        fusion_groups = "1-2,4-5",
        fusion_style = "dashed",
        fusion_opacity = 0.6
      ),
      n_pieces = 16  # Upper bound for random
    )
  )

  for (tc in test_cases) {
    df <- data.frame(value = seq_len(tc$n_pieces))
    p <- ggplot2::ggplot(df, ggplot2::aes(fill = value)) +
      tc$geom()

    expect_error(ggplot2::ggplot_build(p), NA)
  }
})
