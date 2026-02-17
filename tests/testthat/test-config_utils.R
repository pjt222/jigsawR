# Tests for configuration utility functions (R/config_utils.R)

# =============================================================================
# 1. get_puzzle_config
# =============================================================================

test_that("get_puzzle_config returns a list", {
  cfg <- get_puzzle_config()
  expect_type(cfg, "list")
})

test_that("get_puzzle_config has expected top-level fields", {
  cfg <- get_puzzle_config()
  expect_true("styling" %in% names(cfg))
  expect_true("seed" %in% names(cfg))
  expect_true("colors" %in% names(cfg))
  expect_true("constraints" %in% names(cfg))
})

test_that("get_puzzle_config styling has expected fields", {
  cfg <- get_puzzle_config()
  expect_true("tabsize" %in% names(cfg$styling))
  expect_true("stroke_width" %in% names(cfg$styling))
  expect_true("opacity" %in% names(cfg$styling))
})

test_that("get_puzzle_config returns numeric seed", {
  cfg <- get_puzzle_config()
  expect_true(is.numeric(cfg$seed))
})

# =============================================================================
# 2. get_puzzle_colors
# =============================================================================

test_that("get_puzzle_colors returns character vector", {
  colors <- get_puzzle_colors(5)
  expect_type(colors, "character")
  expect_length(colors, 5)
})

test_that("get_puzzle_colors returns requested number of colors", {
  for (n in c(1, 3, 10, 20)) {
    colors <- get_puzzle_colors(n)
    expect_length(colors, n)
  }
})

test_that("get_puzzle_colors black palette returns all black", {
  colors <- get_puzzle_colors(5, palette = "black")
  expect_true(all(colors == "#000000"))
})

test_that("get_puzzle_colors with named viridis palettes", {
  valid_palettes <- c("viridis", "magma", "plasma", "inferno",
                      "cividis", "mako", "rocket", "turbo")
  for (pal in valid_palettes) {
    colors <- get_puzzle_colors(4, palette = pal)
    expect_length(colors, 4)
    # Viridis palettes return hex color strings
    expect_true(all(grepl("^#", colors)))
  }
})

test_that("get_puzzle_colors with inverted palette reverses order", {
  colors_fwd <- get_puzzle_colors(5, palette = "viridis", invert = FALSE)
  colors_rev <- get_puzzle_colors(5, palette = "viridis", invert = TRUE)
  expect_equal(colors_fwd, rev(colors_rev))
})

test_that("get_puzzle_colors falls back for invalid palette name", {
  # Invalid palette should produce a warning via log_warn and fall back to black
  colors <- suppressMessages(get_puzzle_colors(3, palette = "nonexistent"))
  expect_length(colors, 3)
  expect_true(all(colors == "#000000"))
})

# =============================================================================
# 3. detect_puzzle_type
# =============================================================================

test_that("detect_puzzle_type returns 'other' for empty pieces", {
  expect_equal(detect_puzzle_type(list()), "other")
})

test_that("detect_puzzle_type detects rectangular from explicit type", {
  pieces <- list(list(type = "rectangular"))
  expect_equal(detect_puzzle_type(pieces), "rectangular")
})

test_that("detect_puzzle_type detects hexagonal from explicit type", {
  pieces <- list(list(type = "hexagonal"))
  expect_equal(detect_puzzle_type(pieces), "hexagonal")
})

test_that("detect_puzzle_type detects concentric from explicit type", {
  pieces <- list(list(type = "concentric"))
  expect_equal(detect_puzzle_type(pieces), "concentric")
})

test_that("detect_puzzle_type detects rectangular from grid_pos fallback", {
  pieces <- list(list(grid_pos = list(row = 1, col = 1)))
  expect_equal(detect_puzzle_type(pieces), "rectangular")
})

test_that("detect_puzzle_type detects hexagonal from ring_pos fallback", {
  pieces <- list(list(ring_pos = list(ring = 1, pos = 0)))
  expect_equal(detect_puzzle_type(pieces), "hexagonal")
})

test_that("detect_puzzle_type detects concentric from ring_pos + inner_radius", {
  pieces <- list(list(ring_pos = list(ring = 1, pos = 0), inner_radius = 10))
  expect_equal(detect_puzzle_type(pieces), "concentric")
})

# =============================================================================
# 4. get_fallback_config
# =============================================================================

test_that("get_fallback_config returns a list with expected structure", {
  cfg <- get_fallback_config()
  expect_type(cfg, "list")
  expect_true("rectangular" %in% names(cfg))
  expect_true("hexagonal" %in% names(cfg))
  expect_true("concentric" %in% names(cfg))
  expect_true("styling" %in% names(cfg))
  expect_true("colors" %in% names(cfg))
})

test_that("get_fallback_config rectangular defaults are sensible", {
  cfg <- get_fallback_config()
  expect_equal(cfg$rectangular$rows, 2)
  expect_equal(cfg$rectangular$cols, 2)
  expect_true(cfg$rectangular$width > 0)
  expect_true(cfg$rectangular$height > 0)
})

# =============================================================================
# 5. reorder_colors_for_direction
# =============================================================================

test_that("reorder_colors_for_direction returns unchanged for 'forward'", {
  colors <- c("a", "b", "c")
  pieces <- list(list(type = "rectangular"))
  result <- reorder_colors_for_direction(colors, pieces, "forward")
  expect_equal(result, colors)
})

test_that("reorder_colors_for_direction reverses for rectangular 'reverse'", {
  colors <- c("a", "b", "c")
  pieces <- list(list(type = "rectangular"),
                 list(type = "rectangular"),
                 list(type = "rectangular"))
  result <- reorder_colors_for_direction(colors, pieces, "reverse")
  expect_equal(result, rev(colors))
})

test_that("reorder_colors_for_direction handles single color", {
  colors <- "a"
  pieces <- list(list(type = "rectangular"))
  result <- reorder_colors_for_direction(colors, pieces, "reverse")
  expect_equal(result, "a")
})
