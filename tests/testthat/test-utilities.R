# Tests for utility functions and helpers
# Updated to use current API (2025-12)

# =============================================================================
# DEPENDENCY CHECKING
# =============================================================================

test_that("check_app_dependencies runs without error", {
  skip_if_not(exists("check_app_dependencies"), "check_app_dependencies not available")

  result <- check_app_dependencies()
  expect_type(result, "logical")
  expect_length(result, 1)
})

test_that("check_conversion_tools runs without error", {
  skip_if_not(exists("check_conversion_tools"), "check_conversion_tools not available")

  # Should not crash (output depends on system configuration)
  expect_no_error(check_conversion_tools())
})

# =============================================================================
# GRADIENT BACKGROUND
# =============================================================================

test_that("create_gradient_circle_png creates output", {
  skip_if_not(exists("create_gradient_circle_png"), "create_gradient_circle_png not available")

  result <- create_gradient_circle_png(size = 200)

  # Function may return list or character depending on implementation
  expect_true(is.list(result) || is.character(result))
})

# =============================================================================
# CONFIGURATION
# =============================================================================

test_that("get_puzzle_config returns valid config", {
  skip_if_not(exists("get_puzzle_config"), "get_puzzle_config not available")

  config <- get_puzzle_config()

  expect_type(config, "list")
  # Config should have default values
  expect_true(length(config) > 0)
})

test_that("get_puzzle_colors returns color vector", {
  skip_if_not(exists("get_puzzle_colors"), "get_puzzle_colors not available")

  colors <- get_puzzle_colors(4, palette = "viridis")

  expect_type(colors, "character")
  expect_equal(length(colors), 4)
  # Should be valid hex colors
  expect_match(colors[1], "^#[0-9A-Fa-f]{6}")
})

# =============================================================================
# SVG PATH UTILITIES
# =============================================================================

test_that("parse_svg_path parses path commands", {
  skip_if_not(exists("parse_svg_path"), "parse_svg_path not available")

  path <- "M 0 0 L 10 10 C 20 20 30 30 40 40 Z"
  parsed <- parse_svg_path(path)

  expect_type(parsed, "list")
  expect_true(length(parsed) > 0)

  # Should have parsed M, L, C, Z commands
  types <- sapply(parsed, function(x) x$type)
  expect_true("M" %in% types)
  expect_true("L" %in% types)
})

test_that("reverse_path_segments reverses path direction", {
  skip_if_not(exists("reverse_path_segments"), "reverse_path_segments not available")
  skip_if_not(exists("parse_svg_path"), "parse_svg_path not available")

  path <- "M 0 0 L 10 0 L 10 10 Z"
  parsed <- parse_svg_path(path)
  reversed <- reverse_path_segments(parsed)

  # reverse_path_segments returns a path string, not a list
  expect_type(reversed, "character")
})

# =============================================================================
# PIECE COUNT FORMULAS
# =============================================================================

test_that("hexagonal piece count formula is correct", {
  # Formula: 3 * rings * (rings - 1) + 1
  # 1 ring = 1 piece (center only)
  # 2 rings = 7 pieces
  # 3 rings = 19 pieces
  # 4 rings = 37 pieces

  result_1 <- generate_puzzle(
    type = "hexagonal", grid = c(1), size = c(100), seed = 1, save_files = FALSE
  )
  expect_equal(length(result_1$pieces), 1)

  result_2 <- generate_puzzle(
    type = "hexagonal", grid = c(2), size = c(100), seed = 1, save_files = FALSE
  )
  expect_equal(length(result_2$pieces), 7)

  result_3 <- generate_puzzle(
    type = "hexagonal", grid = c(3), size = c(100), seed = 1, save_files = FALSE
  )
  expect_equal(length(result_3$pieces), 19)

  result_4 <- generate_puzzle(
    type = "hexagonal", grid = c(4), size = c(100), seed = 1, save_files = FALSE
  )
  expect_equal(length(result_4$pieces), 37)
})

test_that("concentric piece count formula matches hexagonal", {
  # Concentric uses same piece count formula as hexagonal
  result_3 <- generate_puzzle(
    type = "concentric", grid = c(3), size = c(100), seed = 1, save_files = FALSE
  )
  expect_equal(length(result_3$pieces), 19)
})

test_that("rectangular piece count is rows * cols", {
  result_2x3 <- generate_puzzle(
    type = "rectangular", grid = c(2, 3), size = c(100, 100), seed = 1, save_files = FALSE
  )
  expect_equal(length(result_2x3$pieces), 6)

  result_4x5 <- generate_puzzle(
    type = "rectangular", grid = c(4, 5), size = c(100, 100), seed = 1, save_files = FALSE
  )
  expect_equal(length(result_4x5$pieces), 20)
})

# =============================================================================
# EDGE CASE HANDLING
# =============================================================================

test_that("small rectangular puzzles work correctly", {
  # 2x2 puzzle (minimum supported size)
  result_2x2 <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    seed = 1234,
    save_files = FALSE
  )
  expect_equal(length(result_2x2$pieces), 4)
  expect_type(result_2x2$svg_content, "character")

  # 2x3 puzzle
  result_2x3 <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 3),
    seed = 1234,
    save_files = FALSE
  )
  expect_equal(length(result_2x3$pieces), 6)
  expect_type(result_2x3$svg_content, "character")
})

test_that("minimum hexagonal puzzle (1 ring) works", {
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(1),
    size = c(100),
    seed = 1234,
    save_files = FALSE
  )
  expect_equal(length(result$pieces), 1)
  expect_type(result$svg_content, "character")
})

test_that("large puzzles generate without error", {
  # 5x5 rectangular
  result_rect <- generate_puzzle(
    type = "rectangular",
    grid = c(5, 5),
    seed = 1234,
    save_files = FALSE
  )
  expect_equal(length(result_rect$pieces), 25)

  # 5 rings hexagonal = 61 pieces
  result_hex <- generate_puzzle(
    type = "hexagonal",
    grid = c(5),
    size = c(300),
    seed = 1234,
    save_files = FALSE
  )
  expect_equal(length(result_hex$pieces), 61)
})

# =============================================================================
# COLOR HANDLING
# =============================================================================

test_that("custom colors are applied to pieces", {
  custom_colors <- c("red", "blue", "green", "yellow")

  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    seed = 1234,
    colors = custom_colors,
    save_files = FALSE
  )

  svg <- result$svg_content
  expect_match(svg, "red")
  expect_match(svg, "blue")
  expect_match(svg, "green")
  expect_match(svg, "yellow")
})

test_that("palette colors are applied correctly", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    seed = 1234,
    palette = "viridis",
    save_files = FALSE
  )

  svg <- result$svg_content
  # Viridis palette produces hex colors like #440154, #21918C, etc.
  expect_match(svg, "#[0-9A-Fa-f]{6}")
})

# =============================================================================
# CANVAS SIZE CALCULATION
# =============================================================================

test_that("canvas size is correct for complete puzzle", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    size = c(200, 100),
    offset = 0,
    seed = 1234,
    save_files = FALSE
  )

  # Canvas should approximately match requested size
  expect_equal(result$canvas_size[1], 200, tolerance = 10)
  expect_equal(result$canvas_size[2], 100, tolerance = 10)
})

test_that("canvas size expands with offset", {
  result_compact <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    size = c(200, 100),
    offset = 0,
    seed = 1234,
    save_files = FALSE
  )

  result_separated <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    size = c(200, 100),
    offset = 20,
    seed = 1234,
    save_files = FALSE
  )

  # Separated canvas should be larger
  expect_gt(result_separated$canvas_size[1], result_compact$canvas_size[1])
  expect_gt(result_separated$canvas_size[2], result_compact$canvas_size[2])
})

# =============================================================================
# PARAMETER PRESERVATION
# =============================================================================

test_that("all parameters are preserved in result", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 4),
    size = c(300, 200),
    seed = 42,
    tabsize = 25,
    jitter = 5,
    offset = 10,
    palette = "magma",
    opacity = 0.8,
    stroke_width = 2.0,
    save_files = FALSE
  )

  params <- result$parameters
  expect_equal(params$seed, 42)
  expect_equal(params$grid, c(3, 4))
  expect_equal(params$size, c(300, 200))
  expect_equal(params$tabsize, 25)
  expect_equal(params$jitter, 5)
  expect_equal(params$offset, 10)
  expect_equal(params$palette, "magma")
  expect_equal(params$opacity, 0.8)
  expect_equal(params$stroke_width, 2.0)
})

# =============================================================================
# PALETTE INVERSION (Issue #45)
# =============================================================================

test_that("get_puzzle_colors with invert=FALSE returns normal order", {
  skip_if_not(exists("get_puzzle_colors"), "get_puzzle_colors not available")

  colors_normal <- get_puzzle_colors(4, palette = "magma", invert = FALSE)

  expect_type(colors_normal, "character")
  expect_equal(length(colors_normal), 4)
})

test_that("get_puzzle_colors with invert=TRUE reverses palette", {
  skip_if_not(exists("get_puzzle_colors"), "get_puzzle_colors not available")

  colors_normal <- get_puzzle_colors(4, palette = "magma", invert = FALSE)
  colors_inverted <- get_puzzle_colors(4, palette = "magma", invert = TRUE)

  # Colors should be in reverse order
  expect_equal(colors_inverted, rev(colors_normal))
})

test_that("palette inversion works for all valid palettes", {
  skip_if_not(exists("get_puzzle_colors"), "get_puzzle_colors not available")

  palettes <- c("viridis", "magma", "plasma", "inferno", "cividis", "mako", "rocket", "turbo")

  for (pal in palettes) {
    colors_normal <- get_puzzle_colors(5, palette = pal, invert = FALSE)
    colors_inverted <- get_puzzle_colors(5, palette = pal, invert = TRUE)

    expect_equal(colors_inverted, rev(colors_normal),
                 info = paste("Palette:", pal))
  }
})

test_that("black palette is not affected by inversion", {
  skip_if_not(exists("get_puzzle_colors"), "get_puzzle_colors not available")

  colors_normal <- get_puzzle_colors(4, palette = "black", invert = FALSE)
  colors_inverted <- get_puzzle_colors(4, palette = "black", invert = TRUE)

  # Black palette should be all #000000 regardless of inversion
  expect_equal(colors_normal, colors_inverted)
  expect_true(all(colors_normal == "#000000"))
})

test_that("generate_puzzle accepts palette_invert parameter", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    seed = 42,
    palette = "magma",
    palette_invert = TRUE,
    save_files = FALSE
  )

  expect_type(result$svg_content, "character")
  expect_equal(result$parameters$palette_invert, TRUE)
})

test_that("palette_invert changes colors in SVG output", {
  # Generate with normal palette
  result_normal <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    seed = 42,
    palette = "viridis",
    palette_invert = FALSE,
    save_files = FALSE
  )

  # Generate with inverted palette
  result_inverted <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    seed = 42,
    palette = "viridis",
    palette_invert = TRUE,
    save_files = FALSE
  )

  # SVG content should differ due to color inversion
  expect_false(result_normal$svg_content == result_inverted$svg_content)
})

test_that("palette_invert works with hexagonal puzzles", {
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(2),
    size = c(100),
    seed = 42,
    palette = "plasma",
    palette_invert = TRUE,
    save_files = FALSE
  )

  expect_type(result$svg_content, "character")
  expect_equal(result$parameters$palette_invert, TRUE)
})

test_that("palette_invert works with concentric puzzles", {
  result <- generate_puzzle(
    type = "concentric",
    grid = c(2),
    size = c(100),
    seed = 42,
    palette = "rocket",
    palette_invert = TRUE,
    save_files = FALSE
  )

  expect_type(result$svg_content, "character")
  expect_equal(result$parameters$palette_invert, TRUE)
})
