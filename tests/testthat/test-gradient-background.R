# Tests for Gradient Background in Puzzles
# Converted from standalone tests/test_gradient_background.R

# =============================================================================
# CUSTOM GRADIENT BACKGROUND TESTS
# =============================================================================

test_that("custom gradient background is rendered correctly", {
  gradient_bg <- list(
    type = "gradient",
    center = "#ff0000",
    middle = "#00ff00",
    edge = "#0000ff"
  )

  result <- generate_puzzle(
    type = "rectangular",
    seed = 42,
    grid = c(2, 2),
    size = c(200, 200),
    fill_color = gradient_bg,
    save_files = FALSE
  )

  # Check for gradient elements
  expect_match(result$svg_content, 'radialGradient')
  expect_match(result$svg_content, '#ff0000')
  expect_match(result$svg_content, '#00ff00')
  expect_match(result$svg_content, '#0000ff')
  expect_match(result$svg_content, 'url\\(#')
})

test_that("hexagonal puzzle supports custom gradient background", {
  gradient_bg <- list(
    type = "gradient",
    center = "#aabbcc",
    middle = "#112233",
    edge = "#445566"
  )

  result <- generate_puzzle(
    type = "hexagonal",
    seed = 42,
    grid = c(2),
    size = c(200),
    fill_color = gradient_bg,
    save_files = FALSE
  )

  expect_match(result$svg_content, 'radialGradient')
  expect_match(result$svg_content, '#aabbcc')
  expect_match(result$svg_content, '#112233')
  expect_match(result$svg_content, '#445566')
})

test_that("concentric puzzle supports custom gradient background", {
  gradient_bg <- list(
    type = "gradient",
    center = "#ffffff",
    middle = "#cccccc",
    edge = "#666666"
  )

  result <- generate_puzzle(
    type = "concentric",
    seed = 42,
    grid = c(2),
    size = c(200),
    fill_color = gradient_bg,
    save_files = FALSE
  )

  expect_match(result$svg_content, 'radialGradient')
  expect_match(result$svg_content, '#ffffff')
  expect_match(result$svg_content, '#cccccc')
  expect_match(result$svg_content, '#666666')
})

# =============================================================================
# SOLID COLOR BACKGROUND TESTS
# =============================================================================

test_that("solid color background works correctly", {
  result <- generate_puzzle(
    type = "rectangular",
    seed = 42,
    grid = c(2, 2),
    size = c(200, 200),
    fill_color = "#abcdef",
    save_files = FALSE
  )

  expect_match(result$svg_content, '#abcdef')
  expect_false(grepl('radialGradient', result$svg_content))
})

test_that("no fill_color produces puzzle without gradient", {
  result <- generate_puzzle(
    type = "rectangular",
    seed = 42,
    grid = c(2, 2),
    size = c(200, 200),
    save_files = FALSE
  )

  # Should produce valid SVG without gradient
  expect_match(result$svg_content, '<svg')
  expect_match(result$svg_content, '</svg>')
})

# =============================================================================
# SEPARATED PUZZLE GRADIENT TESTS
# =============================================================================

test_that("separated rectangular puzzle supports gradient background", {
  gradient_bg <- list(
    type = "gradient",
    center = "#ff0000",
    middle = "#00ff00",
    edge = "#0000ff"
  )

  result <- generate_puzzle(
    type = "rectangular",
    seed = 42,
    grid = c(2, 2),
    size = c(200, 200),
    offset = 10,
    fill_color = gradient_bg,
    save_files = FALSE
  )

  expect_match(result$svg_content, 'radialGradient')
  expect_match(result$svg_content, '#ff0000')
})

test_that("separated hexagonal puzzle supports gradient background", {
  gradient_bg <- list(
    type = "gradient",
    center = "#aabbcc",
    middle = "#ddeeff",
    edge = "#112233"
  )

  result <- generate_puzzle(
    type = "hexagonal",
    seed = 42,
    grid = c(2),
    size = c(200),
    offset = 10,
    fill_color = gradient_bg,
    save_files = FALSE
  )

  expect_match(result$svg_content, 'radialGradient')
})

# =============================================================================
# FILL PALETTE TESTS
# =============================================================================

test_that("fill_palette generates per-piece colors", {
  result <- generate_puzzle(
    type = "rectangular",
    seed = 42,
    grid = c(2, 2),
    size = c(200, 200),
    fill_palette = "viridis",
    save_files = FALSE
  )

  # Should have multiple different fill colors
  expect_match(result$svg_content, 'fill=')
  expect_match(result$svg_content, '</svg>')
})

test_that("fill_palette works with different palettes", {
  for (palette in c("viridis", "magma", "plasma", "inferno")) {
    result <- generate_puzzle(
      type = "rectangular",
      seed = 42,
      grid = c(2, 2),
      size = c(200, 200),
      fill_palette = palette,
      save_files = FALSE
    )

    expect_match(result$svg_content, '<svg',
                 info = sprintf("Palette %s should generate valid SVG", palette))
  }
})
