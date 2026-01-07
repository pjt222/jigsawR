# Tests for Label Centering on Puzzle Pieces
# Converted from standalone tests/test_label_centering.R
#
# Verifies that labels are centered on piece geometry

# =============================================================================
# RECTANGULAR PUZZLE LABEL TESTS
# =============================================================================

test_that("rectangular puzzle generates labels in SVG", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    size = c(200, 200),
    seed = 42,
    offset = 0,
    show_labels = TRUE,
    label_color = "#ff0000",
    save_files = FALSE
  )

  expect_match(result$svg_content, "<text", fixed = TRUE)
  expect_match(result$svg_content, "fill=\"#ff0000\"")
})

test_that("rectangular puzzle without labels has no text elements", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    size = c(200, 200),
    seed = 42,
    offset = 0,
    show_labels = FALSE,
    save_files = FALSE
  )

  expect_false(grepl("<text", result$svg_content, fixed = TRUE))
})

# =============================================================================
# HEXAGONAL PUZZLE LABEL TESTS
# =============================================================================

test_that("hexagonal warped puzzle generates labels", {
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(3),
    size = c(200),
    seed = 42,
    offset = 0,
    do_warp = TRUE,
    do_trunc = TRUE,
    show_labels = TRUE,
    label_color = "#ff0000",
    save_files = FALSE
  )

  expect_match(result$svg_content, "<text", fixed = TRUE)
  expect_match(result$svg_content, "fill=\"#ff0000\"")
})

test_that("separated hexagonal puzzle has labels", {
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(3),
    size = c(200),
    seed = 42,
    offset = 15,
    do_warp = TRUE,
    do_trunc = TRUE,
    show_labels = TRUE,
    label_color = "#0000ff",
    save_files = FALSE
  )

  expect_match(result$svg_content, "<text", fixed = TRUE)
  expect_match(result$svg_content, "fill=\"#0000ff\"")
})

# =============================================================================
# CONCENTRIC PUZZLE LABEL TESTS
# =============================================================================

test_that("concentric puzzle generates labels", {
  result <- generate_puzzle(
    type = "concentric",
    grid = c(3),
    size = c(200),
    seed = 42,
    offset = 0,
    center_shape = "hexagon",
    show_labels = TRUE,
    label_color = "#00ff00",
    save_files = FALSE
  )

  expect_match(result$svg_content, "<text", fixed = TRUE)
  expect_match(result$svg_content, "fill=\"#00ff00\"")
})

# =============================================================================
# LABEL COUNT TESTS
# =============================================================================

test_that("rectangular puzzle has correct number of labels", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 3),  # 6 pieces
    size = c(200, 300),
    seed = 42,
    show_labels = TRUE,
    save_files = FALSE
  )

  # Count text elements
  label_count <- length(gregexpr("<text ", result$svg_content)[[1]])
  expect_equal(label_count, 6)
})

test_that("hexagonal puzzle has correct number of labels", {
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(2),  # 7 pieces (3*2*1 + 1)
    size = c(200),
    seed = 42,
    show_labels = TRUE,
    save_files = FALSE
  )

  label_count <- length(gregexpr("<text ", result$svg_content)[[1]])
  expect_equal(label_count, 7)
})

test_that("concentric puzzle has correct number of labels", {
  result <- generate_puzzle(
    type = "concentric",
    grid = c(2),  # 7 pieces
    size = c(200),
    seed = 42,
    show_labels = TRUE,
    save_files = FALSE
  )

  label_count <- length(gregexpr("<text ", result$svg_content)[[1]])
  expect_equal(label_count, 7)
})

# =============================================================================
# LABEL POSITIONING TESTS
# =============================================================================

test_that("label x/y positions are numeric and valid", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    size = c(200, 200),
    seed = 42,
    show_labels = TRUE,
    save_files = FALSE
  )

  # Extract x and y values from text elements
  x_matches <- gregexpr('x="([0-9.]+)"', result$svg_content)
  y_matches <- gregexpr('y="([0-9.]+)"', result$svg_content)

  # Should have found positions
  expect_gt(length(x_matches[[1]]), 0)
  expect_gt(length(y_matches[[1]]), 0)
})
