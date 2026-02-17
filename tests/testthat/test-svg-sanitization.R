# Tests for SVG attribute sanitization (R/unified_renderer.R)

# =============================================================================
# 1. Normal values pass through
# =============================================================================

test_that("hex colors pass through unchanged", {
  expect_equal(sanitize_svg_attr("#FF0000"), "#FF0000")
  expect_equal(sanitize_svg_attr("#00ff00"), "#00ff00")
  expect_equal(sanitize_svg_attr("#123456"), "#123456")
  expect_equal(sanitize_svg_attr("#abc"), "#abc")
})

test_that("named colors pass through unchanged", {
  expect_equal(sanitize_svg_attr("red"), "red")
  expect_equal(sanitize_svg_attr("blue"), "blue")
  expect_equal(sanitize_svg_attr("none"), "none")
  expect_equal(sanitize_svg_attr("black"), "black")
  expect_equal(sanitize_svg_attr("transparent"), "transparent")
})

test_that("CSS functions pass through", {
  expect_equal(sanitize_svg_attr("url(#gradient1)"), "url(#gradient1)")
  expect_equal(sanitize_svg_attr("rgb(255, 0, 0)"), "rgb(255, 0, 0)")
})

test_that("numeric values pass through", {
  expect_equal(sanitize_svg_attr("1.5"), "1.5")
  expect_equal(sanitize_svg_attr("100%"), "100%")
  expect_equal(sanitize_svg_attr("0.75"), "0.75")
})

# =============================================================================
# 2. XSS / injection attempts are stripped
# =============================================================================

test_that("script injection is stripped", {
  result <- sanitize_svg_attr('red" onmouseover="alert(1)')
  expect_false(grepl('"', result))
  expect_false(grepl("=", result))
  expect_true(grepl("red", result))
})

test_that("angle brackets are stripped", {
  result <- sanitize_svg_attr("<script>alert(1)</script>")
  expect_false(grepl("<", result))
  expect_false(grepl(">", result))
})

test_that("single quotes are stripped", {
  result <- sanitize_svg_attr("red' onclick='alert(1)")
  expect_false(grepl("'", result))
})

test_that("semicolons are stripped", {
  result <- sanitize_svg_attr("red; background: url(evil)")
  expect_false(grepl(";", result))
})

# =============================================================================
# 3. NULL / NA handling
# =============================================================================

test_that("NULL returns 'none'", {
  expect_equal(sanitize_svg_attr(NULL), "none")
})

test_that("NA returns 'none'", {
  expect_equal(sanitize_svg_attr(NA), "none")
  expect_equal(sanitize_svg_attr(NA_character_), "none")
})

# =============================================================================
# 4. Non-character coercion
# =============================================================================

test_that("numeric input is coerced to character", {
  expect_equal(sanitize_svg_attr(42), "42")
  expect_equal(sanitize_svg_attr(1.5), "1.5")
})
