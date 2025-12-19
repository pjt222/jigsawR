# Tests for theme_puzzle() function

test_that("theme_puzzle returns a valid ggplot2 theme", {
  t <- theme_puzzle()

  expect_s3_class(t, "theme")
})

test_that("theme_puzzle sets explicit plot background", {
  t <- theme_puzzle()

  expect_s3_class(t$plot.background, "element_rect")
  expect_equal(t$plot.background$fill, "white")
  expect_true(is.na(t$plot.background$colour))
})

test_that("theme_puzzle sets plot margin", {
  t <- theme_puzzle()

  # Check margin exists and has expected values
  expect_true(inherits(t$plot.margin, "unit"))
  # Default margin is 2pt on all sides
  expect_equal(as.numeric(t$plot.margin), c(2, 2, 2, 2))
})

test_that("theme_puzzle accepts custom background color", {
  t <- theme_puzzle(background = "grey95")

  expect_equal(t$plot.background$fill, "grey95")
})

test_that("theme_puzzle handles transparent background via NA", {
  t <- theme_puzzle(background = NA)

  expect_equal(t$plot.background$fill, "transparent")
})

test_that("theme_puzzle handles transparent background via string", {
  t <- theme_puzzle(background = "transparent")

  expect_equal(t$plot.background$fill, "transparent")
})

test_that("theme_puzzle accepts custom margin", {
  custom_margin <- ggplot2::margin(10, 10, 10, 10, "pt")
  t <- theme_puzzle(margin = custom_margin)

  expect_equal(as.numeric(t$plot.margin), c(10, 10, 10, 10))
})

test_that("theme_puzzle works with geom_puzzle_rect", {
  p <- ggplot2::ggplot() +
    geom_puzzle_rect(
      ggplot2::aes(fill = ggplot2::after_stat(piece_id)),
      cols = 2, rows = 2, seed = 42
    ) +
    ggplot2::scale_fill_viridis_c(guide = "none") +
    ggplot2::coord_fixed() +
    theme_puzzle()

  expect_s3_class(p, "ggplot")

  # Build the plot to ensure it renders without error
  built <- ggplot2::ggplot_build(p)
  expect_true(nrow(built$data[[1]]) > 0)
})

test_that("theme_puzzle produces non-blank PNG output", {
  skip_on_cran()

  p <- ggplot2::ggplot() +
    geom_puzzle_rect(
      ggplot2::aes(fill = ggplot2::after_stat(piece_id)),
      cols = 2, rows = 2, seed = 42
    ) +
    ggplot2::scale_fill_viridis_c(guide = "none") +
    ggplot2::coord_fixed() +
    theme_puzzle()

  tmp_file <- tempfile(fileext = ".png")
  on.exit(unlink(tmp_file), add = TRUE)

  ggplot2::ggsave(tmp_file, p, width = 4, height = 4, dpi = 72)

  # File should be significantly larger than blank (3170 bytes)
  file_size <- file.info(tmp_file)$size
  expect_gt(file_size, 5000)
})
