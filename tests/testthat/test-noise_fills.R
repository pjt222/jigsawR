# Tests for noise fill functionality

# Skip all tests if ambient package is not available
skip_if_not_installed("ambient")
skip_if_not_installed("png")
skip_if_not_installed("base64enc")

test_that("check_ambient_available returns TRUE when package is available", {
  # The function should not error when ambient is installed
  # Returns TRUE invisibly
  result <- check_ambient_available()
  expect_true(result)
})

test_that("noise_fill_spec creates correct specification", {
  spec <- noise_fill_spec(
    noise_type = "perlin",
    frequency = 0.03,
    color_low = "#000000",
    color_high = "#ffffff",
    seed = 42
  )

  expect_equal(spec$type, "noise")
  expect_equal(spec$noise_type, "perlin")
  expect_equal(spec$frequency, 0.03)
  expect_equal(spec$color_low, "#000000")
  expect_equal(spec$color_high, "#ffffff")
  expect_equal(spec$seed, 42)
})

test_that("noise_fill_spec handles extra parameters", {
  spec <- noise_fill_spec(
    noise_type = "simplex",
    frequency = 0.02,
    octaves = 6,
    lacunarity = 2.5
  )

  expect_equal(spec$type, "noise")
  expect_equal(spec$octaves, 6)
  expect_equal(spec$lacunarity, 2.5)
})

test_that("generate_noise_texture creates valid base64 PNG", {
  noise_png <- generate_noise_texture(
    width = 64,
    height = 64,
    noise_type = "perlin",
    seed = 42
  )

  expect_type(noise_png, "character")
  expect_true(startsWith(noise_png, "data:image/png;base64,"))
  expect_gt(nchar(noise_png), 100)  # Should have substantial content
})

test_that("generate_noise_texture produces reproducible results with seed", {
  noise1 <- generate_noise_texture(width = 32, height = 32, seed = 123)
  noise2 <- generate_noise_texture(width = 32, height = 32, seed = 123)
  noise3 <- generate_noise_texture(width = 32, height = 32, seed = 456)

  expect_equal(noise1, noise2)  # Same seed = same result
  expect_false(noise1 == noise3)  # Different seed = different result
})

test_that("generate_noise_texture supports all noise types", {
  noise_types <- c("perlin", "simplex", "worley", "cubic", "value", "white")

  for (nt in noise_types) {
    result <- generate_noise_texture(
      width = 32,
      height = 32,
      noise_type = nt,
      seed = 42
    )
    expect_true(startsWith(result, "data:image/png;base64,"),
                info = paste("Failed for noise type:", nt))
  }
})

test_that("generate_noise_texture respects color parameters", {
  # Just verify it runs without error with custom colors
  result <- generate_noise_texture(
    width = 32,
    height = 32,
    color_low = "#ff0000",
    color_high = "#0000ff",
    seed = 42
  )
  expect_true(startsWith(result, "data:image/png;base64,"))

  # With mid color
  result_mid <- generate_noise_texture(
    width = 32,
    height = 32,
    color_low = "#000000",
    color_mid = "#888888",
    color_high = "#ffffff",
    seed = 42
  )
  expect_true(startsWith(result_mid, "data:image/png;base64,"))
})

test_that("create_noise_pattern_defs generates valid SVG pattern", {
  noise_png <- generate_noise_texture(width = 64, height = 64, seed = 42)
  pattern_defs <- create_noise_pattern_defs(noise_png, pattern_id = "testPattern")

  expect_type(pattern_defs, "character")
  expect_match(pattern_defs, "<pattern")
  expect_match(pattern_defs, 'id="testPattern"')
  expect_match(pattern_defs, "<image")
  expect_match(pattern_defs, "</pattern>")
})

test_that("render_noise_background creates valid SVG with pattern", {
  noise_spec <- noise_fill_spec(
    noise_type = "perlin",
    frequency = 0.02,
    color_low = "#1a1a2e",
    color_high = "#4a4a6e",
    seed = 42
  )

  svg_output <- render_noise_background(
    noise_spec,
    canvas_size = c(200, 200),
    canvas_offset = NULL
  )

  expect_type(svg_output, "character")
  expect_match(svg_output, "<defs>")
  expect_match(svg_output, 'id="bgNoisePattern"')
  expect_match(svg_output, "<rect")
  expect_match(svg_output, 'fill="url\\(#bgNoisePattern\\)"')
})

test_that("render_noise_piece_fill_defs creates valid pattern", {
  noise_spec <- noise_fill_spec(
    noise_type = "simplex",
    frequency = 0.03,
    seed = 123
  )

  defs_output <- render_noise_piece_fill_defs(noise_spec)

  expect_type(defs_output, "character")
  expect_match(defs_output, "<defs>")
  expect_match(defs_output, 'id="pieceFillNoisePattern"')
  expect_match(defs_output, "</defs>")
})

test_that("generate_puzzle supports noise background", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    size = c(100, 100),
    seed = 42,
    background = noise_fill_spec(
      noise_type = "perlin",
      frequency = 0.02,
      color_low = "#1a1a2e",
      color_high = "#4a4a6e",
      seed = 100
    ),
    save_files = FALSE
  )

  expect_match(result$svg_content, "bgNoisePattern")
  expect_match(result$svg_content, "<pattern")
})

test_that("generate_puzzle supports noise piece fill", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    size = c(100, 100),
    seed = 42,
    fill_color = noise_fill_spec(
      noise_type = "worley",
      frequency = 0.04,
      seed = 200
    ),
    save_files = FALSE
  )

  expect_match(result$svg_content, "pieceFillNoisePattern")
})

test_that("noise fills work with hexagonal puzzles", {
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(2),
    size = c(100),
    seed = 42,
    background = noise_fill_spec(noise_type = "simplex", seed = 50),
    save_files = FALSE
  )

  expect_match(result$svg_content, "bgNoisePattern")
})

test_that("noise fills work with concentric puzzles", {
  result <- generate_puzzle(
    type = "concentric",
    grid = c(2),
    size = c(100),
    seed = 42,
    fill_color = noise_fill_spec(noise_type = "cubic", seed = 75),
    save_files = FALSE
  )

  expect_match(result$svg_content, "pieceFillNoisePattern")
})

test_that("is_noise_fill_spec correctly identifies noise specs", {
  noise_spec <- noise_fill_spec(noise_type = "perlin")
  expect_true(is_noise_fill_spec(noise_spec))

  # Not a noise spec
  expect_false(is_noise_fill_spec("red"))
  expect_false(is_noise_fill_spec(NULL))
  expect_false(is_noise_fill_spec(list(type = "gradient")))
  expect_false(is_noise_fill_spec(list(color = "blue")))
})
