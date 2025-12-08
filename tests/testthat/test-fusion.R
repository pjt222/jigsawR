# test-fusion.R
# Tests for Issue #40: Meta Pieces (Fusion)

test_that("fusion with generate_puzzle works", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    size = c(200, 200),
    seed = 42,
    fusion_groups = list(c(1, 2)),
    fusion_style = "none",
    save_files = FALSE
  )

  expect_equal(length(result$pieces), 4)
  expect_true(!is.null(result$fusion_data))
  expect_equal(length(result$fusion_data$fused_edges), 2)
})

test_that("fusion string parsing works in generate_puzzle", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    size = c(200, 200),
    seed = 42,
    fusion_groups = "(1,2),(3,4)",
    fusion_style = "none",
    save_files = FALSE
  )

  expect_equal(length(result$parameters$fusion_groups), 2)
  expect_equal(length(result$fusion_data$fused_edges), 4)
})

test_that("fused edges are styled at render time (not path modification)", {
  # With fusion (none mode = hidden fused edges)
  result_fused <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    size = c(200, 200),
    seed = 42,
    fusion_groups = list(c(1, 2)),
    fusion_style = "none",
    save_files = FALSE
  )

  # Without fusion
  result_normal <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    size = c(200, 200),
    seed = 42,
    save_files = FALSE
  )

  # Paths are the same (styling happens at render time, not path generation)
  expect_equal(nchar(result_fused$pieces[[1]]$path), nchar(result_normal$pieces[[1]]$path))

  # Fused pieces have fused_edges metadata
  expect_true(result_fused$pieces[[1]]$fused_edges$E)  # Piece 1 E edge is fused
  expect_true(result_fused$pieces[[2]]$fused_edges$W)  # Piece 2 W edge is fused

  # Non-fused pieces don't have TRUE fused_edges
  expect_false(result_normal$pieces[[1]]$fused_edges$E %||% FALSE)
})

test_that("fused pieces move together when separated", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    size = c(200, 200),
    seed = 42,
    fusion_groups = list(c(1, 2)),
    fusion_style = "none",
    offset = 30,
    save_files = FALSE
  )

  # Extract starting coordinates from paths
  get_path_start <- function(path) {
    parts <- strsplit(path, " ")[[1]]
    list(x = as.numeric(parts[2]), y = as.numeric(parts[3]))
  }

  p1_start <- get_path_start(result$pieces[[1]]$path)
  p2_start <- get_path_start(result$pieces[[2]]$path)

  # Fused pieces should have no gap between them
  # Piece 1 width is 100, so piece 2 should start at x=100
  gap <- p2_start$x - (p1_start$x + 100)
  expect_equal(gap, 0)
})

test_that("non-fused pieces have separation gap", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    size = c(200, 200),
    seed = 42,
    fusion_groups = list(c(1, 2)),
    fusion_style = "none",
    offset = 30,
    save_files = FALSE
  )

  get_path_start <- function(path) {
    parts <- strsplit(path, " ")[[1]]
    list(x = as.numeric(parts[2]), y = as.numeric(parts[3]))
  }

  p1_start <- get_path_start(result$pieces[[1]]$path)
  p3_start <- get_path_start(result$pieces[[3]]$path)

  # Non-fused rows should have gap equal to offset
  row_gap <- p3_start$y - 100  # Piece height is 100
  expect_equal(row_gap, 30)
})

test_that("fusion_style parameter is stored in result", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    size = c(200, 200),
    seed = 42,
    fusion_groups = list(c(1, 2)),
    fusion_style = "dashed",
    save_files = FALSE
  )

  expect_equal(result$parameters$fusion_style, "dashed")
})

test_that("fusion_opacity parameter is stored in result", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    size = c(200, 200),
    seed = 42,
    fusion_groups = list(c(1, 2)),
    fusion_style = "solid",
    fusion_opacity = 0.5,
    save_files = FALSE
  )

  expect_equal(result$parameters$fusion_opacity, 0.5)
})

test_that("larger puzzle fusion works correctly", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    size = c(300, 300),
    seed = 42,
    fusion_groups = "(1,2,3),(5,6)",
    fusion_style = "none",
    offset = 20,
    save_files = FALSE
  )

  expect_equal(length(result$pieces), 9)
  expect_equal(length(result$parameters$fusion_groups), 2)

  # First group should have 3 pieces, second should have 2
  expect_equal(length(result$parameters$fusion_groups[[1]]), 3)
  expect_equal(length(result$parameters$fusion_groups[[2]]), 2)
})

test_that("empty fusion groups work correctly", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    size = c(200, 200),
    seed = 42,
    fusion_groups = NULL,
    save_files = FALSE
  )

  expect_null(result$fusion_data)
  expect_null(result$parameters$fusion_groups)
})

test_that("empty string fusion groups work correctly", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    size = c(200, 200),
    seed = 42,
    fusion_groups = "",
    save_files = FALSE
  )

  expect_null(result$fusion_data)
})
