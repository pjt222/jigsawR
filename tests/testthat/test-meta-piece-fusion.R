# Tests for Meta-Piece (Fused Piece) Positioning
# Converted from standalone tests/test_meta_piece_positioning.R
#
# Verifies that fused pieces move together when offset > 0

# Helper to extract path start point from SVG path
get_path_start <- function(path) {
  segments <- parse_svg_path(path)
  if (length(segments) > 0 && segments[[1]]$type == "M") {
    return(c(x = segments[[1]]$x, y = segments[[1]]$y))
  }
  return(c(x = NA, y = NA))
}

# =============================================================================
# RECTANGULAR META-PIECE TESTS
# =============================================================================

test_that("rectangular fused pieces maintain relative position when separated", {
  # Generate with no offset
  result_compact <- generate_puzzle(
    type = "rectangular",
    seed = 42,
    grid = c(2, 2),
    size = c(200, 200),
    offset = 0,
    fusion_groups = list(c(1, 2)),  # Fuse pieces 1 and 2
    save_files = FALSE
  )

  # Generate with offset
  result_separated <- generate_puzzle(
    type = "rectangular",
    seed = 42,
    grid = c(2, 2),
    size = c(200, 200),
    offset = 30,
    fusion_groups = list(c(1, 2)),
    save_files = FALSE
  )

  # Get path starts for pieces 1 and 2
  p1_start_compact <- get_path_start(result_compact$pieces[[1]]$path)
  p2_start_compact <- get_path_start(result_compact$pieces[[2]]$path)
  p1_start_sep <- get_path_start(result_separated$pieces[[1]]$path)
  p2_start_sep <- get_path_start(result_separated$pieces[[2]]$path)

  # Calculate relative positions
  rel_compact <- p2_start_compact - p1_start_compact
  rel_sep <- p2_start_sep - p1_start_sep

  # For fused pieces, relative position should be the same
  expect_equal(rel_compact["x"], rel_sep["x"], tolerance = 1)
  expect_equal(rel_compact["y"], rel_sep["y"], tolerance = 1)
})

test_that("non-fused rectangular pieces do separate", {
  result_compact <- generate_puzzle(
    type = "rectangular",
    seed = 42,
    grid = c(2, 2),
    size = c(200, 200),
    offset = 0,
    fusion_groups = list(c(1, 2)),  # Only fuse 1 and 2
    save_files = FALSE
  )

  result_separated <- generate_puzzle(
    type = "rectangular",
    seed = 42,
    grid = c(2, 2),
    size = c(200, 200),
    offset = 30,
    fusion_groups = list(c(1, 2)),
    save_files = FALSE
  )

  # Get positions for pieces 1 and 3 (not fused together)
  p1_start_compact <- get_path_start(result_compact$pieces[[1]]$path)
  p3_start_compact <- get_path_start(result_compact$pieces[[3]]$path)
  p1_start_sep <- get_path_start(result_separated$pieces[[1]]$path)
  p3_start_sep <- get_path_start(result_separated$pieces[[3]]$path)

  rel_compact <- p3_start_compact - p1_start_compact
  rel_sep <- p3_start_sep - p1_start_sep

  # Non-fused pieces should have different relative positions (they separate)
  expect_gt(abs(rel_compact["y"] - rel_sep["y"]), 1)
})

# =============================================================================
# HEXAGONAL META-PIECE TESTS
# =============================================================================

test_that("hexagonal fused pieces maintain relative position when separated", {
  result_compact <- generate_puzzle(
    type = "hexagonal",
    seed = 42,
    grid = c(2),
    size = c(200),
    offset = 0,
    fusion_groups = list(c(1, 2)),
    save_files = FALSE
  )

  result_separated <- generate_puzzle(
    type = "hexagonal",
    seed = 42,
    grid = c(2),
    size = c(200),
    offset = 20,
    fusion_groups = list(c(1, 2)),
    save_files = FALSE
  )

  # Use centers for hexagonal pieces
  c1_compact <- result_compact$pieces[[1]]$center
  c2_compact <- result_compact$pieces[[2]]$center
  c1_sep <- result_separated$pieces[[1]]$center
  c2_sep <- result_separated$pieces[[2]]$center

  rel_compact <- c2_compact - c1_compact
  rel_sep <- c2_sep - c1_sep

  # Fused pieces should maintain relative position
  expect_equal(rel_compact[1], rel_sep[1], tolerance = 1)
  expect_equal(rel_compact[2], rel_sep[2], tolerance = 1)
})

# =============================================================================
# CONCENTRIC META-PIECE TESTS
# =============================================================================

test_that("concentric fused pieces maintain relative position when separated", {
  result_compact <- generate_puzzle(
    type = "concentric",
    seed = 42,
    grid = c(2),
    size = c(200),
    offset = 0,
    fusion_groups = list(c(1, 2)),
    save_files = FALSE
  )

  result_separated <- generate_puzzle(
    type = "concentric",
    seed = 42,
    grid = c(2),
    size = c(200),
    offset = 20,
    fusion_groups = list(c(1, 2)),
    save_files = FALSE
  )

  c1_compact <- result_compact$pieces[[1]]$center
  c2_compact <- result_compact$pieces[[2]]$center
  c1_sep <- result_separated$pieces[[1]]$center
  c2_sep <- result_separated$pieces[[2]]$center

  rel_compact <- c2_compact - c1_compact
  rel_sep <- c2_sep - c1_sep

  expect_equal(rel_compact[1], rel_sep[1], tolerance = 1)
  expect_equal(rel_compact[2], rel_sep[2], tolerance = 1)
})

# =============================================================================
# FUSION METADATA PRESERVATION TESTS
# =============================================================================

test_that("fusion metadata is preserved after positioning for all types", {
  for (ptype in c("rectangular", "hexagonal", "concentric")) {
    grid <- if (ptype == "rectangular") c(2, 2) else c(2)
    result <- generate_puzzle(
      type = ptype,
      seed = 42,
      grid = grid,
      size = c(200, 200),
      offset = 20,
      fusion_groups = list(c(1, 2)),
      fusion_style = "dashed",
      fusion_opacity = 0.5,
      save_files = FALSE
    )

    # Check that fused_edges metadata is preserved
    expect_true(!is.null(result$pieces[[1]]$fused_edges))
    expect_true(!is.null(result$pieces[[2]]$fused_edges))

    # Check SVG is generated
    expect_gt(nchar(result$svg_content), 100)
  }
})
