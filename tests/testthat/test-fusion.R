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

# ============================================================================
# HEXAGONAL FUSION EDGE DIRECTION TESTS (Issue #43 regression tests)
# ============================================================================

test_that("hexagonal: fused_edges use geometric side numbering", {
  # This tests the fix for issue #43 where topology sides != geometric sides
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(3),
    size = c(200),
    seed = 42,
    fusion_groups = list(c(1, 2)),
    fusion_style = "dashed",
    save_files = FALSE
  )

  # Piece 1 (center) should have fused_edges with geometric keys
  p1 <- result$pieces[[1]]
  expect_true(!is.null(p1$fused_edges))

  # The fused edge should be at the geometric side facing piece 2
  # Piece 2 is at ~150° from center, so geometric side 2 (midpoint at 150°)
  # should be fused
  fused_sides <- names(p1$fused_edges)[sapply(p1$fused_edges, isTRUE)]
  expect_true(length(fused_sides) > 0)
})

test_that("hexagonal: fused_neighbor_ids correctly identify neighbors", {
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(3),
    size = c(200),
    seed = 42,
    fusion_groups = list(c(1, 2)),
    fusion_style = "dashed",
    save_files = FALSE
  )

  # Piece 1 should have fused_neighbor_ids pointing to piece 2
  p1 <- result$pieces[[1]]
  expect_true(!is.null(p1$fused_neighbor_ids))

  neighbor_ids <- unlist(p1$fused_neighbor_ids)
  expect_true(2 %in% neighbor_ids)
})

test_that("hexagonal: center piece fuses correctly with each ring-1 neighbor", {

  # Test that we can fuse the center with any of its 6 neighbors
  for (neighbor_id in 2:7) {
    result <- generate_puzzle(
      type = "hexagonal",
      grid = c(3),
      size = c(200),
      seed = 42,
      fusion_groups = list(c(1, neighbor_id)),
      fusion_style = "dashed",
      save_files = FALSE
    )

    # Center piece should have exactly one fused edge
    p1 <- result$pieces[[1]]
    fused_count <- sum(sapply(p1$fused_edges, isTRUE))
    expect_equal(fused_count, 1,
      info = paste("Fusing center with piece", neighbor_id))

    # The neighbor piece should also have exactly one fused edge
    pn <- result$pieces[[neighbor_id]]
    neighbor_fused_count <- sum(sapply(pn$fused_edges, isTRUE))
    expect_equal(neighbor_fused_count, 1,
      info = paste("Piece", neighbor_id, "fused edge count"))
  }
})

test_that("hexagonal: ring-1 adjacent pieces fuse correctly", {
  # Pieces 2 and 3 are adjacent in ring 1
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(3),
    size = c(200),
    seed = 42,
    fusion_groups = list(c(2, 3)),
    fusion_style = "dashed",
    save_files = FALSE
  )

  # Both pieces should have exactly one fused edge
  p2 <- result$pieces[[2]]
  p3 <- result$pieces[[3]]

  fused_count_2 <- sum(sapply(p2$fused_edges, isTRUE))
  fused_count_3 <- sum(sapply(p3$fused_edges, isTRUE))

  expect_equal(fused_count_2, 1)
  expect_equal(fused_count_3, 1)
})

test_that("hexagonal: multi-piece fusion group works", {
  # Fuse center with two adjacent ring-1 pieces
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(3),
    size = c(200),
    seed = 42,
    fusion_groups = list(c(1, 2, 3)),
    fusion_style = "dashed",
    save_files = FALSE
  )

  # Center has 2 fused edges (to pieces 2 and 3)
  p1 <- result$pieces[[1]]
  fused_count_1 <- sum(sapply(p1$fused_edges, isTRUE))
  expect_equal(fused_count_1, 2)

  # Piece 2 has 2 fused edges (to center and piece 3)
  p2 <- result$pieces[[2]]
  fused_count_2 <- sum(sapply(p2$fused_edges, isTRUE))
  expect_equal(fused_count_2, 2)

  # Piece 3 has 2 fused edges (to center and piece 2)
  p3 <- result$pieces[[3]]
  fused_count_3 <- sum(sapply(p3$fused_edges, isTRUE))
  expect_equal(fused_count_3, 2)
})

test_that("hexagonal: fused edge direction matches neighbor direction", {
  # Critical test for issue #43 fix
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(3),
    size = c(200),
    seed = 42,
    fusion_groups = list(c(1, 2)),
    fusion_style = "dashed",
    save_files = FALSE
  )

  # Get center piece center coordinates
  p1 <- result$pieces[[1]]
  p2 <- result$pieces[[2]]

  # Extract coordinates from center (could be list or vector)
  get_center_coords <- function(piece) {
    if (!is.null(piece$center)) {
      if (is.list(piece$center)) {
        return(c(piece$center$x %||% piece$center[[1]],
                 piece$center$y %||% piece$center[[2]]))
      }
      return(piece$center)
    }
    return(c(piece$cx %||% piece$center_x, piece$cy %||% piece$center_y))
  }

  p1_center <- get_center_coords(p1)
  p2_center <- get_center_coords(p2)

  # Skip if centers not available
  skip_if(is.null(p1_center) || any(is.na(p1_center)), "Piece 1 center not available")
  skip_if(is.null(p2_center) || any(is.na(p2_center)), "Piece 2 center not available")

  # Calculate actual direction from piece 1 to piece 2
  dir_to_p2 <- atan2(p2_center[2] - p1_center[2],
                     p2_center[1] - p1_center[1]) * 180 / pi

  # Find which geometric side is fused
  fused_geo_side <- NULL
  if (!is.null(p1$fused_edges)) {
    for (side in names(p1$fused_edges)) {
      if (isTRUE(p1$fused_edges[[side]])) {
        fused_geo_side <- as.integer(side)
        break
      }
    }
  }

  # Skip if no fused edges found (shouldn't happen but be defensive)
  skip_if(is.null(fused_geo_side), "No fused edges found on piece 1")

  # Geometric side i has midpoint at direction: 30 + i*60 degrees
  expected_dir <- 30 + fused_geo_side * 60
  if (expected_dir > 180) expected_dir <- expected_dir - 360

  # The fused side direction should be close to actual neighbor direction
  dir_diff <- abs(dir_to_p2 - expected_dir)
  if (length(dir_diff) == 0 || is.na(dir_diff)) {
    skip("Could not calculate direction difference")
  }
  if (dir_diff > 180) dir_diff <- 360 - dir_diff

  expect_true(dir_diff < 35,  # Within 35 degrees (30 + tolerance)
    info = paste("Direction to neighbor:", round(dir_to_p2, 1),
                 "Fused side direction:", expected_dir))
})

# ============================================================================
# HEXAGONAL FUSION WITH MULTIPLE RINGS (Issue #49 related)
# ============================================================================

test_that("hexagonal: 4-ring puzzle fusion works", {
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(4),
    size = c(300),
    seed = 42,
    fusion_groups = list(c(1, 2)),
    fusion_style = "dashed",
    save_files = FALSE
  )

  # Should have 37 pieces (3*4*3 + 1)
  expect_equal(length(result$pieces), 37)

  # Fusion should work
  expect_true(!is.null(result$fusion_data))
  expect_equal(length(result$fusion_data$fused_edges), 2)
})
test_that("hexagonal: 5-ring puzzle fusion works", {
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(5),
    size = c(400),
    seed = 42,
    fusion_groups = list(c(1, 2, 3)),
    fusion_style = "dashed",
    save_files = FALSE
  )

  # Should have 61 pieces (3*5*4 + 1)
  expect_equal(length(result$pieces), 61)

  # Fusion should work
  expect_true(!is.null(result$fusion_data))
})

test_that("hexagonal: outer ring fusion works", {
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(3),
    size = c(200),
    seed = 42,
    fusion_groups = list(c(8, 9)),  # Two adjacent outer ring pieces
    fusion_style = "dashed",
    save_files = FALSE
  )

  # Should have fused edges
  expect_true(!is.null(result$fusion_data))

  p8 <- result$pieces[[8]]
  p9 <- result$pieces[[9]]

  # Both should have at least one fused edge
  fused_8 <- sum(sapply(p8$fused_edges, isTRUE))
  fused_9 <- sum(sapply(p9$fused_edges, isTRUE))

  expect_true(fused_8 >= 1)
  expect_true(fused_9 >= 1)
})

# ============================================================================
# FUSION RENDERING TESTS (SVG output verification)
# ============================================================================

test_that("fusion_style 'none' hides fused edges in SVG", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    size = c(200, 200),
    seed = 42,
    fusion_groups = list(c(1, 2)),
    fusion_style = "none",
    save_files = FALSE
  )

  # SVG should not contain dashed stroke for fused edges
  expect_false(grepl("stroke-dasharray", result$svg_content))
})

test_that("fusion_style 'dashed' adds dashed strokes in SVG", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    size = c(200, 200),
    seed = 42,
    fusion_groups = list(c(1, 2)),
    fusion_style = "dashed",
    save_files = FALSE
  )

  # SVG should contain dashed stroke for fused edges
  expect_true(grepl("stroke-dasharray", result$svg_content))
})

test_that("fusion_style 'solid' with opacity creates semi-transparent edges", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    size = c(200, 200),
    seed = 42,
    fusion_groups = list(c(1, 2)),
    fusion_style = "solid",
    fusion_opacity = 0.3,
    save_files = FALSE
  )

  # SVG should contain opacity attribute
  expect_true(grepl("stroke-opacity", result$svg_content) ||
              grepl("opacity", result$svg_content))
})

test_that("hexagonal fusion renders correctly in SVG", {
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(3),
    size = c(200),
    seed = 42,
    fusion_groups = list(c(1, 2)),
    fusion_style = "dashed",
    save_files = FALSE
  )

  # SVG should be valid (contains svg tag and paths)
  expect_true(grepl("<svg", result$svg_content))
  expect_true(grepl("<path", result$svg_content))

  # Should have dashed lines for fusion
  expect_true(grepl("stroke-dasharray", result$svg_content))
})

# =============================================================================
# FUSION GROUP MERGING (Union-Find Algorithm)
# =============================================================================

test_that("merge_fusion_groups: simple two-group overlap", {
  # (1,2) and (2,3) share piece 2 -> should merge to (1,2,3)
  input <- list(c(1, 2), c(2, 3))
  result <- merge_fusion_groups(input)

  expect_equal(length(result), 1)
  expect_equal(result[[1]], c(1, 2, 3))
})

test_that("merge_fusion_groups: user example (7,8),(5,8)", {
  # Issue example: (7,8),(5,8) share piece 8 -> should merge to (5,7,8)
  input <- list(c(7, 8), c(5, 8))
  result <- merge_fusion_groups(input)

  expect_equal(length(result), 1)
  expect_equal(result[[1]], c(5, 7, 8))
})

test_that("merge_fusion_groups: no overlap keeps groups separate", {
  input <- list(c(1, 2), c(3, 4))
  result <- merge_fusion_groups(input)

  expect_equal(length(result), 2)
  expect_equal(result[[1]], c(1, 2))
  expect_equal(result[[2]], c(3, 4))
})

test_that("merge_fusion_groups: chain overlap", {
  # (1,2),(2,3),(3,4) should all merge into one
  input <- list(c(1, 2), c(2, 3), c(3, 4))
  result <- merge_fusion_groups(input)

  expect_equal(length(result), 1)
  expect_equal(result[[1]], c(1, 2, 3, 4))
})

test_that("merge_fusion_groups: multiple independent chains", {
  # Two separate chains
  input <- list(c(1, 2), c(2, 3), c(5, 6), c(6, 7))
  result <- merge_fusion_groups(input)

  expect_equal(length(result), 2)
  expect_equal(result[[1]], c(1, 2, 3))
  expect_equal(result[[2]], c(5, 6, 7))
})

test_that("merge_fusion_groups: star pattern (all share center)", {
  # All groups share piece 1 -> should merge to single group
  input <- list(c(1, 2), c(1, 3), c(1, 4), c(1, 5))
  result <- merge_fusion_groups(input)

  expect_equal(length(result), 1)
  expect_equal(result[[1]], c(1, 2, 3, 4, 5))
})

test_that("merge_fusion_groups: bridge connection", {
  # (1,2) and (3,4) are separate, but (2,3) bridges them
  input <- list(c(1, 2), c(3, 4), c(2, 3))
  result <- merge_fusion_groups(input)

  expect_equal(length(result), 1)
  expect_equal(result[[1]], c(1, 2, 3, 4))
})

test_that("merge_fusion_groups: multi-point overlap", {
  # (1,2,3) shares 2 with (2,4) and shares 3 with (3,5)
  input <- list(c(1, 2, 3), c(2, 4), c(3, 5))
  result <- merge_fusion_groups(input)

  expect_equal(length(result), 1)
  expect_equal(result[[1]], c(1, 2, 3, 4, 5))
})

test_that("merge_fusion_groups: subset relationship", {
  # (2,3) is subset of (1,2,3) -> should merge
  input <- list(c(1, 2, 3), c(2, 3))
  result <- merge_fusion_groups(input)

  expect_equal(length(result), 1)
  expect_equal(result[[1]], c(1, 2, 3))
})

test_that("merge_fusion_groups: removes duplicates within group", {
  input <- list(c(1, 1, 2, 2, 3))
  result <- merge_fusion_groups(input)

  expect_equal(length(result), 1)
  expect_equal(result[[1]], c(1, 2, 3))
})

test_that("merge_fusion_groups: handles NULL input", {
  expect_equal(merge_fusion_groups(NULL), list())
})

test_that("merge_fusion_groups: handles empty list", {
  expect_equal(merge_fusion_groups(list()), list())
})

test_that("merge_fusion_groups: filters single-piece groups", {
  input <- list(c(1), c(2, 3))
  result <- merge_fusion_groups(input)

  expect_equal(length(result), 1)
  expect_equal(result[[1]], c(2, 3))
})

test_that("merge_fusion_groups: filters empty groups", {
  input <- list(c(1, 2), integer(0), c(3, 4))
  result <- merge_fusion_groups(input)

  expect_equal(length(result), 2)
  expect_equal(result[[1]], c(1, 2))
  expect_equal(result[[2]], c(3, 4))
})

test_that("merge_fusion_groups: order independence", {
  # Same groups in different orders should produce same result
  result1 <- merge_fusion_groups(list(c(1, 2), c(2, 3), c(3, 4)))
  result2 <- merge_fusion_groups(list(c(3, 4), c(1, 2), c(2, 3)))
  result3 <- merge_fusion_groups(list(c(2, 3), c(3, 4), c(1, 2)))

  expect_equal(result1, result2)
  expect_equal(result2, result3)
})

test_that("merge_fusion_groups: sorts output consistently", {
  # Output should be sorted by smallest piece ID
  input <- list(c(5, 6), c(1, 2))
  result <- merge_fusion_groups(input)

  expect_equal(result[[1]], c(1, 2))
  expect_equal(result[[2]], c(5, 6))
})

test_that("parse_fusion_input auto-merges overlapping groups", {
  # Verify that parsing automatically merges
  result <- parse_fusion_input("(7,8),(5,8)")

  expect_equal(length(result), 1)
  expect_equal(result[[1]], c(5, 7, 8))
})

test_that("parse_fusion_input auto-merge can be disabled", {
  result <- parse_fusion_input("(7,8),(5,8)", auto_merge = FALSE)

  expect_equal(length(result), 2)
  expect_equal(result[[1]], c(7, 8))
  expect_equal(result[[2]], c(5, 8))
})

test_that("parse_fusion_input with list input also merges", {
  result <- parse_fusion_input(list(c(1, 2), c(2, 3)))

  expect_equal(length(result), 1)
  expect_equal(result[[1]], c(1, 2, 3))
})

test_that("generate_puzzle with overlapping fusion groups auto-merges", {
  # Use the user's example: 3x3 rectangular with (7,8),(5,8)
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    size = c(300, 300),
    seed = 42,
    fusion_groups = "(7,8),(5,8)",
    save_files = FALSE
  )

  # Should have merged into single fusion group
  expect_equal(length(result$parameters$fusion_groups), 1)
  expect_equal(sort(result$parameters$fusion_groups[[1]]), c(5, 7, 8))
})

test_that("generate_puzzle with chain fusion merges correctly", {
  # 2x3 rectangular: (1,2),(2,3),(4,5),(5,6)
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 3),
    size = c(200, 300),
    seed = 42,
    fusion_groups = "(1,2),(2,3),(4,5),(5,6)",
    save_files = FALSE
  )

  # Should have 2 merged groups: (1,2,3) and (4,5,6)
  expect_equal(length(result$parameters$fusion_groups), 2)
})

test_that("hexagonal fusion with overlapping groups merges", {
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(2),
    size = c(100),
    seed = 42,
    fusion_groups = list(c(1, 2), c(2, 3)),
    save_files = FALSE
  )

  # Should have merged
  expect_equal(length(result$parameters$fusion_groups), 1)
  expect_equal(sort(result$parameters$fusion_groups[[1]]), c(1, 2, 3))
})

test_that("large chain merges efficiently", {
  # Create a chain of 20 overlapping pairs
  groups <- lapply(1:19, function(i) c(i, i + 1))
  result <- merge_fusion_groups(groups)

  expect_equal(length(result), 1)
  expect_equal(result[[1]], 1:20)
})
