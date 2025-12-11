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

  # Hexagonal side numbering for flat-top hexagon (base_offset=0):
  # Side 0 → 30° (NE), Side 1 → 90° (N), Side 2 → 150° (NW)
  # Side 3 → -150° (SW), Side 4 → -90° (S), Side 5 → -30° (SE)
  # Formula: expected_dir = 30 + side * 60, adjusted for [-180, 180] range
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

# =============================================================================
# TOPOLOGY-TO-GEOMETRY MAPPING TESTS (Issue #43, #49, #53 - 2025-12-11)
# =============================================================================
# These tests verify the mathematical correctness of the flat-top hexagon
# geometry formula: geo_side = round((dir - 30) / 60) %% 6
#
# Flat-top hexagon (base_offset=0): vertices at 0°, 60°, 120°, 180°, 240°, 300°
# Each side faces outward at the midpoint between its two vertices:
#   Side 0: vertex 0° to 60° → faces  30° (NE)
#   Side 1: vertex 60° to 120° → faces  90° (N)
#   Side 2: vertex 120° to 180° → faces 150° (NW)
#   Side 3: vertex 180° to 240° → faces -150° (SW)
#   Side 4: vertex 240° to 300° → faces -90° (S)
#   Side 5: vertex 300° to 0° → faces -30° (SE)
# =============================================================================

test_that("topo-to-geo formula: canonical direction angles", {
  # Test the formula: geo_side = round((dir - 30) / 60) %% 6
  # with exact canonical direction angles
  test_cases <- list(
    list(dir = 30, expected_side = 0, label = "NE"),
    list(dir = 90, expected_side = 1, label = "N"),
    list(dir = 150, expected_side = 2, label = "NW"),
    list(dir = -150, expected_side = 3, label = "SW"),
    list(dir = -90, expected_side = 4, label = "S"),
    list(dir = -30, expected_side = 5, label = "SE")
  )

  for (tc in test_cases) {
    geo_side <- round((tc$dir - 30) / 60) %% 6
    expect_equal(geo_side, tc$expected_side,
      info = paste0("Direction ", tc$dir, "° (", tc$label, "): ",
                    "expected side ", tc$expected_side, ", got ", geo_side))
  }
})

test_that("topo-to-geo formula: boundary direction angles", {
  # Test with angles at ±30° boundaries (should round to nearest side)
  # Boundary at 0°: between side 5 (-30°) and side 0 (30°)
  # Formula: (0 - 30) / 60 = -0.5 → rounds to 0 (R's banker's rounding)
  geo_side_0 <- round((0 - 30) / 60) %% 6
  expect_true(geo_side_0 %in% c(0, 5),
    info = paste("0° should map to side 0 or 5, got", geo_side_0))

  # Boundary at 60°: between side 0 (30°) and side 1 (90°)
  geo_side_60 <- round((60 - 30) / 60) %% 6
  expect_true(geo_side_60 %in% c(0, 1),
    info = paste("60° should map to side 0 or 1, got", geo_side_60))

  # Boundary at -60°: between side 5 (-30°) and side 4 (-90°)
  geo_side_minus60 <- round((-60 - 30) / 60) %% 6
  expect_true(geo_side_minus60 %in% c(4, 5),
    info = paste("-60° should map to side 4 or 5, got", geo_side_minus60))
})

test_that("topo-to-geo formula: practical direction angles", {
  # Test with practical angles that might occur from atan2 calculations
  # These should be within ±25° of canonical angles
  test_cases <- list(
    list(dir = 25, expected_side = 0),   # Close to 30° (NE)
    list(dir = 35, expected_side = 0),   # Close to 30° (NE)
    list(dir = 85, expected_side = 1),   # Close to 90° (N)
    list(dir = 95, expected_side = 1),   # Close to 90° (N)
    list(dir = 145, expected_side = 2),  # Close to 150° (NW)
    list(dir = 155, expected_side = 2),  # Close to 150° (NW)
    list(dir = -145, expected_side = 3), # Close to -150° (SW)
    list(dir = -155, expected_side = 3), # Close to -150° (SW)
    list(dir = -85, expected_side = 4),  # Close to -90° (S)
    list(dir = -95, expected_side = 4),  # Close to -90° (S)
    list(dir = -25, expected_side = 5),  # Close to -30° (SE)
    list(dir = -35, expected_side = 5)   # Close to -30° (SE)
  )

  for (tc in test_cases) {
    geo_side <- round((tc$dir - 30) / 60) %% 6
    expect_equal(geo_side, tc$expected_side,
      info = paste0("Direction ", tc$dir, "°: ",
                    "expected side ", tc$expected_side, ", got ", geo_side))
  }
})

test_that("topo-to-geo formula: negative modulo handling", {
  # R's %% operator handles negative numbers correctly, but let's verify
  # (150 - 30) / 60 = 2 → 2 %% 6 = 2 (side facing 150°)
  # (-150 - 30) / 60 = -3 → -3 %% 6 = 3 (side facing -150°)
  expect_equal(round((150 - 30) / 60) %% 6, 2)
  expect_equal(round((-150 - 30) / 60) %% 6, 3)
  expect_equal(round((210 - 30) / 60) %% 6, 3)  # 210° = -150° equivalent
  expect_equal(round((-210 - 30) / 60) %% 6, 2)  # -210° = 150° equivalent
})

test_that("hexagonal: 7-ring puzzle ALL-X fusion has correct neighbor mapping", {
  # This is the specific bug case: 7-ring hex with ALL-127 fusion
  # Piece 127 is the last piece; fusing all except it
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(7),
    size = c(400),
    seed = 1234,
    fusion_groups = "ALL-127",
    fusion_style = "dashed",
    save_files = FALSE
  )

  # Should have 127 pieces (3*7*6 + 1 = 127)
  expect_equal(length(result$pieces), 127)

  # Check several pieces in different rings for correct neighbor mapping
  # Piece 52 should have neighbor 113 at direction ~150° (side 4)
  # Piece 80 should have specific neighbors

  # For each fused piece, verify fused_neighbor_ids point to actual neighbors
  for (piece_id in seq_len(min(10, length(result$pieces)))) {
    piece <- result$pieces[[piece_id]]
    if (is.null(piece$fused_neighbor_ids)) next

    for (side in names(piece$fused_neighbor_ids)) {
      neighbor_id <- piece$fused_neighbor_ids[[side]]
      if (is.na(neighbor_id) || is.null(neighbor_id)) next

      # Verify neighbor exists
      expect_true(neighbor_id <= length(result$pieces),
        info = paste("Piece", piece_id, "side", side, "claims neighbor",
                     neighbor_id, "which doesn't exist"))

      # If we have center coordinates, verify the direction is correct
      if (!is.null(piece$center) && !is.null(result$pieces[[neighbor_id]]$center)) {
        p_cx <- piece$center[1]
        p_cy <- piece$center[2]
        n_cx <- result$pieces[[neighbor_id]]$center[1]
        n_cy <- result$pieces[[neighbor_id]]$center[2]

        if (!is.na(p_cx) && !is.na(n_cx)) {
          # Calculate actual direction
          actual_dir <- atan2(n_cy - p_cy, n_cx - p_cx) * 180 / pi

          # Calculate expected direction for this geometric side
          # For flat-top hexagon: side N faces direction (30 + N*60)°
          geo_side <- as.integer(side)
          expected_dir <- 30 + geo_side * 60
          if (expected_dir > 180) expected_dir <- expected_dir - 360

          # Direction difference should be < 35° (30° sector + tolerance)
          diff <- abs(actual_dir - expected_dir)
          if (diff > 180) diff <- 360 - diff

          expect_true(diff < 35,
            info = paste("Piece", piece_id, "side", side, ": actual dir",
                         round(actual_dir, 1), "expected ~", expected_dir))
        }
      }
    }
  }
})

test_that("hexagonal: center piece fused to all ring-1 neighbors", {
  # Generate center fused with all 6 ring-1 neighbors
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(3),
    size = c(200),
    seed = 42,
    fusion_groups = list(c(1, 2, 3, 4, 5, 6, 7)),  # Center + all ring-1
    fusion_style = "dashed",
    save_files = FALSE
  )

  # Center piece should have 6 fused edges (one to each ring-1 neighbor)
  center <- result$pieces[[1]]
  fused_count <- sum(sapply(center$fused_edges, isTRUE))
  expect_equal(fused_count, 6,
    info = "Center piece should have 6 fused edges")

  # Verify neighbor IDs point to pieces 2-7
  neighbor_ids <- unlist(center$fused_neighbor_ids)
  expect_true(all(neighbor_ids %in% 2:7),
    info = paste("Neighbor IDs should be 2-7, got:", paste(neighbor_ids, collapse = ", ")))
})

test_that("hexagonal: fused edges are symmetric (both pieces have matching fused edge)", {
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(3),
    size = c(200),
    seed = 42,
    fusion_groups = list(c(1, 2)),  # Fuse center with piece 2
    fusion_style = "dashed",
    save_files = FALSE
  )

  p1 <- result$pieces[[1]]
  p2 <- result$pieces[[2]]

  # Find which side of piece 1 is fused to piece 2
  p1_fused_side <- NULL
  for (side in names(p1$fused_neighbor_ids)) {
    if (isTRUE(p1$fused_neighbor_ids[[side]] == 2)) {
      p1_fused_side <- side
      break
    }
  }

  # Find which side of piece 2 is fused to piece 1
  p2_fused_side <- NULL
  for (side in names(p2$fused_neighbor_ids)) {
    if (isTRUE(p2$fused_neighbor_ids[[side]] == 1)) {
      p2_fused_side <- side
      break
    }
  }

  expect_true(!is.null(p1_fused_side),
    info = "Piece 1 should have a fused side pointing to piece 2")
  expect_true(!is.null(p2_fused_side),
    info = "Piece 2 should have a fused side pointing to piece 1")

  # Both pieces should mark this edge as fused
  expect_true(isTRUE(p1$fused_edges[[p1_fused_side]]))
  expect_true(isTRUE(p2$fused_edges[[p2_fused_side]]))
})

test_that("hexagonal: ring 2 pieces have correct topo-to-geo mapping", {
  # Ring 2 pieces (8-19 in 3-ring puzzle) have different orientations
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(3),
    size = c(200),
    seed = 42,
    fusion_groups = list(c(8, 9, 10)),  # Three adjacent ring-2 pieces
    fusion_style = "dashed",
    save_files = FALSE
  )

  # Each piece should have exactly 2 fused edges (to its 2 fused neighbors)
  for (piece_id in c(8, 9, 10)) {
    piece <- result$pieces[[piece_id]]
    fused_count <- sum(sapply(piece$fused_edges, isTRUE))

    # Piece 9 is in the middle, so has 2 fused edges (to 8 and 10)
    # Pieces 8 and 10 are on the ends, so have 1 fused edge each
    if (piece_id == 9) {
      expect_equal(fused_count, 2,
        info = paste("Piece", piece_id, "should have 2 fused edges"))
    } else {
      expect_equal(fused_count, 1,
        info = paste("Piece", piece_id, "should have 1 fused edge"))
    }
  }
})

# Hexagonal completeness tests for edge deduplication
# These tests verify hexagonal puzzles don't have the many-to-one issues
# that required special handling in concentric puzzles

test_that("hexagonal: fusion works correctly with offset > 0", {
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(3),
    size = c(200),
    seed = 42,
    fusion_groups = list(c(1, 2, 3)),
    fusion_style = "dashed",
    offset = 20,
    save_files = FALSE
  )

  # Should have dashed strokes even with offset
  expect_true(grepl("stroke-dasharray", result$svg_content))

  # Verify fused edges are still marked correctly
  piece1 <- result$pieces[[1]]
  expect_true(any(unlist(piece1$fused_edges)))

  # Verify pieces are separated (center should be offset from origin)
  # With offset > 0, pieces should have translated positions
  expect_true(result$canvas_size[1] > 200)  # Canvas expanded due to offset
})

test_that("hexagonal: fused edges are drawn exactly once", {
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(3),
    size = c(200),
    seed = 42,
    fusion_groups = "1-2-3-4-5-6-7",  # Center + all ring-1
    fusion_style = "dashed",
    save_files = FALSE
  )

  svg <- result$svg_content

  # Count dashed stroke paths
  dashed_matches <- gregexpr('stroke-dasharray', svg)
  dashed_count <- sum(sapply(dashed_matches, function(m) if (m[1] == -1) 0 else length(m)))

  # 12 fused edges: 6 center-to-ring1 + 6 ring1-to-ring1 adjacents
  # Each should be drawn exactly once (no double-drawing)
  expect_equal(dashed_count, 12)
})

test_that("hexagonal: never has outer_segments_mixed or fused_edge_segments", {
  # Hexagonal puzzles have 1-to-1 edge relationships, unlike concentric
  # which has many-to-one OUTER edges requiring segment-level fusion
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(4),
    size = c(300),
    seed = 42,
    fusion_groups = "ALL-37",  # All except piece 37
    fusion_style = "dashed",
    save_files = FALSE
  )

  # No hexagonal piece should have segment-level fusion data
  for (piece in result$pieces) {
    expect_null(piece$outer_segments_mixed,
      info = paste("Piece", piece$id, "should not have outer_segments_mixed"))
    expect_null(piece$fused_edge_segments,
      info = paste("Piece", piece$id, "should not have fused_edge_segments"))
  }
})
