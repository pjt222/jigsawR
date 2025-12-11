# Test segment-level fusion for concentric puzzles (Issue #52)
# Tests for many-to-one neighbor relationships where one inner piece's OUTER edge
# spans multiple outer ring pieces' INNER edges

test_that("get_outer_edge_segments returns correct segments", {
  # Piece 7 in ring 1 (position 5) should touch pieces 18 and 19 in ring 2
  segments <- get_outer_edge_segments(7, 3)

  expect_equal(length(segments), 2)

  # First segment: 300° - 330° -> neighbor 18
  expect_equal(segments[[1]]$neighbor_id, 18)
  expect_equal(segments[[1]]$start_angle * 180 / pi, 300, tolerance = 0.01)
  expect_equal(segments[[1]]$end_angle * 180 / pi, 330, tolerance = 0.01)

  # Second segment: 330° - 360° -> neighbor 19
  expect_equal(segments[[2]]$neighbor_id, 19)
  expect_equal(segments[[2]]$start_angle * 180 / pi, 330, tolerance = 0.01)
  expect_equal(segments[[2]]$end_angle * 180 / pi, 360, tolerance = 0.01)
})

test_that("get_outer_edge_segments returns empty for center piece", {
  segments <- get_outer_edge_segments(1, 3)
  expect_equal(length(segments), 0)
})

test_that("get_outer_edge_segments returns empty for boundary ring", {
  # Piece 19 is in the outermost ring (boundary)
  segments <- get_outer_edge_segments(19, 3)
  expect_equal(length(segments), 0)
})

test_that("generate_arc_segment_path creates valid SVG arc", {
  arc <- generate_arc_segment_path(
    radius = 100,
    start_angle = 0,
    end_angle = pi / 6  # 30 degrees
  )

  # Should start with M (moveto)
  expect_true(grepl("^M", arc))

  # Should contain A (arc command)
  expect_true(grepl("A100\\.00,100\\.00", arc))

  # Should have correct start point (radius * cos(0), radius * sin(0)) = (100, 0)
  expect_true(grepl("M100\\.00,0\\.00", arc))
})

test_that("segment-level fusion data is computed for ALL-18 case", {
  # ALL-18 means all pieces except piece 18 are fused
  result <- generate_puzzle(
    type = "concentric",
    seed = 42,
    grid = c(3),
    size = c(240),
    fusion_groups = "ALL-18",
    fusion_style = "dashed"
  )

  piece7 <- result$pieces[[7]]

  # Piece 7 should have mixed segments
  expect_true(isTRUE(piece7$outer_segments_mixed))

  # Should have outer radius stored
  expect_equal(piece7$outer_radius, 80)

  # Should have segment-level fusion data
  expect_true(!is.null(piece7$fused_edge_segments))
  expect_true("OUTER" %in% names(piece7$fused_edge_segments))

  segments <- piece7$fused_edge_segments$OUTER
  expect_equal(length(segments), 2)

  # Segment 1 (neighbor 18) should NOT be fused
  expect_equal(segments[[1]]$neighbor_id, 18)
  expect_false(segments[[1]]$fused)

  # Segment 2 (neighbor 19) should be fused
  expect_equal(segments[[2]]$neighbor_id, 19)
  expect_true(segments[[2]]$fused)
})

test_that("uniform fusion does not trigger segment splitting", {
  # When all neighbors are fused (or none are), no segment splitting needed
  result <- generate_puzzle(
    type = "concentric",
    seed = 42,
    grid = c(3),
    size = c(240),
    fusion_groups = "2-8-9",  # Piece 2 with both its outer neighbors
    fusion_style = "dashed"
  )

  piece2 <- result$pieces[[2]]

  # Piece 2's OUTER edge touches pieces 8 and 9, both in the fusion group
  # So outer_segments_mixed should be FALSE (all segments fused)
  expect_false(isTRUE(piece2$outer_segments_mixed))
})

test_that("SVG contains split arc segments for mixed fusion", {
  result <- generate_puzzle(
    type = "concentric",
    seed = 42,
    grid = c(3),
    size = c(240),
    fusion_groups = "ALL-18",
    fusion_style = "dashed"
  )

  svg <- result$svg_content

  # Count arc paths (paths containing A command)
  arc_pattern <- "<path d=\"M[^\"]+A[^\"]+\""
  arc_matches <- gregexpr(arc_pattern, svg)
  arc_count <- sum(sapply(arc_matches, function(m) if (m[1] == -1) 0 else length(m)))

  # Should have at least 2 arc segments for piece 7's OUTER edge
  expect_gte(arc_count, 2)

  # Check for dashed arc (fused segment)
  dashed_arc_pattern <- "<path d=\"M[^\"]+A[^\"]+\"[^>]*stroke-dasharray"
  dashed_matches <- gregexpr(dashed_arc_pattern, svg)
  dashed_count <- sum(sapply(dashed_matches, function(m) if (m[1] == -1) 0 else length(m)))

  # Should have at least 1 dashed arc
  expect_gte(dashed_count, 1)
})

test_that("no fusion results in no segment data", {
  result <- generate_puzzle(
    type = "concentric",
    seed = 42,
    grid = c(3),
    size = c(240)
    # No fusion_groups
  )

  piece7 <- result$pieces[[7]]

  # Should not have mixed segments without fusion
  expect_null(piece7$outer_segments_mixed)
  expect_null(piece7$fused_edge_segments)
})

test_that("segment-level rendering handles deduplication", {
  # Each segment should only be drawn once
  result <- generate_puzzle(
    type = "concentric",
    seed = 42,
    grid = c(3),
    size = c(240),
    fusion_groups = "ALL-18",
    fusion_style = "dashed"
  )

  svg <- result$svg_content

  # Extract all arc paths
  arc_pattern <- "M[0-9.-]+,[0-9.-]+ A[0-9.-]+,[0-9.-]+ 0 [01],[01] [0-9.-]+,[0-9.-]+"
  arc_matches <- regmatches(svg, gregexpr(arc_pattern, svg))[[1]]

  # Each unique arc should appear exactly once
  expect_equal(length(arc_matches), length(unique(arc_matches)))
})
