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

test_that("SVG contains split edge segments with bezier tabs for mixed fusion", {
  result <- generate_puzzle(
    type = "concentric",
    seed = 42,
    grid = c(3),
    size = c(240),
    fusion_groups = "ALL-18",
    fusion_style = "dashed"
  )

  svg <- result$svg_content

  # Count bezier paths (paths containing C command for cubic bezier curves)
  # These are edge paths with tabs, not simple arcs
  bezier_pattern <- "<path d=\"M[^\"]+C[^\"]+\""
  bezier_matches <- gregexpr(bezier_pattern, svg)
  bezier_count <- sum(sapply(bezier_matches, function(m) if (m[1] == -1) 0 else length(m)))

  # Should have bezier segments for piece 7's OUTER edge (with tabs)
  expect_gte(bezier_count, 2)

  # Check for dashed bezier path (fused segment with tabs)
  dashed_bezier_pattern <- "<path d=\"M[^\"]+C[^\"]+\"[^>]*stroke-dasharray"
  dashed_matches <- gregexpr(dashed_bezier_pattern, svg)
  dashed_count <- sum(sapply(dashed_matches, function(m) if (m[1] == -1) 0 else length(m)))

  # Should have at least 1 dashed segment with bezier tabs
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

# Fix for many-to-one complementary edge marking
test_that("excluded piece's INNER edge is NOT marked fused", {
  # ALL-18 means all pieces EXCEPT 18 are in the fusion group
  # Piece 7 (ring 1) touches both 18 and 19 on its OUTER edge
  # The bug was: piece 18's INNER edge was incorrectly marked as fused
  result <- generate_puzzle(
    type = "concentric",
    seed = 42,
    grid = c(3),
    size = c(240),
    fusion_groups = "ALL-18",
    fusion_style = "dashed"
  )

  # Piece 18 is NOT in the fusion group
  expect_false(18 %in% result$parameters$fusion_groups[[1]])

  # Piece 18's INNER edge should NOT be marked as fused
  piece18 <- result$pieces[[18]]
  expect_false(piece18$fused_edges$INNER)

  # Piece 19 IS in the group, so its INNER edge should be fused
  piece19 <- result$pieces[[19]]
  expect_true(piece19$fused_edges$INNER)
})

test_that("complementary edges marked correctly for many-to-one", {
  # Test with different exclusion: ALL-19
  result <- generate_puzzle(
    type = "concentric",
    seed = 42,
    grid = c(3),
    size = c(240),
    fusion_groups = "ALL-19",
    fusion_style = "dashed"
  )

  # Piece 19 is NOT in the group
  expect_false(19 %in% result$parameters$fusion_groups[[1]])

  # Piece 19's INNER edge should NOT be fused
  piece19 <- result$pieces[[19]]
  expect_false(piece19$fused_edges$INNER)

  # Piece 18 IS in the group, so its INNER edge SHOULD be fused
  # (because piece 7 is in the group and 18's INNER touches 7's OUTER)
  piece18 <- result$pieces[[18]]
  expect_true(piece18$fused_edges$INNER)
})

# Tests for edge deduplication fixes
test_that("many-to-one OUTER edges mark all neighbors' INNER edges", {

  # Pieces 4 and 5 (ring 1) each have OUTER edges touching 2 ring 2 pieces

  # ALL-18 fusion means all these edges should be fused
  result <- generate_puzzle(
    type = "concentric",
    seed = 42,
    grid = c(3),
    size = c(240),
    fusion_groups = "ALL-18",
    fusion_style = "dashed"
  )


  # Piece 5's OUTER edge touches pieces 14 and 15
  piece5 <- result$pieces[[5]]
  expect_false(isTRUE(piece5$outer_segments_mixed))  # Both neighbors fused
  expect_equal(length(piece5$fused_edge_segments$OUTER), 2)

  # Both segments should be marked as fused
  for (seg in piece5$fused_edge_segments$OUTER) {
    expect_true(seg$fused)
    expect_true(seg$neighbor_id %in% c(14, 15))
  }

  # Piece 4's OUTER edge touches pieces 12 and 13
  piece4 <- result$pieces[[4]]
  expect_false(isTRUE(piece4$outer_segments_mixed))
  expect_equal(length(piece4$fused_edge_segments$OUTER), 2)
})

test_that("INNER edges skip drawing when neighbor has outer_segments_mixed", {
  # Piece 7 has outer_segments_mixed = TRUE (18 not fused, 19 fused)
  # Piece 19's INNER edge should NOT be drawn separately in Pass 3
  # because it will be drawn in Pass 3.5 from piece 7's perspective
  result <- generate_puzzle(
    type = "concentric",
    seed = 42,
    grid = c(3),
    size = c(240),
    fusion_groups = "ALL-18",
    fusion_style = "dashed"
  )

  piece7 <- result$pieces[[7]]
  piece19 <- result$pieces[[19]]

  # Verify piece 7 has mixed segments

  expect_true(isTRUE(piece7$outer_segments_mixed))

  # Piece 19's INNER neighbor is piece 7
  expect_equal(piece19$fused_neighbor_ids$INNER, 7)

  # Piece 19's INNER edge is marked fused (both in same group)
  expect_true(piece19$fused_edges$INNER)
})

test_that("edges are not double-drawn causing thick lines", {
  # Generate puzzle and check that each edge path appears only once
  # in stroked form (excluding fill paths)
  result <- generate_puzzle(
    type = "concentric",
    seed = 42,
    grid = c(3),
    size = c(240),
    fusion_groups = "ALL-18",
    fusion_style = "dashed"
  )

  svg <- result$svg_content

  # Get piece 7's segment path to piece 19
  piece7 <- result$pieces[[7]]
  seg19 <- NULL
  for (seg in piece7$fused_edge_segments$OUTER) {
    if (seg$neighbor_id == 19) {
      seg19 <- seg
      break
    }
  }
  expect_false(is.null(seg19))
  expect_false(is.null(seg19$path))

  # Count how many times this path appears with a stroke (not fill paths)
  # Use a distinctive substring from the path
  path_snippet <- substr(seg19$path, 1, 30)

  # Count stroked occurrences (paths with stroke-width)
  stroked_pattern <- sprintf('<path[^>]*%s[^>]*stroke-width',
                             gsub("([.+])", "\\\\\\1", path_snippet))
  stroked_matches <- gregexpr(stroked_pattern, svg, perl = TRUE)
  stroked_count <- sum(sapply(stroked_matches, function(m) if (m[1] == -1) 0 else length(m)))

  # Should appear exactly once as a stroked path

  expect_equal(stroked_count, 1,
               info = "Piece 7's segment to 19 should be drawn exactly once")
})

test_that("total path count is reasonable for ALL-18 fusion", {
  # This test ensures we don't have excessive double-drawing
  result <- generate_puzzle(
    type = "concentric",
    seed = 42,
    grid = c(3),
    size = c(240),
    fusion_groups = "ALL-18",
    fusion_style = "dashed"
  )

  svg <- result$svg_content

  # Count total paths
  total_paths <- length(gregexpr("<path", svg)[[1]])

  # Count dashed paths (fused internal edges)
  dashed_matches <- gregexpr('stroke-dasharray', svg)
  dashed_count <- sum(sapply(dashed_matches, function(m) if (m[1] == -1) 0 else length(m)))

  # With 19 pieces:
  # - 19 fill paths (Pass 1)
  # - ~12 boundary edges for ring 2 (solid, Pass 2)
  # - ~28 internal fused edges (dashed, Pass 3 + Pass 3.5)
  # - 2 segments for piece 7 (1 solid to 18, 1 dashed to 19)
  # Total should be around 60-70 paths

  expect_gte(total_paths, 55)
  expect_lte(total_paths, 75)
  expect_gte(dashed_count, 20)
  expect_lte(dashed_count, 35)
})
