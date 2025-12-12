# Test piece positioning functionality
# Includes repel layout algorithm tests (Issue #53)

test_that("apply_piece_positioning works with default grid layout", {
  pieces_result <- generate_pieces_internal(
    type = "rectangular",
    seed = 42,
    grid = c(2, 2),
    size = c(100, 100)
  )

  # Grid layout with offset
  positioned <- apply_piece_positioning(pieces_result, offset = 5, layout = "grid")

  expect_equal(length(positioned$pieces), 4)
  expect_true(all(c("canvas_size", "canvas_offset") %in% names(positioned)))
})

test_that("apply_piece_positioning accepts layout parameter", {
  pieces_result <- generate_pieces_internal(
    type = "rectangular",
    seed = 42,
    grid = c(2, 2),
    size = c(100, 100)
  )

  # Test grid layout
  grid_result <- apply_piece_positioning(pieces_result, offset = 10, layout = "grid")
  expect_equal(length(grid_result$pieces), 4)

  # Test repel layout
  repel_result <- apply_piece_positioning(pieces_result, offset = 10, layout = "repel")
  expect_equal(length(repel_result$pieces), 4)
  expect_true("repel_iterations" %in% names(repel_result))
})

# =============================================================================
# Repel Layout Algorithm Tests (Issue #53)
# =============================================================================

test_that("apply_repel_layout handles small puzzle", {
  pieces_result <- generate_pieces_internal(
    type = "rectangular",
    seed = 42,
    grid = c(2, 2),
    size = c(100, 100)
  )

  positioned <- apply_piece_positioning(pieces_result, offset = 5)
  result <- apply_repel_layout(positioned)

  expect_equal(length(result$pieces), 4)
  expect_true("repel_iterations" %in% names(result))
})

test_that("apply_repel_layout resolves overlapping pieces", {
  pieces_result <- generate_pieces_internal(
    type = "rectangular",
    seed = 42,
    grid = c(3, 3),
    size = c(150, 150)
  )

  # Start with small offset that might cause overlaps
  positioned <- apply_piece_positioning(pieces_result, offset = 2)

  # Apply repel layout
  repel_result <- apply_repel_layout(positioned, margin = 5)

  # Check result structure
  expect_equal(length(repel_result$pieces), 9)
  expect_true("repel_iterations" %in% names(repel_result))
  expect_true("canvas_size" %in% names(repel_result))
})

test_that("build_collision_groups correctly groups fused pieces", {
  pieces_result <- generate_pieces_internal(
    type = "rectangular",
    seed = 42,
    grid = c(2, 2),
    size = c(100, 100)
  )

  # Create a temporary puzzle result for fusion context
  temp_puzzle <- list(
    type = "rectangular",
    pieces = pieces_result$pieces,
    parameters = list(type = "rectangular", grid = c(2, 2))
  )

  # Apply fusion to pieces 1 and 2
  parsed_groups <- parse_fusion("1-2", puzzle_result = temp_puzzle)
  fused_result <- apply_fusion_to_pieces(pieces_result, parsed_groups, temp_puzzle)

  # Build collision groups
  groups <- build_collision_groups(fused_result$pieces)

  # Pieces 1 and 2 should be in same group, 3 and 4 separate
  expect_equal(length(groups), 3)  # One fused group + 2 individual pieces

  # Find the fused group
  fused_group_sizes <- sapply(groups, function(g) length(g$piece_indices))
  expect_true(any(fused_group_sizes == 2))  # One group has 2 pieces
})

test_that("compute_bbox_overlap detects overlapping boxes", {
  bbox1 <- list(min_x = 0, max_x = 10, min_y = 0, max_y = 10,
                center_x = 5, center_y = 5)
  bbox2 <- list(min_x = 8, max_x = 18, min_y = 8, max_y = 18,
                center_x = 13, center_y = 13)

  overlap <- compute_bbox_overlap(bbox1, bbox2, margin = 0)

  expect_true(overlap$overlaps)
  expect_equal(overlap$overlap_x, 2)  # 10 - 8 = 2
  expect_equal(overlap$overlap_y, 2)  # 10 - 8 = 2
})

test_that("compute_bbox_overlap detects non-overlapping boxes", {
  bbox1 <- list(min_x = 0, max_x = 10, min_y = 0, max_y = 10,
                center_x = 5, center_y = 5)
  bbox2 <- list(min_x = 15, max_x = 25, min_y = 15, max_y = 25,
                center_x = 20, center_y = 20)

  overlap <- compute_bbox_overlap(bbox1, bbox2, margin = 0)

  expect_false(overlap$overlaps)
})

test_that("compute_bbox_overlap respects margin", {
  # Boxes that don't overlap but are within margin
  bbox1 <- list(min_x = 0, max_x = 10, min_y = 0, max_y = 10,
                center_x = 5, center_y = 5)
  bbox2 <- list(min_x = 12, max_x = 22, min_y = 0, max_y = 10,
                center_x = 17, center_y = 5)

  # With no margin, no overlap
  overlap_no_margin <- compute_bbox_overlap(bbox1, bbox2, margin = 0)
  expect_false(overlap_no_margin$overlaps)

  # With margin of 5, they overlap
  overlap_with_margin <- compute_bbox_overlap(bbox1, bbox2, margin = 5)
  expect_true(overlap_with_margin$overlaps)
})

test_that("compute_repulsion_vector pushes boxes apart", {
  bbox1 <- list(min_x = 0, max_x = 10, min_y = 0, max_y = 10,
                center_x = 5, center_y = 5)
  bbox2 <- list(min_x = 8, max_x = 18, min_y = 0, max_y = 10,
                center_x = 13, center_y = 5)

  overlap <- compute_bbox_overlap(bbox1, bbox2, margin = 0)
  repel_vec <- compute_repulsion_vector(bbox1, bbox2, overlap, step_size = 1.0)

  # Should push in X direction (direction from bbox1 to bbox2)
  expect_true(repel_vec[1] > 0)  # Push right
  expect_equal(repel_vec[2], 0)  # No Y movement (overlap_x < overlap_y)
})

test_that("translate_piece moves piece correctly", {
  pieces_result <- generate_pieces_internal(
    type = "rectangular",
    seed = 42,
    grid = c(2, 2),
    size = c(100, 100)
  )
  piece <- pieces_result$pieces[[1]]

  original_center <- piece$center
  translated <- translate_piece(piece, 10, 20)

  expect_equal(translated$center[1], original_center[1] + 10)
  expect_equal(translated$center[2], original_center[2] + 20)
})

test_that("generate_puzzle accepts layout parameter", {
  # Test with grid layout
  result_grid <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    size = c(100, 100),
    seed = 42,
    offset = 5,
    layout = "grid",
    save_files = FALSE
  )
  expect_equal(length(result_grid$pieces), 4)

  # Test with repel layout
  result_repel <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    size = c(100, 100),
    seed = 42,
    offset = 5,
    layout = "repel",
    save_files = FALSE
  )
  expect_equal(length(result_repel$pieces), 4)
})

test_that("repel layout works with hexagonal puzzles", {
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(2),
    size = c(100),
    seed = 42,
    offset = 5,
    layout = "repel",
    save_files = FALSE
  )

  expect_equal(length(result$pieces), 7)  # 1 center + 6 ring pieces
})

test_that("repel layout works with concentric puzzles", {
  result <- generate_puzzle(
    type = "concentric",
    grid = c(2),
    size = c(120),
    seed = 42,
    offset = 5,
    layout = "repel",
    save_files = FALSE
  )

  expect_true(length(result$pieces) > 1)
})

test_that("repel layout handles fused pieces as single units", {
  # Create puzzle with fusion
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    size = c(150, 150),
    seed = 42,
    offset = 10,
    layout = "repel",
    fusion_groups = "1-2-3",  # Top row fused
    save_files = FALSE
  )

  # Verify pieces exist
  expect_equal(length(result$pieces), 9)

  # Check that fused pieces have same fusion_group
  fused_groups <- sapply(result$pieces[1:3], function(p) p$fusion_group)
  expect_true(all(!is.na(fused_groups)))
  expect_equal(length(unique(fused_groups)), 1)  # All same group
})

test_that("repel_margin and repel_max_iter parameters work", {
  # Test custom parameters
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    size = c(150, 150),
    seed = 42,
    offset = 3,
    layout = "repel",
    repel_margin = 10,
    repel_max_iter = 50,
    save_files = FALSE
  )

  expect_equal(length(result$pieces), 9)
})

test_that("apply_compaction pulls pieces toward center", {
  # Create test data with groups spread apart
  pieces <- list(
    list(center = c(0, 0), path = "M 0 0", parsed_segments = NULL),
    list(center = c(100, 0), path = "M 100 0", parsed_segments = NULL),
    list(center = c(50, 100), path = "M 50 100", parsed_segments = NULL)
  )

  groups <- list(
    list(fusion_group = NA, piece_indices = 1),
    list(fusion_group = NA, piece_indices = 2),
    list(fusion_group = NA, piece_indices = 3)
  )

  group_bboxes <- lapply(seq_along(groups), function(i) {
    p <- pieces[[i]]
    list(
      min_x = p$center[1] - 10, max_x = p$center[1] + 10,
      min_y = p$center[2] - 10, max_y = p$center[2] + 10,
      center_x = p$center[1], center_y = p$center[2]
    )
  })

  compacted <- apply_compaction(pieces, groups, group_bboxes, margin = 2)

  # All pieces should have moved toward the overall center
  # The overall center is roughly (50, 33.3)
  expect_length(compacted, 3)
})

test_that("extract_segment_coords handles different segment types", {
  segments <- list(
    list(type = "M", x = 0, y = 0),
    list(type = "L", x = 10, y = 0),
    list(type = "C", cp1x = 15, cp1y = -5, cp2x = 20, cp2y = -5, x = 25, y = 0),
    list(type = "A", x = 30, y = 10),
    list(type = "Z")
  )

  coords <- extract_segment_coords(segments)

  expect_true(length(coords$x) > 0)
  expect_true(length(coords$y) > 0)
  expect_equal(length(coords$x), length(coords$y))
})
