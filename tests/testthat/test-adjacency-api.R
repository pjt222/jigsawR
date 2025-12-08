# Tests for Unified Adjacency API
# test-adjacency-api.R

# ============================================================================
# RECTANGULAR ADJACENCY TESTS
# ============================================================================

test_that("rectangular: corner piece has 2 neighbors", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    size = c(300, 300),
    seed = 42,
    save_files = FALSE
  )

  # Piece 1 is at (0,0) - top-left corner
  neighbors <- get_piece_neighbors(1, result)
  non_boundary <- neighbors[!neighbors$is_boundary, ]

  expect_equal(nrow(non_boundary), 2)
  expect_true("E" %in% non_boundary$direction)  # Right neighbor
  expect_true("S" %in% non_boundary$direction)  # Bottom neighbor
})

test_that("rectangular: edge piece has 3 neighbors", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    size = c(300, 300),
    seed = 42,
    save_files = FALSE
  )

  # Piece 2 is at (1,0) - top edge
  neighbors <- get_piece_neighbors(2, result)
  non_boundary <- neighbors[!neighbors$is_boundary, ]

  expect_equal(nrow(non_boundary), 3)
  expect_true("W" %in% non_boundary$direction)  # Left
  expect_true("E" %in% non_boundary$direction)  # Right
  expect_true("S" %in% non_boundary$direction)  # Bottom
})

test_that("rectangular: center piece has 4 neighbors", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    size = c(300, 300),
    seed = 42,
    save_files = FALSE
  )

  # Piece 5 is at (1,1) - center
  neighbors <- get_piece_neighbors(5, result)
  non_boundary <- neighbors[!neighbors$is_boundary, ]

  expect_equal(nrow(non_boundary), 4)
  expect_true(all(c("N", "E", "S", "W") %in% non_boundary$direction))
})

test_that("rectangular: are_pieces_adjacent works", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    size = c(300, 300),
    seed = 42,
    save_files = FALSE
  )

  # Adjacent pieces
  expect_true(are_pieces_adjacent(1, 2, result))  # Horizontal
  expect_true(are_pieces_adjacent(1, 4, result))  # Vertical
  expect_true(are_pieces_adjacent(5, 2, result))  # Center to top

  # Non-adjacent pieces (diagonal)
  expect_false(are_pieces_adjacent(1, 5, result))  # Corner to center (diagonal)
  expect_false(are_pieces_adjacent(1, 9, result))  # Opposite corners
})

test_that("rectangular: include_boundary parameter works", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    size = c(200, 200),
    seed = 42,
    save_files = FALSE
  )

  # With boundaries
  neighbors_with <- get_piece_neighbors(1, result, include_boundary = TRUE)
  expect_equal(nrow(neighbors_with), 4)  # All 4 directions

  # Without boundaries
  neighbors_without <- get_piece_neighbors(1, result, include_boundary = FALSE)
  expect_equal(nrow(neighbors_without), 2)  # Only E and S
})

# ============================================================================
# HEXAGONAL ADJACENCY TESTS
# ============================================================================

test_that("hexagonal: center piece has 6 neighbors", {
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(3),
    size = c(200),
    seed = 42,
    save_files = FALSE
  )

  # Piece 1 is the center
  neighbors <- get_piece_neighbors(1, result)
  non_boundary <- neighbors[!neighbors$is_boundary, ]

  expect_equal(nrow(non_boundary), 6)
})

test_that("hexagonal: ring 1 piece connects to center", {
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(3),
    size = c(200),
    seed = 42,
    save_files = FALSE
  )

  # Piece 2 is in ring 1
  neighbors <- get_piece_neighbors(2, result)

  # Should have center (piece 1) as a neighbor
  expect_true(1 %in% neighbors$neighbor_id)
})

test_that("hexagonal: are_pieces_adjacent works", {
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(3),
    size = c(200),
    seed = 42,
    save_files = FALSE
  )

  # Center to ring 1
  expect_true(are_pieces_adjacent(1, 2, result))
  expect_true(are_pieces_adjacent(1, 7, result))

  # Ring 1 adjacent pieces (2 and 3 are neighbors)
  expect_true(are_pieces_adjacent(2, 3, result))

  # Non-adjacent: piece 2 and piece 5 (opposite sides of ring 1)
  # Note: They might actually be adjacent depending on the topology
  # Let's check center to outer ring (should be non-adjacent)
  # Piece 1 (center) to piece 8 (ring 2) - not directly adjacent
  expect_false(are_pieces_adjacent(1, 8, result))
})

# ============================================================================
# CONCENTRIC ADJACENCY TESTS
# ============================================================================

test_that("concentric: center piece has 6 neighbors", {
  result <- generate_puzzle(
    type = "concentric",
    grid = c(3),
    size = c(200),
    seed = 42,
    save_files = FALSE
  )

  # Piece 1 is the center
  neighbors <- get_piece_neighbors(1, result)
  non_boundary <- neighbors[!neighbors$is_boundary, ]

  expect_equal(nrow(non_boundary), 6)
})

test_that("concentric: trapezoid piece has correct edges", {
  result <- generate_puzzle(
    type = "concentric",
    grid = c(3),
    size = c(200),
    seed = 42,
    save_files = FALSE
  )

  # Piece 2 is in ring 1 (trapezoid)
  neighbors <- get_piece_neighbors(2, result)

  # Should have 4 edges: INNER, RIGHT, OUTER, LEFT
  expect_equal(nrow(neighbors), 4)
  expect_true(all(c("INNER", "RIGHT", "OUTER", "LEFT") %in% neighbors$direction))

  # INNER should connect to center (piece 1)
  inner_row <- neighbors[neighbors$direction == "INNER", ]
  expect_equal(inner_row$neighbor_id, 1)
})

test_that("concentric: are_pieces_adjacent works", {
  result <- generate_puzzle(
    type = "concentric",
    grid = c(3),
    size = c(200),
    seed = 42,
    save_files = FALSE
  )

  # Center to ring 1
  expect_true(are_pieces_adjacent(1, 2, result))

  # Ring 1 circumferential (piece 2 RIGHT connects to piece 3)
  expect_true(are_pieces_adjacent(2, 3, result))
})

# ============================================================================
# FUSION INPUT PARSING TESTS
# ============================================================================

test_that("parse_fusion_input: single group string", {
  result <- parse_fusion_input("1,2,3")

  expect_equal(length(result), 1)
  expect_equal(result[[1]], c(1L, 2L, 3L))
})

test_that("parse_fusion_input: multiple groups string", {
  result <- parse_fusion_input("(1,2),(7,8,9)")

  expect_equal(length(result), 2)
  expect_equal(result[[1]], c(1L, 2L))
  expect_equal(result[[2]], c(7L, 8L, 9L))
})

test_that("parse_fusion_input: list pass-through", {
  input <- list(c(1, 2), c(3, 4, 5))
  result <- parse_fusion_input(input)

  expect_equal(length(result), 2)
  expect_equal(result[[1]], c(1L, 2L))
  expect_equal(result[[2]], c(3L, 4L, 5L))
})

test_that("parse_fusion_input: empty input", {
  expect_equal(parse_fusion_input(NULL), list())
  expect_equal(parse_fusion_input(""), list())
  expect_equal(parse_fusion_input(list()), list())
})

# ============================================================================
# FUSION GROUP VALIDATION TESTS
# ============================================================================

test_that("validate_fusion_group: connected group is valid", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    size = c(300, 300),
    seed = 42,
    save_files = FALSE
  )

  # Pieces 1, 2 are adjacent (horizontal)
  validation <- validate_fusion_group(c(1, 2), result)
  expect_true(validation$valid)

  # Pieces 1, 2, 4, 5 form a 2x2 block
  validation <- validate_fusion_group(c(1, 2, 4, 5), result)
  expect_true(validation$valid)
})

test_that("validate_fusion_group: disconnected group is invalid", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    size = c(300, 300),
    seed = 42,
    save_files = FALSE
  )

  # Pieces 1 and 9 are opposite corners (not connected)
  validation <- validate_fusion_group(c(1, 9), result)
  expect_false(validation$valid)
})

test_that("validate_all_fusion_groups: overlapping groups rejected", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    size = c(300, 300),
    seed = 42,
    save_files = FALSE
  )

  # Piece 2 appears in both groups
  groups <- list(c(1, 2), c(2, 3))
  validation <- validate_all_fusion_groups(groups, result)

  expect_false(validation$valid)
  expect_true(grepl("multiple groups", validation$message))
})

test_that("validate_all_fusion_groups: valid non-overlapping groups", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    size = c(300, 300),
    seed = 42,
    save_files = FALSE
  )

  # Two separate, non-overlapping groups
  groups <- list(c(1, 2), c(8, 9))
  validation <- validate_all_fusion_groups(groups, result)

  expect_true(validation$valid)
})

# ============================================================================
# PIECE ID NORMALIZATION TESTS
# ============================================================================

test_that("normalize_piece_id: integer pass-through", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    size = c(200, 200),
    seed = 42,
    save_files = FALSE
  )

  expect_equal(normalize_piece_id(1, result), 1L)
  expect_equal(normalize_piece_id(4, result), 4L)
})

test_that("normalize_piece_id: string piece_xi_yi format", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    size = c(300, 300),
    seed = 42,
    save_files = FALSE
  )

  # piece_0_0 -> index 1
  expect_equal(normalize_piece_id("piece_0_0", result), 1L)

  # piece_1_0 -> index 2 (second column, first row)
  expect_equal(normalize_piece_id("piece_1_0", result), 2L)

  # piece_0_1 -> index 4 (first column, second row, in 3x3 grid)
  expect_equal(normalize_piece_id("piece_0_1", result), 4L)

  # piece_2_2 -> index 9 (bottom-right corner in 3x3)
  expect_equal(normalize_piece_id("piece_2_2", result), 9L)
})

test_that("normalize_piece_id: string piece_N format", {
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(3),
    size = c(200),
    seed = 42,
    save_files = FALSE
  )

  expect_equal(normalize_piece_id("piece_1", result), 1L)
  expect_equal(normalize_piece_id("piece_7", result), 7L)
})

# ============================================================================
# EDGE FUSION MECHANISM TESTS
# ============================================================================

test_that("compute_fused_edges: empty input returns empty result", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    size = c(300, 300),
    seed = 42,
    save_files = FALSE
  )

  fused <- compute_fused_edges(NULL, result)
  expect_equal(length(fused$fused_edges), 0)
  expect_equal(length(fused$piece_to_group), 0)

  fused2 <- compute_fused_edges(list(), result)
  expect_equal(length(fused2$fused_edges), 0)
})

test_that("compute_fused_edges: rectangular 2-piece horizontal fusion", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    size = c(300, 300),
    seed = 42,
    save_files = FALSE
  )

  # Fuse pieces 1 and 2 (horizontal neighbors)
  fusion_groups <- list(c(1, 2))
  fused <- compute_fused_edges(fusion_groups, result)

  # Should have 2 fused edges (1-E and 2-W, which are the same edge)
  expect_equal(length(fused$fused_edges), 2)
  expect_true("1-E" %in% fused$fused_edges)
  expect_true("2-W" %in% fused$fused_edges)

  # Piece mappings
  expect_equal(fused$piece_to_group[["1"]], 1)
  expect_equal(fused$piece_to_group[["2"]], 1)
})

test_that("compute_fused_edges: rectangular 2x2 block fusion", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    size = c(300, 300),
    seed = 42,
    save_files = FALSE
  )

  # Fuse pieces 1, 2, 4, 5 (2x2 block in top-left)
  # Grid layout (1-indexed):
  #   1  2  3
  #   4  5  6
  #   7  8  9
  fusion_groups <- list(c(1, 2, 4, 5))
  fused <- compute_fused_edges(fusion_groups, result)

  # Internal edges: 1-E/2-W, 1-S/4-N, 2-S/5-N, 4-E/5-W = 8 edge keys total
  expect_equal(length(fused$fused_edges), 8)

  # Check some specific edges
  expect_true("1-E" %in% fused$fused_edges)
  expect_true("1-S" %in% fused$fused_edges)
  expect_true("4-E" %in% fused$fused_edges)
  expect_true("5-N" %in% fused$fused_edges)
})

test_that("compute_fused_edges: multiple fusion groups", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    size = c(300, 300),
    seed = 42,
    save_files = FALSE
  )

  # Two separate fusion groups
  fusion_groups <- list(c(1, 2), c(8, 9))
  fused <- compute_fused_edges(fusion_groups, result)

  # 2 edges per group = 4 total
  expect_equal(length(fused$fused_edges), 4)

  # Check group assignments
  expect_equal(fused$edge_to_group[["1-E"]], 1)
  expect_equal(fused$edge_to_group[["8-E"]], 2)
})

test_that("is_edge_fused: correctly identifies fused edges", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    size = c(300, 300),
    seed = 42,
    save_files = FALSE
  )

  fusion_groups <- list(c(1, 2))
  fused <- compute_fused_edges(fusion_groups, result)

  # Fused edges
  expect_true(is_edge_fused(1, "E", fused))
  expect_true(is_edge_fused(2, "W", fused))

  # Non-fused edges
  expect_false(is_edge_fused(1, "N", fused))  # Boundary
  expect_false(is_edge_fused(1, "S", fused))  # Not in fusion
  expect_false(is_edge_fused(3, "E", fused))  # Different piece
})

test_that("get_piece_fusion_group: returns correct group", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    size = c(300, 300),
    seed = 42,
    save_files = FALSE
  )

  fusion_groups <- list(c(1, 2), c(8, 9))
  fused <- compute_fused_edges(fusion_groups, result)

  expect_equal(get_piece_fusion_group(1, fused), 1)
  expect_equal(get_piece_fusion_group(2, fused), 1)
  expect_equal(get_piece_fusion_group(8, fused), 2)
  expect_equal(get_piece_fusion_group(9, fused), 2)
  expect_true(is.na(get_piece_fusion_group(5, fused)))  # Not in any group
})

test_that("get_group_boundary_edges: returns correct boundary", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    size = c(300, 300),
    seed = 42,
    save_files = FALSE
  )

  # Fuse 2x2 block
  fusion_groups <- list(c(1, 2, 4, 5))
  fused <- compute_fused_edges(fusion_groups, result)

  boundary <- get_group_boundary_edges(fusion_groups[[1]], result, fused)

  # 2x2 block should have 8 boundary edges:
  # Top: 1-N, 2-N (both puzzle boundary)
  # Right: 2-E, 5-E (connect to pieces 3, 6)
  # Bottom: 4-S, 5-S (connect to pieces 7, 8)
  # Left: 1-W, 4-W (both puzzle boundary)
  expect_equal(nrow(boundary), 8)

  # Check puzzle boundaries
  puzzle_boundaries <- boundary[boundary$is_puzzle_boundary, ]
  expect_equal(nrow(puzzle_boundaries), 4)  # N and W edges of pieces 1, 2, 4
})

test_that("compute_fused_edges: hexagonal fusion", {
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(3),
    size = c(200),
    seed = 42,
    save_files = FALSE
  )

  # Fuse center (piece 1) with one ring-1 piece (piece 2)
  fusion_groups <- list(c(1, 2))
  fused <- compute_fused_edges(fusion_groups, result)

  # Should have 2 fused edges (the shared edge from both perspectives)
  expect_equal(length(fused$fused_edges), 2)

  # Verify edge fusion check works
  expect_true(is_edge_fused(1, "0", fused))  # Center's side 0 connects to piece 2
})

test_that("compute_fused_edges: concentric fusion", {
  result <- generate_puzzle(
    type = "concentric",
    grid = c(3),
    size = c(200),
    seed = 42,
    save_files = FALSE
  )

  # Fuse center (piece 1) with one ring-1 piece (piece 2)
  fusion_groups <- list(c(1, 2))
  fused <- compute_fused_edges(fusion_groups, result)

  # Should have fused edges
  expect_true(length(fused$fused_edges) >= 2)

  # Piece 2's INNER edge should be fused (connects to center)
  expect_true(is_edge_fused(2, "INNER", fused))
})
