# Tests for Hexagonal Topology Utilities
# Converted from standalone tests/test_hexagonal_topology.R

# =============================================================================
# PIECE ID MAPPING TESTS
# =============================================================================

test_that("piece 1 maps to ring 0 (center)", {
  result <- map_piece_id_to_ring(1, rings = 3)

  expect_equal(result$ring, 0)
  expect_equal(result$position, 0)
})

test_that("ring structure has correct piece counts", {
  rings <- 3
  num_pieces <- 3 * rings * (rings - 1) + 1  # 19 pieces

  ring_assignments <- sapply(seq_len(num_pieces), function(i) {
    map_piece_id_to_ring(i, rings)$ring
  })

  # Count pieces per ring
  ring_counts <- table(ring_assignments)

  # Ring 0 should have 1 piece (center)
  expect_equal(as.integer(ring_counts["0"]), 1)

  # Ring 1 should have 6 pieces
  expect_equal(as.integer(ring_counts["1"]), 6)

  # Ring 2 should have 12 pieces
  expect_equal(as.integer(ring_counts["2"]), 12)
})

test_that("all pieces map to valid rings", {
  rings <- 4
  num_pieces <- 3 * rings * (rings - 1) + 1  # 37 pieces

  for (i in seq_len(num_pieces)) {
    result <- map_piece_id_to_ring(i, rings)
    expect_true(result$ring >= 0 && result$ring < rings)
  }
})

# =============================================================================
# CARTESIAN CONVERSION TESTS
# =============================================================================

test_that("ring 0 (center) maps to origin", {
  center <- hex_ring_to_cartesian(ring = 0, angle = 0, ring_spacing = 1.0)

  expect_equal(center$x, 0, tolerance = 0.001)
  expect_equal(center$y, 0, tolerance = 0.001)
})

test_that("ring 1 at angle 0 maps to (ring_spacing, 0)", {
  spacing <- 10
  right <- hex_ring_to_cartesian(ring = 1, angle = 0, ring_spacing = spacing)

  expect_equal(right$x, spacing, tolerance = 0.01)
  expect_equal(right$y, 0, tolerance = 0.001)
})
test_that("ring 1 at angle pi/3 maps correctly", {
  spacing <- 10
  upper_right <- hex_ring_to_cartesian(ring = 1, angle = pi/3, ring_spacing = spacing)

  expected_x <- spacing * cos(pi/3)  # 5.0
  expected_y <- spacing * sin(pi/3)  # 8.66

  expect_equal(upper_right$x, expected_x, tolerance = 0.01)
  expect_equal(upper_right$y, expected_y, tolerance = 0.01)
})

test_that("ring 2 at angle pi/2 maps to (0, 2*spacing)", {
  spacing <- 10
  top <- hex_ring_to_cartesian(ring = 2, angle = pi/2, ring_spacing = spacing)

  expect_equal(top$x, 0, tolerance = 0.01)
  expect_equal(top$y, 2 * spacing, tolerance = 0.01)
})

# =============================================================================
# POSITION CALCULATION TESTS
# =============================================================================

test_that("piece 1 is positioned near origin", {
  rings <- 3
  base_spacing <- 50
  separation_factor <- 1.0

  pos <- calculate_hex_piece_position(1, rings, base_spacing, separation_factor)

  expect_lt(abs(pos$x), base_spacing)
  expect_lt(abs(pos$y), base_spacing)
})

test_that("separation factor affects distance", {
  rings <- 3
  base_spacing <- 50

  pos_low <- calculate_hex_piece_position(2, rings, base_spacing, separation_factor = 1.0)
  pos_high <- calculate_hex_piece_position(2, rings, base_spacing, separation_factor = 1.5)

  dist_low <- sqrt(pos_low$x^2 + pos_low$y^2)
  dist_high <- sqrt(pos_high$x^2 + pos_high$y^2)

  # Higher separation factor should increase distance
  expect_gt(dist_high, dist_low)
})

test_that("all pieces have distinct positions", {
  rings <- 2
  num_pieces <- 3 * rings * (rings - 1) + 1  # 7 pieces
  base_spacing <- 30

  positions <- lapply(seq_len(num_pieces), function(i) {
    calculate_hex_piece_position(i, rings, base_spacing, separation_factor = 1.2)
  })

  # Check all pairwise distances are non-zero
  for (i in seq_len(num_pieces - 1)) {
    for (j in (i + 1):num_pieces) {
      dist <- sqrt((positions[[i]]$x - positions[[j]]$x)^2 +
                   (positions[[i]]$y - positions[[j]]$y)^2)
      expect_gt(dist, 0.1)
    }
  }
})
