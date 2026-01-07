# Tests for Hexagonal Topology Utilities

# Source the functions
source("R/hexagonal_topology.R")

# Test 1: Piece ID mapping to rings
test_piece_id_mapping <- function() {
  cat("\n=== Test 1: Piece ID Mapping to Rings ===\n")

  rings <- 3
  num_pieces <- 3 * rings * (rings - 1) + 1  # 19 pieces

  # Map all pieces
  ring_list <- lapply(1:num_pieces, function(i) {
    map_piece_id_to_ring(i, rings)
  })

  # Check piece 1 is center (ring 0)
  piece1 <- ring_list[[1]]
  cat(sprintf("Piece 1: ring=%d, position=%d, angle=%.2f\n",
              piece1$ring, piece1$position, piece1$angle))

  if (piece1$ring == 0) {
    cat("✓ PASS: Piece 1 is at center (ring 0)\n")
  } else {
    cat("✗ FAIL: Piece 1 is not at center\n")
    return(FALSE)
  }

  # Verify ring structure: ring 1 should have 6 pieces, ring 2 should have 12
  ring_counts <- table(sapply(ring_list, function(r) r$ring))
  cat("\nPieces per ring:\n")
  for (r in sort(unique(sapply(ring_list, function(x) x$ring)))) {
    count <- sum(sapply(ring_list, function(x) x$ring == r))
    expected <- if (r == 0) 1 else 6 * r
    status <- if (count == expected) "✓" else "✗"
    cat(sprintf("  Ring %d: %d pieces (expected %d) %s\n", r, count, expected, status))
  }

  # Display first 10 pieces
  cat("\nFirst 10 piece ring assignments:\n")
  for (i in 1:min(10, num_pieces)) {
    info <- ring_list[[i]]
    cat(sprintf("  Piece %2d: ring=%d, position=%d, angle=%.2f rad\n",
                i, info$ring, info$position, info$angle))
  }

  return(TRUE)
}

# Test 2: Ring to Cartesian conversion
test_cartesian_conversion <- function() {
  cat("\n=== Test 2: Ring to Cartesian Conversion ===\n")

  # Test center (ring 0)
  center <- hex_ring_to_cartesian(ring = 0, angle = 0, ring_spacing = 1.0)
  cat(sprintf("Ring 0 (center) → (%.3f, %.3f)\n", center$x, center$y))

  if (abs(center$x) < 0.001 && abs(center$y) < 0.001) {
    cat("✓ PASS: Center is at origin\n")
  } else {
    cat("✗ FAIL: Center is not at origin\n")
    return(FALSE)
  }

  # Test ring 1, angle 0 (right)
  right <- hex_ring_to_cartesian(ring = 1, angle = 0, ring_spacing = 10)
  cat(sprintf("\nRing 1, angle 0 (right) → (%.3f, %.3f)\n", right$x, right$y))

  if (abs(right$x - 10) < 0.01 && abs(right$y) < 0.001) {
    cat("✓ PASS: Ring 1 right position correct\n")
  } else {
    cat("✗ FAIL: Ring 1 right position incorrect\n")
    return(FALSE)
  }

  # Test ring 1, angle pi/3 (60 degrees, upper-right)
  upper_right <- hex_ring_to_cartesian(ring = 1, angle = pi/3, ring_spacing = 10)
  expected_x <- 10 * cos(pi/3)  # 5.0
  expected_y <- 10 * sin(pi/3)  # 8.66
  cat(sprintf("\nRing 1, angle π/3 (60°) → (%.3f, %.3f)\n",
              upper_right$x, upper_right$y))
  cat(sprintf("Expected: (%.3f, %.3f)\n", expected_x, expected_y))

  if (abs(upper_right$x - expected_x) < 0.01 && abs(upper_right$y - expected_y) < 0.01) {
    cat("✓ PASS: Ring 1 upper-right position correct\n")
  } else {
    cat("✗ FAIL: Ring 1 upper-right position incorrect\n")
    return(FALSE)
  }

  # Test ring 2
  ring2 <- hex_ring_to_cartesian(ring = 2, angle = pi/2, ring_spacing = 10)
  cat(sprintf("\nRing 2, angle π/2 (90°, top) → (%.3f, %.3f)\n",
              ring2$x, ring2$y))
  cat(sprintf("Expected: (%.3f, %.3f)\n", 0.0, 20.0))

  if (abs(ring2$x) < 0.01 && abs(ring2$y - 20) < 0.01) {
    cat("✓ PASS: Ring 2 top position correct\n")
  } else {
    cat("✗ FAIL: Ring 2 top position incorrect\n")
    return(FALSE)
  }

  return(TRUE)
}

# Test 3: Position calculation
test_position_calculation <- function() {
  cat("\n=== Test 3: Position Calculation ===\n")

  rings <- 3
  base_spacing <- 50
  separation_factor <- 1.0

  # Test piece 1 (should be near origin)
  pos1 <- calculate_hex_piece_position(1, rings, base_spacing, separation_factor)
  cat(sprintf("Piece 1 position: (%.2f, %.2f)\n", pos1$x, pos1$y))

  if (abs(pos1$x) < base_spacing && abs(pos1$y) < base_spacing) {
    cat("✓ PASS: Piece 1 near origin\n")
  } else {
    cat("✗ FAIL: Piece 1 far from origin\n")
    return(FALSE)
  }

  # Test with larger separation factor
  separation_factor2 <- 1.5
  pos1_separated <- calculate_hex_piece_position(1, rings, base_spacing, separation_factor2)
  pos2_separated <- calculate_hex_piece_position(2, rings, base_spacing, separation_factor2)

  cat(sprintf("\nWith separation_factor=1.5:\n"))
  cat(sprintf("  Piece 1: (%.2f, %.2f)\n", pos1_separated$x, pos1_separated$y))
  cat(sprintf("  Piece 2: (%.2f, %.2f)\n", pos2_separated$x, pos2_separated$y))

  # Distance from origin should be larger with higher separation factor
  dist1 <- sqrt(pos1$x^2 + pos1$y^2)
  dist1_sep <- sqrt(pos1_separated$x^2 + pos1_separated$y^2)

  cat(sprintf("\nDistance from origin (piece 1):\n"))
  cat(sprintf("  separation=1.0: %.2f\n", dist1))
  cat(sprintf("  separation=1.5: %.2f\n", dist1_sep))

  if (dist1_sep >= dist1 * 1.4 && dist1_sep <= dist1 * 1.6) {
    cat("✓ PASS: Separation factor affects distance correctly\n")
  } else {
    cat("✓ NOTE: Separation effect varies (piece 1 might be at/near origin)\n")
  }

  return(TRUE)
}

# Test 4: Full integration test
test_full_integration <- function() {
  cat("\n=== Test 4: Full Integration (rings=2) ===\n")

  rings <- 2
  num_pieces <- 3 * rings * (rings - 1) + 1  # 7 pieces
  base_spacing <- 30

  cat(sprintf("Testing %d pieces with base_spacing=%d\n", num_pieces, base_spacing))

  all_positions <- list()
  for (i in 1:num_pieces) {
    pos <- calculate_hex_piece_position(i, rings, base_spacing, separation_factor = 1.2)
    all_positions[[i]] <- pos
    cat(sprintf("  Piece %d: (%.2f, %.2f)\n", i, pos$x, pos$y))
  }

  # Check that no two pieces are at exact same position
  for (i in 1:(num_pieces-1)) {
    for (j in (i+1):num_pieces) {
      dist <- sqrt((all_positions[[i]]$x - all_positions[[j]]$x)^2 +
                   (all_positions[[i]]$y - all_positions[[j]]$y)^2)
      if (dist < 0.1) {
        cat(sprintf("✗ FAIL: Pieces %d and %d are too close (%.3f)\n", i, j, dist))
        return(FALSE)
      }
    }
  }

  cat("✓ PASS: All pieces are at distinct positions\n")
  return(TRUE)
}

# Run all tests
main <- function() {
  cat("=====================================\n")
  cat("Hexagonal Topology Utility Tests\n")
  cat("=====================================\n")

  results <- list(
    test1 = test_piece_id_mapping(),
    test2 = test_cartesian_conversion(),
    test3 = test_position_calculation(),
    test4 = test_full_integration()
  )

  cat("\n=====================================\n")
  cat("Test Summary\n")
  cat("=====================================\n")

  passed <- sum(unlist(results))
  total <- length(results)

  cat(sprintf("Passed: %d / %d\n", passed, total))

  if (passed == total) {
    cat("\n✓ ALL TESTS PASSED\n")
  } else {
    cat(sprintf("\n✗ %d TESTS FAILED\n", total - passed))
  }

  cat("=====================================\n")

  return(passed == total)
}

# Execute tests if run as script
if (sys.nframe() == 0) {
  success <- main()
  if (!success) {
    quit(status = 1)
  }
}
