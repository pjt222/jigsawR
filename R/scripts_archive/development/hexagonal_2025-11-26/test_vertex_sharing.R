#!/usr/bin/env Rscript
# Test vertex sharing with new axial coordinate system

source("R/hexagonal_topology.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")

rings <- 3
diameter <- 240
num_pieces <- 3 * rings * (rings - 1) + 1
piece_radius <- diameter / (rings * 4)

# Calculate vertices using new system
piece_vertices <- list()
base_offset <- 0  # Flat-top hexagon

for (piece_id in 1:num_pieces) {
  axial_coords <- map_piece_id_to_axial(piece_id, rings)

  # Use correct hex_size for flat-top hexagons
  # sqrt(3) * hex_size = sqrt(3) * piece_radius (for vertical neighbors to touch)
  hex_size <- piece_radius

  cart_coords <- axial_to_cartesian(
    q = axial_coords$q,
    r = axial_coords$r,
    hex_size = hex_size
  )

  vertices <- list()
  for (i in 0:5) {
    vertex_angle <- i * pi / 3 + base_offset
    vx <- cart_coords$x + piece_radius * cos(vertex_angle)
    vy <- cart_coords$y + piece_radius * sin(vertex_angle)
    vertices[[i + 1]] <- c(vx, vy)
  }
  piece_vertices[[piece_id]] <- vertices
}

cat("Testing vertex sharing with hexagonal lattice coordinates\n")
cat("=========================================================\n\n")

# Test pieces 2 and 9 (should be neighbors)
p2 <- piece_vertices[[2]]
p9 <- piece_vertices[[9]]

cat("Piece 2 (ring 1, position 0):\n")
for (i in 1:6) {
  cat(sprintf("  V%d: (%.2f, %.2f)\n", i-1, p2[[i]][1], p2[[i]][2]))
}

cat("\nPiece 9 (ring 2, position 1):\n")
for (i in 1:6) {
  cat(sprintf("  V%d: (%.2f, %.2f)\n", i-1, p9[[i]][1], p9[[i]][2]))
}

cat("\nSearching for shared vertices:\n")
matches <- 0
for (i in 1:6) {
  for (j in 1:6) {
    diff <- sqrt(sum((p2[[i]] - p9[[j]])^2))
    if (diff < 0.1) {
      cat(sprintf("  ✓ MATCH: Piece 2 V%d = Piece 9 V%d (distance: %.4f)\n", i-1, j-1, diff))
      matches <- matches + 1
    }
  }
}

if (matches >= 2) {
  cat(sprintf("\n✓ SUCCESS: Found %d shared vertices (expected 2)\n", matches))
} else {
  cat(sprintf("\n✗ FAILED: Only found %d shared vertices (expected 2)\n", matches))
}

# Count all neighbor relationships
cat("\n\nCounting ALL neighbor relationships:\n")
neighbor_count <- 0

for (piece_id in 1:num_pieces) {
  vertices <- piece_vertices[[piece_id]]

  for (side in 0:5) {
    v1 <- vertices[[side + 1]]
    v2 <- vertices[[(side + 1) %% 6 + 1]]

    # Find neighbor
    for (test_id in (piece_id+1):num_pieces) {
      if (test_id > num_pieces) break

      test_vertices <- piece_vertices[[test_id]]

      for (test_side in 0:5) {
        test_v1 <- test_vertices[[test_side + 1]]
        test_v2 <- test_vertices[[(test_side + 1) %% 6 + 1]]

        tol <- 0.01
        if ((all(abs(v1 - test_v2) < tol) && all(abs(v2 - test_v1) < tol)) ||
            (all(abs(v1 - test_v1) < tol) && all(abs(v2 - test_v2) < tol))) {
          neighbor_count <- neighbor_count + 1
          break
        }
      }
    }
  }
}

cat(sprintf("Total internal edges found: %d\n", neighbor_count))
cat("Expected internal edges for 3 rings:\n")
cat("  - Ring 0-1 connections: 6 edges\n")
cat("  - Ring 1-2 connections: 12 edges\n")
cat("  - Ring 1 internal: 0 edges (single ring)\n")
cat("  - Ring 2 internal: 0 edges (single ring)\n")
cat("  - Total: 18 edges\n")

if (neighbor_count == 18) {
  cat("\n✓✓✓ ALL TESTS PASSED! Hexagonal lattice coordinate system working correctly!\n")
} else {
  cat(sprintf("\n✗ Found %d edges, expected 18\n", neighbor_count))
}
