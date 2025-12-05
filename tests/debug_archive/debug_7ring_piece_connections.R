# Debug 7-ring puzzle piece connections
# Investigate pieces 4, 12, 13, 26, 27, 28 specifically

cat("=== Debug 7-Ring Puzzle Piece Connections ===\n\n")

source("R/logging.R")
source("R/concentric_geometry.R")
source("R/hexagonal_bezier_generation.R")
source("R/concentric_edge_generation.R")

# Parameters matching user's test
rings <- 7
diameter <- 500
seed <- 42
tabsize <- diameter * 0.10  # 10% = 50mm

num_pieces <- get_concentric_piece_count(rings)
piece_height <- get_concentric_piece_height(diameter, rings)

cat(sprintf("Rings: %d\n", rings))
cat(sprintf("Diameter: %d mm\n", diameter))
cat(sprintf("Piece height: %.2f mm\n", piece_height))
cat(sprintf("Tab size: %.2f mm (10%%)\n", tabsize))
cat(sprintf("Total pieces: %d\n\n", num_pieces))

# Ring breakdown
cat("=== Ring Breakdown ===\n")
cumulative <- 0
for (r in 0:(rings-1)) {
  if (r == 0) {
    pieces <- 1
    first_id <- 1
  } else {
    pieces <- 6 * r
    first_id <- cumulative + 1
  }
  last_id <- cumulative + pieces
  angular_span <- if (r == 0) 360 else 360 / pieces
  cat(sprintf("Ring %d: %d pieces (IDs %d-%d), each spans %.1f°\n",
              r, pieces, first_id, last_id, angular_span))
  cumulative <- cumulative + pieces
}
cat("\n")

# Get all vertices
all_vertices <- get_all_concentric_vertices(rings, diameter, "hexagon")

# Detailed analysis of specific pieces
pieces_to_check <- c(4, 12, 13, 26, 27, 28)

cat("=== Detailed Piece Analysis ===\n\n")

for (piece_id in pieces_to_check) {
  piece <- all_vertices[[piece_id]]

  cat(sprintf("======== PIECE %d ========\n", piece_id))
  cat(sprintf("Type: %s\n", piece$type))

  if (piece$type == "hexagon") {
    cat("Ring: 0 (center)\n")
    cat(sprintf("Vertices: %d\n", length(piece$vertices)))
    for (i in seq_along(piece$vertices)) {
      v <- piece$vertices[[i]]
      angle <- atan2(v[2], v[1]) * 180 / pi
      cat(sprintf("  V%d: (%.2f, %.2f) at %.1f°\n", i, v[1], v[2], angle))
    }
  } else {
    cat(sprintf("Ring: %d, Position: %d\n", piece$ring, piece$position))
    cat(sprintf("Angular range: %.1f° to %.1f°\n",
                piece$start_angle * 180/pi, piece$end_angle * 180/pi))

    cat("Vertices:\n")
    vertex_names <- c("inner-start", "inner-end", "outer-end", "outer-start")
    for (i in 1:4) {
      v <- piece$vertices[[i]]
      angle <- atan2(v[2], v[1]) * 180 / pi
      dist <- sqrt(v[1]^2 + v[2]^2)
      cat(sprintf("  V%d (%s): (%.2f, %.2f) at %.1f°, r=%.2f\n",
                  i, vertex_names[i], v[1], v[2], angle, dist))
    }

    # Get neighbors
    cat("\nNeighbors:\n")

    # INNER neighbor
    inner_neighbor <- get_inner_neighbor(piece_id, rings)
    if (length(inner_neighbor) > 0) {
      inner_info <- all_vertices[[inner_neighbor]]
      cat(sprintf("  INNER -> Piece %d (ring %d, pos %d)\n",
                  inner_neighbor, inner_info$ring, inner_info$position))
      cat(sprintf("    Angular range: %.1f° to %.1f°\n",
                  inner_info$start_angle * 180/pi, inner_info$end_angle * 180/pi))
    }

    # OUTER neighbors
    outer_neighbors <- get_outer_neighbors(piece_id, rings)
    if (length(outer_neighbors) > 0) {
      cat(sprintf("  OUTER -> Pieces %s\n", paste(outer_neighbors, collapse=", ")))
      for (nid in outer_neighbors) {
        ninfo <- all_vertices[[nid]]
        cat(sprintf("    Piece %d: ring %d, pos %d, %.1f° to %.1f°\n",
                    nid, ninfo$ring, ninfo$position,
                    ninfo$start_angle * 180/pi, ninfo$end_angle * 180/pi))
      }
    } else {
      cat("  OUTER -> BOUNDARY\n")
    }

    # Circumferential neighbors
    pieces_in_ring <- 6 * piece$ring
    ring_start <- 3 * piece$ring * (piece$ring - 1) + 2
    next_pos <- (piece$position + 1) %% pieces_in_ring
    prev_pos <- (piece$position - 1 + pieces_in_ring) %% pieces_in_ring
    right_neighbor <- ring_start + next_pos
    left_neighbor <- ring_start + prev_pos
    cat(sprintf("  RIGHT -> Piece %d\n", right_neighbor))
    cat(sprintf("  LEFT -> Piece %d\n", left_neighbor))
  }

  cat("\n")
}

# Now check the OUTER edge alignment specifically for pieces 4, 12, 13
cat("=== OUTER Edge Alignment Check ===\n\n")

# Piece 4 (ring 1) -> connects to ring 2
cat("--- Piece 4 (ring 1, pos 2) OUTER edge ---\n")
piece4 <- all_vertices[[4]]
v3_4 <- piece4$vertices[[3]]
v4_4 <- piece4$vertices[[4]]
cat(sprintf("V3 (outer-end): (%.2f, %.2f) at %.1f°\n", v3_4[1], v3_4[2], atan2(v3_4[2], v3_4[1]) * 180/pi))
cat(sprintf("V4 (outer-start): (%.2f, %.2f) at %.1f°\n", v4_4[1], v4_4[2], atan2(v4_4[2], v4_4[1]) * 180/pi))

outer_neighbors_4 <- get_outer_neighbors(4, rings)
cat(sprintf("Outer neighbors: %s\n", paste(outer_neighbors_4, collapse=", ")))
for (nid in outer_neighbors_4) {
  ninfo <- all_vertices[[nid]]
  nv1 <- ninfo$vertices[[1]]
  nv2 <- ninfo$vertices[[2]]
  cat(sprintf("  Piece %d inner edge: (%.2f,%.2f) to (%.2f,%.2f)\n",
              nid, nv1[1], nv1[2], nv2[1], nv2[2]))
  cat(sprintf("    V1 at %.1f°, V2 at %.1f°\n",
              atan2(nv1[2], nv1[1]) * 180/pi, atan2(nv2[2], nv2[1]) * 180/pi))
}

cat("\n--- Piece 12 (ring 2) OUTER edge ---\n")
piece12 <- all_vertices[[12]]
v3_12 <- piece12$vertices[[3]]
v4_12 <- piece12$vertices[[4]]
cat(sprintf("V3 (outer-end): (%.2f, %.2f) at %.1f°\n", v3_12[1], v3_12[2], atan2(v3_12[2], v3_12[1]) * 180/pi))
cat(sprintf("V4 (outer-start): (%.2f, %.2f) at %.1f°\n", v4_12[1], v4_12[2], atan2(v4_12[2], v4_12[1]) * 180/pi))

outer_neighbors_12 <- get_outer_neighbors(12, rings)
cat(sprintf("Outer neighbors: %s\n", paste(outer_neighbors_12, collapse=", ")))
for (nid in outer_neighbors_12) {
  ninfo <- all_vertices[[nid]]
  nv1 <- ninfo$vertices[[1]]
  nv2 <- ninfo$vertices[[2]]
  cat(sprintf("  Piece %d inner edge: (%.2f,%.2f) to (%.2f,%.2f)\n",
              nid, nv1[1], nv1[2], nv2[1], nv2[2]))
}

cat("\n--- Piece 13 (ring 2) OUTER edge ---\n")
piece13 <- all_vertices[[13]]
v3_13 <- piece13$vertices[[3]]
v4_13 <- piece13$vertices[[4]]
cat(sprintf("V3 (outer-end): (%.2f, %.2f) at %.1f°\n", v3_13[1], v3_13[2], atan2(v3_13[2], v3_13[1]) * 180/pi))
cat(sprintf("V4 (outer-start): (%.2f, %.2f) at %.1f°\n", v4_13[1], v4_13[2], atan2(v4_13[2], v4_13[1]) * 180/pi))

outer_neighbors_13 <- get_outer_neighbors(13, rings)
cat(sprintf("Outer neighbors: %s\n", paste(outer_neighbors_13, collapse=", ")))
for (nid in outer_neighbors_13) {
  ninfo <- all_vertices[[nid]]
  nv1 <- ninfo$vertices[[1]]
  nv2 <- ninfo$vertices[[2]]
  cat(sprintf("  Piece %d inner edge: (%.2f,%.2f) to (%.2f,%.2f)\n",
              nid, nv1[1], nv1[2], nv2[1], nv2[2]))
}

# Check piece 27's INNER edge
cat("\n--- Piece 27 (ring 3) INNER edge ---\n")
piece27 <- all_vertices[[27]]
v1_27 <- piece27$vertices[[1]]
v2_27 <- piece27$vertices[[2]]
cat(sprintf("V1 (inner-start): (%.2f, %.2f) at %.1f°\n", v1_27[1], v1_27[2], atan2(v1_27[2], v1_27[1]) * 180/pi))
cat(sprintf("V2 (inner-end): (%.2f, %.2f) at %.1f°\n", v2_27[1], v2_27[2], atan2(v2_27[2], v2_27[1]) * 180/pi))

inner_neighbor_27 <- get_inner_neighbor(27, rings)
cat(sprintf("Inner neighbor: Piece %d\n", inner_neighbor_27))

inner_info <- all_vertices[[inner_neighbor_27]]
cat(sprintf("  Piece %d angular range: %.1f° to %.1f°\n",
            inner_neighbor_27, inner_info$start_angle * 180/pi, inner_info$end_angle * 180/pi))
cat(sprintf("  Piece %d V3: (%.2f, %.2f)\n", inner_neighbor_27, inner_info$vertices[[3]][1], inner_info$vertices[[3]][2]))
cat(sprintf("  Piece %d V4: (%.2f, %.2f)\n", inner_neighbor_27, inner_info$vertices[[4]][1], inner_info$vertices[[4]][2]))

# Check if piece 27's inner edge endpoints match the inner piece's outer corners
cat("\nVertex matching check:\n")
cat(sprintf("  Piece 27 V1 matches Piece %d V3? %s\n", inner_neighbor_27,
            abs(v1_27[1] - inner_info$vertices[[3]][1]) < 0.01 &&
            abs(v1_27[2] - inner_info$vertices[[3]][2]) < 0.01))
cat(sprintf("  Piece 27 V1 matches Piece %d V4? %s\n", inner_neighbor_27,
            abs(v1_27[1] - inner_info$vertices[[4]][1]) < 0.01 &&
            abs(v1_27[2] - inner_info$vertices[[4]][2]) < 0.01))

cat("\n=== Debug Complete ===\n")
