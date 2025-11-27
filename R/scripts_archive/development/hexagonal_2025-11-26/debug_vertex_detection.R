#!/usr/bin/env Rscript
# Debug vertex-based neighbor detection

source("R/hexagonal_topology.R")
source("R/hexagonal_bezier_generation.R")

rings <- 3
diameter <- 240
num_pieces <- 3 * rings * (rings - 1) + 1
piece_radius <- diameter / (rings * 4)

# Calculate vertices for all pieces (same as in generate_hex_edge_map)
piece_vertices <- list()
base_offset <- pi / 6

for (piece_id in 1:num_pieces) {
  ring_info <- map_piece_id_to_ring(piece_id, rings)
  
  if (ring_info$ring == 0) {
    center_x <- 0
    center_y <- 0
  } else {
    ring_spacing <- sqrt(3) * piece_radius
    angle <- ring_info$angle
    center_x <- ring_info$ring * ring_spacing * cos(angle)
    center_y <- ring_info$ring * ring_spacing * sin(angle)
  }
  
  vertices <- list()
  for (i in 0:5) {
    vertex_angle <- i * pi / 3 + base_offset
    vx <- center_x + piece_radius * cos(vertex_angle)
    vy <- center_y + piece_radius * sin(vertex_angle)
    vertices[[i + 1]] <- c(vx, vy)
  }
  piece_vertices[[piece_id]] <- vertices
}

# Test piece 2, side 5 (should connect to piece 8, side 2)
cat("Testing Piece 2, Side 5\n")
cat("=======================\n\n")

piece_id <- 2
side <- 5
vertices <- piece_vertices[[piece_id]]

v1 <- vertices[[side + 1]]
v2 <- vertices[[(side + 1) %% 6 + 1]]

cat(sprintf("Edge vertices: v1=(%.2f, %.2f), v2=(%.2f, %.2f)\n\n",
            v1[1], v1[2], v2[1], v2[2]))

# Search for neighbor
neighbor_found <- FALSE
tol <- 0.01

for (test_id in 1:num_pieces) {
  if (test_id == piece_id) next
  
  test_vertices <- piece_vertices[[test_id]]
  
  for (test_side in 0:5) {
    test_v1 <- test_vertices[[test_side + 1]]
    test_v2 <- test_vertices[[(test_side + 1) %% 6 + 1]]
    
    # Check reverse match
    match_reverse <- all(abs(v1 - test_v2) < tol) && all(abs(v2 - test_v1) < tol)
    # Check forward match
    match_forward <- all(abs(v1 - test_v1) < tol) && all(abs(v2 - test_v2) < tol)
    
    if (match_reverse || match_forward) {
      cat(sprintf("FOUND: Piece %d side %d (match_reverse=%s, match_forward=%s)\n",
                  test_id, test_side, match_reverse, match_forward))
      cat(sprintf("  Test edge: v1=(%.2f, %.2f), v2=(%.2f, %.2f)\n",
                  test_v1[1], test_v1[2], test_v2[1], test_v2[2]))
      neighbor_found <- TRUE
      break
    }
  }
  if (neighbor_found) break
}

if (!neighbor_found) {
  cat("NO NEIGHBOR FOUND!\n")
  cat("Checking distances to piece 8 vertices:\n")
  test_vertices <- piece_vertices[[8]]
  for (i in 1:6) {
    dist1 <- sqrt(sum((v1 - test_vertices[[i]])^2))
    dist2 <- sqrt(sum((v2 - test_vertices[[i]])^2))
    cat(sprintf("  Piece 8 V%d: dist to v1=%.4f, dist to v2=%.4f\n", i-1, dist1, dist2))
  }
}
