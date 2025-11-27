#!/usr/bin/env Rscript
# Trace ALL neighbor detections

source("R/hexagonal_topology.R")

rings <- 3
diameter <- 240
num_pieces <- 3 * rings * (rings - 1) + 1
piece_radius <- diameter / (rings * 4)

# Calculate vertices
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

# Find all neighbors
cat("All Neighbor Relationships:\n")
cat("===========================\n\n")

neighbor_count <- 0

for (piece_id in 1:num_pieces) {
  vertices <- piece_vertices[[piece_id]]
  
  for (side in 0:5) {
    v1 <- vertices[[side + 1]]
    v2 <- vertices[[(side + 1) %% 6 + 1]]
    
    # Find neighbor
    neighbor_id <- NA
    neighbor_side <- NA
    tol <- 0.01
    
    for (test_id in 1:num_pieces) {
      if (test_id == piece_id) next
      
      test_vertices <- piece_vertices[[test_id]]
      
      for (test_side in 0:5) {
        test_v1 <- test_vertices[[test_side + 1]]
        test_v2 <- test_vertices[[(test_side + 1) %% 6 + 1]]
        
        if ((all(abs(v1 - test_v2) < tol) && all(abs(v2 - test_v1) < tol)) ||
            (all(abs(v1 - test_v1) < tol) && all(abs(v2 - test_v2) < tol))) {
          neighbor_id <- test_id
          neighbor_side <- test_side
          break
        }
      }
      if (!is.na(neighbor_id)) break
    }
    
    if (!is.na(neighbor_id)) {
      cat(sprintf("Piece %2d side %d ←→ Piece %2d side %d\n",
                  piece_id, side, neighbor_id, neighbor_side))
      neighbor_count <- neighbor_count + 1
    }
  }
}

cat(sprintf("\nTotal neighbor relationships found: %d\n", neighbor_count))
cat(sprintf("Expected (each internal edge counted twice): %d\n", 12 * 2))  # 12 internal edges in ring 0-1
