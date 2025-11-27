#!/usr/bin/env Rscript
source("R/hexagonal_topology.R")

rings <- 3
piece_radius <- 240 / (rings * 4)
ring_spacing <- sqrt(3) * piece_radius
base_offset <- pi / 6

calc_vertices <- function(piece_id, rings, piece_radius, ring_spacing) {
  ring_info <- map_piece_id_to_ring(piece_id, rings)
  
  if (ring_info$ring == 0) {
    center_x <- 0
    center_y <- 0
  } else {
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
  
  return(list(center = c(center_x, center_y), vertices = vertices))
}

p2 <- calc_vertices(2, rings, piece_radius, ring_spacing)
p8 <- calc_vertices(8, rings, piece_radius, ring_spacing)

cat("Piece 2 vertices:\n")
for (i in 1:6) {
  cat(sprintf("  V%d: (%.2f, %.2f)\n", i-1, p2$vertices[[i]][1], p2$vertices[[i]][2]))
}

cat("\nPiece 8 vertices:\n")
for (i in 1:6) {
  cat(sprintf("  V%d: (%.2f, %.2f)\n", i-1, p8$vertices[[i]][1], p8$vertices[[i]][2]))
}

cat("\nLooking for shared vertices...\n")
for (i in 1:6) {
  for (j in 1:6) {
    diff <- sqrt(sum((p2$vertices[[i]] - p8$vertices[[j]])^2))
    if (diff < 0.1) {
      cat(sprintf("MATCH: Piece 2 V%d = Piece 8 V%d (distance: %.4f)\n", i-1, j-1, diff))
    }
  }
}
