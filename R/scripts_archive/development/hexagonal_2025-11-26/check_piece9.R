#!/usr/bin/env Rscript
source("R/hexagonal_topology.R")

rings <- 3
piece_radius <- 240 / (rings * 4)
ring_spacing <- sqrt(3) * piece_radius
base_offset <- pi / 6

calc_vertices <- function(piece_id) {
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
  
  list(center = c(center_x, center_y), vertices = vertices, ring_info = ring_info)
}

p2 <- calc_vertices(2)
p9 <- calc_vertices(9)

cat("Piece 2 (ring 1, position 0, angle 0°):\n")
cat(sprintf("  Center: (%.2f, %.2f)\n", p2$center[1], p2$center[2]))
for (i in 1:6) {
  cat(sprintf("  V%d: (%.2f, %.2f)\n", i-1, p2$vertices[[i]][1], p2$vertices[[i]][2]))
}

cat("\nPiece 9 (ring 2, position 1, angle 30°):\n")
cat(sprintf("  Center: (%.2f, %.2f)\n", p9$center[1], p9$center[2]))
for (i in 1:6) {
  cat(sprintf("  V%d: (%.2f, %.2f)\n", i-1, p9$vertices[[i]][1], p9$vertices[[i]][2]))
}

cat("\nLooking for shared vertices...\n")
for (i in 1:6) {
  for (j in 1:6) {
    diff <- sqrt(sum((p2$vertices[[i]] - p9$vertices[[j]])^2))
    if (diff < 0.1) {
      cat(sprintf("MATCH: Piece 2 V%d = Piece 9 V%d (distance: %.4f)\n", i-1, j-1, diff))
    }
  }
}

if (sum(sapply(1:6, function(i) {
  any(sapply(1:6, function(j) {
    sqrt(sum((p2$vertices[[i]] - p9$vertices[[j]])^2)) < 0.1
  }))
})) == 0) {
  cat("NO SHARED VERTICES FOUND!\n")
  cat("\nThis explains why piece 9 has no neighbors.\n")
  cat("The issue: Ring 2 pieces at odd positions don't share vertices with ring 1.\n")
}
