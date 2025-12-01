#!/usr/bin/env Rscript
# Debug vertex warping

source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")

cat("=== Debug Vertex Warping ===\n\n")

rings <- 2
diameter <- 240
piece_radius <- diameter / (rings * 4)
puzzle_radius <- piece_radius * (rings * 2 - 1)

cat(sprintf("piece_radius = %.2f\n", piece_radius))
cat(sprintf("puzzle_radius = %.2f\n\n", puzzle_radius))

# Calculate vertices for piece 2 (outer ring)
piece_id <- 2
axial_coords <- map_piece_id_to_axial(piece_id, rings)
cart_coords <- axial_to_cartesian(axial_coords$q, axial_coords$r, piece_radius)
center_x <- cart_coords$x
center_y <- cart_coords$y

cat(sprintf("Piece %d center: (%.2f, %.2f)\n\n", piece_id, center_x, center_y))

# Calculate vertices with and without warp
cat("Vertices for piece 2:\n")
cat("Side | Original (x, y)     | Dist | Boundary? | Warped (x, y)      | Dist\n")
cat("-----|---------------------|------|-----------|--------------------|----- \n")

for (i in 0:5) {
  vertex_angle <- i * pi / 3
  vx <- center_x + piece_radius * cos(vertex_angle)
  vy <- center_y + piece_radius * sin(vertex_angle)
  orig_dist <- sqrt(vx^2 + vy^2)

  is_boundary <- is_boundary_vertex(vx, vy, puzzle_radius, tolerance = piece_radius * 0.1)

  if (is_boundary) {
    warped <- apply_hex_warp(vx, vy)
    warp_x <- warped$x
    warp_y <- warped$y
    warp_dist <- sqrt(warp_x^2 + warp_y^2)
    cat(sprintf("  %d  | (%7.2f, %7.2f) | %5.2f | YES       | (%7.2f, %7.2f) | %5.2f\n",
                i, vx, vy, orig_dist, warp_x, warp_y, warp_dist))
  } else {
    cat(sprintf("  %d  | (%7.2f, %7.2f) | %5.2f | NO        | (same)             |\n",
                i, vx, vy, orig_dist))
  }
}

cat("\n=== Boundary detection ===\n")
cat(sprintf("Boundary range: %.2f to %.2f\n",
            puzzle_radius * sqrt(0.75) - piece_radius * 0.1,
            puzzle_radius + piece_radius * 0.1))
