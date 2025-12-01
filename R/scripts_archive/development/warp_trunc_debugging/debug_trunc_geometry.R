#!/usr/bin/env Rscript
# Debug truncation geometry for hexagonal puzzles

source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")

cat("=== Hexagonal Truncation Geometry ===\n\n")

rings <- 2
diameter <- 240
piece_radius <- diameter / (rings * 4)

cat(sprintf("Rings: %d\n", rings))
cat(sprintf("Diameter: %.2f\n", diameter))
cat(sprintf("Piece radius: %.2f\n", piece_radius))

# Calculate the truncation boundary
# In complete mode, the puzzle uses radius = diameter/2
# The hexagonal boundary has:
# - Vertices at distance: radius
# - Edge midpoints at distance: radius * sqrt(0.75)

# For our separated pieces, we need to figure out where the outer vertices are
# and where the truncation boundary should be

# Get outer ring piece positions and their vertices
num_pieces <- 3 * rings * (rings - 1) + 1
cat(sprintf("\nTotal pieces: %d\n", num_pieces))

# Calculate vertices for outer ring pieces
cat("\n=== Outer Ring Pieces (ring 1) ===\n")
for (piece_id in 2:num_pieces) {
  ring_info <- map_piece_id_to_ring(piece_id, rings)
  if (ring_info$ring != rings - 1) next  # Only outer ring

  axial_coords <- map_piece_id_to_axial(piece_id, rings)
  cart_coords <- axial_to_cartesian(axial_coords$q, axial_coords$r, piece_radius)
  center_x <- cart_coords$x
  center_y <- cart_coords$y
  center_dist <- sqrt(center_x^2 + center_y^2)

  cat(sprintf("\nPiece %d: center (%.2f, %.2f), dist=%.2f\n",
              piece_id, center_x, center_y, center_dist))

  # Calculate vertices
  for (i in 0:5) {
    vertex_angle <- i * pi / 3
    vx <- center_x + piece_radius * cos(vertex_angle)
    vy <- center_y + piece_radius * sin(vertex_angle)
    v_dist <- sqrt(vx^2 + vy^2)

    # Check if this is a boundary vertex (shared by < 3 pieces)
    # For outer ring, vertices pointing outward are boundary vertices
    is_boundary <- v_dist > center_dist

    if (is_boundary) {
      cat(sprintf("  V%d: (%.2f, %.2f), dist=%.2f [BOUNDARY]\n",
                  i, vx, vy, v_dist))
    }
  }
}

# Calculate what the truncation boundary should be
# The puzzle radius should inscribe all pieces
cat("\n=== Truncation Boundary ===\n")

# Find maximum vertex distance (this is the natural boundary)
max_dist <- 0
for (piece_id in 1:num_pieces) {
  axial_coords <- map_piece_id_to_axial(piece_id, rings)
  cart_coords <- axial_to_cartesian(axial_coords$q, axial_coords$r, piece_radius)

  for (i in 0:5) {
    vertex_angle <- i * pi / 3
    vx <- cart_coords$x + piece_radius * cos(vertex_angle)
    vy <- cart_coords$y + piece_radius * sin(vertex_angle)
    v_dist <- sqrt(vx^2 + vy^2)
    max_dist <- max(max_dist, v_dist)
  }
}

cat(sprintf("Maximum vertex distance: %.2f\n", max_dist))
cat(sprintf("This should be the truncation circle radius (for do_warp)\n"))

# For hexagonal truncation (do_warp=FALSE), we need a regular hexagon
# that inscribes all pieces. The hexagon vertices are at max_dist,
# and edge midpoints are at max_dist * sqrt(0.75)
hex_vertex_radius <- max_dist
hex_edge_radius <- max_dist * sqrt(0.75)

cat(sprintf("Hexagonal truncation:\n"))
cat(sprintf("  Vertex radius: %.2f\n", hex_vertex_radius))
cat(sprintf("  Edge midpoint radius: %.2f\n", hex_edge_radius))
