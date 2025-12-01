#!/usr/bin/env Rscript
# Find angles of boundary vertices

source("R/hexagonal_topology.R")

rings <- 2
diameter <- 240
piece_radius <- diameter / (rings * 4)

# Find angles of max distance vertices
cat("Boundary vertices with max distance (79.37):\n")
for (piece_id in 2:7) {
  axial <- map_piece_id_to_axial(piece_id, rings)
  cart <- axial_to_cartesian(axial$q, axial$r, piece_radius)

  for (i in 0:5) {
    angle <- i * pi / 3
    vx <- cart$x + piece_radius * cos(angle)
    vy <- cart$y + piece_radius * sin(angle)
    dist <- sqrt(vx^2 + vy^2)

    if (abs(dist - 79.37) < 0.1) {
      v_angle <- atan2(vy, vx) * 180 / pi
      cat(sprintf("  Piece %d V%d: (%.1f, %.1f), angle=%.1f deg, dist=%.2f\n",
                  piece_id, i, vx, vy, v_angle, dist))
    }
  }
}

cat("\nBoundary vertices with min distance (60.00):\n")
for (piece_id in 2:7) {
  axial <- map_piece_id_to_axial(piece_id, rings)
  cart <- axial_to_cartesian(axial$q, axial$r, piece_radius)

  for (i in 0:5) {
    angle <- i * pi / 3
    vx <- cart$x + piece_radius * cos(angle)
    vy <- cart$y + piece_radius * sin(angle)
    dist <- sqrt(vx^2 + vy^2)

    if (abs(dist - 60.00) < 0.1) {
      v_angle <- atan2(vy, vx) * 180 / pi
      cat(sprintf("  Piece %d V%d: (%.1f, %.1f), angle=%.1f deg, dist=%.2f\n",
                  piece_id, i, vx, vy, v_angle, dist))
    }
  }
}
