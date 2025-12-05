# Debug cross product calculation

source("R/hexagonal_topology.R")

# Test boundary vertices
rings <- 3
diameter <- 240
piece_radius <- diameter / (rings * 4)

# Get an outer ring piece
axial <- map_piece_id_to_axial(8, rings)
cart <- axial_to_cartesian(axial$q, axial$r, piece_radius)

cat(sprintf("Piece 8 center: (%.2f, %.2f)\n", cart$x, cart$y))

# Get vertices
for (i in 0:5) {
  vx <- cart$x + piece_radius * cos(i * pi / 3)
  vy <- cart$y + piece_radius * sin(i * pi / 3)
  warped <- apply_hex_warp(vx, vy)
  cat(sprintf("  V%d: orig=(%.2f, %.2f), warped=(%.2f, %.2f)\n",
              i, vx, vy, warped$x, warped$y))
}

# Test with mean projection
avg_dist <- 90  # approximate average

# Project to circle
test_warped <- apply_hex_warp(cart$x + piece_radius, cart$y)
dist <- sqrt(test_warped$x^2 + test_warped$y^2)
projected <- c(
  test_warped$x / dist * avg_dist,
  test_warped$y / dist * avg_dist
)

cat(sprintf("\nProjected: (%.2f, %.2f)\n", projected[1], projected[2]))
cat(sprintf("Any NA? %s\n", any(is.na(projected))))
