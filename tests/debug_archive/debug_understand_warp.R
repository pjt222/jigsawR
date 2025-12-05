# Debug: Understand the warp transformation geometry

cat("=" , rep("=", 70), "\n", sep = "")
cat("Understanding the Warp Transformation\n")
cat("=" , rep("=", 70), "\n\n", sep = "")

source("R/hexagonal_topology.R")

rings <- 3
diameter <- 240
piece_radius <- diameter / (rings * 4)

cat(sprintf("Parameters:\n"))
cat(sprintf("  rings: %d\n", rings))
cat(sprintf("  diameter: %d mm\n", diameter))
cat(sprintf("  piece_radius: %.2f mm\n", piece_radius))
cat("\n")

# Calculate the ORIGINAL (pre-warp) vertices for all pieces
num_pieces <- 3 * rings * (rings - 1) + 1

cat("Original hexagonal grid vertices (before warp):\n\n")

# Calculate distances from origin for original vertices
original_max_dist <- 0
original_boundary_dists <- c()

for (piece_id in 1:num_pieces) {
  axial_coords <- map_piece_id_to_axial(piece_id, rings)
  cart_coords <- axial_to_cartesian(axial_coords$q, axial_coords$r, piece_radius)

  for (i in 0:5) {
    vertex_angle <- i * pi / 3
    vx <- cart_coords$x + piece_radius * cos(vertex_angle)
    vy <- cart_coords$y + piece_radius * sin(vertex_angle)
    dist <- sqrt(vx^2 + vy^2)

    original_max_dist <- max(original_max_dist, dist)

    # Check if this is a boundary vertex (outer ring)
    ring_info <- map_piece_id_to_ring(piece_id, rings)
    if (ring_info$ring == rings - 1) {
      original_boundary_dists <- c(original_boundary_dists, dist)
    }
  }
}

cat(sprintf("Original max vertex distance: %.2f\n", original_max_dist))
cat(sprintf("Original boundary distance range: [%.2f, %.2f]\n",
            min(original_boundary_dists), max(original_boundary_dists)))
cat("\n")

# Now apply warp transformation and see what happens
cat("After warp transformation:\n\n")

warped_boundary_dists <- c()

for (piece_id in 1:num_pieces) {
  axial_coords <- map_piece_id_to_axial(piece_id, rings)
  cart_coords <- axial_to_cartesian(axial_coords$q, axial_coords$r, piece_radius)
  ring_info <- map_piece_id_to_ring(piece_id, rings)

  for (i in 0:5) {
    vertex_angle <- i * pi / 3
    vx <- cart_coords$x + piece_radius * cos(vertex_angle)
    vy <- cart_coords$y + piece_radius * sin(vertex_angle)

    # Apply warp transformation (same as apply_hex_warp)
    angl <- atan2(vy, vx) + pi
    angl60 <- angl %% (pi / 3)
    angl30 <- abs((pi / 6) - angl60)
    l <- sqrt(0.75) / cos(angl30)
    warped_x <- vx / l
    warped_y <- vy / l
    warped_dist <- sqrt(warped_x^2 + warped_y^2)

    if (ring_info$ring == rings - 1) {
      warped_boundary_dists <- c(warped_boundary_dists, warped_dist)
    }
  }
}

cat(sprintf("Warped boundary distance range: [%.2f, %.2f]\n",
            min(warped_boundary_dists), max(warped_boundary_dists)))
cat(sprintf("Warped boundary distance mean: %.2f\n", mean(warped_boundary_dists)))
cat("\n")

# The KEY question: what should the arc radius be?
cat("=" , rep("=", 70), "\n", sep = "")
cat("Key Question: What arc radius should border edges use?\n")
cat("=" , rep("=", 70), "\n\n", sep = "")

cat("Option 1: diameter/2 = ", diameter/2, " (complete mode uses this for the outline)\n")
cat("Option 2: Average warped boundary dist = ", round(mean(warped_boundary_dists), 2), "\n")
cat("Option 3: Max original boundary dist = ", round(original_max_dist, 2), "\n")
cat("\n")

# Let's understand the geometry better
# In the original hexagonal grid, the outermost vertices are at distance:
# This depends on the hexagonal layout

# For a regular hexagonal arrangement with 'rings' rings:
# The outermost vertices are at the corners of the outer hexagon
# The distance is: (2*rings - 2) * piece_radius for pointy-top hexagons

theoretical_outer_dist <- (2 * rings - 2) * piece_radius * sqrt(3) / 2 + piece_radius
cat(sprintf("Theoretical outer vertex distance: ~%.2f\n", theoretical_outer_dist))

# After warp, ALL points at distance r from origin stay at distance r
# (warp only changes angle, not magnitude for points ON the circle)
# But points that are NOT on a circle get moved...

# Actually, let's verify: does warp preserve distance for boundary vertices?
cat("\nVerifying: Does warp preserve distance?\n")

# Take a point at the max original distance and warp it
test_x <- original_max_dist  # along x-axis
test_y <- 0

angl <- atan2(test_y, test_x) + pi
angl60 <- angl %% (pi / 3)
angl30 <- abs((pi / 6) - angl60)
l <- sqrt(0.75) / cos(angl30)
warped_x <- test_x / l
warped_y <- test_y / l
warped_dist <- sqrt(warped_x^2 + warped_y^2)

cat(sprintf("  Point at (%.2f, 0): original dist=%.2f, warped dist=%.2f\n",
            test_x, original_max_dist, warped_dist))
cat(sprintf("  Warp factor l=%.4f (divide by this)\n", l))

# The warp transformation maps a hexagonal shape to a circular shape
# Points on the boundary of the original hexagon get mapped to the boundary of a circle
# But the RADIUS of that circle depends on the warp formula

cat("\n")
cat("=" , rep("=", 70), "\n", sep = "")
cat("INSIGHT: The warp formula sqrt(0.75)/cos(angle) maps hexagon to circle\n")
cat("A hexagon with 'radius' R (center to vertex) gets mapped to a circle\n")
cat("with radius R * sqrt(0.75) = R * 0.866\n")
cat("=" , rep("=", 70), "\n\n", sep = "")

expected_circle_radius <- original_max_dist * sqrt(0.75)
cat(sprintf("Expected circle radius after warp: %.2f * sqrt(0.75) = %.2f\n",
            original_max_dist, expected_circle_radius))
cat(sprintf("Actual average warped boundary dist: %.2f\n", mean(warped_boundary_dists)))
