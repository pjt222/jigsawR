# Debug: What happens if we DON'T project boundary vertices?

cat("=" , rep("=", 70), "\n", sep = "")
cat("Testing: No projection of boundary vertices\n")
cat("=" , rep("=", 70), "\n\n", sep = "")

source("R/hexagonal_topology.R")

rings <- 3
diameter <- 240
piece_radius <- diameter / (rings * 4)
num_pieces <- 3 * rings * (rings - 1) + 1

cat(sprintf("Parameters: rings=%d, diameter=%d, piece_radius=%.2f\n\n",
            rings, diameter, piece_radius))

# Step 1: Calculate original vertices
piece_vertices_original <- list()
for (piece_id in 1:num_pieces) {
  axial <- map_piece_id_to_axial(piece_id, rings)
  cart <- axial_to_cartesian(axial$q, axial$r, piece_radius)

  vertices <- list()
  for (i in 0:5) {
    angle <- i * pi / 3
    vx <- cart$x + piece_radius * cos(angle)
    vy <- cart$y + piece_radius * sin(angle)
    vertices[[i + 1]] <- c(vx, vy)
  }
  piece_vertices_original[[piece_id]] <- vertices
}

# Step 2: Apply ONLY warp transformation (no projection)
piece_vertices_warped <- list()
for (piece_id in 1:num_pieces) {
  vertices <- list()
  for (i in 1:6) {
    v <- piece_vertices_original[[piece_id]][[i]]

    # Apply warp transformation
    angl <- atan2(v[2], v[1]) + pi
    angl60 <- angl %% (pi / 3)
    angl30 <- abs((pi / 6) - angl60)
    l <- sqrt(0.75) / cos(angl30)

    warped_x <- v[1] / l
    warped_y <- v[2] / l
    vertices[[i]] <- c(warped_x, warped_y)
  }
  piece_vertices_warped[[piece_id]] <- vertices
}

# Step 3: Analyze piece sizes (vertices only, no bezier)
cat("Piece sizes with warp ONLY (no projection to circle):\n\n")

ring_widths <- list("0" = c(), "1" = c(), "2" = c())
ring_heights <- list("0" = c(), "1" = c(), "2" = c())

for (piece_id in 1:num_pieces) {
  ring_info <- map_piece_id_to_ring(piece_id, rings)

  vertices_x <- sapply(piece_vertices_warped[[piece_id]], `[`, 1)
  vertices_y <- sapply(piece_vertices_warped[[piece_id]], `[`, 2)

  width <- max(vertices_x) - min(vertices_x)
  height <- max(vertices_y) - min(vertices_y)

  ring <- as.character(ring_info$ring)
  ring_widths[[ring]] <- c(ring_widths[[ring]], width)
  ring_heights[[ring]] <- c(ring_heights[[ring]], height)
}

cat(sprintf("%-8s %-10s %-12s %-12s\n", "Ring", "Count", "Avg Width", "Avg Height"))
cat(sprintf("%-8s %-10s %-12s %-12s\n", "----", "-----", "---------", "----------"))

for (ring in c("0", "1", "2")) {
  cat(sprintf("%-8s %-10d %-12.2f %-12.2f\n",
              ring,
              length(ring_widths[[ring]]),
              mean(ring_widths[[ring]]),
              mean(ring_heights[[ring]])))
}

center_area <- mean(ring_widths[["0"]]) * mean(ring_heights[["0"]])
outer_area <- mean(ring_widths[["2"]]) * mean(ring_heights[["2"]])
cat(sprintf("\nOuter/Center area ratio: %.2f\n", outer_area / center_area))

# Step 4: Look at boundary vertex distances
cat("\n\nBoundary vertex distances (after warp only, no projection):\n")

boundary_dists <- c()
for (piece_id in 1:num_pieces) {
  ring_info <- map_piece_id_to_ring(piece_id, rings)
  if (ring_info$ring != rings - 1) next  # Skip non-boundary pieces

  for (i in 1:6) {
    v <- piece_vertices_warped[[piece_id]][[i]]
    dist <- sqrt(v[1]^2 + v[2]^2)

    # Check if this is actually a boundary vertex (shared by < 3 pieces)
    # For simplicity, just look at distances > 60 (outer vertices)
    if (dist > 50) {
      boundary_dists <- c(boundary_dists, dist)
    }
  }
}

cat(sprintf("Boundary vertex distance range: [%.2f, %.2f]\n",
            min(boundary_dists), max(boundary_dists)))
cat(sprintf("Boundary vertex distance mean: %.2f\n", mean(boundary_dists)))
cat(sprintf("Boundary vertex distance std: %.2f\n", sd(boundary_dists)))

cat("\n")
cat("=" , rep("=", 70), "\n", sep = "")
cat("INSIGHT: Without projection, boundary vertices are at DIFFERENT distances.\n")
cat("The arc for each border edge should use a radius appropriate for THAT edge,\n")
cat("not a global average.\n")
cat("=" , rep("=", 70), "\n")
