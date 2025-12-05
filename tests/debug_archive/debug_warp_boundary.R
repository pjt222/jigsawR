# Debug: Understand the correct boundary size after warp transformation

cat("=" , rep("=", 70), "\n", sep = "")
cat("Debug: Warp Boundary Analysis\n")
cat("=" , rep("=", 70), "\n\n", sep = "")

source("R/hexagonal_topology.R")

# Test parameters matching the Shiny app
rings <- 3
diameter <- 240
piece_radius <- diameter / (rings * 4)  # = 20mm

cat(sprintf("Parameters:\n"))
cat(sprintf("  rings: %d\n", rings))
cat(sprintf("  diameter: %d mm\n", diameter))
cat(sprintf("  piece_radius (diameter / (rings*4)): %.2f mm\n", piece_radius))
cat(sprintf("  circle_radius (diameter / 2): %.2f mm\n", diameter / 2))
cat("\n")

# Calculate what the original complete mode does:
# hex_scale multiplies by: radius / (2*n - 4/3)
# where radius = diameter/2 = 120, n = rings = 3
original_radius <- diameter / 2
original_n <- rings
scale_factor <- 1.0 / (2 * original_n - 4.0/3) * original_radius

cat("Original complete mode calculations:\n")
cat(sprintf("  radius (diameter/2): %.2f mm\n", original_radius))
cat(sprintf("  scale_factor (1/(2n-4/3) * radius): %.4f\n", scale_factor))
cat(sprintf("  At yi=1-2*n=%d, max xi range: [-%d, %d]\n", 1 - 2 * original_n, original_n, original_n - 2))
cat("\n")

# The outermost boundary in grid coordinates is at yi = 1 - 2*n = -5 (for n=3)
# and xi ranges from -n to n-2 = -3 to 1
# After hex_scale, these become actual coordinates

# But in our separated mode, we use piece_radius differently
# Let's trace through what happens to outer ring vertices

cat("Separated mode analysis:\n")
cat(sprintf("  piece_radius (used in axial_to_cartesian): %.2f mm\n", piece_radius))

# Get positions of all outer ring pieces (ring 2 for 3-ring puzzle)
num_pieces <- 3 * rings * (rings - 1) + 1
outer_ring_pieces <- c()

for (pid in 1:num_pieces) {
  info <- map_piece_id_to_axial(pid, rings)
  if (info$ring == rings - 1) {
    outer_ring_pieces <- c(outer_ring_pieces, pid)
  }
}

cat(sprintf("  Outer ring pieces: %d pieces\n", length(outer_ring_pieces)))

# Calculate vertex positions for outer ring pieces BEFORE warp
cat("\nOuter ring vertex analysis (BEFORE warp):\n")
max_vertex_dist <- 0
vertex_positions <- list()

for (pid in outer_ring_pieces) {
  axial <- map_piece_id_to_axial(pid, rings)
  cart <- axial_to_cartesian(axial$q, axial$r, piece_radius)

  # Calculate 6 vertices of this hexagon
  for (i in 0:5) {
    vertex_angle <- i * pi / 3  # Flat-top hexagon
    vx <- cart$x + piece_radius * cos(vertex_angle)
    vy <- cart$y + piece_radius * sin(vertex_angle)
    dist <- sqrt(vx^2 + vy^2)
    max_vertex_dist <- max(max_vertex_dist, dist)
    vertex_positions[[length(vertex_positions) + 1]] <- c(vx, vy, dist)
  }
}

cat(sprintf("  Max vertex distance from center (before warp): %.2f mm\n", max_vertex_dist))

# Apply warp to these vertices
cat("\nOuter ring vertex analysis (AFTER warp):\n")
max_warped_dist <- 0

for (vpos in vertex_positions) {
  warped <- apply_hex_warp(vpos[1], vpos[2])
  dist <- sqrt(warped$x^2 + warped$y^2)
  max_warped_dist <- max(max_warped_dist, dist)
}

cat(sprintf("  Max vertex distance from center (after warp): %.2f mm\n", max_warped_dist))

# The current code projects boundary to diameter/2 = 120, but that's WAY too big
# The actual warped boundary should stay approximately where it lands after warp

cat("\n" , rep("=", 70), "\n", sep = "")
cat("CONCLUSION:\n")
cat(sprintf("  Current code projects boundary to: %.2f mm (WRONG!)\n", diameter / 2))
cat(sprintf("  Actual warped boundary should be ~: %.2f mm\n", max_warped_dist))
cat(sprintf("  Ratio (current/actual): %.2f x too large!\n", (diameter / 2) / max_warped_dist))
cat("=" , rep("=", 70), "\n", sep = "")
