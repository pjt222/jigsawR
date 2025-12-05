# Debug: Compare complete mode boundary handling vs separated mode

cat("=" , rep("=", 70), "\n", sep = "")
cat("Complete Mode vs Separated Mode: Border Handling\n")
cat("=" , rep("=", 70), "\n\n", sep = "")

source("R/hexagonal_topology.R")
source("R/hexagonal_puzzle.R")

rings <- 3
diameter <- 240
piece_radius <- diameter / (rings * 4)

cat("=== COMPLETE MODE ANALYSIS ===\n\n")

# In complete mode, when do_warp=TRUE and do_trunc=TRUE:
# 1. The border is drawn as a SEPARATE circle using:
#    "a radius radius 0 1 0 (2*radius) 0" where radius = diameter/2 = 120

# But the internal edges use hex_process_r which applies:
# - hex_scale: scales coordinates by (1.0 / (2*n - 4.0/3)) * radius
# - hex_rotate: rotates by angle
# - hex_warp: if do_warp, applies the warp transformation
# - hex_translate: adds (radius + offset, radius + offset) to center the puzzle

# Let's trace through hex_scale for our parameters
n <- rings
radius <- diameter / 2
scale_factor <- (1.0 / (2 * n - 4.0 / 3)) * radius

cat(sprintf("Complete mode hex_scale factor:\n"))
cat(sprintf("  n = %d\n", n))
cat(sprintf("  radius = diameter/2 = %.2f\n", radius))
cat(sprintf("  scale = (1/(2*n - 4/3)) * radius = (1/%.4f) * %.2f = %.4f\n",
            2*n - 4/3, radius, scale_factor))

# So in complete mode, the "base" coordinates are in a different scale
# The hexagonal grid iteration uses:
# yi from -(2n-1) to (2n-1), step 2 (so -5 to 5 for n=3)
# For each yi, xi varies based on row

cat("\n")
cat("Complete mode coordinate ranges (before scale/warp/translate):\n")
yl <- 2 * n - 1
cat(sprintf("  yl = 2*n - 1 = %d\n", yl))
cat(sprintf("  yi range: [%d, %d]\n", -(yl - 1), yl - 1))

# After hex_scale, these become actual coordinates
cat(sprintf("\nAfter hex_scale (y):\n"))
cat(sprintf("  max |yi| = %d, scaled = %.2f\n", yl - 1, (yl - 1) * scale_factor))
cat(sprintf("  This is the y-extent of pieces (before sqrt(0.75) factor in scale)\n"))

# The sqrt(0.75) factor is applied to y in hex_scale
actual_y_extent <- (yl - 1) * scale_factor * sqrt(0.75)
cat(sprintf("  Actual y extent = %.2f\n", actual_y_extent))

cat("\n\n=== SEPARATED MODE ANALYSIS ===\n\n")

# In separated mode, we use:
# piece_radius = diameter / (rings * 4)
# Vertices are placed at distance piece_radius from piece centers
# Piece centers are calculated using axial_to_cartesian

cat(sprintf("Separated mode piece_radius: %.2f\n", piece_radius))

# The furthest piece center
# For 3 rings, the outer pieces have ring=2 (0-indexed)
# Their centers are at various positions

cat("\nOuter piece center distances:\n")
num_pieces <- 3 * rings * (rings - 1) + 1

for (piece_id in 1:num_pieces) {
  ring_info <- map_piece_id_to_ring(piece_id, rings)
  if (ring_info$ring == rings - 1) {
    axial <- map_piece_id_to_axial(piece_id, rings)
    cart <- axial_to_cartesian(axial$q, axial$r, piece_radius)
    center_dist <- sqrt(cart$x^2 + cart$y^2)
    cat(sprintf("  Piece %d: center at (%.2f, %.2f), dist=%.2f\n",
                piece_id, cart$x, cart$y, center_dist))
  }
}

# The furthest vertices are at piece center distance + piece_radius
# in the outward direction

cat("\n\n=== KEY DIFFERENCE ===\n\n")

cat("Complete mode:\n")
cat("  - Uses its own coordinate system based on hex_scale\n")
cat("  - Border is drawn as a SEPARATE overlay circle with radius = diameter/2\n")
cat("  - Pieces don't have individual border arcs\n")
cat("  - The puzzle is a single connected path + border overlay\n\n")

cat("Separated mode:\n")
cat("  - Uses piece_radius = diameter/(rings*4)\n")
cat("  - Each piece has its own complete path including border arcs\n")
cat("  - Border arc radius needs to match the geometry\n\n")

cat("The PROBLEM: In separated mode, we're trying to use arc radius = 88mm\n")
cat("(computed from average warped distances), but the actual warped boundary\n")
cat("vertices are NOT all at 88mm - they range from ~60mm to ~88mm!\n\n")

cat("When we project vertices to the circle AND use that radius for arcs,\n")
cat("the pieces that had vertices closer to the origin get STRETCHED.\n")
