#!/usr/bin/env Rscript
# Test: Do pieces fit tightly at offset=0?

source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")

cat("=== DO PIECES FIT TIGHTLY AT OFFSET=0? ===\n\n")

rings <- 2
diameter <- 240
seed <- 42

# Generate pieces with offset=0 (tight fit expected)
piece_radius <- diameter / (rings * 4)
base_spacing <- piece_radius * 2
offset <- 0
separation_factor <- 1.0 + (offset / base_spacing)

cat(sprintf("Parameters:\n"))
cat(sprintf("  diameter = %d\n", diameter))
cat(sprintf("  rings = %d\n", rings))
cat(sprintf("  piece_radius = %.2f\n", piece_radius))
cat(sprintf("  offset = %d (should be tight fit)\n", offset))
cat(sprintf("  separation_factor = %.3f\n\n", separation_factor))

# Generate pieces
cat("Generating pieces...\n")
pieces <- generate_hex_pieces_with_edge_map(
  rings = rings,
  seed = seed,
  diameter = diameter,
  tabsize = 27,
  jitter = 5,
  separated = TRUE,
  base_spacing = base_spacing,
  separation_factor = separation_factor
)

cat("\n=== PIECE CENTERS ===\n")
for (i in 1:length(pieces)) {
  p <- pieces[[i]]
  cat(sprintf("Piece %d (ring %d): center=(%.2f, %.2f)\n", p$id, p$ring, p$center_x, p$center_y))
}

# Check: For touching hexagons (offset=0), what should the distance be?
# For a flat-top hexagon with piece_radius (distance from center to vertex):
# - Width = 2 * piece_radius
# - Height = sqrt(3) * piece_radius
# - Distance between adjacent centers (for touching hexagons) = sqrt(3) * piece_radius

expected_distance <- sqrt(3) * piece_radius
cat(sprintf("\n=== EXPECTED vs ACTUAL ===\n"))
cat(sprintf("For TOUCHING hexagons with piece_radius=%.2f:\n", piece_radius))
cat(sprintf("  Expected center-to-center distance = sqrt(3) * %.2f = %.2f mm\n\n", piece_radius, expected_distance))

# Calculate actual distance between piece 1 (center) and piece 2
p1 <- pieces[[1]]
p2 <- pieces[[2]]
actual_distance <- sqrt((p2$center_x - p1$center_x)^2 + (p2$center_y - p1$center_y)^2)
cat(sprintf("Actual distance P1 to P2 = sqrt((%.2f-%.2f)^2 + (%.2f-%.2f)^2) = %.2f mm\n",
            p2$center_x, p1$center_x, p2$center_y, p1$center_y, actual_distance))

ratio <- actual_distance / expected_distance
cat(sprintf("\nRatio actual/expected = %.2f\n", ratio))

if (abs(ratio - 1.0) < 0.01) {
  cat("PASS: Pieces are correctly positioned for tight fit!\n")
} else if (abs(ratio - 2.0) < 0.01) {
  cat("FAIL: Pieces are DOUBLED apart (the bug we identified)!\n")
  cat("At offset=0, pieces should be touching but they have GAPS.\n")
} else {
  cat(sprintf("UNEXPECTED: Ratio is %.2f\n", ratio))
}

# What about the edge coordinates?
cat("\n=== EDGE COORDINATE CHECK ===\n")
cat("The edges are generated using generate_hex_edge_map() which uses piece_radius correctly.\n")
cat("But piece CENTERS are calculated with the doubled base_spacing.\n\n")

# Let's check where edges think vertices are
cat("Edge Map uses:\n")
cat("  hex_size = piece_radius = 30\n")
cat("  axial_to_cartesian(q, r, hex_size=30)\n\n")

# For piece 2, axial coords are (q=-1, r=1)
q <- -1
r <- 1
edge_center_x <- piece_radius * (3/2 * q)
edge_center_y <- piece_radius * (sqrt(3)/2 * q + sqrt(3) * r)
cat(sprintf("Edge generation thinks piece 2 center is at: (%.2f, %.2f)\n", edge_center_x, edge_center_y))
cat(sprintf("But piece center is stored as: (%.2f, %.2f)\n", p2$center_x, p2$center_y))

cat("\n=== SUMMARY ===\n")
cat("The edges are generated at the CORRECT compact positions.\n")
cat("But when separation is applied, the code uses base_spacing (60) as piece_radius,\n")
cat("which doubles the positions.\n\n")
cat("Even at offset=0, there's a 2x gap between pieces because:\n")
cat("  separation_factor = 1.0 (no additional separation)\n")
cat("  BUT the positions are calculated using base_spacing=60 instead of piece_radius=30\n")
cat("  So positions are already 2x what they should be!\n")
