# Debug Ring 2 OUTER edge connections in 5-ring puzzle
# The issue is that ring 2 pieces have OUTER edges connecting to ring 3 pieces

cat("=== Debug Ring 2 OUTER Edges ===\n\n")

source("R/logging.R")
source("R/concentric_geometry.R")

rings <- 5
diameter <- 400

# Get all vertices
all_vertices <- get_all_concentric_vertices(rings, diameter, "hexagon")

# Check piece 8 (ring 2, position 0)
piece8 <- all_vertices[[8]]
cat("=== Piece 8 (Ring 2, Position 0) ===\n")
cat(sprintf("V1 (inner-start): (%.2f, %.2f)\n", piece8$vertices[[1]][1], piece8$vertices[[1]][2]))
cat(sprintf("V2 (inner-end):   (%.2f, %.2f)\n", piece8$vertices[[2]][1], piece8$vertices[[2]][2]))
cat(sprintf("V3 (outer-end):   (%.2f, %.2f)\n", piece8$vertices[[3]][1], piece8$vertices[[3]][2]))
cat(sprintf("V4 (outer-start): (%.2f, %.2f)\n", piece8$vertices[[4]][1], piece8$vertices[[4]][2]))
cat(sprintf("Start angle: %.2f rad (%.1f°)\n", piece8$start_angle, piece8$start_angle * 180 / pi))
cat(sprintf("End angle:   %.2f rad (%.1f°)\n", piece8$end_angle, piece8$end_angle * 180 / pi))

# Get outer neighbors
source("R/concentric_edge_generation.R")
outer_neighbors <- get_outer_neighbors(8, rings)
cat(sprintf("\nOuter neighbors (from get_outer_neighbors): %s\n", paste(outer_neighbors, collapse=", ")))

# Check each outer neighbor
cat("\n--- Outer neighbor details ---\n")
for (neighbor_id in outer_neighbors) {
  neighbor <- all_vertices[[neighbor_id]]
  cat(sprintf("\nPiece %d (Ring %d, Position %d):\n", neighbor_id, neighbor$ring, neighbor$position))
  cat(sprintf("  V1 (inner-start): (%.2f, %.2f)\n", neighbor$vertices[[1]][1], neighbor$vertices[[1]][2]))
  cat(sprintf("  V2 (inner-end):   (%.2f, %.2f)\n", neighbor$vertices[[2]][1], neighbor$vertices[[2]][2]))
  cat(sprintf("  Start angle: %.2f rad (%.1f°)\n", neighbor$start_angle, neighbor$start_angle * 180 / pi))
  cat(sprintf("  End angle:   %.2f rad (%.1f°)\n", neighbor$end_angle, neighbor$end_angle * 180 / pi))
}

# What should piece 8's OUTER edge connect to?
# V3 = (103.92, 60.00) - angle = 30°
# V4 = (120.00, 0.00)  - angle = 0°
# So we need to go from 30° to 0°

cat("\n\n=== ANALYSIS ===\n")
cat("Piece 8's V3 is at (103.92, 60.00) = angle 30°\n")
cat("Piece 8's V4 is at (120.00, 0.00) = angle 0°\n")
cat("OUTER edge should go from 30° to 0° (decreasing angle)\n\n")

# Ring 3 has 18 pieces, each spanning 20°
# Pieces that overlap with piece 8's OUTER (0° to 30°):
# - Position 0: 0° to 20° -> Piece 20
# - Position 1: 20° to 40° -> but wait, we need pieces covering 0-30°

cat("Ring 3 piece angular coverage:\n")
ring3_start <- 3 * 3 * 2 + 2  # = 20
for (pos in 0:2) {
  pid <- ring3_start + pos
  p <- all_vertices[[pid]]
  cat(sprintf("  Piece %d (pos %d): %.1f° to %.1f°\n",
              pid, pos, p$start_angle * 180/pi, p$end_angle * 180/pi))
}

cat("\n")
cat("Expected: Piece 8's OUTER (0°-30°) should connect to:\n")
cat("  - Piece 20 (0°-20°) - inner edge at (120.00, 0.00) to (112.76, 41.04)\n")
cat("  - Piece 21 (20°-40°) - but only the 20°-30° part\n")
cat("    Inner edge: (112.76, 41.04) to (91.93, 77.13)\n")
cat("    But piece 8's V3 is at (103.92, 60.00) which is at 30°!\n")

cat("\n")
cat("THE PROBLEM:\n")
cat("  Piece 8's V3 (30°) should match the junction between pieces 20 and 21\n")
cat("  Piece 21's V1 = (112.76, 41.04) is at angle atan2(41.04, 112.76) = 20°\n")
cat("  But piece 8's V3 is at 30°!\n")

cat("\n")
cat("This is a VERTEX MISMATCH - the outer ring piece boundaries don't align\n")
cat("with the inner ring piece corners.\n")
