# Debug vertex directions to understand the correct edge orientation
# This will help us fix the INNER edge direction issue

cat("=== Debug Vertex Directions ===\n\n")

source("R/logging.R")
source("R/concentric_geometry.R")

rings <- 3
diameter <- 240

# Get all vertices
all_vertices <- get_all_concentric_vertices(rings, diameter, "hexagon")

# Center piece (hexagon)
center <- all_vertices[[1]]
cat("=== CENTER PIECE (hexagon) ===\n")
cat("Vertices go counterclockwise from (r, 0):\n")
for (i in 1:6) {
  v <- center$vertices[[i]]
  angle <- atan2(v[2], v[1]) * 180 / pi
  cat(sprintf("  V%d: (%.2f, %.2f) - angle=%.1f°\n", i, v[1], v[2], angle))
}

cat("\nCenter edges (going counterclockwise, V[i] -> V[i+1]):\n")
for (i in 1:6) {
  v1 <- center$vertices[[i]]
  v2 <- center$vertices[[(i %% 6) + 1]]
  neighbor_id <- 1 + i
  cat(sprintf("  Edge %d -> Piece %d: (%.2f,%.2f) -> (%.2f,%.2f)\n",
              i, neighbor_id, v1[1], v1[2], v2[1], v2[2]))
}

# Ring 1 pieces
cat("\n=== RING 1 PIECES ===\n")
cat("Each trapezoid: V1=inner-start, V2=inner-end, V3=outer-end, V4=outer-start\n")
cat("INNER edge should match center's OUTER edge to the same piece\n\n")

for (piece_id in 2:7) {
  piece <- all_vertices[[piece_id]]
  cat(sprintf("--- Piece %d (pos %d) ---\n", piece_id, piece$position))

  v1 <- piece$vertices[[1]]
  v2 <- piece$vertices[[2]]
  v3 <- piece$vertices[[3]]
  v4 <- piece$vertices[[4]]

  cat(sprintf("  V1 (inner-start): (%.2f, %.2f)\n", v1[1], v1[2]))
  cat(sprintf("  V2 (inner-end):   (%.2f, %.2f)\n", v2[1], v2[2]))
  cat(sprintf("  V3 (outer-end):   (%.2f, %.2f)\n", v3[1], v3[2]))
  cat(sprintf("  V4 (outer-start): (%.2f, %.2f)\n", v4[1], v4[2]))

  cat(sprintf("  INNER edge: (%.2f,%.2f) -> (%.2f,%.2f)\n", v1[1], v1[2], v2[1], v2[2]))

  # What center edge does this correspond to?
  # The center's edge going to this piece is edge_idx = piece_id - 1
  edge_idx <- piece_id - 1
  center_v1 <- center$vertices[[edge_idx]]
  center_v2 <- center$vertices[[(edge_idx %% 6) + 1]]

  cat(sprintf("  Center edge %d: (%.2f,%.2f) -> (%.2f,%.2f)\n",
              edge_idx, center_v1[1], center_v1[2], center_v2[1], center_v2[2]))

  # Check if they match
  if (abs(v1[1] - center_v1[1]) < 0.01 && abs(v1[2] - center_v1[2]) < 0.01 &&
      abs(v2[1] - center_v2[1]) < 0.01 && abs(v2[2] - center_v2[2]) < 0.01) {
    cat("  MATCH: Inner edge V1->V2 = Center edge forward\n")
  } else if (abs(v1[1] - center_v2[1]) < 0.01 && abs(v1[2] - center_v2[2]) < 0.01 &&
             abs(v2[1] - center_v1[1]) < 0.01 && abs(v2[2] - center_v1[2]) < 0.01) {
    cat("  MATCH: Inner edge V1->V2 = Center edge REVERSE\n")
  } else {
    cat("  NO DIRECT MATCH - vertices don't align!\n")
    cat(sprintf("    v1 matches center_v1? %s\n", abs(v1[1] - center_v1[1]) < 0.01 && abs(v1[2] - center_v1[2]) < 0.01))
    cat(sprintf("    v1 matches center_v2? %s\n", abs(v1[1] - center_v2[1]) < 0.01 && abs(v1[2] - center_v2[2]) < 0.01))
    cat(sprintf("    v2 matches center_v1? %s\n", abs(v2[1] - center_v1[1]) < 0.01 && abs(v2[2] - center_v1[2]) < 0.01))
    cat(sprintf("    v2 matches center_v2? %s\n", abs(v2[1] - center_v2[1]) < 0.01 && abs(v2[2] - center_v2[2]) < 0.01))
  }

  cat("\n")
}

# Now check OUTER edge direction for ring 1
cat("\n=== OUTER EDGE DIRECTION (Ring 1 -> Ring 2) ===\n")
cat("For piece 2, OUTER edge goes V3 -> V4 (outer-end to outer-start)\n")
cat("This connects to pieces 8 and 9\n\n")

piece2 <- all_vertices[[2]]
v3 <- piece2$vertices[[3]]
v4 <- piece2$vertices[[4]]

cat(sprintf("Piece 2 V3 (outer-end):   (%.2f, %.2f)\n", v3[1], v3[2]))
cat(sprintf("Piece 2 V4 (outer-start): (%.2f, %.2f)\n\n", v4[1], v4[2]))

# Pieces 8 and 9 inner edges
piece8 <- all_vertices[[8]]
piece9 <- all_vertices[[9]]

cat("Piece 8 INNER edge (V1->V2):\n")
cat(sprintf("  V1: (%.2f, %.2f)\n", piece8$vertices[[1]][1], piece8$vertices[[1]][2]))
cat(sprintf("  V2: (%.2f, %.2f)\n", piece8$vertices[[2]][1], piece8$vertices[[2]][2]))

cat("Piece 9 INNER edge (V1->V2):\n")
cat(sprintf("  V1: (%.2f, %.2f)\n", piece9$vertices[[1]][1], piece9$vertices[[1]][2]))
cat(sprintf("  V2: (%.2f, %.2f)\n", piece9$vertices[[2]][1], piece9$vertices[[2]][2]))

cat("\n")
cat("For piece 2's OUTER to go V3->V4, the segments need to chain:\n")
cat("  V3 -> piece9.V1 (junction) -> V4\n")
cat("OR the reverse depending on angle sorting\n\n")

# Check which order they should be in
# V3 is at (40, 69.28), V4 is at (80, 0)
# Going from V3 to V4, we go clockwise (angle decreases)
# So we need piece 9 first (higher angle), then piece 8 (lower angle)

cat("Angle analysis:\n")
angle_v3 <- atan2(v3[2], v3[1]) * 180 / pi
angle_v4 <- atan2(v4[2], v4[1]) * 180 / pi
angle_8 <- (piece8$start_angle + piece8$end_angle) / 2 * 180 / pi
angle_9 <- (piece9$start_angle + piece9$end_angle) / 2 * 180 / pi

cat(sprintf("  V3 angle: %.1f°\n", angle_v3))
cat(sprintf("  V4 angle: %.1f°\n", angle_v4))
cat(sprintf("  Piece 8 mid-angle: %.1f°\n", angle_8))
cat(sprintf("  Piece 9 mid-angle: %.1f°\n", angle_9))

cat("\nTo go V3->V4 (60°->0°), we need to visit piece 9 (45°) then piece 8 (15°)\n")
cat("So outer segments should be sorted by DECREASING angle for V3->V4 direction.\n")

cat("\n=== Debug Complete ===\n")
