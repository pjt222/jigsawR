# Analyze concentric ring topology
# Check vertex positions, neighbor relationships, and edge sharing

cat("=== Analyzing Concentric Ring Topology ===\n\n")

source("R/logging.R")
source("R/concentric_geometry.R")

# Parameters
rings <- 3
diameter <- 240
piece_height <- get_concentric_piece_height(diameter, rings)

cat(sprintf("Rings: %d\n", rings))
cat(sprintf("Diameter: %d mm\n", diameter))
cat(sprintf("Piece height: %.2f mm\n", piece_height))
cat(sprintf("Total pieces: %d\n\n", get_concentric_piece_count(rings)))

# Analyze ring structure
cat("=== Ring Structure ===\n")
for (ring in 0:(rings-1)) {
  if (ring == 0) {
    pieces_in_ring <- 1
  } else {
    pieces_in_ring <- 6 * ring
  }
  inner_r <- ring * piece_height
  outer_r <- (ring + 1) * piece_height

  cat(sprintf("Ring %d: %d pieces, inner_radius=%.1f, outer_radius=%.1f\n",
              ring, pieces_in_ring, inner_r, outer_r))

  if (ring > 0) {
    arc_angle_deg <- 360 / pieces_in_ring
    cat(sprintf("  Arc angle per piece: %.2f degrees\n", arc_angle_deg))
  }
}
cat("\n")

# Analyze piece vertices
cat("=== Piece Vertices (first pieces of each ring) ===\n")

# Ring 0 (center)
center <- calculate_concentric_vertices(1, rings, diameter, "hexagon")
cat("\nPiece 1 (center hexagon):\n")
cat(sprintf("  Type: %s\n", center$type))
for (i in 1:length(center$vertices)) {
  v <- center$vertices[[i]]
  angle_deg <- atan2(v[2], v[1]) * 180 / pi
  dist <- sqrt(v[1]^2 + v[2]^2)
  cat(sprintf("  V%d: (%.2f, %.2f) - angle=%.1f°, dist=%.2f\n", i, v[1], v[2], angle_deg, dist))
}

# Ring 1 (first trapezoid)
trap1 <- calculate_concentric_vertices(2, rings, diameter, "hexagon")
cat("\nPiece 2 (ring 1, position 0):\n")
cat(sprintf("  Type: %s\n", trap1$type))
cat(sprintf("  Ring: %d, Position: %d\n", trap1$ring, trap1$position))
cat(sprintf("  Start angle: %.2f rad (%.1f°)\n", trap1$start_angle, trap1$start_angle * 180 / pi))
cat(sprintf("  End angle: %.2f rad (%.1f°)\n", trap1$end_angle, trap1$end_angle * 180 / pi))
for (i in 1:length(trap1$vertices)) {
  v <- trap1$vertices[[i]]
  angle_deg <- atan2(v[2], v[1]) * 180 / pi
  dist <- sqrt(v[1]^2 + v[2]^2)
  edge_names <- c("inner-start", "inner-end", "outer-end", "outer-start")
  cat(sprintf("  V%d (%s): (%.2f, %.2f) - angle=%.1f°, dist=%.2f\n",
              i, edge_names[i], v[1], v[2], angle_deg, dist))
}

# Ring 2 (first trapezoid in outer ring)
trap2 <- calculate_concentric_vertices(8, rings, diameter, "hexagon")  # First piece in ring 2
cat("\nPiece 8 (ring 2, position 0):\n")
cat(sprintf("  Type: %s\n", trap2$type))
cat(sprintf("  Ring: %d, Position: %d\n", trap2$ring, trap2$position))
cat(sprintf("  Start angle: %.2f rad (%.1f°)\n", trap2$start_angle, trap2$start_angle * 180 / pi))
cat(sprintf("  End angle: %.2f rad (%.1f°)\n", trap2$end_angle, trap2$end_angle * 180 / pi))
for (i in 1:length(trap2$vertices)) {
  v <- trap2$vertices[[i]]
  angle_deg <- atan2(v[2], v[1]) * 180 / pi
  dist <- sqrt(v[1]^2 + v[2]^2)
  edge_names <- c("inner-start", "inner-end", "outer-end", "outer-start")
  cat(sprintf("  V%d (%s): (%.2f, %.2f) - angle=%.1f°, dist=%.2f\n",
              i, edge_names[i], v[1], v[2], angle_deg, dist))
}

cat("\n=== CRITICAL: Edge Sharing Analysis ===\n")

# In concentric mode, a piece's OUTER edge in ring r
# may connect to MULTIPLE pieces' INNER edges in ring r+1
# because ring r+1 has more pieces than ring r

cat("\nRing piece counts:\n")
for (ring in 0:(rings-1)) {
  if (ring == 0) {
    pieces_in_ring <- 1
  } else {
    pieces_in_ring <- 6 * ring
  }
  cat(sprintf("  Ring %d: %d pieces\n", ring, pieces_in_ring))
}

cat("\nEdge sharing between rings:\n")
cat("  Ring 0 (1 piece) -> Ring 1 (6 pieces): Each center edge connects to 1 piece\n")
cat("  Ring 1 (6 pieces) -> Ring 2 (12 pieces): Each ring 1 OUTER edge connects to 2 pieces!\n")

cat("\n=== Detailed OUTER edge sharing (Ring 1 -> Ring 2) ===\n")

# For ring 1, each piece spans 60 degrees (360/6)
# For ring 2, each piece spans 30 degrees (360/12)
# So each ring 1 piece's outer edge should connect to 2 ring 2 pieces

ring1_pieces <- 6
ring2_pieces <- 12

cat("\nRing 1 pieces and their angular coverage:\n")
for (pos in 0:(ring1_pieces-1)) {
  piece_id <- 2 + pos  # Ring 1 pieces are 2-7
  arc_angle <- 2 * pi / ring1_pieces
  start_angle <- pos * arc_angle
  end_angle <- (pos + 1) * arc_angle

  cat(sprintf("\nPiece %d (ring 1, pos %d): %.1f° to %.1f°\n",
              piece_id, pos, start_angle * 180 / pi, end_angle * 180 / pi))

  # Find which ring 2 pieces overlap with this angle range
  cat("  Overlapping ring 2 pieces:\n")
  for (pos2 in 0:(ring2_pieces-1)) {
    piece2_id <- 8 + pos2  # Ring 2 pieces are 8-19
    arc_angle2 <- 2 * pi / ring2_pieces
    start_angle2 <- pos2 * arc_angle2
    end_angle2 <- (pos2 + 1) * arc_angle2

    # Check if there's overlap
    overlap_start <- max(start_angle, start_angle2)
    overlap_end <- min(end_angle, end_angle2)

    if (overlap_start < overlap_end) {
      overlap_deg <- (overlap_end - overlap_start) * 180 / pi
      cat(sprintf("    Piece %d (ring 2, pos %d): %.1f° to %.1f° - overlap: %.1f°\n",
                  piece2_id, pos2, start_angle2 * 180 / pi, end_angle2 * 180 / pi, overlap_deg))
    }
  }
}

cat("\n=== Current Neighbor Detection Analysis ===\n")
cat("Testing get_concentric_neighbor() for piece 2 (ring 1):\n")

for (edge in 1:4) {
  neighbor <- get_concentric_neighbor(2, edge, rings)
  edge_names <- c("INNER", "RIGHT", "OUTER", "LEFT")
  if (neighbor$is_boundary) {
    cat(sprintf("  Edge %d (%s): BOUNDARY\n", edge, edge_names[edge]))
  } else {
    cat(sprintf("  Edge %d (%s): connects to piece %d (edge %d)\n",
                edge, edge_names[edge], neighbor$neighbor_id, neighbor$neighbor_edge))
  }
}

cat("\nTesting get_concentric_neighbor() for piece 8 (ring 2, first piece):\n")

for (edge in 1:4) {
  neighbor <- get_concentric_neighbor(8, edge, rings)
  edge_names <- c("INNER", "RIGHT", "OUTER", "LEFT")
  if (neighbor$is_boundary) {
    cat(sprintf("  Edge %d (%s): BOUNDARY\n", edge, edge_names[edge]))
  } else {
    cat(sprintf("  Edge %d (%s): connects to piece %d (edge %d)\n",
                edge, edge_names[edge], neighbor$neighbor_id, neighbor$neighbor_edge))
  }
}

cat("\n=== PROBLEM IDENTIFIED ===\n")
cat("The current implementation assumes 1:1 edge connections, but:\n")
cat("- Ring 1 piece's OUTER edge spans 60° and connects to TWO ring 2 pieces (each 30°)\n")
cat("- This means we need MULTIPLE TABS on a single OUTER edge!\n")
cat("- Or we need to subdivide the OUTER edge into multiple segments\n")

cat("\n=== Verification: Do vertices actually match? ===\n")

# Check if ring 1 piece's outer vertices match ring 2 pieces' inner vertices
piece2 <- calculate_concentric_vertices(2, rings, diameter, "hexagon")
piece8 <- calculate_concentric_vertices(8, rings, diameter, "hexagon")
piece9 <- calculate_concentric_vertices(9, rings, diameter, "hexagon")

cat("\nPiece 2 outer vertices:\n")
v3 <- piece2$vertices[[3]]  # outer-end
v4 <- piece2$vertices[[4]]  # outer-start
cat(sprintf("  V3 (outer-end): (%.4f, %.4f)\n", v3[1], v3[2]))
cat(sprintf("  V4 (outer-start): (%.4f, %.4f)\n", v4[1], v4[2]))

cat("\nPiece 8 inner vertices:\n")
v1_8 <- piece8$vertices[[1]]  # inner-start
v2_8 <- piece8$vertices[[2]]  # inner-end
cat(sprintf("  V1 (inner-start): (%.4f, %.4f)\n", v1_8[1], v1_8[2]))
cat(sprintf("  V2 (inner-end): (%.4f, %.4f)\n", v2_8[1], v2_8[2]))

cat("\nPiece 9 inner vertices:\n")
v1_9 <- piece9$vertices[[1]]  # inner-start
v2_9 <- piece9$vertices[[2]]  # inner-end
cat(sprintf("  V1 (inner-start): (%.4f, %.4f)\n", v1_9[1], v1_9[2]))
cat(sprintf("  V2 (inner-end): (%.4f, %.4f)\n", v2_9[1], v2_9[2]))

# Check matches
match_8_start <- abs(v4[1] - v1_8[1]) < 0.01 && abs(v4[2] - v1_8[2]) < 0.01
match_8_end <- abs(v2_8[1] - v1_9[1]) < 0.01 && abs(v2_8[2] - v1_9[2]) < 0.01
match_9_end <- abs(v2_9[1] - v3[1]) < 0.01 && abs(v2_9[2] - v3[2]) < 0.01

cat("\nVertex matching:\n")
cat(sprintf("  Piece 2 V4 (outer-start) = Piece 8 V1 (inner-start)? %s\n", ifelse(match_8_start, "YES", "NO")))
cat(sprintf("  Piece 8 V2 (inner-end) = Piece 9 V1 (inner-start)? %s\n", ifelse(match_8_end, "YES", "NO")))
cat(sprintf("  Piece 9 V2 (inner-end) = Piece 2 V3 (outer-end)? %s\n", ifelse(match_9_end, "YES", "NO")))

cat("\n=== CONCLUSION ===\n")
cat("The OUTER edge of piece 2 connects to the INNER edges of pieces 8 AND 9.\n")
cat("We need to generate TWO tabs on piece 2's OUTER edge, or use a different approach.\n")
