#!/usr/bin/env Rscript
# Analyze hexagonal piece neighbor relationships

source("R/hexagonal_topology.R")

cat("Analyzing hexagonal neighbor relationships for 3-ring puzzle\n\n")

rings <- 3
num_pieces <- 3 * rings * (rings - 1) + 1

# Calculate piece positions
positions <- list()
for (i in 1:num_pieces) {
  ring_info <- map_piece_id_to_ring(i, rings)

  if (ring_info$ring == 0) {
    positions[[i]] <- list(
      id = i,
      ring = ring_info$ring,
      position = ring_info$position,
      x = 0,
      y = 0
    )
  } else {
    # Calculate cartesian position from polar
    radius <- ring_info$ring * 1.0  # Arbitrary unit
    positions[[i]] <- list(
      id = i,
      ring = ring_info$ring,
      position = ring_info$position,
      x = radius * cos(ring_info$angle),
      y = radius * sin(ring_info$angle)
    )
  }
}

# Analyze center piece (ID 1)
cat("Center piece (ID 1) neighbors:\n")
cat("  Ring 1 pieces (2-7) should all be neighbors\n")
for (id in 2:7) {
  pos <- positions[[id]]
  cat(sprintf("  Piece %d: angle = %.1f°, position in ring = %d\n",
              id, pos$position * 360 / 6, pos$position))
}

cat("\n")

# Analyze ring 1 piece (ID 2)
cat("Piece 2 (ring 1, position 0) neighbors:\n")
cat("  Should connect to:\n")
cat("    - Center (piece 1) on one side\n")
cat("    - Pieces 3 and 7 (adjacent in ring 1) on two sides\n")
cat("    - Pieces 8, 9, 19 (ring 2) on three sides\n")

cat("\n")

# For honeycomb structure, each ring-1 piece connects to:
# 1. Center (1 neighbor)
# 2. Two adjacent pieces in same ring (2 neighbors)
# 3. Three pieces in outer ring (3 neighbors)
# Total: 6 neighbors (full hexagon)

cat("Hexagonal neighbor pattern:\n")
cat("  Flat-top hexagon sides (0-5):\n")
cat("    Side 0 (right):        typically outer ring\n")
cat("    Side 1 (upper-right):  outer ring\n")
cat("    Side 2 (upper-left):   same ring (counterclockwise)\n")
cat("    Side 3 (left):         center\n")
cat("    Side 4 (lower-left):   same ring (clockwise)\n")
cat("    Side 5 (lower-right):  outer ring\n")

cat("\n\nKey insight for edge_id calculation:\n")
cat("  For ring 1 pieces:\n")
cat("    - piece_id = 2 + position (where position = 0..5)\n")
cat("    - Side 3 always connects to center (piece 1)\n")
cat("    - Side 2 connects to previous piece in ring: (piece_id - 2) or 6\n")
cat("    - Side 4 connects to next piece in ring: (piece_id - 1) or 1\n")
