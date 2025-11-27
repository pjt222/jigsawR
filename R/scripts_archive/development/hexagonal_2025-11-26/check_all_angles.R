#!/usr/bin/env Rscript
source("R/hexagonal_topology.R")

rings <- 3

cat("Ring 1 pieces (2-7):\n")
for (piece_id in 2:7) {
  ring_info <- map_piece_id_to_ring(piece_id, rings)
  cat(sprintf("  Piece %d: ring=%d, position=%d, angle=%.2f rad (%.0f deg)\n",
              piece_id, ring_info$ring, ring_info$position, 
              ring_info$angle, ring_info$angle * 180 / pi))
}

cat("\nRing 2 pieces (8-19):\n")
for (piece_id in 8:13) {
  ring_info <- map_piece_id_to_ring(piece_id, rings)
  cat(sprintf("  Piece %d: ring=%d, position=%d, angle=%.2f rad (%.0f deg)\n",
              piece_id, ring_info$ring, ring_info$position,
              ring_info$angle, ring_info$angle * 180 / pi))
}

cat("\nExpected: Ring 1 pieces at 0°, 60°, 120°, 180°, 240°, 300°\n")
cat("Expected: Ring 2 pieces at 0°, 30°, 60°, 90°, 120°, etc (12 pieces)\n")
