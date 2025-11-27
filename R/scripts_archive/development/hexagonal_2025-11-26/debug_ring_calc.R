#!/usr/bin/env Rscript
source("R/hexagonal_topology.R")

rings <- 3
piece_id <- 8

ring_info <- map_piece_id_to_ring(piece_id, rings)
cat("Piece", piece_id, "\n")
cat("  Ring:", ring_info$ring, "\n")
cat("  Position:", ring_info$position, "\n\n")

ring <- ring_info$ring
position <- ring_info$position
pieces_in_ring <- 6 * ring

# Calculate first piece ID in this ring
first_in_ring <- 1 + sum(6 * (1:(ring - 1)))
if (ring == 1) first_in_ring <- 2

cat("Calculated first_in_ring:", first_in_ring, "\n")
cat("Pieces in ring:", pieces_in_ring, "\n\n")

# Same-ring neighbors
prev_pos <- (position - 1 + pieces_in_ring) %% pieces_in_ring
next_pos <- (position + 1) %% pieces_in_ring

cat("Previous position:", prev_pos, "→ piece", first_in_ring + prev_pos, "\n")
cat("Next position:", next_pos, "→ piece", first_in_ring + next_pos, "\n")

cat("\nActual ring 2 pieces should be: 8-19\n")
cat("If position 0 is piece 8:\n")
cat("  prev (pos 11) should be piece 19\n")
cat("  next (pos 1) should be piece 9\n")
