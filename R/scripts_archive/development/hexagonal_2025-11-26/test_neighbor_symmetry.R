#!/usr/bin/env Rscript
source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")

cat("Testing neighbor symmetry\n")
cat("=========================\n\n")

rings <- 3

# Test piece 2 ←→ piece 8
cat("Piece 2 (ring 1) ←→ Piece 8 (ring 2)\n")
cat("Piece 2 side 1 → ", get_hex_neighbor(2, 1, rings), "\n")
cat("Piece 8 should connect back to piece 2\n")

for (side in 0:5) {
  neighbor <- get_hex_neighbor(8, side, rings)
  if (!is.na(neighbor) && neighbor == 2) {
    cat(sprintf("Piece 8 side %d → Piece 2 ✓\n", side))
  }
}

cat("\n")

# Check piece 8's same-ring neighbors
cat("Piece 8 same-ring neighbors:\n")
ring_info_8 <- map_piece_id_to_ring(8, rings)
cat(sprintf("Piece 8: ring %d, position %d\n", ring_info_8$ring, ring_info_8$position))

cat("Side 2 (previous): ", get_hex_neighbor(8, 2, rings), "\n")
cat("Side 4 (next): ", get_hex_neighbor(8, 4, rings), "\n")

# What should they be?
pieces_in_ring <- 6 * ring_info_8$ring
first_in_ring <- 8
prev_pos <- (ring_info_8$position - 1 + pieces_in_ring) %% pieces_in_ring
next_pos <- (ring_info_8$position + 1) %% pieces_in_ring

cat(sprintf("Expected side 2 → piece %d\n", first_in_ring + prev_pos))
cat(sprintf("Expected side 4 → piece %d\n", first_in_ring + next_pos))
