#!/usr/bin/env Rscript
source("R/hexagonal_neighbors.R")

rings <- 3

cat("Ring 2 pieces (8, 9, 10) and their ring 1 connections:\n")
cat("======================================================\n\n")

for (piece_id in c(8, 9, 10)) {
  cat(sprintf("Piece %d:\n", piece_id))
  for (side in 0:5) {
    neighbor <- get_hex_neighbor(piece_id, side, rings)
    if (!is.na(neighbor) && neighbor >= 2 && neighbor <= 7) {
      cat(sprintf("  Side %d → Piece %d (ring 1)\n", side, neighbor))
    }
  }
  cat("\n")
}

cat("\nExpected symmetry:\n")
cat("  Piece 2 side 1 → Piece 8, so Piece 8 should have ONE side → Piece 2\n")
cat("  Piece 2 side 0 → Piece 9, so Piece 9 should have ONE side → Piece 2\n")
cat("  Piece 2 side 5 → Piece 10, so Piece 10 should have ONE side → Piece 2\n")
