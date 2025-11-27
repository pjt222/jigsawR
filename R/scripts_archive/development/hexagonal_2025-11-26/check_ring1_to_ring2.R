#!/usr/bin/env Rscript
source("R/hexagonal_neighbors.R")

rings <- 3

cat("Ring 1 pieces and their ring 2 connections:\n")
cat("============================================\n\n")

for (piece_id in 2:7) {
  cat(sprintf("Piece %d:\n", piece_id))
  for (side in 0:5) {
    neighbor <- get_hex_neighbor(piece_id, side, rings)
    if (!is.na(neighbor) && neighbor >= 8) {
      cat(sprintf("  Side %d → Piece %d (ring 2)\n", side, neighbor))
    }
  }
  cat("\n")
}

cat("\nExpected: Each ring 1 piece should connect to ring 2 on sides 0, 1, 5\n")
cat("Piece 2 should connect to pieces 8, 9, and one more\n")
