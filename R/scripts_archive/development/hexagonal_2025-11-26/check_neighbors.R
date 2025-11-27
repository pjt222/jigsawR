#!/usr/bin/env Rscript
source("R/hexagonal_neighbors.R")

cat("Checking neighbor relationships for 3-ring puzzle\n")
cat("==================================================\n\n")

rings <- 3

# Check piece 8 (outer ring) - should connect to piece 2 (ring 1)
cat("Piece 8 (ring 2) neighbors:\n")
for (side in 0:5) {
  neighbor <- get_hex_neighbor(8, side, rings)
  cat(sprintf("  Side %d: %s\n", side, if (is.na(neighbor)) "NA (border)" else sprintf("Piece %d", neighbor)))
}

cat("\n")

# Check piece 2 (ring 1) neighbors
cat("Piece 2 (ring 1) neighbors:\n")
for (side in 0:5) {
  neighbor <- get_hex_neighbor(2, side, rings)
  cat(sprintf("  Side %d: %s\n", side, if (is.na(neighbor)) "NA (border)" else sprintf("Piece %d", neighbor)))
}

cat("\n")
cat("Expected: Piece 8 should connect to piece 2 on at least one side\n")
