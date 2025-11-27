#!/usr/bin/env Rscript
# Test hexagonal grid iteration pattern

n <- 3
yl <- 2 * n - 1

cat("yl =", yl, "\n")
cat("yi range: from", -yl + 2, "to", yl - 2, "\n\n")

piece_id <- 1
for (yi in seq(-yl + 2, yl - 2, by = 2)) {
  xl <- 2 * n - 1 - (abs(yi) - 1) / 2
  cat("yi =", sprintf("%3d", yi), ", xl =", sprintf("%3.1f", xl))
  cat(", xi range: [", sprintf("%3.1f", -xl + 1), ",", sprintf("%3.1f", xl - 2), "]\n")
  for (xi in seq(-xl + 1, xl - 2, by = 1)) {
    cat(sprintf("  Piece %2d: xi=%3.1f, yi=%3d\n", piece_id, xi, yi))
    piece_id <- piece_id + 1
  }
}

cat("\nTotal pieces:", piece_id - 1, "\n")
