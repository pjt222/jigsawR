#!/usr/bin/env Rscript
# Test hexagonal grid iteration pattern to understand piece numbering

rings <- 3
n <- rings
yl <- 2 * n - 1

cat('Hexagonal grid iteration for', rings, 'rings:\n')
cat('========================================\n\n')

piece_id <- 1
for (yi in seq(-yl + 2, yl - 2, by = 2)) {
  xl <- 2 * n - 1 - (abs(yi) - 1) / 2
  cat(sprintf('yi=%3d: xl=%.1f, xi range: [%.1f to %.1f]\n', yi, xl, -xl + 1, xl - 2))

  for (xi in seq(-xl + 1, xl - 2, by = 1)) {
    cat(sprintf('  Piece %2d: (xi=%3.1f, yi=%3d)\n', piece_id, xi, yi))
    piece_id <- piece_id + 1
  }
  cat('\n')
}

cat('Total pieces:', piece_id - 1, '\n')
cat('Expected:', 3 * rings * (rings - 1) + 1, '\n')
