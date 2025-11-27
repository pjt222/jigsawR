# Test hexagonal grid iteration to understand piece numbering
rings <- 3
num_pieces <- 3 * rings * (rings - 1) + 1
cat('Rings:', rings, '\n')
cat('Total pieces:', num_pieces, '\n\n')

# Simulate the grid iteration from hex_gen_dh
n <- rings
yl <- 2 * n - 1
cat('Grid iteration pattern:\n')
cat('yl =', yl, '(2*n - 1)\n\n')

piece_id <- 1
for (yi in seq(-yl + 2, yl - 2, by = 2)) {
  xl <- 2 * n - 1 - (abs(yi) - 1) / 2
  cat('yi =', sprintf('%3d', yi), ', xl =', sprintf('%3.1f', xl), ', xi range: [',
      sprintf('%3.1f', -xl + 1), ',', sprintf('%3.1f', xl - 2), ']\n')
  for (xi in seq(-xl + 1, xl - 2, by = 1)) {
    cat('  Piece', sprintf('%2d', piece_id), ': (xi =', sprintf('%3.1f', xi), ', yi =', sprintf('%3d', yi), ')\n')
    piece_id <- piece_id + 1
  }
}
