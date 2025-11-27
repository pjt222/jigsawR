#!/usr/bin/env Rscript
source('R/hexagonal_topology.R')

# Check rotation angles for ring 2 (outer ring)
# Ring 0: piece 1 (center)
# Ring 1: pieces 2-7 (6 pieces)
# Ring 2: pieces 8-19 (12 pieces)

cat('Ring 2 pieces (8-19) rotation angles:\n')
cat('=====================================\n')
for (id in 8:19) {
  ring_info <- map_piece_id_to_ring(id, rings = 3)
  cat(sprintf('Piece %2d: ring=%d, pos=%2d, angle=%.2f° (%.4f rad)\n',
              id, ring_info$ring, ring_info$position,
              ring_info$angle * 180 / pi, ring_info$angle))
}

cat('\nExpected for 12 pieces in ring 2:\n')
cat('Every 30 degrees (360/12 = 30)\n')
for (i in 0:11) {
  expected <- i * 30
  cat(sprintf('Position %2d: %.2f°\n', i, expected))
}
