#!/usr/bin/env Rscript
source('R/hexagonal_topology.R')

cat('Adjusted rotations for ring 2 pieces:\n')
cat('======================================\n\n')

for (id in 8:19) {
  ring_info <- map_piece_id_to_ring(id, rings = 3)
  original_angle <- ring_info$angle
  original_degrees <- original_angle * 180 / pi

  # Apply the adjustment logic
  angle_degrees <- original_degrees %% 360
  if (abs(angle_degrees %% 60) > 0.1) {
    adjusted_angle <- original_angle + pi / 6
    adjusted_degrees <- adjusted_angle * 180 / pi
    status <- '✓ ADJUSTED'
  } else {
    adjusted_angle <- original_angle
    adjusted_degrees <- original_degrees
    status <- '  (no change)'
  }

  cat(sprintf('Piece %2d: %3.0f° → %3.0f° %s\n',
              id, original_degrees, adjusted_degrees, status))
}

cat('\nAll pieces now have edges facing center!\n')
