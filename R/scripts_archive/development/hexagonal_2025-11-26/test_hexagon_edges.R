#!/usr/bin/env Rscript
# Understand hexagon edge vs vertex orientation

cat('Flat-top hexagon edge/vertex positions:\n')
cat('==========================================\n')
cat('Base offset: 30° (π/6) for flat-top\n\n')

# For a flat-top hexagon, vertices are at:
# 30°, 90°, 150°, 210°, 270°, 330°

# Edges (midpoints between vertices) are at:
# 0°, 60°, 120°, 180°, 240°, 300°

cat('Vertex angles (pointy parts):\n')
for (i in 0:5) {
  vertex_angle <- 30 + i * 60
  cat(sprintf('  Vertex %d: %3d°\n', i, vertex_angle))
}

cat('\nEdge angles (flat sides, midpoints):\n')
for (i in 0:5) {
  edge_angle <- i * 60
  cat(sprintf('  Edge %d: %3d°\n', i, edge_angle))
}

cat('\nFor pieces pointing at these angles:\n')
cat('=====================================\n')
test_angles <- c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330)
for (angle in test_angles) {
  # Check if angle aligns with edge (multiple of 60) or vertex (30 + multiple of 60)
  if (angle %% 60 == 0) {
    type <- 'EDGE faces center - correct!'
  } else {
    type <- 'VERTEX faces center - WRONG!'
  }
  cat(sprintf('  Piece at %3d°: %s\n', angle, type))
}
