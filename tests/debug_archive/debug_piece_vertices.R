# Debug: Analyze piece vertices to understand size variation

cat("=" , rep("=", 70), "\n", sep = "")
cat("Debug: Piece Vertex Analysis for warp+trunc mode\n")
cat("=" , rep("=", 70), "\n\n", sep = "")

source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")

rings <- 3
diameter <- 240
seed <- 1234
piece_radius <- diameter / (rings * 4)

cat(sprintf("Parameters:\n"))
cat(sprintf("  rings: %d\n", rings))
cat(sprintf("  diameter: %d\n", diameter))
cat(sprintf("  piece_radius: %.2f\n", piece_radius))
cat("\n")

# Generate edge data with warp+trunc
cat("Generating edge map with do_warp=TRUE, do_trunc=TRUE...\n\n")
edge_data <- generate_hex_edge_map(rings, seed, diameter, tabsize = 20, jitter = 4,
                                    do_warp = TRUE, do_trunc = TRUE)

# Analyze the vertices for each piece
num_pieces <- 3 * rings * (rings - 1) + 1

# For each piece, look at its 6 edges and extract the START vertex
# (which is the actual hexagon vertex, not the bezier control points)
cat("Analyzing piece vertex distances from origin:\n\n")

for (piece_id in c(1, 2, 8, 10, 13)) {  # Center, ring1, and some ring2 pieces
  ring_info <- map_piece_id_to_ring(piece_id, rings)

  cat(sprintf("Piece %d (ring %d):\n", piece_id, ring_info$ring))

  vertex_dists <- c()
  for (side in 0:5) {
    edge_key <- sprintf("%d-%d", piece_id, side)
    edge <- edge_data$piece_edge_map[[edge_key]]

    v <- edge$start
    dist <- sqrt(v[1]^2 + v[2]^2)
    vertex_dists <- c(vertex_dists, dist)

    cat(sprintf("  Side %d: start=(%.2f, %.2f), dist=%.2f, type=%s\n",
                side, v[1], v[2], dist, edge$type))
  }

  cat(sprintf("  Vertex distance range: [%.2f, %.2f]\n", min(vertex_dists), max(vertex_dists)))
  cat("\n")
}

# Now let's look at what the "base" hexagon vertices should be
# before any bezier tabs are added
cat("\n")
cat("=" , rep("=", 70), "\n", sep = "")
cat("Expected hexagon sizes (vertex-to-vertex, no tabs):\n")
cat("=" , rep("=", 70), "\n\n", sep = "")

# For a regular hexagon with "radius" r (center to vertex),
# the width (flat-to-flat for pointy-top, or vertex-to-vertex for flat-top) is 2*r
# The height is sqrt(3)*r for pointy-top, or 2*r for flat-top

# Our hexagons are flat-top (vertices at 0, 60, 120, 180, 240, 300 degrees)
# Width (horizontal) = 2 * piece_radius
# Height (vertical) = sqrt(3) * piece_radius

cat(sprintf("Base piece_radius: %.2f\n", piece_radius))
cat(sprintf("Expected hexagon width (2*r): %.2f\n", 2 * piece_radius))
cat(sprintf("Expected hexagon height (sqrt(3)*r): %.2f\n", sqrt(3) * piece_radius))
cat("\n")

# After warp transformation, what should the sizes be?
# The warp transformation maps the hexagonal grid to a circle
# Inner pieces should shrink slightly, outer pieces should stretch slightly
# But the KEY question is: are they stretching MORE than expected?

cat("Checking actual bounding boxes of pieces (vertices only, no bezier):\n\n")

for (piece_id in 1:num_pieces) {
  ring_info <- map_piece_id_to_ring(piece_id, rings)

  vertices_x <- c()
  vertices_y <- c()

  for (side in 0:5) {
    edge_key <- sprintf("%d-%d", piece_id, side)
    edge <- edge_data$piece_edge_map[[edge_key]]
    v <- edge$start
    vertices_x <- c(vertices_x, v[1])
    vertices_y <- c(vertices_y, v[2])
  }

  width <- max(vertices_x) - min(vertices_x)
  height <- max(vertices_y) - min(vertices_y)

  if (piece_id == 1 || ring_info$ring == 2) {
    cat(sprintf("Piece %2d (ring %d): vertex bbox = %.2f x %.2f\n",
                piece_id, ring_info$ring, width, height))
  }
}
