# Debug circle center piece edge directions

cat("=== Debug Circle Center Piece ===\n\n")

source("R/logging.R")
source("R/concentric_geometry.R")
source("R/hexagonal_bezier_generation.R")
source("R/concentric_edge_generation.R")

rings <- 3
diameter <- 200
seed <- 42
tabsize <- diameter * 0.10

# Generate with circle center
cat("Generating with center_shape = 'circle'...\n\n")
edge_data <- generate_concentric_edge_map(
  rings = rings,
  seed = seed,
  diameter = diameter,
  tabsize = tabsize,
  jitter = 4,
  center_shape = "circle"
)

# Check piece 1 (center)
piece1_info <- edge_data$piece_vertices[[1]]
piece1_edges <- edge_data$piece_edges[[1]]

cat("=== Piece 1 (Center) Info ===\n")
cat(sprintf("Type: %s\n", piece1_info$type))
cat(sprintf("Radius: %.2f\n", piece1_info$radius))
cat(sprintf("Number of edges: %d\n\n", length(piece1_edges)))

# Show each edge
cat("=== Piece 1 Edges ===\n")
for (i in seq_along(piece1_edges)) {
  edge <- piece1_edges[[i]]
  cat(sprintf("\nEdge %d:\n", i))
  cat(sprintf("  type: %s\n", edge$type))
  cat(sprintf("  edge_ref: %s\n", edge$edge_ref))
  cat(sprintf("  is_forward: %s\n", edge$is_forward))

  # Get actual edge data
  edge_info <- edge_data$edge_map[[edge$edge_ref]]
  if (!is.null(edge_info)) {
    cat(sprintf("  start: (%.2f, %.2f)\n", edge_info$start[1], edge_info$start[2]))
    cat(sprintf("  end: (%.2f, %.2f)\n", edge_info$end[1], edge_info$end[2]))
    cat(sprintf("  piece1: %d, piece2: %d\n", edge_info$piece1, edge_info$piece2))

    # Calculate angles
    start_angle <- atan2(edge_info$start[2], edge_info$start[1]) * 180 / pi
    end_angle <- atan2(edge_info$end[2], edge_info$end[1]) * 180 / pi
    cat(sprintf("  start angle: %.1f°, end angle: %.1f°\n", start_angle, end_angle))
  }
}

# Check ring 1 pieces (2-7) - their INNER edges
cat("\n\n=== Ring 1 Pieces (2-7) INNER Edges ===\n")
for (pid in 2:7) {
  piece_info <- edge_data$piece_vertices[[pid]]
  piece_edges <- edge_data$piece_edges[[pid]]

  inner_edges <- piece_edges[sapply(piece_edges, function(e) e$type == "inner")]

  cat(sprintf("\nPiece %d (pos %d):\n", pid, piece_info$position))
  cat(sprintf("  V1 (inner-start): (%.2f, %.2f)\n",
              piece_info$vertices[[1]][1], piece_info$vertices[[1]][2]))
  cat(sprintf("  V2 (inner-end): (%.2f, %.2f)\n",
              piece_info$vertices[[2]][1], piece_info$vertices[[2]][2]))

  if (length(inner_edges) > 0) {
    edge <- inner_edges[[1]]
    cat(sprintf("  Inner edge ref: %s\n", edge$edge_ref))
    cat(sprintf("  is_forward: %s\n", edge$is_forward))

    edge_info <- edge_data$edge_map[[edge$edge_ref]]
    if (!is.null(edge_info)) {
      cat(sprintf("  Edge start: (%.2f, %.2f)\n", edge_info$start[1], edge_info$start[2]))
      cat(sprintf("  Edge end: (%.2f, %.2f)\n", edge_info$end[1], edge_info$end[2]))
    }
  }
}

# Build and display center piece path
cat("\n\n=== Center Piece Path ===\n")
path <- build_concentric_piece_path(1, edge_data)
cat(sprintf("Path length: %d chars\n", nchar(path)))
cat(sprintf("Path: %s\n", substr(path, 1, 200)))
cat("...\n")

# Also generate hexagon center for comparison
cat("\n\n=== Comparison: Hexagon Center ===\n")
edge_data_hex <- generate_concentric_edge_map(
  rings = rings,
  seed = seed,
  diameter = diameter,
  tabsize = tabsize,
  jitter = 4,
  center_shape = "hexagon"
)

piece1_hex <- edge_data_hex$piece_vertices[[1]]
cat(sprintf("Hexagon center type: %s\n", piece1_hex$type))
cat(sprintf("Number of vertices: %d\n", length(piece1_hex$vertices)))

path_hex <- build_concentric_piece_path(1, edge_data_hex)
cat(sprintf("Hexagon path length: %d chars\n", nchar(path_hex)))

cat("\n=== Done ===\n")
