#!/usr/bin/env Rscript
# Debug edge map to understand the structure

source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")

cat("=== Debug Edge Map ===\n\n")

rings <- 2
diameter <- 240
seed <- 42

# Generate edge map with warp
cat("Generating edge map with warp...\n")
edge_data <- suppressWarnings(generate_hex_edge_map(
  rings = rings,
  seed = seed,
  diameter = diameter,
  tabsize = 27,
  jitter = 5,
  do_warp = TRUE
))

cat(sprintf("Generated %d unique internal edges\n\n", edge_data$num_edges))

# Show piece edge map for piece 2 (outer ring)
cat("=== Piece 2 Edge Map (outer ring piece) ===\n")
for (side in 0:5) {
  edge_key <- sprintf("2-%d", side)
  edge <- edge_data$piece_edge_map[[edge_key]]

  cat(sprintf("\nSide %d (%s):\n", side, edge_key))
  cat(sprintf("  Type: %s\n", edge$type))
  cat(sprintf("  Start: (%.2f, %.2f)\n", edge$start[1], edge$start[2]))
  cat(sprintf("  End: (%.2f, %.2f)\n", edge$end[1], edge$end[2]))
  cat(sprintf("  Forward path: %s\n", substr(edge$forward, 1, 80)))
  if (edge$type == "border") {
    cat(sprintf("  Warped: %s\n", edge$warped))
  }
}

# Show piece edge map for piece 1 (center)
cat("\n\n=== Piece 1 Edge Map (center piece) ===\n")
for (side in 0:5) {
  edge_key <- sprintf("1-%d", side)
  edge <- edge_data$piece_edge_map[[edge_key]]

  cat(sprintf("\nSide %d (%s):\n", side, edge_key))
  cat(sprintf("  Type: %s\n", edge$type))
  if (edge$type == "border") {
    cat(sprintf("  Warped: %s\n", edge$warped))
  }
}
