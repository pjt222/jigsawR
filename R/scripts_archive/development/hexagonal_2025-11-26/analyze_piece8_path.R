#!/usr/bin/env Rscript
source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")

rings <- 3
seed <- 42

edge_data <- generate_hex_edge_map(rings, seed, 240, 27, 5)

cat("Piece 8 Edge Analysis\n")
cat("=====================\n\n")

for (side in 0:5) {
  edge_key <- sprintf("8-%d", side)
  edge <- edge_data$piece_edge_map[[edge_key]]
  
  cat(sprintf("Side %d: type=%s\n", side, edge$type))
  if (edge$type == "internal") {
    # Find which piece it connects to
    neighbor <- get_hex_neighbor(8, side, rings)
    cat(sprintf("  Connects to: Piece %d\n", neighbor))
    cat(sprintf("  Edge key: %s\n", edge$edge_key))
  }
  cat(sprintf("  Forward path (first 60 chars): %s\n", substring(edge$forward, 1, 60)))
  cat("\n")
}

# Check for duplicate content
cat("Checking for duplicate edges in path...\n")
all_edges <- c()
for (side in 0:5) {
  edge_key <- sprintf("8-%d", side)
  edge <- edge_data$piece_edge_map[[edge_key]]
  all_edges <- c(all_edges, edge$forward)
}

# Check if any edge appears multiple times
unique_count <- length(unique(all_edges))
total_count <- length(all_edges)
cat(sprintf("Total edges: %d, Unique edges: %d\n", total_count, unique_count))

if (unique_count < total_count) {
  cat("WARNING: Some edges are duplicated!\n")
}
