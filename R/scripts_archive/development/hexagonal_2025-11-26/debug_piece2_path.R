#!/usr/bin/env Rscript
# Debug piece 2 path assembly

source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")

rings <- 2
seed <- 42

# Generate edge map
edge_data <- generate_hex_edge_map(
  rings = rings,
  seed = seed,
  diameter = 240,
  tabsize = 27,
  jitter = 5
)

# Examine piece 2's edges
cat("Piece 2 Edge Analysis\n")
cat("=====================\n\n")

for (side in 0:5) {
  edge_key <- sprintf("2-%d", side)
  edge <- edge_data$piece_edge_map[[edge_key]]

  cat(sprintf("Side %d:\n", side))
  cat(sprintf("  Type: %s\n", edge$type))
  cat(sprintf("  Start: (%.2f, %.2f)\n", edge$start[1], edge$start[2]))
  cat(sprintf("  End: (%.2f, %.2f)\n", edge$end[1], edge$end[2]))
  cat(sprintf("  Forward: %s\n", substring(edge$forward, 1, 50)))

  if (edge$type == "internal") {
    neighbor <- get_hex_neighbor(2, side, rings)
    cat(sprintf("  Connects to: Piece %d\n", neighbor))
    cat(sprintf("  Edge key: %s\n", edge$edge_key))
  }
  cat("\n")
}

# Show the current path assembly
cat("Current Path Assembly:\n")
cat("=====================\n")
path_parts <- c()
first_edge <- edge_data$piece_edge_map[["2-0"]]
path_parts <- c(sprintf("M %.2f %.2f", first_edge$start[1], first_edge$start[2]))

for (side in 0:5) {
  edge_key <- sprintf("2-%d", side)
  edge <- edge_data$piece_edge_map[[edge_key]]
  path_parts <- c(path_parts, edge$forward)
}
path_parts <- c(path_parts, "Z")

cat(paste(path_parts, collapse = " "))
cat("\n\n")

# What it SHOULD be
cat("Corrected Path Assembly:\n")
cat("========================\n")
cat("(Description of correct structure)\n\n")

cat("Issue: Border edges contain 'L x y' commands\n")
cat("These L commands create duplicate line-to operations\n")
cat("The path should flow: M -> edges -> Z\n")
cat("Each edge should continue from where the previous ended\n")
