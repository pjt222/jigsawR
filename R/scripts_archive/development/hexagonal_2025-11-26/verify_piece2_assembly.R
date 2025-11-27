#!/usr/bin/env Rscript
# Verify piece 2 path assembly is geometrically correct

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

cat("Piece 2: Detailed Edge Connection Analysis\n")
cat("===========================================\n\n")

# Check that edges connect properly end-to-end
for (side in 0:5) {
  next_side <- (side + 1) %% 6

  edge_key <- sprintf("2-%d", side)
  next_key <- sprintf("2-%d", next_side)

  edge <- edge_data$piece_edge_map[[edge_key]]
  next_edge <- edge_data$piece_edge_map[[next_key]]

  cat(sprintf("Side %d → Side %d:\n", side, next_side))
  cat(sprintf("  Edge %d ends at: (%.2f, %.2f)\n", side, edge$end[1], edge$end[2]))
  cat(sprintf("  Edge %d starts at: (%.2f, %.2f)\n", next_side, next_edge$start[1], next_edge$start[2]))

  # Check if they connect
  connects <- all(abs(edge$end - next_edge$start) < 0.01)
  cat(sprintf("  Connection: %s\n", ifelse(connects, "✓ GOOD", "✗ BROKEN")))
  cat("\n")
}

# Now assemble the path and show it
cat("\nPath Assembly:\n")
cat("==============\n")

first_edge <- edge_data$piece_edge_map[["2-0"]]
cat(sprintf("M %.2f %.2f  # Start at side 0 start\n", first_edge$start[1], first_edge$start[2]))

for (side in 0:5) {
  edge_key <- sprintf("2-%d", side)
  edge <- edge_data$piece_edge_map[[edge_key]]

  cat(sprintf("\n# Side %d (%s edge):\n", side, edge$type))
  cat(edge$forward, "\n")
}

cat("Z  # Close path\n")
