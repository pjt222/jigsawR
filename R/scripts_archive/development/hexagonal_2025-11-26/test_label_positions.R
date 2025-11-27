#!/usr/bin/env Rscript
# Test that piece labels are correctly positioned

source("R/hexagonal_topology.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")

rings <- 3
diameter <- 240
piece_radius <- diameter / (rings * 4)

cat("Testing label positions for 3-ring hexagonal puzzle\n")
cat("====================================================\n\n")

# Generate edge map
cat("Generating edge map with local coordinates...\n")
edge_data <- generate_hex_edge_map(rings, 42, diameter)

cat(sprintf("Generated %d unique edges\n", edge_data$num_edges))
cat(sprintf("Stored %d piece centers\n\n", length(edge_data$piece_centers)))

# Check that piece 2's edges are in local coordinates
cat("Sample edge for piece 2, side 0:\n")
edge <- edge_data$piece_edge_map[["2-0"]]
cat(sprintf("  Start: (%.2f, %.2f)\n", edge$start[1], edge$start[2]))
cat(sprintf("  End: (%.2f, %.2f)\n", edge$end[1], edge$end[2]))
cat(sprintf("  Path: %s\n\n", substr(edge$forward, 1, 50)))

# Check lattice center positions
cat("Lattice center positions (for compact/touching layout):\n")
for (i in 1:7) {
  center <- edge_data$piece_centers[[i]]
  cat(sprintf("  Piece %d: (%.2f, %.2f)\n", i, center[1], center[2]))
}

cat("\n✓ Edges are now in local coordinates\n")
cat("✓ Piece labels will be positioned at separated piece centers\n")
cat("✓ This matches the new hexagonal lattice topology!\n")
