#!/usr/bin/env Rscript
# Debug bezier path content

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

# Check piece 2, side 2 (internal edge to piece 7)
edge <- edge_data$piece_edge_map[["2-2"]]

cat("Piece 2, Side 2 (Internal edge to Piece 7)\n")
cat("===========================================\n\n")
cat("Type:", edge$type, "\n")
cat("Edge key:", edge$edge_key, "\n")
cat("Is forward:", edge$is_forward, "\n\n")

cat("Forward path (full):\n")
cat(edge$forward, "\n\n")

cat("Character count:", nchar(edge$forward), "\n\n")

# Check if it has 3 complete bezier curves
# Each curve: C x1 y1 x2 y2 x3 y3
# Pattern: "C " + 6 numbers with spaces
cat("Expected pattern: C x1 y1 x2 y2 x3 y3 C x1 y1 x2 y2 x3 y3 C x1 y1 x2 y2 x3 y3\n")
cat("Count 'C' occurrences:", lengths(regmatches(edge$forward, gregexpr("C", edge$forward))), "\n")
