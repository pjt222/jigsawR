#!/usr/bin/env Rscript
source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")

pieces <- suppressWarnings(generate_hex_pieces_with_edge_map(
  rings = 2, seed = 42, diameter = 240,
  separated = TRUE, separation_factor = 1.2, do_warp = TRUE
))
cat("Generated", length(pieces), "pieces\n")

# Check border piece's path for arc commands
p7 <- pieces[[7]]
has_arc <- grepl("A ", p7$path)
cat("Piece 7 has arc commands:", has_arc, "\n")

# Show first 200 chars of path
cat("Path preview:\n")
cat(substr(p7$path, 1, 200), "\n")
