#!/usr/bin/env Rscript
# Test SVG-only function

source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")
source("R/hexagonal_individual_pieces.R")

# Test SVG-only function
svg <- create_hexagonal_individual_pieces_svg(rings = 2, seed = 42)
cat("SVG length:", nchar(svg), "characters\n")
cat("Has paths:", grepl("<path", svg), "\n")
cat("Has title:", grepl("<title>", svg), "\n")
cat("Path count:", length(gregexpr("<path", svg)[[1]]), "\n")
