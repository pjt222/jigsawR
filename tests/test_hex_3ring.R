#!/usr/bin/env Rscript
# Test 3-ring hexagonal puzzle

source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")
source("R/hexagonal_individual_pieces.R")

# Test with 3-ring puzzle (19 pieces)
result <- generate_hexagonal_individual_pieces(
  rings = 3,
  seed = 123,
  diameter = 300,
  output_dir = "output/hex_individual_3ring",
  save_combined = TRUE,
  save_individual = TRUE
)

cat("\n=== 3-RING RESULTS ===\n")
cat("Pieces generated:", result$parameters$num_pieces, "\n")
cat("Individual files:", length(result$files$individual), "\n")
cat("Files in directory:", length(list.files("output/hex_individual_3ring", pattern = "\\.svg$")), "\n")
