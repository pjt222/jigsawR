#!/usr/bin/env Rscript
# Test hexagonal individual pieces generation

cat("Loading source files...\n")
source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")
source("R/hexagonal_individual_pieces.R")

cat("Testing generate_hexagonal_individual_pieces()...\n")

# Test with 2-ring puzzle (7 pieces)
result <- generate_hexagonal_individual_pieces(
  rings = 2,
  seed = 42,
  diameter = 240,
  output_dir = "output/hex_individual_test",
  save_combined = TRUE,
  save_individual = TRUE
)

cat("\n=== RESULTS ===\n")
cat("Pieces generated:", result$parameters$num_pieces, "\n")
cat("Individual files:", length(result$files$individual), "\n")
cat("Combined file:", result$files$combined, "\n")

cat("\nFiles created:\n")
files <- list.files("output/hex_individual_test", pattern = "\\.svg$")
for (f in files) {
  cat(" -", f, "\n")
}

cat("\n=== TEST PASSED ===\n")
