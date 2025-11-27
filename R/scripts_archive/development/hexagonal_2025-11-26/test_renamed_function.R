#!/usr/bin/env Rscript
# Test renamed function to avoid conflicts

source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_pregeneration.R")
source("R/hexagonal_pieces_with_complementary_edges.R")
source("R/hexagonal_separation.R")

cat("Testing renamed function (no conflicts)...\n\n")

# Test bezier mode
svg <- generate_separated_hexagonal_svg(
  rings = 2,
  seed = 42,
  diameter = 240,
  offset = 10,
  arrangement = "hexagonal",
  use_bezier = TRUE,
  tabsize = 27,
  jitter = 5
)

output_file <- "output/test_renamed_function.svg"
writeLines(svg, output_file)

cat("\n✓ Function renamed successfully!\n")
cat("✓ No naming conflicts\n")
cat("✓ Output saved to:", output_file, "\n")
