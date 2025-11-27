#!/usr/bin/env Rscript
# Test integrated hexagonal separation with bezier option

source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_pregeneration.R")
source("R/hexagonal_pieces_with_complementary_edges.R")
source("R/hexagonal_separation.R")

cat("Testing integrated hexagonal separation function\n")
cat("================================================\n\n")

# Test 1: Placeholder mode (default)
cat("Test 1: Placeholder hexagons (use_bezier = FALSE)\n")
cat("--------------------------------------------------\n")
svg_placeholder <- generate_separated_hexagonal_svg(
  rings = 2,
  seed = 42,
  diameter = 240,
  offset = 10,
  arrangement = "hexagonal",
  use_bezier = FALSE
)

output_file1 <- "output/test_placeholder_mode.svg"
dir.create("output", showWarnings = FALSE, recursive = TRUE)
writeLines(svg_placeholder, output_file1)
cat("✓ Placeholder mode saved to:", output_file1, "\n\n")

# Test 2: Bezier mode
cat("Test 2: Bezier curves with tabs (use_bezier = TRUE)\n")
cat("----------------------------------------------------\n")
svg_bezier <- generate_separated_hexagonal_svg(
  rings = 2,
  seed = 42,
  diameter = 240,
  offset = 10,
  arrangement = "hexagonal",
  use_bezier = TRUE,
  tabsize = 27,
  jitter = 5
)

output_file2 <- "output/test_bezier_mode.svg"
writeLines(svg_bezier, output_file2)
cat("✓ Bezier mode saved to:", output_file2, "\n\n")

# Test 3: 3-ring puzzle with bezier
cat("Test 3: 3-ring puzzle with bezier curves\n")
cat("-----------------------------------------\n")
svg_3ring <- generate_separated_hexagonal_svg(
  rings = 3,
  seed = 123,
  diameter = 240,
  offset = 15,
  arrangement = "hexagonal",
  use_bezier = TRUE,
  tabsize = 25,
  jitter = 6
)

output_file3 <- "output/test_3ring_bezier.svg"
writeLines(svg_3ring, output_file3)
cat("✓ 3-ring bezier saved to:", output_file3, "\n\n")

cat("========================================\n")
cat("✓ All tests completed successfully!\n")
cat("========================================\n")
