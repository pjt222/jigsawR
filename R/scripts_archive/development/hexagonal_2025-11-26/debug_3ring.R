#!/usr/bin/env Rscript
source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")
source("R/hexagonal_separation.R")

cat("Generating 3-ring puzzle...\n")
svg <- generate_separated_hexagonal_svg(
  rings = 3,
  seed = 42,
  diameter = 240,
  offset = 10,
  arrangement = "hexagonal",
  use_bezier = TRUE,
  tabsize = 27,
  jitter = 5
)

writeLines(svg, "output/debug_3ring.svg")
cat("Saved to output/debug_3ring.svg\n")
cat("SVG length:", nchar(svg), "characters\n")
