#!/usr/bin/env Rscript
# Test hexagonal rotation fix

source('R/hexagonal_topology.R')
source('R/hexagonal_puzzle.R')
source('R/hexagonal_separation.R')

cat("Generating separated hexagonal layout with rotation...\n")

svg_content <- generate_separated_hexagonal_svg(
  rings = 3,
  seed = 42,
  diameter = 240,
  offset = 20,
  arrangement = 'hexagonal'
)

# Save to output directory
if (!dir.exists('output')) dir.create('output')
writeLines(svg_content, 'output/hex_separated_rotated_test.svg')
cat('Generated: output/hex_separated_rotated_test.svg\n')
