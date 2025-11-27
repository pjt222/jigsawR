#!/usr/bin/env Rscript
source('R/hexagonal_topology.R')
source('R/hexagonal_puzzle.R')
source('R/hexagonal_separation.R')

cat("Testing flat-top hexagon orientation\n")

svg_content <- generate_separated_hexagonal_svg(
  rings = 3,
  seed = 42,
  diameter = 240,
  offset = 20,
  arrangement = 'hexagonal'
)

if (!dir.exists('output')) dir.create('output')
writeLines(svg_content, 'output/hex_flat_top_test.svg')
cat('Generated: output/hex_flat_top_test.svg\n')
cat('Now hexagons should have flat edges facing each other!\n')
