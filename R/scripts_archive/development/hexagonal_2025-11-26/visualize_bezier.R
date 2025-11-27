#!/usr/bin/env Rscript
# Visualize hexagonal bezier pieces

source("R/hexagonal_topology.R")
source("R/hexagonal_bezier_generation.R")

# Generate 2-ring puzzle
cat("Generating 2-ring hexagonal puzzle with bezier curves...\n")
pieces <- generate_all_hex_pieces_bezier(
  rings = 2,
  seed = 42,
  diameter = 240,
  separated = TRUE
)

# Calculate SVG dimensions
all_x <- sapply(pieces, function(p) p$center_x)
all_y <- sapply(pieces, function(p) p$center_y)
margin <- 50
min_x <- min(all_x) - margin
max_x <- max(all_x) + margin
min_y <- min(all_y) - margin
max_y <- max(all_y) + margin
width <- max_x - min_x
height <- max_y - min_y

# Create SVG
svg_content <- sprintf(
  '<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" viewBox="%.2f %.2f %.2f %.2f">
  <style>
    .puzzle-piece {
      fill: white;
      stroke: black;
      stroke-width: 1;
      stroke-linejoin: round;
    }
    .piece-label {
      font-family: Arial, sans-serif;
      font-size: 12px;
      text-anchor: middle;
      fill: black;
    }
  </style>
  <rect x="%.2f" y="%.2f" width="%.2f" height="%.2f" fill="#f0f0f0"/>
',
  min_x, min_y, width, height,
  min_x, min_y, width, height
)

# Add pieces
for (p in pieces) {
  svg_content <- paste0(svg_content, sprintf(
    '  <path class="puzzle-piece" d="%s"/>\n',
    p$path
  ))
  svg_content <- paste0(svg_content, sprintf(
    '  <text class="piece-label" x="%.2f" y="%.2f">%d</text>\n',
    p$center_x, p$center_y, p$id
  ))
}

svg_content <- paste0(svg_content, '</svg>')

# Save to file
output_file <- "output/hex_bezier_2ring.svg"
dir.create("output", showWarnings = FALSE, recursive = TRUE)
writeLines(svg_content, output_file)

cat("SVG saved to:", output_file, "\n")
cat("Open this file in a web browser to view the puzzle!\n")
