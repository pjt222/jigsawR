#!/usr/bin/env Rscript
# Test 3-ring puzzle generation

source("R/hexagonal_topology.R")
source("R/hexagonal_bezier_generation.R")

# Generate 3-ring puzzle
cat("Generating 3-ring hexagonal puzzle (19 pieces)...\n")
pieces <- generate_all_hex_pieces_bezier(
  rings = 3,
  seed = 42,
  diameter = 240,
  separated = TRUE
)

cat("Generated", length(pieces), "pieces\n\n")

# Count piece types
center_count <- sum(sapply(pieces, function(p) p$type == "center"))
inner_count <- sum(sapply(pieces, function(p) p$type == "inner"))
edge_count <- sum(sapply(pieces, function(p) p$type == "edge"))

cat("Piece type distribution:\n")
cat("  Center:", center_count, "\n")
cat("  Inner:", inner_count, "\n")
cat("  Edge:", edge_count, "\n\n")

# Check ring distribution
for (ring in 0:2) {
  count <- sum(sapply(pieces, function(p) p$ring == ring))
  expected <- if (ring == 0) 1 else 6 * ring
  cat("Ring", ring, "- Pieces:", count, "(expected:", expected, ")\n")
}

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
      font-size: 10px;
      text-anchor: middle;
      fill: black;
    }
  </style>
  <rect x="%.2f" y="%.2f" width="%.2f" height="%.2f" fill="#f0f0f0"/>
',
  min_x, min_y, width, height,
  min_x, min_y, width, height
)

# Add pieces with different colors by ring
colors <- c("#e3f2fd", "#90caf9", "#42a5f5")  # Light to dark blue
for (p in pieces) {
  fill_color <- colors[p$ring + 1]
  svg_content <- paste0(svg_content, sprintf(
    '  <path class="puzzle-piece" d="%s" fill="%s"/>\n',
    p$path, fill_color
  ))
  svg_content <- paste0(svg_content, sprintf(
    '  <text class="piece-label" x="%.2f" y="%.2f">%d</text>\n',
    p$center_x, p$center_y, p$id
  ))
}

svg_content <- paste0(svg_content, '</svg>')

# Save to file
output_file <- "output/hex_bezier_3ring.svg"
writeLines(svg_content, output_file)

cat("\nSVG saved to:", output_file, "\n")
