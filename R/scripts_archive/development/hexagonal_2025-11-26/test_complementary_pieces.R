#!/usr/bin/env Rscript
# Test hexagonal pieces with complementary edges

source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_pregeneration.R")
source("R/hexagonal_pieces_with_complementary_edges.R")

cat("Generating hexagonal puzzle with complementary edges\n")
cat("=====================================================\n\n")

# Generate 2-ring puzzle
rings <- 2
pieces <- generate_hex_pieces_with_edges(
  rings = rings,
  seed = 42,
  diameter = 240,
  separated = TRUE
)

cat(sprintf("\nGenerated %d pieces\n\n", length(pieces)))

# Display piece info
for (p in pieces) {
  cat(sprintf("Piece %d: ring=%d, type=%s, position=(%.1f, %.1f)\n",
              p$id, p$ring, p$type, p$center_x, p$center_y))
}

cat("\n")

# Create SVG visualization
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
      stroke-width: 1.5;
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

# Add pieces with different colors by ring
colors <- c("#e3f2fd", "#90caf9")
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
output_file <- "output/hex_complementary_2ring.svg"
dir.create("output", showWarnings = FALSE, recursive = TRUE)
writeLines(svg_content, output_file)

cat("SVG saved to:", output_file, "\n")
cat("\n✓ Pieces generated with COMPLEMENTARY edges!\n")
cat("  Adjacent pieces should fit together perfectly.\n")
