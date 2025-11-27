#!/usr/bin/env Rscript
# Test 3-ring hexagonal puzzle with complementary edges

source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_pregeneration.R")
source("R/hexagonal_pieces_with_complementary_edges.R")

cat("Generating 3-ring hexagonal puzzle with complementary edges\n")
cat("===========================================================\n\n")

# Generate 3-ring puzzle
rings <- 3
pieces <- generate_hex_pieces_with_edges(
  rings = rings,
  seed = 42,
  diameter = 240,
  separated = TRUE
)

cat(sprintf("\nGenerated %d pieces\n", length(pieces)))

# Count piece types
center_count <- sum(sapply(pieces, function(p) p$type == "center"))
inner_count <- sum(sapply(pieces, function(p) p$type == "inner"))
edge_count <- sum(sapply(pieces, function(p) p$type == "edge"))

cat("\nPiece distribution:\n")
cat(sprintf("  Center: %d\n", center_count))
cat(sprintf("  Inner: %d\n", inner_count))
cat(sprintf("  Edge: %d\n\n", edge_count))

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
      font-size: 10px;
      text-anchor: middle;
      fill: black;
      font-weight: bold;
    }
  </style>
  <rect x="%.2f" y="%.2f" width="%.2f" height="%.2f" fill="#f5f5f5"/>
',
  min_x, min_y, width, height,
  min_x, min_y, width, height
)

# Add pieces with colors by ring
colors <- c("#e3f2fd", "#90caf9", "#42a5f5")
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
output_file <- "output/hex_complementary_3ring.svg"
writeLines(svg_content, output_file)

cat("SVG saved to:", output_file, "\n")
cat("\n✓ 3-ring puzzle generated with COMPLEMENTARY edges!\n")
cat("  Adjacent pieces have matching tab/socket shapes.\n")
