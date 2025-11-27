#!/usr/bin/env Rscript
# Visual test for hexagonal piece generation

source("R/hexagonal_topology.R")
source("R/rotation_utils.R")
source("R/hexagonal_puzzle.R")
source("R/hexagonal_edge_generation.R")

cat("Generating visual test for hexagonal pieces\n")

# Generate all pieces for a 3-ring puzzle (separated)
pieces <- generate_all_hex_pieces(
  rings = 3,
  seed = 42,
  diameter = 240,
  separated = TRUE,
  base_spacing = 60,
  separation_factor = 1.2
)

cat(sprintf("Generated %d pieces\n", length(pieces)))

# Calculate viewBox
all_x <- sapply(pieces, function(p) p$center_x)
all_y <- sapply(pieces, function(p) p$center_y)

margin <- 50
min_x <- min(all_x) - margin
max_x <- max(all_x) + margin
min_y <- min(all_y) - margin
max_y <- max(all_y) + margin

width <- max_x - min_x
height <- max_y - min_y

# Generate SVG
svg_lines <- c(
  sprintf('<svg width="%.1fmm" height="%.1fmm" viewBox="%.2f %.2f %.2f %.2f" xmlns="http://www.w3.org/2000/svg">',
          width, height, min_x, min_y, width, height),
  '  <title>Hexagonal Pieces - Direct Generation (Placeholder)</title>',
  '  <g id="pieces">'
)

# Color palette
colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#FFA07A", "#98D8C8",
            "#F3A683", "#778BEB", "#E77F67", "#CF6A87", "#786FA6")

# Add each piece
for (i in seq_along(pieces)) {
  piece <- pieces[[i]]

  # Select color based on piece type
  if (piece$type == "center") {
    color <- "#FFD93D"
  } else if (piece$type == "edge") {
    color <- "#6BCB77"
  } else {
    color <- colors[((i - 1) %% length(colors)) + 1]
  }

  # Translate path to absolute coordinates
  path <- piece$path

  svg_lines <- c(svg_lines,
    sprintf('    <path d="%s" fill="none" stroke="%s" stroke-width="2" opacity="0.9"/>',
            path, color),
    sprintf('    <text x="%.2f" y="%.2f" text-anchor="middle" dominant-baseline="central" font-size="8" fill="%s">%d</text>',
            piece$center_x, piece$center_y, color, i),
    sprintf('    <text x="%.2f" y="%.2f" text-anchor="middle" dominant-baseline="central" font-size="6" fill="%s">%.0f°</text>',
            piece$center_x, piece$center_y + 12, color, piece$rotation_degrees)
  )
}

svg_lines <- c(svg_lines, '  </g>', '</svg>')

# Save to file
if (!dir.exists("output")) dir.create("output")
output_file <- "output/hex_pieces_direct_gen_test.svg"
writeLines(paste(svg_lines, collapse = "\n"), output_file)

cat(sprintf("Saved to: %s\n", output_file))
cat("View the SVG to see hexagonal pieces with rotation!\n")
