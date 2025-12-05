#!/usr/bin/env Rscript
# Debug warp paths - examine each piece carefully

source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")
source("R/hexagonal_separation.R")

cat("=== Generating pieces WITHOUT warp ===\n")
pieces_no_warp <- suppressWarnings(generate_hex_pieces_with_edge_map(
  rings = 2, seed = 42, diameter = 240,
  separated = TRUE, separation_factor = 1.2, do_warp = FALSE
))

cat("\n=== Generating pieces WITH warp ===\n")
pieces_with_warp <- suppressWarnings(generate_hex_pieces_with_edge_map(
  rings = 2, seed = 42, diameter = 240,
  separated = TRUE, separation_factor = 1.2, do_warp = TRUE
))

# Compare each piece
for (i in 1:length(pieces_no_warp)) {
  p_no <- pieces_no_warp[[i]]
  p_warp <- pieces_with_warp[[i]]

  cat(sprintf("\n========== Piece %d (ring %d) ==========\n", p_no$id, p_no$ring))

  cat("\nWithout warp:\n")
  cat(p_no$path, "\n")

  cat("\nWith warp:\n")
  cat(p_warp$path, "\n")

  # Check if paths are different
  if (p_no$path == p_warp$path) {
    cat("\n>>> SAME PATH - warp had no effect!\n")
  } else {
    cat("\n>>> DIFFERENT - warp modified this piece\n")
  }

  # Check for arc commands
  has_arc <- grepl("A ", p_warp$path)
  cat(sprintf("Has arc commands: %s\n", has_arc))
}

# Save both SVGs for visual comparison
cat("\n=== Saving SVG files for comparison ===\n")

svg_no_warp <- generate_separated_hexagonal_svg(
  rings = 2, seed = 42, diameter = 240,
  offset = 20, arrangement = "hexagonal",
  use_bezier = TRUE, do_warp = FALSE
)

svg_with_warp <- generate_separated_hexagonal_svg(
  rings = 2, seed = 42, diameter = 240,
  offset = 20, arrangement = "hexagonal",
  use_bezier = TRUE, do_warp = TRUE
)

writeLines(svg_no_warp, "output/debug_no_warp.svg")
writeLines(svg_with_warp, "output/debug_with_warp.svg")

cat("Saved: output/debug_no_warp.svg\n")
cat("Saved: output/debug_with_warp.svg\n")
