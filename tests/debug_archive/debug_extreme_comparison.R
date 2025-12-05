# Debug: Create extreme comparison to show the difference

source("R/logging.R")
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  if (!grepl("scripts_archive", f)) source(f)
}

cat("=== Extreme Comparison: 2-ring puzzle ===\n\n")

# 2-ring puzzle has only 7 pieces and larger border segments
# This should make the difference more visible
rings <- 2
diameter <- 300
seed <- 1234

warped_hex <- generate_puzzle(
  type = "hexagonal",
  seed = seed,
  grid = c(rings),
  size = c(diameter),
  do_warp = TRUE,
  do_trunc = TRUE,
  do_circular_border = FALSE
)

perfect_circle <- generate_puzzle(
  type = "hexagonal",
  seed = seed,
  grid = c(rings),
  size = c(diameter),
  do_warp = TRUE,
  do_trunc = TRUE,
  do_circular_border = TRUE
)

# For 2-ring, outer pieces are 2-7
cat("Piece 2 (outer piece) comparison:\n\n")
warped_path <- warped_hex$pieces[[2]]$path
circle_path <- perfect_circle$pieces[[2]]$path

cat("Warped Hexagon (L commands):\n")
cat(warped_path, "\n\n")

cat("Perfect Circle (A commands):\n")
cat(circle_path, "\n\n")

# Calculate theoretical sag for 2-ring
boundary_vertices <- 6  # Much fewer vertices
arc_angle_degrees <- 360 / boundary_vertices  # 60 degrees!
arc_angle_radians <- arc_angle_degrees * pi / 180
circle_radius <- diameter / 2
sag <- circle_radius * (1 - cos(arc_angle_radians / 2))

cat("=== Mathematical Analysis (2-ring) ===\n")
cat(sprintf("Arc angle per segment: %.0f degrees\n", arc_angle_degrees))
cat(sprintf("Arc sag: %.1f mm\n", sag))
cat("\nWith 60-degree arcs, the difference should be much more visible!\n")

# Save for visual comparison
writeLines(warped_hex$svg_content, "output/debug_warped_hex_2ring.svg")
writeLines(perfect_circle$svg_content, "output/debug_perfect_circle_2ring.svg")
cat("\n2-ring SVGs saved to output/ for visual comparison\n")
cat("Open both files to see the difference in border shape.\n")
