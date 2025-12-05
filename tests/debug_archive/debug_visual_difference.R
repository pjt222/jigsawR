# Debug: Analyze the visual difference between L and A commands

source("R/logging.R")
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  if (!grepl("scripts_archive", f)) source(f)
}

cat("=== Analyzing Visual Difference Between L and A Border Commands ===\n\n")

# Generate both puzzles
rings <- 7  # Use more rings to see more border segments
diameter <- 400
seed <- 1234

cat("Generating 7-ring puzzles for visual comparison...\n\n")

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

# Count L and A commands in boundary pieces (outer ring pieces)
# For 7-ring: outer ring starts at piece 92 (pieces 92-127)
outer_ring_start <- 3 * rings * (rings - 1) - (6 * (rings - 1)) + 1 + 1  # First piece of outer ring

cat("Outer ring starts at piece:", outer_ring_start, "\n")
cat("Total pieces:", length(warped_hex$pieces), "\n\n")

# Sample some outer pieces
sample_pieces <- c(92, 100, 110, 120, 127)

cat("Comparing border commands in sample outer pieces:\n")
cat("(Each outer piece has 2-3 border edges)\n\n")

for (pid in sample_pieces) {
  if (pid <= length(warped_hex$pieces)) {
    warped_path <- warped_hex$pieces[[pid]]$path
    circle_path <- perfect_circle$pieces[[pid]]$path

    # Count L and A commands
    warped_L <- length(gregexpr(" L ", warped_path)[[1]])
    warped_A <- length(gregexpr(" A ", warped_path)[[1]])
    circle_L <- length(gregexpr(" L ", circle_path)[[1]])
    circle_A <- length(gregexpr(" A ", circle_path)[[1]])

    cat(sprintf("Piece %d:\n", pid))
    cat(sprintf("  Warped Hex:     L=%d, A=%d\n", warped_L, warped_A))
    cat(sprintf("  Perfect Circle: L=%d, A=%d\n", circle_L, circle_A))
  }
}

# Calculate the mathematical difference for a single arc segment
# For a circle with radius 200 and ~36 vertices on boundary (6 * rings),
# each arc spans approximately 360/36 = 10 degrees

cat("\n=== Mathematical Analysis ===\n")
circle_radius <- diameter / 2
boundary_vertices <- 6 * rings  # Approximate - actually fewer due to vertex sharing
arc_angle_degrees <- 360 / boundary_vertices

cat(sprintf("Circle radius: %.1f\n", circle_radius))
cat(sprintf("Approximate boundary vertices: %d\n", boundary_vertices))
cat(sprintf("Arc angle per segment: ~%.1f degrees\n", arc_angle_degrees))

# For a 10-degree arc at radius 200, the "sag" (difference between arc and chord) is:
# sag = r * (1 - cos(angle/2))
arc_angle_radians <- arc_angle_degrees * pi / 180
sag <- circle_radius * (1 - cos(arc_angle_radians / 2))
cat(sprintf("Arc sag (deviation from straight line): ~%.3f mm\n", sag))
cat("\nThis is the maximum pixel difference between L and A at the midpoint of each border segment.\n")

# Save both for visual comparison
writeLines(warped_hex$svg_content, "output/debug_warped_hex_7ring.svg")
writeLines(perfect_circle$svg_content, "output/debug_perfect_circle_7ring.svg")
cat("\n7-ring SVGs saved to output/ for visual comparison\n")
