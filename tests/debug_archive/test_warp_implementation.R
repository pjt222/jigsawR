#!/usr/bin/env Rscript
# Test: Warp implementation for separated hexagonal pieces

source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")
source("R/hexagonal_separation.R")

cat("=== TESTING WARP IMPLEMENTATION ===\n\n")

rings <- 2
diameter <- 240
seed <- 42

# Test 1: Generate without warp
cat("Test 1: Generate pieces WITHOUT warp\n")
cat("--------------------------------------\n")
pieces_no_warp <- generate_hex_pieces_with_edge_map(
  rings = rings,
  seed = seed,
  diameter = diameter,
  tabsize = 27,
  jitter = 5,
  separated = TRUE,
  separation_factor = 1.2,
  do_warp = FALSE
)

cat(sprintf("Generated %d pieces without warp\n", length(pieces_no_warp)))

# Check border edges (should be straight lines "L x y")
piece1_path <- pieces_no_warp[[1]]$path
has_arc_no_warp <- grepl("A ", piece1_path)
cat(sprintf("Piece 1 has arc commands: %s\n", has_arc_no_warp))

# Test 2: Generate with warp
cat("\nTest 2: Generate pieces WITH warp\n")
cat("--------------------------------------\n")
pieces_with_warp <- generate_hex_pieces_with_edge_map(
  rings = rings,
  seed = seed,
  diameter = diameter,
  tabsize = 27,
  jitter = 5,
  separated = TRUE,
  separation_factor = 1.2,
  do_warp = TRUE
)

cat(sprintf("Generated %d pieces with warp\n", length(pieces_with_warp)))

# Check border edges (should have arc commands "A rx ry ...")
# Border pieces are in the outer ring
num_pieces <- length(pieces_with_warp)
border_piece_id <- num_pieces  # Last piece should be in outer ring

border_piece_path <- pieces_with_warp[[border_piece_id]]$path
has_arc_with_warp <- grepl("A ", border_piece_path)
cat(sprintf("Border piece %d has arc commands: %s\n", border_piece_id, has_arc_with_warp))

# Test 3: Test warp function directly
cat("\nTest 3: Test apply_hex_warp function\n")
cat("--------------------------------------\n")

# Test a point at hex corner (should be warped inward)
corner_x <- 30
corner_y <- 0
warped <- apply_hex_warp(corner_x, corner_y)
cat(sprintf("Corner (%.2f, %.2f) -> warped (%.2f, %.2f)\n",
            corner_x, corner_y, warped$x, warped$y))

# The warped point should be closer to origin for corners
original_dist <- sqrt(corner_x^2 + corner_y^2)
warped_dist <- sqrt(warped$x^2 + warped$y^2)
cat(sprintf("Original distance: %.2f, Warped distance: %.2f\n", original_dist, warped_dist))

if (warped_dist < original_dist) {
  cat("PASS: Corner point warped inward (as expected for hex-to-circle)\n")
} else {
  cat("FAIL: Corner point should be warped inward\n")
}

# Test a point at hex edge midpoint (should stay same distance)
# For flat-top hexagon, edge midpoints are at 30°, 90°, 150°, etc.
edge_mid_angle <- 30 * pi / 180
edge_mid_x <- 30 * cos(edge_mid_angle)
edge_mid_y <- 30 * sin(edge_mid_angle)
warped_mid <- apply_hex_warp(edge_mid_x, edge_mid_y)
mid_orig_dist <- sqrt(edge_mid_x^2 + edge_mid_y^2)
mid_warp_dist <- sqrt(warped_mid$x^2 + warped_mid$y^2)
cat(sprintf("\nEdge midpoint at 30° (%.2f, %.2f) -> warped (%.2f, %.2f)\n",
            edge_mid_x, edge_mid_y, warped_mid$x, warped_mid$y))
cat(sprintf("Original distance: %.2f, Warped distance: %.2f\n", mid_orig_dist, mid_warp_dist))

if (abs(mid_warp_dist - mid_orig_dist) < 0.1) {
  cat("PASS: Edge midpoint stays at same distance (as expected)\n")
} else {
  cat("FAIL: Edge midpoint should stay at same distance\n")
}

# Test 4: Generate full SVG with warp
cat("\nTest 4: Generate separated SVG with warp\n")
cat("--------------------------------------\n")

svg_with_warp <- generate_separated_hexagonal_svg(
  rings = rings,
  seed = seed,
  diameter = diameter,
  offset = 10,
  arrangement = "hexagonal",
  use_bezier = TRUE,
  tabsize = 27,
  jitter = 5,
  do_warp = TRUE,
  do_trunc = FALSE
)

svg_has_arc <- grepl("A ", svg_with_warp)
cat(sprintf("SVG with warp contains arc commands: %s\n", svg_has_arc))

# Save test outputs
output_dir <- "output"
if (!dir.exists(output_dir)) dir.create(output_dir)

writeLines(svg_with_warp, file.path(output_dir, "test_warp_enabled.svg"))
cat(sprintf("\nSaved: %s\n", file.path(output_dir, "test_warp_enabled.svg")))

# Also generate without warp for comparison
svg_no_warp <- generate_separated_hexagonal_svg(
  rings = rings,
  seed = seed,
  diameter = diameter,
  offset = 10,
  arrangement = "hexagonal",
  use_bezier = TRUE,
  tabsize = 27,
  jitter = 5,
  do_warp = FALSE,
  do_trunc = FALSE
)

writeLines(svg_no_warp, file.path(output_dir, "test_warp_disabled.svg"))
cat(sprintf("Saved: %s\n", file.path(output_dir, "test_warp_disabled.svg")))

cat("\n=== SUMMARY ===\n")
cat("Compare the two SVG files:\n")
cat("  - test_warp_disabled.svg: Hexagonal boundary (straight edges)\n")
cat("  - test_warp_enabled.svg: Circular boundary (arc edges)\n")
cat("\nBorder pieces should have curved edges in the warped version.\n")
