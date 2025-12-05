#!/usr/bin/env Rscript
# Test warp and trunc comparison between old and new implementations

source("R/logging.R")
source("R/config_utils.R")
source("R/hexagonal_puzzle.R")  # Old complete mode
source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")
source("R/unified_piece_generation.R")
source("R/piece_positioning.R")
source("R/unified_renderer.R")

cat("\n=== Warp/Trunc Comparison Test ===\n\n")

# Test parameters
rings <- 3
diameter <- 200
seed <- 42
tabsize <- 27
jitter <- 5

# Ensure output dir exists
if (!dir.exists("output")) dir.create("output")

# Test all 4 combinations
test_cases <- list(
  list(do_warp = FALSE, do_trunc = FALSE, name = "neither"),
  list(do_warp = FALSE, do_trunc = TRUE,  name = "trunc_only"),
  list(do_warp = TRUE,  do_trunc = FALSE, name = "warp_only"),
  list(do_warp = TRUE,  do_trunc = TRUE,  name = "both")
)

cat("Testing all 4 combinations...\n\n")

for (tc in test_cases) {
  cat(sprintf("=== Testing: %s (warp=%s, trunc=%s) ===\n",
              tc$name, tc$do_warp, tc$do_trunc))

  # 1. Generate using OLD complete mode (hexagonal_puzzle.R)
  cat("  Generating OLD complete mode...\n")
  old_result <- generate_hex_jigsaw_svg(
    seed = seed,
    tabsize = tabsize,
    jitter = jitter,
    diameter = diameter,
    rings = rings,
    do_warp = tc$do_warp,
    do_trunc = tc$do_trunc
  )
  old_svg <- old_result$svg

  # Save old SVG
  old_file <- sprintf("output/compare_old_%s.svg", tc$name)
  writeLines(old_svg, old_file)
  cat(sprintf("    Saved: %s (%d chars)\n", old_file, nchar(old_svg)))

  # 2. Generate using NEW unified pipeline
  cat("  Generating NEW unified pipeline...\n")
  new_pieces <- generate_pieces_internal(
    type = "hexagonal",
    seed = seed,
    grid = c(rings),
    size = c(diameter),
    tabsize = tabsize,
    jitter = jitter,
    do_warp = tc$do_warp,
    do_trunc = tc$do_trunc
  )

  # Position (offset=0 for compact)
  positioned <- apply_piece_positioning(new_pieces, offset = 0)

  # Render
  new_svg <- render_puzzle_svg(
    positioned,
    fill = "none",
    stroke_width = 1.5,
    palette = "black",
    background = "none",
    opacity = 1.0
  )

  # Save new SVG
  new_file <- sprintf("output/compare_new_%s.svg", tc$name)
  writeLines(new_svg, new_file)
  cat(sprintf("    Saved: %s (%d chars)\n", new_file, nchar(new_svg)))

  # Compare border paths
  cat("  Analyzing OLD border path...\n")
  old_border <- old_result$border
  cat(sprintf("    Border length: %d chars\n", nchar(old_border)))
  cat(sprintf("    Border starts: %s\n", substr(old_border, 1, 100)))

  # Check for expected content based on mode
  has_arc <- grepl(" a | A ", old_border)
  has_line <- grepl(" L ", old_border)

  if (tc$do_trunc && tc$do_warp) {
    # Should have arcs
    if (has_arc) {
      cat("    OLD: Has arcs (CORRECT for trunc+warp)\n")
    } else {
      cat("    OLD: MISSING arcs (WRONG for trunc+warp)\n")
    }
  } else if (tc$do_trunc) {
    # Should have straight lines forming hexagon
    if (has_line) {
      cat("    OLD: Has straight lines (expected for trunc only)\n")
    }
  }

  cat("\n")
}

cat("=== Visual Comparison Required ===\n")
cat("Please open the output files in a browser to compare:\n")
cat("  - compare_old_*.svg (deprecated complete mode)\n")
cat("  - compare_new_*.svg (unified pipeline)\n\n")

# Now let's specifically check the warp formula
cat("=== Testing Warp Formula ===\n\n")

# Test point at hex corner (should move inward)
test_x <- 50
test_y <- 0  # Right corner

warped <- apply_hex_warp(test_x, test_y)
cat(sprintf("Input: (%0.2f, %0.2f)\n", test_x, test_y))
cat(sprintf("Output (apply_hex_warp): (%0.2f, %0.2f)\n", warped$x, warped$y))
cat(sprintf("Original distance: %0.2f\n", sqrt(test_x^2 + test_y^2)))
cat(sprintf("Warped distance: %0.2f\n", sqrt(warped$x^2 + warped$y^2)))
cat(sprintf("Ratio: %0.4f (should be ~0.866 for corner)\n",
            sqrt(warped$x^2 + warped$y^2) / sqrt(test_x^2 + test_y^2)))

# Compare with original hex_warp from hexagonal_puzzle.R
cat("\nComparing with original hex_warp function from hexagonal_puzzle.R:\n")
# Temporarily enable do_warp to test
.hex_jigsaw_env$do_warp <- TRUE
.hex_jigsaw_env$radius <- 100
.hex_jigsaw_env$offset <- 20

# The hex_warp function expects a list(x=, y=)
test_vec <- list(x = test_x, y = test_y)
original_warped <- hex_warp(test_vec)
cat(sprintf("Original hex_warp output: (%0.2f, %0.2f)\n", original_warped$x, original_warped$y))
cat(sprintf("Original ratio: %0.4f\n",
            sqrt(original_warped$x^2 + original_warped$y^2) / sqrt(test_x^2 + test_y^2)))

.hex_jigsaw_env$do_warp <- FALSE

cat("\n=== Test Complete ===\n")
