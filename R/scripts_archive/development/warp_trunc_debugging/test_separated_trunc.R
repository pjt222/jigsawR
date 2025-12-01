#!/usr/bin/env Rscript
# Test do_trunc for separated hexagonal mode

cat("=== Testing do_trunc for Separated Hexagonal Mode ===\n\n")

# Source dependencies
source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")
source("R/hexagonal_separation.R")
source("R/hexagonal_puzzle.R")

# Test parameters
rings <- 2
seed <- 42
diameter <- 240
offset <- 15

cat("Parameters:\n")
cat(sprintf("  rings: %d\n", rings))
cat(sprintf("  seed: %d\n", seed))
cat(sprintf("  diameter: %.0f\n", diameter))
cat(sprintf("  offset: %.0f\n", offset))
cat("\n")

# Test 1: Separated mode WITHOUT truncation
cat("--- Test 1: Separated mode WITHOUT do_trunc ---\n")
svg_no_trunc <- generate_separated_hexagonal_svg(
  rings = rings,
  seed = seed,
  diameter = diameter,
  offset = offset,
  arrangement = "hexagonal",
  use_bezier = TRUE,
  do_warp = FALSE,
  do_trunc = FALSE
)
cat(sprintf("SVG length: %d characters\n\n", nchar(svg_no_trunc)))

# Test 2: Separated mode WITH truncation
cat("--- Test 2: Separated mode WITH do_trunc ---\n")
svg_with_trunc <- generate_separated_hexagonal_svg(
  rings = rings,
  seed = seed,
  diameter = diameter,
  offset = offset,
  arrangement = "hexagonal",
  use_bezier = TRUE,
  do_warp = FALSE,
  do_trunc = TRUE
)
cat(sprintf("SVG length: %d characters\n\n", nchar(svg_with_trunc)))

# Save both for visual comparison
output_dir <- "output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

writeLines(svg_no_trunc, file.path(output_dir, "test_separated_no_trunc.svg"))
writeLines(svg_with_trunc, file.path(output_dir, "test_separated_with_trunc.svg"))

cat("Saved test files:\n")
cat("  - output/test_separated_no_trunc.svg\n")
cat("  - output/test_separated_with_trunc.svg\n")
cat("\n")

# Test 3: Verify do_warp and do_trunc are mutually exclusive (warp takes precedence)
cat("--- Test 3: Both do_warp and do_trunc (warp should take precedence) ---\n")
svg_both <- generate_separated_hexagonal_svg(
  rings = rings,
  seed = seed,
  diameter = diameter,
  offset = offset,
  arrangement = "hexagonal",
  use_bezier = TRUE,
  do_warp = TRUE,
  do_trunc = TRUE
)
cat(sprintf("SVG length: %d characters\n\n", nchar(svg_both)))

writeLines(svg_both, file.path(output_dir, "test_separated_warp_and_trunc.svg"))
cat("  - output/test_separated_warp_and_trunc.svg\n")

cat("\n=== Tests complete! ===\n")
cat("Compare the SVG files visually to verify:\n")
cat("  1. no_trunc: jagged star-shaped boundary\n")
cat("  2. with_trunc: clean hexagonal boundary\n")
cat("  3. warp_and_trunc: circular boundary (warp takes precedence)\n")
