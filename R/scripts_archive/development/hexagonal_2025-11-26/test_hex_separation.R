# Visual test script for hexagonal separation

source("R/hexagonal_topology.R")
source("R/hexagonal_separation.R")

cat("=====================================\n")
cat("Hexagonal Separation Visual Tests\n")
cat("=====================================\n\n")

# Test with different ring counts
for (rings in c(2, 3, 4)) {
  cat(sprintf("Generating %d-ring puzzle...\n", rings))

  svg_content <- generate_separated_hexagonal_svg(
    rings = rings,
    seed = 42,
    offset = 15,
    arrangement = "hexagonal",
    diameter = 240
  )

  filename <- sprintf("output/test_hex_separation_%drings.svg", rings)
  writeLines(svg_content, filename)
  cat(sprintf("✓ Generated %s\n\n", filename))
}

cat("=====================================\n")
cat("Visual Inspection Checklist:\n")
cat("=====================================\n")
cat("1. Pieces form hexagonal pattern (not spiral)\n")
cat("2. Center piece is at center of SVG\n")
cat("3. Ring 1 pieces surround center\n")
cat("4. Spacing is uniform\n")
cat("5. No overlapping pieces\n")
cat("6. All pieces visible in viewBox\n")
cat("7. Piece numbers increase sensibly\n")
cat("=====================================\n")
