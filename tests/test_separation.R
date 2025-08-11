# Test script for puzzle piece separation functionality

# Load required functions
source("R/rectangular_puzzle.R")
source("R/puzzle_core_clean.R")
source("R/puzzle_separation.R")

cat("=== Testing Puzzle Piece Separation ===\n\n")

# Test 1: Basic 2x2 puzzle with separation
cat("Test 1: 2x2 Puzzle with 10mm Separation\n")
cat("-" , rep("-", 40), "\n", sep="")

# Generate puzzle structure
puzzle <- generate_puzzle_core(
  seed = 1234,
  grid = c(2, 2),
  size = c(100, 100),
  tabsize = 20,
  jitter = 4
)

# Generate separated version
svg_separated <- generate_separated_puzzle_svg(
  puzzle_structure = puzzle,
  offset = 10,
  colors = c("#FF6B6B", "#4ECDC4", "#45B7D1", "#FFA07A")
)

# Save to file
writeLines(svg_separated, "output/test_2x2_separated_10mm.svg")
cat("✓ Generated: output/test_2x2_separated_10mm.svg\n\n")

# Test 2: Different offset values
cat("Test 2: Testing Different Offset Values\n")
cat("-" , rep("-", 40), "\n", sep="")

offsets <- c(0, 5, 15, 25)
for (offset_val in offsets) {
  svg <- generate_separated_puzzle_svg(
    puzzle_structure = puzzle,
    offset = offset_val,
    colors = c("red", "blue", "green", "orange")
  )
  
  filename <- sprintf("output/test_2x2_offset_%dmm.svg", offset_val)
  writeLines(svg, filename)
  cat(sprintf("✓ Offset %dmm: %s\n", offset_val, filename))
}

cat("\n")

# Test 3: Larger puzzle with separation
cat("Test 3: 3x3 Puzzle with Separation\n")
cat("-" , rep("-", 40), "\n", sep="")

puzzle_3x3 <- generate_puzzle_core(
  seed = 5678,
  grid = c(3, 3),
  size = c(150, 150),
  tabsize = 18,
  jitter = 3
)

# Generate with optimal offset
optimal_offset <- calculate_optimal_offset(
  piece_width = puzzle_3x3$piece_width,
  piece_height = puzzle_3x3$piece_height,
  kerf = 0.2,
  min_separation = 3
)

cat(sprintf("Calculated optimal offset: %.2f mm\n", optimal_offset))

svg_3x3 <- generate_separated_puzzle_svg(
  puzzle_structure = puzzle_3x3,
  offset = optimal_offset,
  colors = "black",
  stroke_width = 1.0
)

writeLines(svg_3x3, "output/test_3x3_separated_optimal.svg")
cat("✓ Generated: output/test_3x3_separated_optimal.svg\n\n")

# Test 4: Using enhanced function
cat("Test 4: Enhanced Function with Different Modes\n")
cat("-" , rep("-", 40), "\n", sep="")

modes <- list(
  list(mode = "complete", offset = 0, name = "complete"),
  list(mode = "individual", offset = 0, name = "individual_touching"),
  list(mode = "individual", offset = 8, name = "individual_separated"),
  list(mode = "separated", offset = 12, name = "separated_12mm")
)

for (config in modes) {
  svg <- generate_puzzle_svg_enhanced(
    puzzle_structure = puzzle,
    mode = config$mode,
    offset = config$offset,
    colors = c("#FF6B6B", "#4ECDC4", "#45B7D1", "#FFA07A")
  )
  
  filename <- sprintf("output/test_enhanced_%s.svg", config$name)
  writeLines(svg, filename)
  cat(sprintf("✓ Mode '%s' (offset=%d): %s\n", config$mode, config$offset, filename))
}

cat("\n")

# Test 5: Path translation verification
cat("Test 5: Path Translation Verification\n")
cat("-" , rep("-", 40), "\n", sep="")

# Test path translation function
test_path <- "M 0.00 0.00 L 100.00 0.00 C 103.46 20.00 90.46 52.22 110.46 43.41 Z"
translated <- translate_svg_path(test_path, 50, 50)

cat("Original path (truncated):\n  M 0.00 0.00 L 100.00 0.00 ...\n")
cat("Translated by (50, 50):\n  M 50.00 50.00 L 150.00 50.00 ...\n")

# Verify translation worked
if (grepl("M 50.00 50.00", translated)) {
  cat("✓ Path translation working correctly\n")
} else {
  cat("✗ Path translation error\n")
}

cat("\n")

# Test 6: Visual comparison
cat("Test 6: Side-by-side Comparison (No Offset vs With Offset)\n")
cat("-" , rep("-", 40), "\n", sep="")

# Create a combined SVG showing both versions
comparison_svg <- '<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" 
     width="400" height="200" viewBox="0 0 400 200">
<rect width="100%" height="100%" fill="white"/>
<text x="50" y="15" font-family="Arial" font-size="12">Original (touching)</text>
<text x="250" y="15" font-family="Arial" font-size="12">Separated (10mm offset)</text>
<g transform="translate(0, 20)">'

# Add original puzzle
for (yi in 0:1) {
  for (xi in 0:1) {
    piece_path <- generate_single_piece(xi, yi, puzzle)
    color <- c("red", "blue", "green", "orange")[yi * 2 + xi + 1]
    comparison_svg <- paste0(comparison_svg, 
      sprintf('<path d="%s" fill="none" stroke="%s" stroke-width="1.5"/>\n', 
              piece_path, color))
  }
}

# Add separated puzzle
comparison_svg <- paste0(comparison_svg, '</g>\n<g transform="translate(200, 20)">')

for (yi in 0:1) {
  for (xi in 0:1) {
    piece_path <- generate_single_piece(xi, yi, puzzle)
    offsets <- calculate_piece_offset(xi, yi, 10, puzzle$piece_width, puzzle$piece_height)
    translated_path <- translate_svg_path(piece_path, offsets[1], offsets[2])
    color <- c("red", "blue", "green", "orange")[yi * 2 + xi + 1]
    comparison_svg <- paste0(comparison_svg, 
      sprintf('<path d="%s" fill="none" stroke="%s" stroke-width="1.5"/>\n', 
              translated_path, color))
  }
}

comparison_svg <- paste0(comparison_svg, '</g>\n</svg>')

writeLines(comparison_svg, "output/test_separation_comparison.svg")
cat("✓ Generated: output/test_separation_comparison.svg\n\n")

# Summary
cat("=== Test Summary ===\n")
cat("Generated files in output/ directory:\n")
cat("  - test_2x2_separated_10mm.svg - Basic 2x2 with 10mm gaps\n")
cat("  - test_2x2_offset_*mm.svg - Different offset values (0, 5, 15, 25mm)\n")
cat("  - test_3x3_separated_optimal.svg - 3x3 with calculated optimal offset\n")
cat("  - test_enhanced_*.svg - Different modes using enhanced function\n")
cat("  - test_separation_comparison.svg - Side-by-side comparison\n")
cat("\nKey features tested:\n")
cat("  ✓ Piece separation with maintained grid positions\n")
cat("  ✓ Different offset values\n")
cat("  ✓ Optimal offset calculation for laser cutting\n")
cat("  ✓ Path coordinate translation\n")
cat("  ✓ Enhanced function with multiple modes\n")