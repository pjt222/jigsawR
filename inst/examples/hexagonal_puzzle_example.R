# Hexagonal Puzzle Generation Example
# Demonstrates the new hexagonal puzzle support with individual pieces and separation

library(jigsawR)

# Ensure output directory exists
output_dir <- "output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

cat("=== Hexagonal Puzzle Generation Examples ===\n\n")

# Example 1: Basic hexagonal puzzle
cat("1. Generating basic hexagonal puzzle (3 rings)...\n")
result1 <- generate_puzzle(
  type = "hexagonal",
  grid = c(3, 3),  # First value is rings for hexagonal
  size = c(240, 240),  # First value is diameter for hexagonal
  seed = 1234,
  tabsize = 27,
  jitter = 5,
  output = "complete",
  background = "white",
  save_files = TRUE,
  output_dir = output_dir,
  filename_prefix = "hex_basic"
)
cat("   Saved:", result1$files$complete_svg, "\n\n")

# Example 2: Hexagonal puzzle with individual pieces
cat("2. Generating hexagonal puzzle with individual pieces...\n")
result2 <- generate_puzzle(
  type = "hexagonal",
  grid = c(4, 4),  # 4 rings
  size = c(300, 300),  # 300mm diameter
  seed = 5678,
  tabsize = 25,
  jitter = 4,
  output = "individual",
  colors = c("#FF6B6B", "#4ECDC4", "#45B7D1", "#FFA07A"),
  background = "none",
  save_files = TRUE,
  output_dir = output_dir,
  filename_prefix = "hex_individual"
)
cat("   Saved:", result2$files$individual_svg, "\n\n")

# Example 3: Circular puzzle (hexagonal with warping)
cat("3. Generating circular puzzle (hexagonal with circular warp)...\n")
# Note: This requires extending the generate_puzzle function to pass do_warp parameter
# For now, we'll use the direct function
source(system.file("R", "hexagonal_puzzle.R", package = "jigsawR"))
init_hex_jigsaw(seed = 9999, rings = 3, diameter = 200, 
                do_warp = TRUE, do_trunc = TRUE)
hex_first()
svg_content <- generate_hex_jigsaw_svg(
  rings = 3,
  diameter = 200,
  seed = 9999,
  tabsize = 30,
  jitter = 3,
  do_warp = TRUE,
  do_trunc = TRUE
)
filename <- file.path(output_dir, "circular_puzzle.svg")
save_hex_jigsaw_svg(svg_content, filename)
cat("   Saved:", filename, "\n\n")

# Example 4: Hexagonal puzzle with separation (for laser cutting)
cat("4. Generating separated hexagonal puzzle for laser cutting...\n")
# Source separation functions
source(system.file("R", "hexagonal_separation.R", package = "jigsawR"))
separated_svg <- generate_separated_hexagonal_svg(
  rings = 2,
  seed = 2468,
  diameter = 180,
  offset = 10,
  arrangement = "hexagonal",
  tabsize = 28,
  jitter = 5,
  colors = NULL,
  stroke_width = 1,
  background = "none"
)
filename <- file.path(output_dir, "hex_separated.svg")
writeLines(separated_svg, filename)
cat("   Saved:", filename, "\n\n")

# Example 5: Compare rectangular vs hexagonal
cat("5. Comparing rectangular and hexagonal puzzles...\n")

# Rectangular
rect_result <- generate_puzzle(
  type = "rectangular",
  grid = c(3, 3),
  size = c(200, 200),
  seed = 1111,
  output = "complete",
  save_files = TRUE,
  output_dir = output_dir,
  filename_prefix = "compare_rect"
)

# Hexagonal
hex_result <- generate_puzzle(
  type = "hexagonal",
  grid = c(3, 3),
  size = c(200, 200),
  seed = 1111,
  output = "complete",
  save_files = TRUE,
  output_dir = output_dir,
  filename_prefix = "compare_hex"
)

cat("   Rectangular (3x3 = 9 pieces):", rect_result$files$complete_svg, "\n")
cat("   Hexagonal (3 rings = 19 pieces):", hex_result$files$complete_svg, "\n\n")

# Summary
cat("=== Summary ===\n")
cat("Hexagonal puzzle features now supported:\n")
cat("- Basic hexagonal puzzle generation\n")
cat("- Individual piece visualization\n")
cat("- Circular puzzles (with warp and truncation)\n")
cat("- Piece separation for laser cutting\n")
cat("- Integration with Shiny app\n\n")

cat("Note: Full individual piece extraction for hexagonal puzzles\n")
cat("      (tracing each piece boundary) is planned for future development.\n")
cat("      Current implementation shows the complete puzzle structure.\n")