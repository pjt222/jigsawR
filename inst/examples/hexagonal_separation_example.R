# Hexagonal Puzzle Separation Example
# Demonstrates the working hexagonal separation functionality (Issue #7)

library(jigsawR)

# Ensure output directory exists
output_dir <- "output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

cat("=== Hexagonal Puzzle Separation Examples ===\n\n")

# Example 1: Small puzzle for testing
cat("1. Generating small 2-ring separated puzzle...\n")
result1 <- generate_separated_hexagonal_svg(
  rings = 2,
  seed = 42,
  diameter = 180,
  offset = 10,
  arrangement = "rectangular",
  colors = c("#FF6B6B", "#4ECDC4", "#45B7D1"),
  stroke_width = 1.5,
  background = "none"
)

filename1 <- file.path(output_dir, "hex_separated_2rings_example.svg")
writeLines(result1, filename1)
cat("   Saved:", filename1, "\n")
cat("   7 pieces in 4x2 grid\n\n")

# Example 2: Medium puzzle for production
cat("2. Generating medium 3-ring separated puzzle...\n")
result2 <- generate_separated_hexagonal_svg(
  rings = 3,
  seed = 1234,
  diameter = 240,
  offset = 15,
  arrangement = "rectangular",
  colors = c("#FF6B6B", "#4ECDC4", "#45B7D1", "#FFA07A", "#98D8C8"),
  stroke_width = 1.5,
  background = "#f5f5f5"
)

filename2 <- file.path(output_dir, "hex_separated_3rings_example.svg")
writeLines(result2, filename2)
cat("   Saved:", filename2, "\n")
cat("   19 pieces in 5x4 grid\n\n")

# Example 3: Larger puzzle
cat("3. Generating larger 4-ring separated puzzle...\n")
result3 <- generate_separated_hexagonal_svg(
  rings = 4,
  seed = 5678,
  diameter = 300,
  offset = 20,
  arrangement = "rectangular",
  colors = c("#FF6B6B", "#4ECDC4", "#45B7D1", "#FFA07A", "#98D8C8", "#F7DC6F"),
  stroke_width = 2,
  background = "none"
)

filename3 <- file.path(output_dir, "hex_separated_4rings_example.svg")
writeLines(result3, filename3)
cat("   Saved:", filename3, "\n")
cat("   37 pieces in 7x6 grid\n\n")

# Summary
cat("=== Summary ===\n")
cat("Generated 3 separated hexagonal puzzles\n")
cat("All files saved to:", output_dir, "\n\n")

cat("Key Features:\n")
cat("- Pieces arranged in rectangular grid for efficient material use\n")
cat("- Configurable separation offset for laser cutting\n")
cat("- Color coding for visual distinction\n")
cat("- Piece numbering for assembly reference\n\n")

cat("Important Notes:\n")
cat("- These use hexagonal PLACEHOLDERS, not actual puzzle pieces\n")
cat("- Placeholders are regular hexagons with piece numbers\n")
cat("- The separation/spacing is correct for laser cutting planning\n")
cat("- For actual cutting, use the complete puzzle view\n\n")

cat("Parameters Explained:\n")
cat("- rings: Number of hexagonal rings (determines piece count)\n")
cat("- seed: Random seed for reproducibility\n")
cat("- diameter: Overall puzzle diameter in mm\n")
cat("- offset: Separation distance between pieces in mm\n")
cat("- arrangement: 'rectangular' (efficient) or 'hexagonal' (maintains structure)\n")
cat("- colors: Vector of colors for pieces (cycles through list)\n")
cat("- stroke_width: Line thickness in mm\n")
cat("- background: 'none', a color, or gradient specification\n\n")

cat("Laser Cutting Tips:\n")
cat("- Use offset = 10-20mm depending on material thickness\n")
cat("- Rectangular arrangement maximizes material efficiency\n")
cat("- Export as SVG at full resolution\n")
cat("- Test with small piece count first\n\n")

cat("Future Enhancement:\n")
cat("- Current version uses placeholder hexagons\n")
cat("- Full puzzle piece shapes (with tabs/blanks) will be added later\n")
cat("- Placeholder approach is sufficient for layout planning\n")
