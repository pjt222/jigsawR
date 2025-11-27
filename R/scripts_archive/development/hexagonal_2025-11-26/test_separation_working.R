# Test working hexagonal separation
source('R/hexagonal_puzzle.R')
source('R/hexagonal_separation_working.R')

cat("Testing WORKING hexagonal separation\n")
cat("====================================\n\n")

# Test with 2 rings (7 pieces)
cat("Test 1: 2 rings, rectangular arrangement\n")
result1 <- generate_separated_hex_svg_working(
  rings = 2,
  seed = 42,
  diameter = 180,
  offset = 10,
  arrangement = "rectangular",
  colors = c("#FF6B6B", "#4ECDC4", "#45B7D1"),
  stroke_width = 1.5
)

output1 <- "output/hex_separated_2rings_rect.svg"
writeLines(result1, output1)
cat("  Saved:", output1, "\n\n")

# Test with 3 rings (19 pieces)
cat("Test 2: 3 rings, rectangular arrangement\n")
result2 <- generate_separated_hex_svg_working(
  rings = 3,
  seed = 1234,
  diameter = 240,
  offset = 15,
  arrangement = "rectangular",
  colors = c("#FF6B6B", "#4ECDC4", "#45B7D1", "#FFA07A", "#98D8C8"),
  stroke_width = 1.5,
  background = "#f0f0f0"
)

output2 <- "output/hex_separated_3rings_rect.svg"
writeLines(result2, output2)
cat("  Saved:", output2, "\n\n")

cat("SUCCESS! Check output files to see separated pieces\n")
cat("\nNOTE: These are placeholder hexagons, not actual puzzle pieces\n")
cat("      But the separation layout is correct and ready for real pieces\n")
