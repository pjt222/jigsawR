# Create visual test cases to identify the missing border issue
devtools::load_all()

# Test 1: Pieces 1-2 fused (top row)
cat("=== Test 1: Top row fused (pieces 1-2) ===\n")
result1 <- generate_puzzle(
  type = "rectangular",
  grid = c(2, 2),
  size = c(200, 200),
  seed = 42,
  offset = 50,
  fusion_groups = list(c(1, 2)),
  fusion_style = "solid",
  fusion_opacity = 1.0,
  save_files = TRUE,
  output_dir = "output/fusion_tests",
  palette = "viridis",
  background = "white"
)
cat("Saved:", result1$files$svg, "\n\n")

# Test 2: Pieces 1-3 fused (left column)
cat("=== Test 2: Left column fused (pieces 1-3) ===\n")
result2 <- generate_puzzle(
  type = "rectangular",
  grid = c(2, 2),
  size = c(200, 200),
  seed = 42,
  offset = 50,
  fusion_groups = list(c(1, 3)),
  fusion_style = "solid",
  fusion_opacity = 1.0,
  save_files = TRUE,
  output_dir = "output/fusion_tests",
  palette = "viridis",
  background = "white"
)
cat("Saved:", result2$files$svg, "\n\n")

# Test 3: All 4 pieces fused
cat("=== Test 3: All pieces fused (1-2-3-4) ===\n")
result3 <- generate_puzzle(
  type = "rectangular",
  grid = c(2, 2),
  size = c(200, 200),
  seed = 42,
  offset = 50,
  fusion_groups = list(c(1, 2, 3, 4)),
  fusion_style = "solid",
  fusion_opacity = 1.0,
  save_files = TRUE,
  output_dir = "output/fusion_tests",
  palette = "viridis",
  background = "white"
)
cat("Saved:", result3$files$svg, "\n\n")

cat("=== Visual Inspection Required ===\n")
cat("Please open these SVG files and check:\n")
cat("1. Are ALL borders visible?\n")
cat("2. Are fused edges drawn with opacity?\n")
cat("3. Are any edges completely missing?\n")
