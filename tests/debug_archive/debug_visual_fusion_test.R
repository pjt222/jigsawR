devtools::load_all()

cat("Generating visual test for fusion edges\n")
cat("========================================\n\n")

# Generate concentric puzzle with fusion groups
result <- generate_puzzle(
  type = "concentric",
  seed = 42,
  grid = c(3),
  size = c(300),
  tabsize = 7,
  jitter = 2,
  offset = 50,  # Separated mode
  fusion_groups = list(c(1, 2), c(5, 6, 7)),
  fusion_style = "dashed",
  fusion_opacity = 0.5,
  fill_color = "none",  # No fill - only outlines
  save_files = TRUE,
  output_dir = "output",
  filename_prefix = "fusion_test"
)

# Count dashed paths in SVG
svg <- result$svg_content
matches <- gregexpr("stroke-dasharray", svg)[[1]]
n_dashed <- if (matches[1] == -1) 0 else length(matches)

cat(sprintf("SVG generated: %s\n", result$files$svg))
cat(sprintf("Total dashed paths: %d\n", n_dashed))
cat("\nExpected: 3 dashed paths (one per fused edge pair after dedup):\n")
cat("  1. Piece 1 edge '1' / Piece 2 INNER\n")
cat("  2. Piece 5 RIGHT / Piece 6 LEFT (deduped)\n")
cat("  3. Piece 6 RIGHT / Piece 7 LEFT (deduped)\n")

if (n_dashed == 3) {
  cat("\n✓ PASS: Correct number of dashed paths!\n")
} else {
  cat(sprintf("\n✗ FAIL: Expected 3 dashed paths, got %d\n", n_dashed))
}

# Also generate with fill to verify piece shapes are correct
result2 <- generate_puzzle(
  type = "concentric",
  seed = 42,
  grid = c(3),
  size = c(300),
  tabsize = 7,
  jitter = 2,
  offset = 50,
  fusion_groups = list(c(1, 2), c(5, 6, 7)),
  fusion_style = "dashed",
  fusion_opacity = 0.5,
  fill_color = "#ffffff",  # White fill
  save_files = TRUE,
  output_dir = "output",
  filename_prefix = "fusion_test_filled"
)

cat(sprintf("\nAlso generated filled version: %s\n", result2$files$svg))
