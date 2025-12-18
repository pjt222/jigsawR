# Test fusion functionality

devtools::load_all()

cat("=== Testing Fusion Functionality ===\n\n")

# Test 1: Basic rectangular fusion
cat("Test 1: Rectangular puzzle with fusion 1-2\n")
result <- generate_puzzle(
  type = "rectangular",
  grid = c(2, 2),
  seed = 42,
  fusion_groups = "1-2",
  fusion_style = "dashed",
  fusion_opacity = 0.3,
  save_files = FALSE
)

cat("Fusion data present:", !is.null(result$fusion_data), "\n")
if (!is.null(result$fusion_data)) {
  cat("Number of fused edges:", length(result$fusion_data$fused_edges), "\n")
  cat("Fused edges:\n")
  print(result$fusion_data$fused_edges)
}

cat("\nPiece 1 fused_edges:\n")
print(result$pieces[[1]]$fused_edges)
cat("\nPiece 2 fused_edges:\n")
print(result$pieces[[2]]$fused_edges)

# Check parameters
cat("\nParameters:\n")
cat("  fusion_style:", result$parameters$fusion_style, "\n")
cat("  fusion_opacity:", result$parameters$fusion_opacity, "\n")
cat("  fusion_groups:"); print(result$parameters$fusion_groups)

# Test 2: Check SVG rendering
cat("\n=== Test 2: SVG Rendering ===\n")
svg <- render_puzzle_svg(result, inline = TRUE)
cat("SVG length:", nchar(svg), "\n")

# Check for dashed stroke (fusion indicator)
has_dasharray <- grepl("stroke-dasharray", svg)
cat("Contains stroke-dasharray:", has_dasharray, "\n")

# Check for fusion opacity
has_fusion_opacity <- grepl('opacity="0.30"', svg)
cat("Contains fusion opacity (0.30):", has_fusion_opacity, "\n")

# Look for specific patterns
if (!has_dasharray && !has_fusion_opacity) {
  cat("\n!!! PROBLEM: No fusion styling found in SVG !!!\n")
  cat("This suggests fusion rendering is not working.\n")
}

cat("\n=== Test 3: Hexagonal fusion ===\n")
hex_result <- generate_puzzle(
  type = "hexagonal",
  grid = c(2),
  size = c(100),
  seed = 42,
  fusion_groups = "1-2",
  fusion_style = "dashed",
  fusion_opacity = 0.5,
  save_files = FALSE
)

cat("Hex fusion data present:", !is.null(hex_result$fusion_data), "\n")
cat("Hex parameters fusion_style:", hex_result$parameters$fusion_style, "\n")
cat("Hex parameters fusion_opacity:", hex_result$parameters$fusion_opacity, "\n")

cat("\nHex Piece 1 fused_edges:\n")
print(hex_result$pieces[[1]]$fused_edges)

cat("\n=== Tests Complete ===\n")
