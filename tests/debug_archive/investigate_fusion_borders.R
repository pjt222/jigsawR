# Investigation: Missing borders in fused pieces with separation
# Context: Fused pieces missing some borders, only visible in separation mode

# Load package functions
devtools::load_all()

# Test case with large offset to make issues visible
result <- generate_puzzle(
  type = "rectangular",
  grid = c(2, 2),
  size = c(200, 200),
  seed = 42,
  offset = 50,  # Large separation
  fusion_groups = list(c(1, 2)),  # Fuse top two pieces
  fusion_style = "solid",
  fusion_opacity = 1.0,
  palette = "viridis",
  background = "white",
  save_files = TRUE,
  output_dir = "output/fusion_investigation"
)

cat("\n=== Test Case Created ===\n")
cat("Puzzle: 2x2 rectangular with pieces 1-2 fused\n")
cat("Offset: 50mm (large separation)\n")
cat("Fusion style: solid (opacity 1.0)\n")
cat("Output file:", result$files$svg, "\n")

# Investigate piece structure
cat("\n=== Piece 1 (fused with 2) ===\n")
cat("Fused edges structure:\n")
print(str(result$pieces[[1]]$fused_edges))
cat("Path preview:", substr(result$pieces[[1]]$path, 1, 100), "...\n")

cat("\n=== Piece 2 (fused with 1) ===\n")
cat("Fused edges structure:\n")
print(str(result$pieces[[2]]$fused_edges))
cat("Path preview:", substr(result$pieces[[2]]$path, 1, 100), "...\n")

# Test edge extraction
cat("\n=== Edge Path Extraction Test ===\n")

piece1_edges <- get_piece_edge_paths(result$pieces[[1]])
piece1_names <- get_piece_edge_names(result$pieces[[1]])

cat("Piece 1 edge names:", paste(piece1_names, collapse=", "), "\n")
for (edge_name in piece1_names) {
  edge_path <- piece1_edges[[edge_name]]
  has_path <- !is.null(edge_path) && nzchar(edge_path)
  is_fused <- isTRUE(result$pieces[[1]]$fused_edges[[edge_name]])
  cat(sprintf("  %s: path=%s, fused=%s\n", edge_name, has_path, is_fused))
  if (has_path) {
    cat(sprintf("    Preview: %s...\n", substr(edge_path, 1, 60)))
  }
}

cat("\n=== Expected vs Actual ===\n")
cat("Expected: All 4 edges (N, E, S, W) should have paths\n")
cat("Expected: E edge should be marked as fused (shared between pieces 1-2)\n")
cat("Expected: All edges should be drawn (fused edges with opacity/style)\n")
cat("\nCheck the SVG file to see if all borders are visible.\n")
