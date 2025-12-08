# Check all 4 pieces for missing edges
devtools::load_all()

result <- generate_puzzle(
  type = "rectangular",
  grid = c(2, 2),
  size = c(200, 200),
  seed = 42,
  offset = 50,
  fusion_groups = list(c(1, 2)),
  save_files = FALSE
)

cat("=== All Pieces Edge Analysis ===\n\n")

for (i in 1:4) {
  piece <- result$pieces[[i]]
  edge_paths <- get_piece_edge_paths(piece)
  
  cat(sprintf("Piece %d:\n", i))
  
  # Check if piece has fusion data
  if (is.null(piece$fused_edges)) {
    cat("  No fusion data - should draw full outline\n")
  } else {
    cat("  Has fusion data:\n")
    for (edge_name in c("N", "E", "S", "W")) {
      is_fused <- isTRUE(piece$fused_edges[[edge_name]])
      edge_path <- edge_paths[[edge_name]]
      has_path <- !is.null(edge_path) && nzchar(edge_path)
      
      status <- if (!has_path) "MISSING PATH!" else if (is_fused) "fused" else "normal"
      cat(sprintf("    %s: %s\n", edge_name, status))
    }
  }
  cat("\n")
}

cat("=== Potential Issues ===\n")
cat("If pieces 3 and 4 have NULL fused_edges, they use the fallback path\n")
cat("in render_pieces_with_fusion_styled() line 831-838.\n")
cat("This draws the FULL piece outline, not individual edges.\n")
cat("But in separated mode, we need individual edges to show properly!\n")
