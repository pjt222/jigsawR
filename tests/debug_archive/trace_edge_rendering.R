# Trace exactly which edges are being drawn for pieces 1 and 2
devtools::load_all()

result <- generate_puzzle(
  type = "rectangular",
  grid = c(2, 2),
  size = c(200, 200),
  seed = 42,
  offset = 50,
  fusion_groups = list(c(1, 2)),
  fusion_style = "solid",
  fusion_opacity = 1.0,
  save_files = FALSE
)

cat("=== Piece 1 Analysis ===\n")
piece1 <- result$pieces[[1]]
cat("Position:", piece1$position$x, piece1$position$y, "\n")
cat("Fused edges:", names(which(unlist(piece1$fused_edges))), "\n")

piece1_edges <- get_piece_edge_paths(piece1)
for (edge_name in c("N", "E", "S", "W")) {
  is_fused <- isTRUE(piece1$fused_edges[[edge_name]])
  path <- piece1_edges[[edge_name]]
  cat(sprintf("  %s: fused=%s, path=%s\n", edge_name, is_fused, substr(path, 1, 40)))
}

cat("\n=== Piece 2 Analysis ===\n")
piece2 <- result$pieces[[2]]
cat("Position:", piece2$position$x, piece2$position$y, "\n")
cat("Fused edges:", names(which(unlist(piece2$fused_edges))), "\n")

piece2_edges <- get_piece_edge_paths(piece2)
for (edge_name in c("N", "E", "S", "W")) {
  is_fused <- isTRUE(piece2$fused_edges[[edge_name]])
  path <- piece2_edges[[edge_name]]
  cat(sprintf("  %s: fused=%s, path=%s\n", edge_name, is_fused, substr(path, 1, 40)))
}

cat("\n=== Problem Diagnosis ===\n")
cat("Looking at the logic in render_pieces_with_fusion_styled():\n")
cat("- Pass 2 (lines 826-859): Draws edges where fused_edges[[edge_name]] is NOT TRUE\n")
cat("- Pass 3 (lines 861-895): Draws edges where fused_edges[[edge_name]] IS TRUE\n")
cat("\nThe logic appears correct. Let me check if edges are being skipped properly...\n")
