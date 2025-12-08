# Analyze Test 3 (all pieces fused) - this should show the issue clearly
devtools::load_all()

result <- generate_puzzle(
  type = "rectangular",
  grid = c(2, 2),
  size = c(200, 200),
  seed = 42,
  offset = 50,
  fusion_groups = list(c(1, 2, 3, 4)),
  save_files = FALSE
)

cat("=== All 4 Pieces Fused Together ===\n\n")
cat("Expected: MANY fused edges (internal edges within the meta-piece)\n")
cat("Expected: Few normal edges (only the outer perimeter)\n\n")

for (i in 1:4) {
  piece <- result$pieces[[i]]
  
  cat(sprintf("Piece %d fused edges: ", i))
  fused_list <- sapply(piece$fused_edges, isTRUE)
  cat(paste(names(which(fused_list)), collapse=", "), "\n")
}

cat("\n=== Edge Classification ===\n")
cat("For a 2x2 grid with all pieces fused:\n")
cat("- Piece 1: N=boundary, E=fused(2), S=fused(3), W=boundary\n")
cat("- Piece 2: N=boundary, E=boundary, S=fused(4), W=fused(1)\n")
cat("- Piece 3: N=fused(1), E=fused(4), S=boundary, W=boundary\n")
cat("- Piece 4: N=fused(2), E=boundary, S=boundary, W=fused(3)\n")
cat("\nTotal: 8 boundary edges (should be normal)\n")
cat("Total: 8 internal edges (should be fused)\n")
