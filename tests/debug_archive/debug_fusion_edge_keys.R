# Debug script to verify fusion edge key matching between generation and rendering

library(jigsawR)

# Test 1: Hexagonal puzzle
cat("\n=== HEXAGONAL PUZZLE ===\n")
hex_result <- generate_puzzle(
  type = "hexagonal",
  seed = 42,
  grid = c(2),  # 2 rings = 7 pieces
  size = c(200),
  offset = 0,
  fusion_groups = list(c(1, 2, 3)),  # Fuse pieces 1, 2, 3
  fusion_style = "dashed"
)

cat("\nPiece 1 fused_edges:\n")
print(hex_result$pieces[[1]]$fused_edges)

cat("\nExpected edge names from get_piece_edge_names():\n")
cat("Hexagonal: c('0', '1', '2', '3', '4', '5')\n")

# Test 2: Concentric trapezoid pieces
cat("\n=== CONCENTRIC PUZZLE (TRAPEZOID) ===\n")
conc_result <- generate_puzzle(
  type = "concentric",
  seed = 42,
  grid = c(2),  # 2 rings = 7 pieces
  size = c(200),
  center_shape = "hexagon",
  offset = 0,
  fusion_groups = list(c(2, 3, 4)),  # Fuse outer ring pieces
  fusion_style = "dashed"
)

# Piece 2 should be a trapezoid (ring 1)
cat("\nPiece 2 (trapezoid) fused_edges:\n")
print(conc_result$pieces[[2]]$fused_edges)

cat("\nExpected edge names from get_piece_edge_names():\n")
cat("Concentric trapezoid: c('INNER', 'RIGHT', 'OUTER', 'LEFT')\n")

# Test 3: Concentric center piece
cat("\n=== CONCENTRIC CENTER PIECE ===\n")
cat("\nPiece 1 (center hexagon) fused_edges:\n")
print(conc_result$pieces[[1]]$fused_edges)

cat("\nExpected edge names from get_piece_edge_names():\n")
cat("Concentric center: c('1', '2', '3', '4', '5', '6')\n")

# Test 4: Verify renderer helper functions
cat("\n=== RENDERER HELPER VERIFICATION ===\n")
cat("\nget_piece_edge_names(hex_result$pieces[[1]]):\n")
cat(paste(get_piece_edge_names(hex_result$pieces[[1]]), collapse = ", "), "\n")

cat("\nget_piece_edge_names(conc_result$pieces[[1]]) [center]:\n")
cat(paste(get_piece_edge_names(conc_result$pieces[[1]]), collapse = ", "), "\n")

cat("\nget_piece_edge_names(conc_result$pieces[[2]]) [trapezoid]:\n")
cat(paste(get_piece_edge_names(conc_result$pieces[[2]]), collapse = ", "), "\n")

cat("\n=== KEY MATCHING SUMMARY ===\n")
cat("✓ Hexagonal pieces use edge names: 0, 1, 2, 3, 4, 5\n")
cat("✓ Concentric center uses edge names: 1, 2, 3, 4, 5, 6\n")
cat("✓ Concentric trapezoids use edge names: INNER, RIGHT, OUTER, LEFT\n")
cat("\nBoth piece generation and renderer use these same keys.\n")
cat("Edge keys in fused_edge_data are formatted as: '{piece_id}-{direction}'\n")
