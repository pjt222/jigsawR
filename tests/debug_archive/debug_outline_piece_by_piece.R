devtools::load_all()

cat("Piece-by-piece outline analysis\n")
cat("================================\n")
cat("Concentric puzzle with fusion groups (1,2) and (5,6,7)\n")
cat("Offset: 40mm\n\n")

# Generate with 40mm separation
result <- generate_puzzle(
  type = "concentric",
  seed = 42,
  grid = c(2),
  size = c(200),
  offset = 40,
  fusion_groups = list(c(1, 2), c(5, 6, 7)),
  fusion_style = "dashed",
  save_files = TRUE,
  output_dir = "output"
)

# Colors being used
colors <- viridis::viridis(7)
cat("Colors assigned:\n")
for (i in 1:7) {
  cat(sprintf("  Piece %d: %s\n", i, colors[i]))
}

cat("\n=== PIECE BY PIECE ANALYSIS ===\n")

for (i in seq_along(result$pieces)) {
  piece <- result$pieces[[i]]

  cat(sprintf("\n--- PIECE %d ---\n", i))
  cat(sprintf("Ring: %d, Position: %d\n",
              piece$ring_pos$ring, piece$ring_pos$position))
  cat(sprintf("Color: %s\n", colors[i]))

  # Get fusion info
  fusion_group <- piece$fusion_group
  cat(sprintf("Fusion group: %s\n",
              if (is.na(fusion_group) || is.null(fusion_group)) "none" else fusion_group))

  # Edge analysis
  edge_paths <- get_piece_edge_paths(piece)
  edge_names <- get_piece_edge_names(piece)

  cat("\nEdges:\n")
  for (edge_name in edge_names) {
    is_fused <- isTRUE(piece$fused_edges[[edge_name]])
    has_path <- !is.null(edge_paths[[edge_name]]) && nzchar(edge_paths[[edge_name]])

    # What SHOULD happen:
    # - Non-fused edges: drawn with piece's color (solid)
    # - Fused edges: drawn with piece's color (dashed, opacity 0.3)
    expected_style <- if (is_fused) "dashed + opacity 0.3" else "solid"

    cat(sprintf("  %s: fused=%s, has_path=%s, expected_style=%s\n",
                edge_name, is_fused, has_path, expected_style))

    if (has_path) {
      # Show path preview
      path <- edge_paths[[edge_name]]
      segs <- parse_svg_path(path)
      seg_types <- paste(sapply(segs, function(s) s$type), collapse=" ")
      cat(sprintf("    Segments: %s\n", seg_types))
    }
  }
}

cat("\n=== EXPECTED RENDERING ===\n")
cat("Pass 1: All pieces filled (no stroke)\n")
cat("Pass 2: Non-fused edges drawn with solid strokes\n")
cat("Pass 3: Fused edges drawn with dashed strokes + opacity\n")

cat("\n=== WHAT TO CHECK VISUALLY ===\n")
cat("For each piece, verify:\n")
cat("1. All 4 edges (trapezoid) or 6 edges (center) are visible\n")
cat("2. Non-fused edges have solid colored stroke\n")
cat("3. Fused edges have dashed stroke with reduced opacity\n")
cat("4. Edge colors match the piece's assigned color\n")

cat("\n=== ISSUES TO LOOK FOR ===\n")
cat("- Missing edges (edge path empty or not drawn)\n")
cat("- Wrong color on an edge (edge path assigned to wrong piece)\n")
cat("- Edge segments mixed up (part of one edge drawn with another)\n")

cat("\nSVG saved to: output/puzzle_conc2_seed42_separated.svg\n")
