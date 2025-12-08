devtools::load_all()

cat("Border/Outline Issue Investigation\n")
cat("===================================\n")
cat("Concentric puzzle with:\n")
cat("  - Fusion groups: (1,2), (5,6,7)\n")
cat("  - Tab size: 7\n")
cat("  - Jitter: 2\n")
cat("  - Separation: 50mm\n")
cat("  - Internal edge style: dashed\n\n")

# Determine ring count based on piece numbers mentioned (16, 17, 18, 19)
# 3 rings: 1 + 6 + 12 = 19 pieces
cat("=== PUZZLE STRUCTURE ===\n")
rings <- 3
n_pieces <- 3 * rings * (rings - 1) + 1
cat(sprintf("Rings: %d\n", rings))
cat(sprintf("Total pieces: %d\n\n", n_pieces))

cat("Ring layout:\n")
cat("  Ring 0: Piece 1 (center hexagon)\n")
cat("  Ring 1: Pieces 2-7 (6 trapezoids around center)\n")
cat("  Ring 2: Pieces 8-19 (12 trapezoids outer ring)\n\n")

# Generate the puzzle
result <- generate_puzzle(
  type = "concentric",
  seed = 42,
  grid = c(rings),
  size = c(300),
  tabsize = 7,
  jitter = 2,
  offset = 50,
  fusion_groups = list(c(1, 2), c(5, 6, 7)),
  fusion_style = "dashed",
  fusion_opacity = 0.3,
  fill_color = "none",
  save_files = TRUE,
  output_dir = "output",
  filename_prefix = "debug_border"
)

cat("=== PIECE ADJACENCY ANALYSIS ===\n\n")

# List expected adjacencies for pieces mentioned in the issue
cat("Expected adjacencies (non-fused edges should have borders):\n")
cat("  Piece 3 borders: center(1), piece 2, piece 4, ring2 pieces\n")
cat("  Piece 4 borders: center(1), piece 3, piece 5, ring2 pieces\n")
cat("  Piece 5 borders: center(1), piece 4, piece 6 (FUSED), ring2 pieces\n")
cat("  Piece 6 borders: center(1), piece 5 (FUSED), piece 7 (FUSED), ring2 pieces\n")
cat("  Piece 7 borders: center(1), piece 6 (FUSED), piece 2, ring2 pieces\n\n")

cat("=== EDGE ANALYSIS PER PIECE ===\n\n")

for (i in seq_along(result$pieces)) {
  piece <- result$pieces[[i]]

  ring <- if (!is.null(piece$ring_pos)) piece$ring_pos$ring else NA
  pos <- if (!is.null(piece$ring_pos)) piece$ring_pos$position else NA

  cat(sprintf("--- PIECE %d (ring %d, pos %d) ---\n", i, ring, pos))

  # Get edge paths
  edge_paths <- get_piece_edge_paths(piece)
  edge_names <- get_piece_edge_names(piece)

  for (edge_name in edge_names) {
    path <- edge_paths[[edge_name]]
    is_fused <- isTRUE(piece$fused_edges[[edge_name]])
    has_path <- !is.null(path) && nzchar(path)

    # Parse path to count segments
    if (has_path) {
      segs <- parse_svg_path(path)
      seg_types <- paste(sapply(segs, function(s) s$type), collapse=" ")
      n_segs <- length(segs)
    } else {
      seg_types <- "EMPTY"
      n_segs <- 0
    }

    status <- if (!has_path) "MISSING" else if (is_fused) "fused" else "solid"

    cat(sprintf("  %s: %s (segs=%d: %s)\n",
                edge_name, status, n_segs, seg_types))
  }
  cat("\n")
}

cat("=== SVG PATH ANALYSIS ===\n\n")

# Read the generated SVG
svg_lines <- readLines("output/debug_border_conc3_seed42_separated.svg")

# Count paths by type
fill_paths <- svg_lines[grepl("fill=\"none\" stroke=\"none\"", svg_lines)]
stroke_paths <- svg_lines[grepl("stroke=\"#", svg_lines) & !grepl("stroke=\"none\"", svg_lines)]
dashed_paths <- svg_lines[grepl("stroke-dasharray", svg_lines)]

cat(sprintf("Fill-only paths: %d\n", length(fill_paths)))
cat(sprintf("Stroke paths (non-fused edges): %d\n", length(stroke_paths) - length(dashed_paths)))
cat(sprintf("Dashed paths (fused edges): %d\n", length(dashed_paths)))

# Expected counts
# 19 pieces, each with 4 edges (except center with 6)
# Center: 6 edges, 1 fused (to piece 2)
# Ring 1 (pieces 2-7): 4 edges each
#   Piece 2: INNER fused (to 1), others solid
#   Pieces 3,4: all solid (4 each)
#   Piece 5: RIGHT fused (to 6), others solid
#   Piece 6: LEFT fused (to 5), RIGHT fused (to 7), others solid
#   Piece 7: LEFT fused (to 6), others solid
# Ring 2 (pieces 8-19): 4 edges each, all solid

cat("\nExpected edge counts:\n")
cat("  Center (piece 1): 5 solid + 1 fused = 6\n")
cat("  Piece 2: 3 solid + 1 fused = 4\n")
cat("  Pieces 3,4: 4 solid each = 8\n")
cat("  Piece 5: 3 solid + 1 fused = 4\n")
cat("  Piece 6: 2 solid + 2 fused = 4\n")
cat("  Piece 7: 3 solid + 1 fused = 4\n")
cat("  Ring 2 (pieces 8-19): 4 solid each = 48\n")
cat("  Total solid: 5+3+8+3+2+3+48 = 72\n")
cat("  Total fused: 1+1+1+2+1 = 6 (but drawn only 3 due to dedup)\n")

cat("\n=== COMPARING FILL vs STROKE PATHS ===\n\n")

# Check if any piece fill paths don't have corresponding stroke paths
for (i in 1:min(7, length(result$pieces))) {
  piece <- result$pieces[[i]]
  fill_path <- piece$path

  # Get edge paths
  edge_paths <- get_piece_edge_paths(piece)

  # Combine all edge paths
  all_edges <- paste(unlist(edge_paths[edge_paths != ""]), collapse = " ")

  # Compare total path length
  fill_len <- nchar(fill_path)
  edge_len <- nchar(all_edges)

  cat(sprintf("Piece %d: fill_path=%d chars, edge_paths=%d chars\n",
              i, fill_len, edge_len))
}

cat("\nSVG saved to: output/debug_border_conc3_seed42_separated.svg\n")
cat("\nPlease visually inspect this file to identify the missing borders.\n")
