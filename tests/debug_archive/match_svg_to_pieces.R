# Match SVG paths to piece edges to find the duplicates
devtools::load_all()

# Read SVG
svg_lines <- readLines("output/fusion_investigation/puzzle_2x2_seed42_separated.svg")
path_lines <- grep('<path d=', svg_lines, value = TRUE)

# Get puzzle data
result <- generate_puzzle(
  type = "rectangular",
  grid = c(2, 2),
  size = c(200, 200),
  seed = 42,
  offset = 50,
  fusion_groups = list(c(1, 2)),
  save_files = FALSE
)

# Extract edge paths for each piece
cat("=== Matching SVG Paths to Pieces ===\n\n")

for (i in 1:2) {  # Focus on pieces 1 and 2 (the fused ones)
  piece <- result$pieces[[i]]
  edge_paths <- get_piece_edge_paths(piece)
  
  cat(sprintf("Piece %d:\n", i))
  
  for (edge_name in c("N", "E", "S", "W")) {
    edge_path <- edge_paths[[edge_name]]
    is_fused <- isTRUE(piece$fused_edges[[edge_name]])
    
    # Try to find this path in SVG
    edge_pattern <- substr(edge_path, 1, 30)  # Use first 30 chars as pattern
    edge_pattern <- gsub("([\\[\\]\\(\\)\\.])", "\\\\\\1", edge_pattern)  # Escape regex chars
    
    matches <- grep(edge_pattern, path_lines, value = TRUE)
    
    cat(sprintf("  %s (fused=%s): ", edge_name, is_fused))
    if (length(matches) == 0) {
      cat("NOT FOUND IN SVG\n")
    } else if (length(matches) == 1) {
      if (grepl('opacity="', matches[1])) {
        cat("drawn as FUSED EDGE\n")
      } else {
        cat("drawn as NORMAL EDGE\n")
      }
    } else {
      cat(sprintf("DRAWN %d TIMES:\n", length(matches)))
      for (m in matches) {
        if (grepl('opacity="', m)) {
          cat("      - as FUSED EDGE\n")
        } else if (grepl('stroke="none"', m)) {
          cat("      - as FILL (invisible)\n")
        } else {
          cat("      - as NORMAL EDGE\n")
        }
      }
    }
  }
  cat("\n")
}
