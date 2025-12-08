devtools::load_all()

cat("Checking deduplication key matching for fused edges\n")
cat("===================================================\n\n")

# Generate the exact puzzle
result <- generate_puzzle(
  type = "concentric",
  seed = 42,
  grid = c(3),
  size = c(300),
  tabsize = 7,
  jitter = 2,
  offset = 50,
  fusion_groups = list(c(1, 2), c(5, 6, 7)),
  fusion_style = "dashed",
  fusion_opacity = 0.3,
  fill_color = "none",
  save_files = FALSE
)

cat("=== FUSED EDGE ENDPOINT KEYS ===\n\n")

# Track keys like the renderer does
all_fused_keys <- list()

for (i in seq_along(result$pieces)) {
  piece <- result$pieces[[i]]

  if (is.null(piece$fused_edges)) next

  edge_paths <- get_piece_edge_paths(piece)
  edge_names <- get_piece_edge_names(piece)

  for (edge_name in edge_names) {
    if (!isTRUE(piece$fused_edges[[edge_name]])) next

    edge_path <- edge_paths[[edge_name]]
    if (is.null(edge_path) || !nzchar(edge_path)) next

    segs <- parse_svg_path(edge_path)
    if (length(segs) < 2) next

    start_x <- segs[[1]]$x
    start_y <- segs[[1]]$y

    end_seg <- NULL
    for (j in length(segs):2) {
      if (segs[[j]]$type != "Z") {
        end_seg <- segs[[j]]
        break
      }
    }

    if (!is.null(end_seg)) {
      end_x <- end_seg$x
      end_y <- end_seg$y

      # Create key like renderer does
      p1 <- sprintf("%.1f,%.1f", start_x, start_y)
      p2 <- sprintf("%.1f,%.1f", end_x, end_y)
      edge_key <- if (p1 < p2) paste(p1, p2, sep = "|") else paste(p2, p1, sep = "|")

      cat(sprintf("Piece %2d, %s: (%7.2f, %7.2f) -> (%7.2f, %7.2f)\n",
                  i, edge_name, start_x, start_y, end_x, end_y))
      cat(sprintf("          Key: %s\n\n", edge_key))

      all_fused_keys[[length(all_fused_keys) + 1]] <- list(
        piece = i,
        edge = edge_name,
        key = edge_key,
        start = c(start_x, start_y),
        end = c(end_x, end_y)
      )
    }
  }
}

cat("=== KEY MATCHING ANALYSIS ===\n\n")

# Find matching keys
keys_only <- sapply(all_fused_keys, function(x) x$key)
unique_keys <- unique(keys_only)

cat(sprintf("Total fused edges: %d\n", length(keys_only)))
cat(sprintf("Unique keys: %d\n", length(unique_keys)))
cat(sprintf("Expected after dedup: %d\n\n", length(unique_keys)))

for (key in unique_keys) {
  matches <- which(keys_only == key)
  if (length(matches) > 1) {
    cat(sprintf("KEY MATCHES (should dedup):\n"))
    for (m in matches) {
      cat(sprintf("  - Piece %d, edge %s\n", all_fused_keys[[m]]$piece, all_fused_keys[[m]]$edge))
    }
    cat("\n")
  } else {
    cat(sprintf("NO MATCH for piece %d, edge %s (key: %s)\n",
                all_fused_keys[[matches[1]]]$piece,
                all_fused_keys[[matches[1]]]$edge,
                key))
  }
}

cat("\n=== EXPECTED FUSED EDGE PAIRS ===\n")
cat("Fusion group 1: pieces 1,2\n")
cat("  - Piece 1 edge '1' should match Piece 2 edge 'INNER'\n")
cat("Fusion group 2: pieces 5,6,7\n")
cat("  - Piece 5 edge 'RIGHT' should match Piece 6 edge 'LEFT'\n")
cat("  - Piece 6 edge 'RIGHT' should match Piece 7 edge 'LEFT'\n")
