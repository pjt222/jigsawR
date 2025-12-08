devtools::load_all()

cat("Tracing fused edge rendering\n")
cat("============================\n\n")

# Generate puzzle
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
  save_files = FALSE
)

# Manually trace the deduplication logic
cat("=== TRACING FUSED EDGE DEDUPLICATION ===\n\n")

drawn_fused_edges <- character()
pieces <- result$pieces

for (i in seq_along(pieces)) {
  piece <- pieces[[i]]

  if (is.null(piece$fused_edges)) next

  edge_paths <- get_piece_edge_paths(piece)
  edge_names <- get_piece_edge_names(piece)

  for (edge_name in edge_names) {
    if (!isTRUE(piece$fused_edges[[edge_name]])) next

    edge_path <- edge_paths[[edge_name]]
    if (is.null(edge_path) || !nzchar(edge_path)) {
      cat(sprintf("Piece %d %s: [SKIPPED - empty path]\n", i, edge_name))
      next
    }

    segs <- parse_svg_path(edge_path)
    if (length(segs) < 2) {
      cat(sprintf("Piece %d %s: [SKIPPED - too few segments]\n", i, edge_name))
      next
    }

    start_x <- segs[[1]]$x
    start_y <- segs[[1]]$y

    end_seg <- NULL
    for (j in length(segs):2) {
      if (segs[[j]]$type != "Z") {
        end_seg <- segs[[j]]
        break
      }
    }

    if (is.null(end_seg)) {
      cat(sprintf("Piece %d %s: [SKIPPED - no end segment]\n", i, edge_name))
      next
    }

    end_x <- end_seg$x
    end_y <- end_seg$y

    # Create canonical key
    p1 <- sprintf("%.1f,%.1f", start_x, start_y)
    p2 <- sprintf("%.1f,%.1f", end_x, end_y)
    edge_key <- if (p1 < p2) paste(p1, p2, sep = "|") else paste(p2, p1, sep = "|")

    if (edge_key %in% drawn_fused_edges) {
      cat(sprintf("Piece %d %s: (%.1f, %.1f) -> (%.1f, %.1f) Key: %s [DEDUPED - already drawn]\n",
                  i, edge_name, start_x, start_y, end_x, end_y, edge_key))
    } else {
      cat(sprintf("Piece %d %s: (%.1f, %.1f) -> (%.1f, %.1f) Key: %s [NEW - will draw]\n",
                  i, edge_name, start_x, start_y, end_x, end_y, edge_key))
      drawn_fused_edges <- c(drawn_fused_edges, edge_key)
    }
  }
}

cat(sprintf("\nTotal unique fused edges to draw: %d\n", length(drawn_fused_edges)))
cat("Expected: 3 (piece1-1/piece2-INNER, piece5-RIGHT/piece6-LEFT, piece6-RIGHT/piece7-LEFT)\n")
