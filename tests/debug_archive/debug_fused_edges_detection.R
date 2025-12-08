devtools::load_all()

cat("Checking fused edge detection for all pieces\n")
cat("=============================================\n\n")

result <- generate_puzzle(
  type = "concentric",
  seed = 42,
  grid = c(3),
  size = c(300),
  tabsize = 7,
  jitter = 2,
  offset = 0,
  fusion_groups = list(c(1, 2), c(5, 6, 7)),
  fusion_style = "dashed",
  save_files = FALSE
)

cat(sprintf("Total pieces: %d\n\n", length(result$pieces)))

cat("=== FUSED EDGES PER PIECE ===\n\n")
for (i in seq_along(result$pieces)) {
  piece <- result$pieces[[i]]
  fused <- piece$fused_edges

  has_any_fused <- any(sapply(fused, isTRUE))
  if (has_any_fused) {
    cat(sprintf("Piece %d:\n", i))
    for (edge_name in names(fused)) {
      if (isTRUE(fused[[edge_name]])) {
        cat(sprintf("  %s: FUSED\n", edge_name))
      }
    }
    cat("\n")
  }
}

cat("=== CHECKING PIECES 5,6,7 SPECIFICALLY ===\n\n")
for (i in c(5, 6, 7)) {
  piece <- result$pieces[[i]]
  cat(sprintf("Piece %d:\n", i))
  cat(sprintf("  type: %s\n", piece$type))
  cat(sprintf("  ring_pos: ring=%d, position=%d\n",
              piece$ring_pos$ring, piece$ring_pos$position))
  cat(sprintf("  fusion_group: %s\n", piece$fusion_group))
  cat("  fused_edges:\n")
  for (edge_name in names(piece$fused_edges)) {
    cat(sprintf("    %s: %s\n", edge_name, piece$fused_edges[[edge_name]]))
  }
  cat("\n")
}

cat("=== EDGE PATH EXTRACTION FOR PIECES 5,6,7 ===\n\n")
for (i in c(5, 6, 7)) {
  piece <- result$pieces[[i]]
  edge_paths <- get_piece_edge_paths(piece)

  cat(sprintf("Piece %d edge paths:\n", i))
  for (name in names(edge_paths)) {
    path <- edge_paths[[name]]
    if (!is.null(path) && nzchar(path)) {
      segs <- parse_svg_path(path)
      start <- sprintf("(%.2f, %.2f)", segs[[1]]$x, segs[[1]]$y)

      end_seg <- NULL
      for (j in length(segs):2) {
        if (segs[[j]]$type != "Z") {
          end_seg <- segs[[j]]
          break
        }
      }
      end <- if (!is.null(end_seg)) sprintf("(%.2f, %.2f)", end_seg$x, end_seg$y) else "?"

      is_fused <- isTRUE(piece$fused_edges[[name]])
      cat(sprintf("  %s: %s -> %s %s\n", name, start, end, if(is_fused) "[FUSED]" else ""))
    } else {
      cat(sprintf("  %s: (empty)\n", name))
    }
  }
  cat("\n")
}

cat("=== DEDUPLICATION KEY CALCULATION ===\n\n")
drawn_keys <- character()
for (i in c(5, 6, 7)) {
  piece <- result$pieces[[i]]
  edge_paths <- get_piece_edge_paths(piece)

  for (edge_name in names(piece$fused_edges)) {
    if (!isTRUE(piece$fused_edges[[edge_name]])) next

    edge_path <- edge_paths[[edge_name]]
    if (is.null(edge_path) || !nzchar(edge_path)) {
      cat(sprintf("Piece %d %s: empty path\n", i, edge_name))
      next
    }

    segs <- parse_svg_path(edge_path)
    if (length(segs) < 2) {
      cat(sprintf("Piece %d %s: too few segments\n", i, edge_name))
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

    if (!is.null(end_seg)) {
      end_x <- end_seg$x
      end_y <- end_seg$y

      p1 <- sprintf("%.1f,%.1f", start_x, start_y)
      p2 <- sprintf("%.1f,%.1f", end_x, end_y)
      edge_key <- if (p1 < p2) paste(p1, p2, sep = "|") else paste(p2, p1, sep = "|")

      status <- if (edge_key %in% drawn_keys) "DUPLICATE" else "NEW"
      drawn_keys <- c(drawn_keys, edge_key)

      cat(sprintf("Piece %d %s: (%7.2f, %7.2f) -> (%7.2f, %7.2f)\n",
                  i, edge_name, start_x, start_y, end_x, end_y))
      cat(sprintf("          Key: %s [%s]\n\n", edge_key, status))
    }
  }
}

cat("=== KEY SUMMARY ===\n")
cat(sprintf("Total fused edge keys: %d\n", length(drawn_keys)))
cat(sprintf("Unique keys: %d\n", length(unique(drawn_keys))))
cat(sprintf("Duplicate keys found: %d\n", length(drawn_keys) - length(unique(drawn_keys))))
