devtools::load_all()

cat("Checking fused edge coordinates BEFORE and AFTER positioning\n")
cat("============================================================\n\n")

# Generate pieces WITHOUT positioning (offset=0)
result_compact <- generate_puzzle(
  type = "concentric",
  seed = 42,
  grid = c(3),
  size = c(300),
  tabsize = 7,
  jitter = 2,
  offset = 0,  # NO separation
  fusion_groups = list(c(1, 2), c(5, 6, 7)),
  fusion_style = "dashed",
  save_files = FALSE
)

cat("=== COMPACT (offset=0) - Fused edges should MATCH ===\n\n")

# Check pieces 5, 6, 7 edges
for (i in c(5, 6, 7)) {
  piece <- result_compact$pieces[[i]]
  edge_paths <- get_piece_edge_paths(piece)

  cat(sprintf("Piece %d:\n", i))

  for (edge in c("LEFT", "RIGHT")) {
    path <- edge_paths[[edge]]
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

      is_fused <- isTRUE(piece$fused_edges[[edge]])
      cat(sprintf("  %s: %s -> %s %s\n", edge, start, end, if(is_fused) "[FUSED]" else ""))
    }
  }
  cat("\n")
}

cat("Expected pairs (should have same coordinates):\n")
cat("  Piece 5 RIGHT <-> Piece 6 LEFT\n")
cat("  Piece 6 RIGHT <-> Piece 7 LEFT\n\n")

# Now check AFTER positioning
result_separated <- generate_puzzle(
  type = "concentric",
  seed = 42,
  grid = c(3),
  size = c(300),
  tabsize = 7,
  jitter = 2,
  offset = 50,  # WITH separation
  fusion_groups = list(c(1, 2), c(5, 6, 7)),
  fusion_style = "dashed",
  save_files = FALSE
)

cat("=== SEPARATED (offset=50) - Fused edges should STILL match ===\n\n")

for (i in c(5, 6, 7)) {
  piece <- result_separated$pieces[[i]]
  edge_paths <- get_piece_edge_paths(piece)

  cat(sprintf("Piece %d:\n", i))

  for (edge in c("LEFT", "RIGHT")) {
    path <- edge_paths[[edge]]
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

      is_fused <- isTRUE(piece$fused_edges[[edge]])
      cat(sprintf("  %s: %s -> %s %s\n", edge, start, end, if(is_fused) "[FUSED]" else ""))
    }
  }
  cat("\n")
}

cat("=== PIECE CENTERS (to verify group moved together) ===\n\n")
cat("Compact:\n")
for (i in c(5, 6, 7)) {
  c <- result_compact$pieces[[i]]$center
  cat(sprintf("  Piece %d: (%.2f, %.2f)\n", i, c[1], c[2]))
}

cat("\nSeparated:\n")
for (i in c(5, 6, 7)) {
  c <- result_separated$pieces[[i]]$center
  cat(sprintf("  Piece %d: (%.2f, %.2f)\n", i, c[1], c[2]))
}

cat("\nTranslation applied (should be same for all in group):\n")
for (i in c(5, 6, 7)) {
  c1 <- result_compact$pieces[[i]]$center
  c2 <- result_separated$pieces[[i]]$center
  dx <- c2[1] - c1[1]
  dy <- c2[2] - c1[2]
  cat(sprintf("  Piece %d: dx=%.2f, dy=%.2f\n", i, dx, dy))
}
