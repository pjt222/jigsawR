devtools::load_all()

cat("Edge Coordinate Analysis - Checking if edges are split correctly\n")
cat("================================================================\n\n")

# Generate without separation to see original positions
result <- generate_puzzle(
  type = "concentric",
  seed = 42,
  grid = c(2),
  size = c(200),
  offset = 0,  # No separation
  fusion_groups = list(c(1, 2), c(5, 6, 7)),
  fusion_style = "dashed",
  save_files = FALSE
)

cat("Analyzing edge start/end points for each piece...\n\n")

for (i in seq_along(result$pieces)) {
  piece <- result$pieces[[i]]

  cat(sprintf("=== PIECE %d (ring %d, pos %d) ===\n",
              i, piece$ring_pos$ring, piece$ring_pos$position))

  edge_paths <- get_piece_edge_paths(piece)
  edge_names <- get_piece_edge_names(piece)

  for (edge_name in edge_names) {
    path <- edge_paths[[edge_name]]
    if (is.null(path) || !nzchar(path)) {
      cat(sprintf("  %s: EMPTY PATH!\n", edge_name))
      next
    }

    segs <- parse_svg_path(path)
    if (length(segs) < 2) {
      cat(sprintf("  %s: TOO SHORT\n", edge_name))
      next
    }

    start_x <- segs[[1]]$x
    start_y <- segs[[1]]$y

    # Find end point
    end_seg <- NULL
    for (j in length(segs):2) {
      if (segs[[j]]$type != "Z") {
        end_seg <- segs[[j]]
        break
      }
    }

    is_fused <- isTRUE(piece$fused_edges[[edge_name]])

    if (!is.null(end_seg)) {
      cat(sprintf("  %s: (%.1f,%.1f) -> (%.1f,%.1f) %s\n",
                  edge_name, start_x, start_y, end_seg$x, end_seg$y,
                  if (is_fused) "[FUSED]" else ""))
    }
  }
  cat("\n")
}

# Check for matching fused edges between pieces
cat("=== CHECKING FUSED EDGE MATCHING ===\n\n")

# Fusion group 1: pieces 1 and 2
cat("Fusion Group 1 (pieces 1 & 2):\n")
p1_edge1 <- get_piece_edge_paths(result$pieces[[1]])[["1"]]
p2_inner <- get_piece_edge_paths(result$pieces[[2]])[["INNER"]]

segs1 <- parse_svg_path(p1_edge1)
segs2 <- parse_svg_path(p2_inner)

cat(sprintf("  Piece 1 Edge 1 start: (%.1f, %.1f)\n", segs1[[1]]$x, segs1[[1]]$y))
cat(sprintf("  Piece 2 INNER start:  (%.1f, %.1f)\n", segs2[[1]]$x, segs2[[1]]$y))
cat(sprintf("  Match: %s\n\n", all.equal(c(segs1[[1]]$x, segs1[[1]]$y),
                                          c(segs2[[1]]$x, segs2[[1]]$y))))

# Fusion group 2: pieces 5, 6, 7
cat("Fusion Group 2 (pieces 5, 6, 7):\n")
p5_right <- get_piece_edge_paths(result$pieces[[5]])[["RIGHT"]]
p6_left <- get_piece_edge_paths(result$pieces[[6]])[["LEFT"]]
p6_right <- get_piece_edge_paths(result$pieces[[6]])[["RIGHT"]]
p7_left <- get_piece_edge_paths(result$pieces[[7]])[["LEFT"]]

segs5r <- parse_svg_path(p5_right)
segs6l <- parse_svg_path(p6_left)
segs6r <- parse_svg_path(p6_right)
segs7l <- parse_svg_path(p7_left)

cat(sprintf("  P5 RIGHT start: (%.1f, %.1f)\n", segs5r[[1]]$x, segs5r[[1]]$y))
cat(sprintf("  P6 LEFT start:  (%.1f, %.1f)\n", segs6l[[1]]$x, segs6l[[1]]$y))
cat(sprintf("  P6 RIGHT start: (%.1f, %.1f)\n", segs6r[[1]]$x, segs6r[[1]]$y))
cat(sprintf("  P7 LEFT start:  (%.1f, %.1f)\n", segs7l[[1]]$x, segs7l[[1]]$y))

cat("\nNote: Fused edges between adjacent pieces should share endpoints.\n")
cat("P5.RIGHT end should equal P6.LEFT start (or reversed)\n")
cat("P6.RIGHT end should equal P7.LEFT start (or reversed)\n")
