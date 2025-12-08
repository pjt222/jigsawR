devtools::load_all()

cat("Debugging center piece fusion\n")
cat("=============================\n\n")

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

# Get pieces 1 and 2
p1 <- result$pieces[[1]]
p2 <- result$pieces[[2]]

cat("=== PIECE 1 (CENTER) ===\n")
cat(sprintf("Type: %s\n", p1$type))
cat(sprintf("Ring: %d\n", p1$ring_pos$ring))
cat("\nFused edges:\n")
for (name in names(p1$fused_edges)) {
  if (isTRUE(p1$fused_edges[[name]])) {
    cat(sprintf("  %s: FUSED\n", name))
  }
}

cat("\nEdge paths (center has 6 edges):\n")
p1_edges <- get_piece_edge_paths(p1)
for (name in names(p1_edges)) {
  path <- p1_edges[[name]]
  if (!is.null(path) && nzchar(path)) {
    segs <- parse_svg_path(path)
    start <- sprintf("(%.2f, %.2f)", segs[[1]]$x, segs[[1]]$y)
    end_seg <- NULL
    for (j in length(segs):2) {
      if (segs[[j]]$type != "Z") { end_seg <- segs[[j]]; break }
    }
    end <- if (!is.null(end_seg)) sprintf("(%.2f, %.2f)", end_seg$x, end_seg$y) else "?"
    is_fused <- isTRUE(p1$fused_edges[[name]])
    cat(sprintf("  %s: %s -> %s %s\n", name, start, end, if(is_fused) "[FUSED]" else ""))
  }
}

cat("\n=== PIECE 2 ===\n")
cat(sprintf("Type: %s\n", p2$type))
cat(sprintf("Ring: %d, Position: %d\n", p2$ring_pos$ring, p2$ring_pos$position))
cat("\nFused edges:\n")
for (name in names(p2$fused_edges)) {
  if (isTRUE(p2$fused_edges[[name]])) {
    cat(sprintf("  %s: FUSED\n", name))
  }
}

cat("\nEdge paths:\n")
p2_edges <- get_piece_edge_paths(p2)
for (name in names(p2_edges)) {
  path <- p2_edges[[name]]
  if (!is.null(path) && nzchar(path)) {
    segs <- parse_svg_path(path)
    start <- sprintf("(%.2f, %.2f)", segs[[1]]$x, segs[[1]]$y)
    end_seg <- NULL
    for (j in length(segs):2) {
      if (segs[[j]]$type != "Z") { end_seg <- segs[[j]]; break }
    }
    end <- if (!is.null(end_seg)) sprintf("(%.2f, %.2f)", end_seg$x, end_seg$y) else "?"
    is_fused <- isTRUE(p2$fused_edges[[name]])
    cat(sprintf("  %s: %s -> %s %s\n", name, start, end, if(is_fused) "[FUSED]" else ""))
  }
}

cat("\n=== KEY COMPARISON ===\n")
# Get the fused edge from piece 1 (edge "1")
p1_edge_path <- p1_edges[["1"]]
if (!is.null(p1_edge_path) && nzchar(p1_edge_path)) {
  segs <- parse_svg_path(p1_edge_path)
  start <- c(segs[[1]]$x, segs[[1]]$y)
  end_seg <- NULL
  for (j in length(segs):2) {
    if (segs[[j]]$type != "Z") { end_seg <- segs[[j]]; break }
  }
  end <- c(end_seg$x, end_seg$y)

  p1_key <- sprintf("%.1f,%.1f", start[1], start[2])
  p2_key <- sprintf("%.1f,%.1f", end[1], end[2])
  key1 <- if (p1_key < p2_key) paste(p1_key, p2_key, sep = "|") else paste(p2_key, p1_key, sep = "|")
  cat(sprintf("Piece 1 edge '1': (%.2f, %.2f) -> (%.2f, %.2f)\n", start[1], start[2], end[1], end[2]))
  cat(sprintf("Key: %s\n\n", key1))
}

# Get the fused edge from piece 2 (edge "INNER")
p2_inner_path <- p2_edges[["INNER"]]
if (!is.null(p2_inner_path) && nzchar(p2_inner_path)) {
  segs <- parse_svg_path(p2_inner_path)
  start <- c(segs[[1]]$x, segs[[1]]$y)
  end_seg <- NULL
  for (j in length(segs):2) {
    if (segs[[j]]$type != "Z") { end_seg <- segs[[j]]; break }
  }
  end <- c(end_seg$x, end_seg$y)

  p1_key <- sprintf("%.1f,%.1f", start[1], start[2])
  p2_key <- sprintf("%.1f,%.1f", end[1], end[2])
  key2 <- if (p1_key < p2_key) paste(p1_key, p2_key, sep = "|") else paste(p2_key, p1_key, sep = "|")
  cat(sprintf("Piece 2 INNER: (%.2f, %.2f) -> (%.2f, %.2f)\n", start[1], start[2], end[1], end[2]))
  cat(sprintf("Key: %s\n\n", key2))
}

cat(sprintf("Keys match: %s\n", key1 == key2))
