devtools::load_all()

cat("Checking piece translation after offset\n")
cat("========================================\n\n")

# Generate without offset
result0 <- generate_puzzle(
  type = "concentric",
  seed = 42,
  grid = c(3),
  size = c(300),
  offset = 0,
  fusion_groups = list(c(1, 2), c(5, 6, 7)),
  save_files = FALSE
)

# Generate with offset
result50 <- generate_puzzle(
  type = "concentric",
  seed = 42,
  grid = c(3),
  size = c(300),
  offset = 50,
  fusion_groups = list(c(1, 2), c(5, 6, 7)),
  save_files = FALSE
)

# Compare piece 5
p5_0 <- result0$pieces[[5]]
p5_50 <- result50$pieces[[5]]

cat("=== PIECE 5 ===\n\n")
cat("Without offset (offset=0):\n")
cat(sprintf("  Center: (%.2f, %.2f)\n", p5_0$center[1], p5_0$center[2]))
segs0 <- parse_svg_path(p5_0$path)
cat(sprintf("  Path M point: (%.2f, %.2f)\n", segs0[[1]]$x, segs0[[1]]$y))

cat("\nWith offset (offset=50):\n")
cat(sprintf("  Center: (%.2f, %.2f)\n", p5_50$center[1], p5_50$center[2]))
segs50 <- parse_svg_path(p5_50$path)
cat(sprintf("  Path M point: (%.2f, %.2f)\n", segs50[[1]]$x, segs50[[1]]$y))

# Calculate translation
dx <- segs50[[1]]$x - segs0[[1]]$x
dy <- segs50[[1]]$y - segs0[[1]]$y
cat(sprintf("\nTranslation applied: dx=%.2f, dy=%.2f\n", dx, dy))

# Check if piece has translation info stored
cat("\n\nPiece 5 attributes (offset=50):\n")
for (name in names(p5_50)) {
  val <- p5_50[[name]]
  if (is.numeric(val) && length(val) <= 2) {
    cat(sprintf("  %s: %s\n", name, paste(round(val, 2), collapse=", ")))
  } else if (is.character(val) || is.logical(val)) {
    cat(sprintf("  %s: %s\n", name, val))
  } else if (is.list(val)) {
    cat(sprintf("  %s: (list)\n", name))
  } else {
    cat(sprintf("  %s: (path/other)\n", name))
  }
}

# Test edge splitting for offset=50 piece
cat("\n\n=== EDGE SPLITTING FOR OFFSET=50 ===\n\n")
cat("Piece 5 (offset=50) path:\n")
cat(substr(p5_50$path, 1, 200), "...\n\n")

edge_paths <- get_piece_edge_paths(p5_50)
cat("Edge paths:\n")
for (name in names(edge_paths)) {
  path <- edge_paths[[name]]
  if (!is.null(path) && nzchar(path)) {
    cat(sprintf("  %s: %s...\n", name, substr(path, 1, 60)))
  } else {
    cat(sprintf("  %s: EMPTY\n", name))
  }
}
