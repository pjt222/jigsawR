# Test script to investigate concentric edge splitting issue

library(devtools)
load_all()

# Generate a simple concentric puzzle
result <- generate_puzzle(
  type = "concentric",
  seed = 42,
  grid = c(2),  # 2 rings = 7 pieces
  size = c(200),
  offset = 10,
  save_files = FALSE
)

# Pick a trapezoid piece (piece 2-7 are trapezoids)
piece <- result$pieces[[2]]

cat("=== Piece Information ===\n")
cat("Piece ID:", piece$id, "\n")
cat("Piece Type:", piece$type, "\n")
cat("Ring:", piece$ring_pos$ring, "\n")
cat("Position:", piece$ring_pos$position, "\n")
cat("\n=== Original Path ===\n")
cat(piece$path, "\n\n")

# Parse the path
source("R/bezier_utils.R")
segments <- parse_svg_path(piece$path)

cat("=== Segment Count ===\n")
cat("Total segments:", length(segments), "\n")

# Filter out M and Z
content_segs <- segments[sapply(segments, function(s) !s$type %in% c("M", "Z"))]
cat("Content segments (non-M, non-Z):", length(content_segs), "\n\n")

cat("=== Segment Breakdown ===\n")
for (i in seq_along(segments)) {
  seg <- segments[[i]]
  if (seg$type == "M") {
    cat(sprintf("Seg %2d: M %.2f %.2f\n", i, seg$x, seg$y))
  } else if (seg$type == "L") {
    cat(sprintf("Seg %2d: L %.2f %.2f\n", i, seg$x, seg$y))
  } else if (seg$type == "C") {
    cat(sprintf("Seg %2d: C (bezier tab)\n", i))
  } else if (seg$type == "Z") {
    cat(sprintf("Seg %2d: Z\n", i))
  }
}

# Now test the split function
cat("\n=== Testing split_concentric_path_into_edges ===\n")
source("R/unified_renderer.R")
edge_paths <- split_concentric_path_into_edges(piece$path, piece)

cat("Edge names:", paste(names(edge_paths), collapse=", "), "\n\n")

for (edge_name in names(edge_paths)) {
  edge_path <- edge_paths[[edge_name]]
  if (edge_path == "") {
    cat(sprintf("Edge %s: EMPTY\n", edge_name))
  } else {
    edge_segs <- parse_svg_path(edge_path)
    edge_content <- edge_segs[sapply(edge_segs, function(s) !s$type %in% c("M", "Z"))]
    cat(sprintf("Edge %s: %d segments\n", edge_name, length(edge_content)))
    cat("  Path:", edge_path, "\n")
  }
}

# Calculate expected distribution
n_content <- length(content_segs)
n_edges <- 4
segs_per_edge <- ceiling(n_content / n_edges)

cat("\n=== Distribution Math ===\n")
cat("Content segments:", n_content, "\n")
cat("Edges:", n_edges, "\n")
cat("Segments per edge (ceiling):", segs_per_edge, "\n")
cat("Expected distribution:\n")
for (i in 1:n_edges) {
  edge_name <- c("INNER", "RIGHT", "OUTER", "LEFT")[i]
  # Calculate which segment indices belong to this edge
  start_idx <- (i - 1) * segs_per_edge + 1
  end_idx <- min(i * segs_per_edge, n_content)
  actual_count <- max(0, end_idx - start_idx + 1)
  cat(sprintf("  Edge %d (%s): segments %d-%d = %d segments\n", 
              i, edge_name, start_idx, end_idx, actual_count))
}

