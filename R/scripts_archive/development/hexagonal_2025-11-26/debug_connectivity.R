# Debug connectivity graph building
source('R/hexagonal_puzzle.R')
source('R/bezier_utils.R')
source('R/hexagonal_piece_extraction.R')

cat("DEBUGGING CONNECTIVITY GRAPH\n")
cat("============================\n\n")

# Generate 2-ring puzzle
init_hex_jigsaw(seed = 42, rings = 2, diameter = 200)
h_path <- hex_gen_dh()
v_path <- hex_gen_dv()
b_path <- hex_gen_db()

# Split paths
h_segs <- split_path_by_move(h_path)
v_segs <- split_path_by_move(v_path)
b_segs <- split_path_by_move(b_path)

all_segs <- c(h_segs, v_segs, b_segs)

cat(sprintf("Total segments: %d\n\n", length(all_segs)))

# Print first few segment endpoints
cat("First 5 segment endpoints:\n")
for (i in 1:min(5, length(all_segs))) {
  seg <- all_segs[[i]]
  cat(sprintf("  Segment %d: (%.2f, %.2f) -> (%.2f, %.2f)\n",
              i, seg$start[1], seg$start[2], seg$end[1], seg$end[2]))
}
cat("\n")

# Look for segments that should connect
# Horizontal segment ends at (195, 76.7) and vertical should start there
cat("Looking for potential connections:\n")
target_point <- c(195, 76.7)
cat(sprintf("Target point: (%.2f, %.2f)\n", target_point[1], target_point[2]))

for (i in seq_along(all_segs)) {
  seg <- all_segs[[i]]

  # Check distance to target
  start_dist <- sqrt(sum((seg$start - target_point)^2))
  end_dist <- sqrt(sum((seg$end - target_point)^2))

  if (start_dist < 1.0 || end_dist < 1.0) {
    cat(sprintf("  Segment %d: start_dist=%.3f, end_dist=%.3f\n", i, start_dist, end_dist))
    cat(sprintf("    Start: (%.4f, %.4f)\n", seg$start[1], seg$start[2]))
    cat(sprintf("    End:   (%.4f, %.4f)\n", seg$end[1], seg$end[2]))
  }
}
cat("\n")

# Test connectivity with different tolerances
cat("Testing connectivity with different tolerances:\n")
for (tol in c(0.1, 0.5, 1.0, 2.0)) {
  graph <- build_segment_graph(all_segs, tolerance = tol)
  connections <- sapply(graph, function(n) length(n$connections))
  cat(sprintf("  Tolerance %.1f: min=%d, max=%d, mean=%.1f\n",
              tol, min(connections), max(connections), mean(connections)))
}
