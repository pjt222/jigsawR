# Test path splitting to understand structure
source('R/hexagonal_puzzle.R')
source('R/bezier_utils.R')
source('R/hexagonal_piece_extraction.R')

cat("TESTING PATH SPLITTING\n")
cat("======================\n\n")

# Generate 2-ring puzzle
init_hex_jigsaw(seed = 42, rings = 2, diameter = 200)
h_path <- hex_gen_dh()
v_path <- hex_gen_dv()
b_path <- hex_gen_db()

cat("Testing split_path_by_move()...\n\n")

# Split horizontal path
h_segs <- split_path_by_move(h_path)
cat(sprintf("Horizontal path: %d segments\n", length(h_segs)))
if (length(h_segs) > 0) {
  cat(sprintf("  Segment 1: starts at (%.1f, %.1f), ends at (%.1f, %.1f)\n",
              h_segs[[1]]$start[1], h_segs[[1]]$start[2],
              h_segs[[1]]$end[1], h_segs[[1]]$end[2]))
  cat(sprintf("  Path length: %d chars\n", nchar(h_segs[[1]]$path)))
}
cat("\n")

# Split vertical path
v_segs <- split_path_by_move(v_path)
cat(sprintf("Vertical path: %d segments\n", length(v_segs)))
if (length(v_segs) > 0) {
  cat(sprintf("  Segment 1: starts at (%.1f, %.1f), ends at (%.1f, %.1f)\n",
              v_segs[[1]]$start[1], v_segs[[1]]$start[2],
              v_segs[[1]]$end[1], v_segs[[1]]$end[2]))
}
cat("\n")

# Split border path
b_segs <- split_path_by_move(b_path)
cat(sprintf("Border path: %d segments\n", length(b_segs)))
if (length(b_segs) > 0) {
  cat(sprintf("  Segment 1: starts at (%.1f, %.1f), ends at (%.1f, %.1f)\n",
              b_segs[[1]]$start[1], b_segs[[1]]$start[2],
              b_segs[[1]]$end[1], b_segs[[1]]$end[2]))
}
cat("\n")

# Test full extraction
cat("Testing full extraction...\n")
extraction <- extract_hex_pieces_from_paths(h_path, v_path, b_path, rings = 2)

cat("\nSUCCESS: Path splitting works!\n")
cat("Next step: Implement piece boundary tracing\n")
