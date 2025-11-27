# Analyze hexagonal coordinate system to understand edge mapping
source('R/hexagonal_puzzle.R')

# Test with 2 rings
init_hex_jigsaw(seed = 42, rings = 2, diameter = 200)

cat("Analyzing hexagonal coordinate system (2 rings)\n")
cat("==============================================\n\n")

# Understand horizontal segments
cat("HORIZONTAL SEGMENTS:\n")
cat("-------------------\n")
n <- 2
yl <- 2 * n - 1  # = 3

cat("yl =", yl, "(range of y coordinates)\n")
cat("y ranges from", -yl + 2, "to", yl - 2, "by 2\n\n")

segment_count <- 0
for (yi in seq(-yl + 2, yl - 2, by = 2)) {
  xl <- 2 * n - 1 - (abs(yi) - 1) / 2
  cat(sprintf("yi=%d: xl=%.1f, x ranges from %.0f to %.0f\n",
              yi, xl, -xl + 1, xl - 2))
  num_segs <- length(seq(-xl + 1, xl - 2, by = 1))
  segment_count <- segment_count + num_segs
  cat(sprintf("  -> %d segments\n", num_segs))
}
cat("\nTotal horizontal segments:", segment_count, "\n\n")

# Understand vertical segments
cat("VERTICAL SEGMENTS:\n")
cat("-----------------\n")
segment_count <- 0
for (yi in seq(-yl, yl - 1, by = 2)) {
  xl <- 2 * n - 1 - (abs(yi + 1)) / 2
  cat(sprintf("yi=%d: xl=%.1f, x ranges from %.0f to %.0f\n",
              yi, xl, -xl + 2, xl - 2))
  num_segs <- length(seq(-xl + 2, xl - 2, by = 2))
  segment_count <- segment_count + num_segs
  cat(sprintf("  -> %d segments\n", num_segs))
}
cat("\nTotal vertical segments:", segment_count, "\n\n")

# Understand border segments
cat("BORDER SEGMENTS:\n")
cat("---------------\n")
yi <- 1 - 2 * n  # = -3
cat("yi =", yi, "\n")
cat("x ranges from", -n, "to", n - 2, "(", length(seq(-n, n - 2, by = 1)), "segments per rotation)\n")
cat("6 rotations (0, π/3, 2π/3, π, 4π/3, 5π/3)\n")
total_border <- 6 * length(seq(-n, n - 2, by = 1))
cat("Total border segments:", total_border, "\n\n")

# Hexagonal piece positions
cat("PIECE POSITIONS (axial coordinates):\n")
cat("-----------------------------------\n")
cat("Center: (q=0, r=0)\n")

# Ring 1
cat("\nRing 1 (6 pieces):\n")
q <- 0
r <- -1
hex_directions <- list(c(1,0), c(1,-1), c(0,-1), c(-1,0), c(-1,1), c(0,1))
piece_num <- 2

for (direction in 1:6) {
  for (step in 1:1) {  # ring = 1, so 1 step
    cat(sprintf("  Piece %d: (q=%d, r=%d)\n", piece_num, q, r))
    piece_num <- piece_num + 1
    if (!(direction == 6 && step == 1)) {
      q <- q + hex_directions[[direction]][1]
      r <- r + hex_directions[[direction]][2]
    }
  }
}

cat("\n")
cat("CHALLENGE: Map (q, r) coordinates to (xi, yi) edge indices\n")
cat("=========================================================\n")
