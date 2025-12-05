# Test 7-ring puzzle with circle center

cat("=== Test 7-Ring Puzzle with Circle Center ===\n\n")

source("R/logging.R")
source("R/config_utils.R")
source("R/concentric_geometry.R")
source("R/hexagonal_bezier_generation.R")
source("R/concentric_edge_generation.R")
source("R/unified_piece_generation.R")
source("R/piece_positioning.R")
source("R/unified_renderer.R")
source("R/jigsawR_clean.R")

# Test with circle center
cat("Generating 7-ring puzzle with circle center...\n")
result <- generate_puzzle(
  type = "concentric",
  grid = c(7),
  size = c(500),
  seed = 42,
  tabsize = 50,
  jitter = 4,
  offset = 25,
  center_shape = "circle",
  save_files = TRUE,
  output_dir = "output",
  filename_prefix = "7ring_circle_center"
)

cat(sprintf("Puzzle saved: %s\n", result$files$svg))
cat(sprintf("Pieces: %d\n", length(result$pieces)))
cat(sprintf("Canvas: %.0f x %.0f mm\n\n", result$canvas_size[1], result$canvas_size[2]))

# Verify center piece
center_piece <- result$pieces[[1]]
cat("=== Center Piece (ID 1) ===\n")
cat(sprintf("Path length: %d chars\n", nchar(center_piece$path)))

# Count bezier curves
bezier_count <- length(gregexpr("C ", center_piece$path)[[1]])
cat(sprintf("Bezier curves: %d (expected 18 for 6 edges * 3 curves each)\n", bezier_count))

# Verify path is closed
if (grepl("^M ", center_piece$path) && grepl("Z\\s*$", center_piece$path)) {
  cat("Path is properly closed (M...Z)\n")
} else {
  cat("ERROR: Path is not properly closed!\n")
}

# Also verify ring 1 pieces connect correctly
cat("\n=== Ring 1 Pieces (2-7) ===\n")
for (pid in 2:7) {
  piece <- result$pieces[[pid]]
  beziers <- length(gregexpr("C ", piece$path)[[1]])
  cat(sprintf("Piece %d: %d bezier curves\n", pid, beziers))
}

cat("\n=== Test Complete ===\n")
