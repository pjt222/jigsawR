# Test 7-ring puzzle through unified generate_puzzle API

cat("=== Testing 7-Ring Puzzle via generate_puzzle() ===\n\n")

# Load package functions
source("R/logging.R")
source("R/config_utils.R")
source("R/concentric_geometry.R")
source("R/hexagonal_bezier_generation.R")
source("R/concentric_edge_generation.R")
source("R/unified_piece_generation.R")
source("R/piece_positioning.R")
source("R/unified_renderer.R")
source("R/jigsawR_clean.R")

# Generate complete puzzle (offset = 0)
cat("Generating complete puzzle (offset = 0)...\n")
result_complete <- generate_puzzle(
  type = "concentric",
  grid = c(7),
  size = c(500),
  seed = 42,
  tabsize = 50,
  jitter = 4,
  offset = 0,
  center_shape = "hexagon",
  save_files = TRUE,
  output_dir = "output",
  filename_prefix = "7ring_complete_unified"
)

cat(sprintf("Complete puzzle saved: %s\n", result_complete$files$svg))
cat(sprintf("Pieces: %d\n", length(result_complete$pieces)))
cat(sprintf("Canvas: %.0f x %.0f mm\n\n", result_complete$canvas_size[1], result_complete$canvas_size[2]))

# Generate separated puzzle (offset = 25)
cat("Generating separated puzzle (offset = 25)...\n")
result_separated <- generate_puzzle(
  type = "concentric",
  grid = c(7),
  size = c(500),
  seed = 42,
  tabsize = 50,
  jitter = 4,
  offset = 25,
  center_shape = "hexagon",
  save_files = TRUE,
  output_dir = "output",
  filename_prefix = "7ring_separated_unified"
)

cat(sprintf("Separated puzzle saved: %s\n", result_separated$files$svg))
cat(sprintf("Pieces: %d\n", length(result_separated$pieces)))
cat(sprintf("Canvas: %.0f x %.0f mm\n\n", result_separated$canvas_size[1], result_separated$canvas_size[2]))

# Verify piece 27 has 2 inner edges in both cases
cat("=== Verifying Piece 27 Structure ===\n")

# Get piece 27 from results
piece_27_complete <- result_complete$pieces[[27]]
piece_27_separated <- result_separated$pieces[[27]]

cat(sprintf("Complete mode - Piece 27 path length: %d chars\n", nchar(piece_27_complete$path)))
cat(sprintf("Separated mode - Piece 27 path length: %d chars\n", nchar(piece_27_separated$path)))

# Count bezier commands in paths
count_bezier <- function(path) {
  length(gregexpr("C ", path)[[1]])
}

cat(sprintf("Complete mode - Piece 27 bezier curves: %d\n", count_bezier(piece_27_complete$path)))
cat(sprintf("Separated mode - Piece 27 bezier curves: %d\n", count_bezier(piece_27_separated$path)))

cat("\n=== Test Complete ===\n")
