# Debug: Compare "Warped Hexagon" vs "Perfect Circle" outputs
# They should differ in border edge commands (L vs A)

source("R/logging.R")
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  if (!grepl("scripts_archive", f)) source(f)
}

cat("=== Comparing Warped Hexagon vs Perfect Circle ===\n\n")

# Test parameters
rings <- 3
diameter <- 200
seed <- 1234

cat("--- Warped Hexagon Mode ---\n")
cat("Parameters: do_warp=TRUE, do_trunc=TRUE, do_circular_border=FALSE\n\n")

warped_hex <- generate_puzzle(
  type = "hexagonal",
  seed = seed,
  grid = c(rings),
  size = c(diameter),
  do_warp = TRUE,
  do_trunc = TRUE,
  do_circular_border = FALSE
)

cat("\n--- Perfect Circle Mode ---\n")
cat("Parameters: do_warp=TRUE, do_trunc=TRUE, do_circular_border=TRUE\n\n")

perfect_circle <- generate_puzzle(
  type = "hexagonal",
  seed = seed,
  grid = c(rings),
  size = c(diameter),
  do_warp = TRUE,
  do_trunc = TRUE,
  do_circular_border = TRUE
)

# Compare a boundary piece path (piece 1 is always in the outer ring for rings >= 2)
# For 3-ring puzzle: pieces 8-19 are in outer ring
outer_piece_id <- 8

cat("\n=== Comparing Piece", outer_piece_id, "(outer ring) ===\n\n")

warped_path <- warped_hex$pieces[[outer_piece_id]]$path
circle_path <- perfect_circle$pieces[[outer_piece_id]]$path

cat("Warped Hexagon path (first 400 chars):\n")
cat(substr(warped_path, 1, 400), "\n\n")

cat("Perfect Circle path (first 400 chars):\n")
cat(substr(circle_path, 1, 400), "\n\n")

# Check for arc commands
warped_has_arc <- grepl(" A ", warped_path)
circle_has_arc <- grepl(" A ", circle_path)

cat("=== Analysis ===\n")
cat("Warped Hexagon has arc commands (A):", warped_has_arc, "\n")
cat("Perfect Circle has arc commands (A):", circle_has_arc, "\n")

# Paths should be different
paths_identical <- identical(warped_path, circle_path)
cat("Paths are identical:", paths_identical, "\n")

if (paths_identical) {
  cat("\nERROR: Paths should NOT be identical!\n")
  cat("Expected: Warped Hexagon uses 'L' for borders, Perfect Circle uses 'A' for borders\n")
} else {
  cat("\nSUCCESS: Paths are different as expected\n")
}

# Save both for visual comparison
writeLines(warped_hex$svg_content, "output/debug_warped_hexagon.svg")
writeLines(perfect_circle$svg_content, "output/debug_perfect_circle.svg")
cat("\nSVGs saved to output/ for visual comparison\n")
