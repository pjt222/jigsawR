#!/usr/bin/env Rscript
# Run hexagonal individual pieces tests

# Source required files
source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")
source("R/hexagonal_individual_pieces.R")

cat("Running hexagonal individual pieces tests...\n\n")

# Test 1: Basic piece count for 2-ring
cat("Test 1: 2-ring piece count...\n")
result <- generate_hexagonal_individual_pieces(
  rings = 2,
  seed = 42,
  output_dir = tempdir(),
  save_individual = FALSE,
  save_combined = FALSE
)
stopifnot(length(result$pieces) == 7)
cat("  PASS: 7 pieces generated\n")

# Test 2: 3-ring piece count
cat("Test 2: 3-ring piece count...\n")
result <- generate_hexagonal_individual_pieces(
  rings = 3,
  seed = 42,
  output_dir = tempdir(),
  save_individual = FALSE,
  save_combined = FALSE
)
stopifnot(length(result$pieces) == 19)
cat("  PASS: 19 pieces generated\n")

# Test 3: SVG path validity
cat("Test 3: SVG path validity...\n")
for (i in seq_along(result$pieces)) {
  piece <- result$pieces[[i]]
  stopifnot(!is.null(piece$path))
  stopifnot(nchar(piece$path) > 20)
  stopifnot(grepl("^M", piece$path))
  stopifnot(grepl("Z$", piece$path))
}
cat("  PASS: All paths valid\n")

# Test 4: Reproducibility
cat("Test 4: Reproducibility with same seed...\n")
result1 <- generate_hexagonal_individual_pieces(rings = 2, seed = 12345, output_dir = tempdir(), save_individual = FALSE, save_combined = FALSE)
result2 <- generate_hexagonal_individual_pieces(rings = 2, seed = 12345, output_dir = tempdir(), save_individual = FALSE, save_combined = FALSE)
for (i in seq_along(result1$pieces)) {
  stopifnot(result1$pieces[[i]]$path == result2$pieces[[i]]$path)
}
cat("  PASS: Same seed produces identical results\n")

# Test 5: Different seeds produce different results
cat("Test 5: Different seeds...\n")
result1 <- generate_hexagonal_individual_pieces(rings = 2, seed = 11111, output_dir = tempdir(), save_individual = FALSE, save_combined = FALSE)
result2 <- generate_hexagonal_individual_pieces(rings = 2, seed = 22222, output_dir = tempdir(), save_individual = FALSE, save_combined = FALSE)
paths_differ <- FALSE
for (i in seq_along(result1$pieces)) {
  if (result1$pieces[[i]]$path != result2$pieces[[i]]$path) {
    paths_differ <- TRUE
    break
  }
}
stopifnot(paths_differ)
cat("  PASS: Different seeds produce different results\n")

# Test 6: Piece metadata
cat("Test 6: Piece metadata...\n")
result <- generate_hexagonal_individual_pieces(rings = 2, seed = 42, output_dir = tempdir(), save_individual = FALSE, save_combined = FALSE)
for (i in seq_along(result$pieces)) {
  piece <- result$pieces[[i]]
  stopifnot(!is.null(piece$id))
  stopifnot(!is.null(piece$ring))
  stopifnot(!is.null(piece$center_x))
  stopifnot(!is.null(piece$center_y))
  stopifnot(piece$id == i)
}
cat("  PASS: All pieces have correct metadata\n")

# Test 7: Center piece is ring 0
cat("Test 7: Center piece ring...\n")
stopifnot(result$pieces[[1]]$ring == 0)
for (i in 2:7) {
  stopifnot(result$pieces[[i]]$ring == 1)
}
cat("  PASS: Ring assignments correct\n")

# Test 8: Parameters preserved
cat("Test 8: Parameters preserved...\n")
result <- generate_hexagonal_individual_pieces(
  rings = 3, seed = 999, diameter = 300, tabsize = 30, jitter = 8,
  output_dir = tempdir(), save_individual = FALSE, save_combined = FALSE
)
stopifnot(result$parameters$rings == 3)
stopifnot(result$parameters$seed == 999)
stopifnot(result$parameters$diameter == 300)
stopifnot(result$parameters$tabsize == 30)
stopifnot(result$parameters$jitter == 8)
cat("  PASS: Parameters preserved in result\n")

# Test 9: SVG-only function
cat("Test 9: SVG-only function...\n")
svg <- create_hexagonal_individual_pieces_svg(rings = 2, seed = 42)
stopifnot(is.character(svg))
stopifnot(nchar(svg) > 1000)
stopifnot(grepl("^<svg", svg))
stopifnot(grepl("</svg>$", svg))
stopifnot(grepl("<title>", svg))
cat("  PASS: SVG-only function works\n")

# Test 10: File saving
cat("Test 10: File saving...\n")
temp_dir <- file.path(tempdir(), "hex_test_individual")
if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)
result <- generate_hexagonal_individual_pieces(
  rings = 2, seed = 42, output_dir = temp_dir,
  save_individual = TRUE, save_combined = TRUE
)
stopifnot(length(result$files$individual) == 7)
stopifnot(all(file.exists(result$files$individual)))
stopifnot(file.exists(result$files$combined))

# Verify file contents
for (file in result$files$individual) {
  content <- paste(readLines(file), collapse = "\n")
  stopifnot(grepl("<svg", content))
  stopifnot(grepl("</svg>", content))
  stopifnot(grepl("<path", content))
}
unlink(temp_dir, recursive = TRUE)
cat("  PASS: Files saved correctly\n")

# Test 11: calculate_path_bounds function
cat("Test 11: Path bounds calculation...\n")
test_path <- "M 10 20 L 30 40 L 50 20 Z"
bounds <- calculate_path_bounds(test_path)
stopifnot(bounds$min_x == 10)
stopifnot(bounds$max_x == 50)
stopifnot(bounds$min_y == 20)
stopifnot(bounds$max_y == 40)
stopifnot(bounds$width == 40)
stopifnot(bounds$height == 20)
cat("  PASS: Path bounds calculated correctly\n")

# Test 12: Edge data generation
cat("Test 12: Edge data generation...\n")
edge_data <- generate_hex_edge_map(rings = 2, seed = 42, diameter = 240, tabsize = 27, jitter = 5)
stopifnot(edge_data$num_edges > 0)
stopifnot(length(edge_data$edge_map) > 0)
stopifnot(length(edge_data$piece_edge_map) > 0)
cat("  PASS: Edge data generated\n")

cat("\n=== ALL 12 TESTS PASSED ===\n")
