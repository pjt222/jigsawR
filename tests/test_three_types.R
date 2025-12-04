# Test all three puzzle types after refactoring
# Verifies that "concentric" is now a proper top-level type

cat("=== Testing Three Puzzle Types ===\n\n")

# Source all necessary files
source("R/logging.R")
source("R/config_utils.R")
source("R/bezier_utils.R")
source("R/rectangular_puzzle.R")
source("R/puzzle_core_clean.R")
source("R/concentric_geometry.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_topology.R")
source("R/hexagonal_edge_generation_fixed.R")
source("R/concentric_edge_generation.R")
source("R/unified_piece_generation.R")
source("R/piece_positioning.R")
source("R/unified_renderer.R")
source("R/jigsawR_clean.R")

# Test 1: Rectangular puzzle
cat("Test 1: Rectangular puzzle\n")
result_rect <- generate_puzzle(
  type = "rectangular",
  grid = c(2, 3),
  size = c(200, 150),
  seed = 42,
  offset = 0,
  save_files = FALSE
)
cat(sprintf("  Type: %s\n", result_rect$type))
cat(sprintf("  Pieces: %d\n", length(result_rect$pieces)))
cat(sprintf("  Canvas: %.0f x %.0f\n", result_rect$canvas_size[1], result_rect$canvas_size[2]))
stopifnot(result_rect$type == "rectangular")
stopifnot(length(result_rect$pieces) == 6)  # 2x3 = 6 pieces
cat("  PASSED\n\n")

# Test 2: Hexagonal puzzle
cat("Test 2: Hexagonal puzzle\n")
result_hex <- generate_puzzle(
  type = "hexagonal",
  grid = c(3),
  size = c(200),
  seed = 42,
  offset = 0,
  do_warp = TRUE,
  do_trunc = TRUE,
  save_files = FALSE
)
cat(sprintf("  Type: %s\n", result_hex$type))
cat(sprintf("  Pieces: %d\n", length(result_hex$pieces)))
cat(sprintf("  Canvas: %.0f x %.0f\n", result_hex$canvas_size[1], result_hex$canvas_size[2]))
stopifnot(result_hex$type == "hexagonal")
stopifnot(length(result_hex$pieces) == 19)  # 3*3*(3-1)+1 = 19 pieces
cat("  PASSED\n\n")

# Test 3: Concentric puzzle (with hexagon center)
cat("Test 3: Concentric puzzle (hexagon center)\n")
result_conc_hex <- generate_puzzle(
  type = "concentric",
  grid = c(3),
  size = c(200),
  seed = 42,
  center_shape = "hexagon",
  offset = 0,
  save_files = FALSE
)
cat(sprintf("  Type: %s\n", result_conc_hex$type))
cat(sprintf("  Pieces: %d\n", length(result_conc_hex$pieces)))
cat(sprintf("  Canvas: %.0f x %.0f\n", result_conc_hex$canvas_size[1], result_conc_hex$canvas_size[2]))
stopifnot(result_conc_hex$type == "concentric")
stopifnot(length(result_conc_hex$pieces) == 19)  # 3*3*(3-1)+1 = 19 pieces
cat("  PASSED\n\n")

# Test 4: Concentric puzzle (with circle center)
cat("Test 4: Concentric puzzle (circle center)\n")
result_conc_circ <- generate_puzzle(
  type = "concentric",
  grid = c(3),
  size = c(200),
  seed = 42,
  center_shape = "circle",
  offset = 0,
  save_files = FALSE
)
cat(sprintf("  Type: %s\n", result_conc_circ$type))
cat(sprintf("  Pieces: %d\n", length(result_conc_circ$pieces)))
cat(sprintf("  Center shape param: %s\n", result_conc_circ$parameters$center_shape))
stopifnot(result_conc_circ$type == "concentric")
stopifnot(result_conc_circ$parameters$center_shape == "circle")
cat("  PASSED\n\n")

# Test 5: Concentric puzzle with offset (separation)
cat("Test 5: Concentric puzzle with offset\n")
result_conc_sep <- generate_puzzle(
  type = "concentric",
  grid = c(3),
  size = c(200),
  seed = 42,
  center_shape = "hexagon",
  offset = 15,
  save_files = FALSE
)
cat(sprintf("  Type: %s\n", result_conc_sep$type))
cat(sprintf("  Offset: %d\n", result_conc_sep$parameters$offset))
cat(sprintf("  Canvas: %.0f x %.0f\n", result_conc_sep$canvas_size[1], result_conc_sep$canvas_size[2]))
stopifnot(result_conc_sep$type == "concentric")
stopifnot(result_conc_sep$parameters$offset == 15)
# Canvas should be larger with offset
stopifnot(result_conc_sep$canvas_size[1] > result_conc_hex$canvas_size[1])
cat("  PASSED\n\n")

# Test 6: Invalid type should fail
cat("Test 6: Invalid type should fail\n")
tryCatch({
  generate_puzzle(type = "invalid_type", grid = c(2, 2), size = c(100, 100))
  cat("  ERROR: Should have thrown an error!\n")
  stop("Test failed - no error thrown for invalid type")
}, error = function(e) {
  if (grepl("Invalid type", e$message)) {
    cat(sprintf("  Correctly threw error: %s\n", e$message))
    cat("  PASSED\n\n")
  } else {
    stop(paste("Unexpected error:", e$message))
  }
})

# Test 7: center_shape should only be set for concentric type
cat("Test 7: center_shape only set for concentric type\n")
stopifnot(is.null(result_rect$parameters$center_shape))
stopifnot(is.null(result_hex$parameters$center_shape))
stopifnot(!is.null(result_conc_hex$parameters$center_shape))
cat("  Rectangular center_shape: NULL (correct)\n")
cat("  Hexagonal center_shape: NULL (correct)\n")
cat("  Concentric center_shape: ", result_conc_hex$parameters$center_shape, " (correct)\n")
cat("  PASSED\n\n")

# Test 8: Generate files for visual inspection
cat("Test 8: Generate files for visual inspection\n")
if (!dir.exists("output")) dir.create("output")

# Rectangular
result <- generate_puzzle(
  type = "rectangular",
  grid = c(3, 4),
  size = c(300, 200),
  seed = 123,
  offset = 10,
  save_files = TRUE,
  output_dir = "output",
  filename_prefix = "test_rectangular"
)
cat(sprintf("  Saved: %s\n", result$files$svg))

# Hexagonal
result <- generate_puzzle(
  type = "hexagonal",
  grid = c(4),
  size = c(250),
  seed = 123,
  offset = 10,
  do_warp = TRUE,
  do_trunc = TRUE,
  do_circular_border = TRUE,
  save_files = TRUE,
  output_dir = "output",
  filename_prefix = "test_hexagonal"
)
cat(sprintf("  Saved: %s\n", result$files$svg))

# Concentric
result <- generate_puzzle(
  type = "concentric",
  grid = c(4),
  size = c(250),
  seed = 123,
  center_shape = "circle",
  offset = 10,
  save_files = TRUE,
  output_dir = "output",
  filename_prefix = "test_concentric"
)
cat(sprintf("  Saved: %s\n", result$files$svg))
cat("  PASSED\n\n")

cat("=== ALL TESTS PASSED ===\n")
