# Debug viewBox clipping for hexagonal puzzles with warp/trunc
# This script tests the canvas size calculation when warp/trunc are enabled

# Set up console output
cat("=" , rep("=", 70), "\n", sep = "")
cat("Debug: ViewBox Clipping for Hexagonal Puzzles\n")
cat("=" , rep("=", 70), "\n\n", sep = "")

# Source required functions
source("R/logging.R")
source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")
source("R/unified_piece_generation.R")
source("R/piece_positioning.R")

# Test parameters
rings <- 3
diameter <- 240
seed <- 1234

cat("Test parameters:\n")
cat(sprintf("  rings: %d\n", rings))
cat(sprintf("  diameter: %d mm\n", diameter))
cat(sprintf("  seed: %d\n", seed))
cat("\n")

# Calculate expected bounds
piece_radius <- diameter / (rings * 4)
circle_radius <- diameter / 2
cat(sprintf("Calculated values:\n"))
cat(sprintf("  piece_radius: %.2f mm\n", piece_radius))
cat(sprintf("  circle_radius (when warped): %.2f mm\n", circle_radius))
cat("\n")

# Test all four combinations
test_cases <- list(
  list(do_warp = FALSE, do_trunc = FALSE, name = "neither (zigzag)"),
  list(do_warp = FALSE, do_trunc = TRUE,  name = "trunc_only (clean hexagon)"),
  list(do_warp = TRUE,  do_trunc = FALSE, name = "warp_only (warped zigzag)"),
  list(do_warp = TRUE,  do_trunc = TRUE,  name = "both (perfect circle)")
)

for (test in test_cases) {
  cat("-", rep("-", 70), "\n", sep = "")
  cat(sprintf("Testing: %s (do_warp=%s, do_trunc=%s) [offset=0]\n",
              test$name, test$do_warp, test$do_trunc))
  cat("-", rep("-", 70), "\n", sep = "")

  # Generate pieces
  pieces_result <- generate_pieces_internal(
    type = "hexagonal",
    seed = seed,
    grid = c(rings),
    size = c(diameter),
    tabsize = 20,
    jitter = 4,
    do_warp = test$do_warp,
    do_trunc = test$do_trunc
  )

  # Get canvas info
  canvas_size <- pieces_result$canvas_size
  canvas_offset <- pieces_result$canvas_offset

  cat(sprintf("  canvas_size: %.2f x %.2f\n", canvas_size[1], canvas_size[2]))
  cat(sprintf("  canvas_offset: (%.2f, %.2f)\n", canvas_offset[1], canvas_offset[2]))

  # Calculate actual bounds from piece paths
  all_x <- c()
  all_y <- c()

  for (piece in pieces_result$pieces) {
    # Extract coordinates from path
    path <- piece$path
    numbers <- as.numeric(unlist(regmatches(path, gregexpr("-?[0-9]+\\.?[0-9]*", path))))
    numbers <- numbers[!is.na(numbers)]

    if (length(numbers) >= 2) {
      x_coords <- numbers[seq(1, length(numbers), by = 2)]
      y_coords <- numbers[seq(2, length(numbers), by = 2)]
      all_x <- c(all_x, x_coords)
      all_y <- c(all_y, y_coords)
    }
  }

  actual_min_x <- min(all_x)
  actual_max_x <- max(all_x)
  actual_min_y <- min(all_y)
  actual_max_y <- max(all_y)

  cat(sprintf("  Actual path bounds:\n"))
  cat(sprintf("    X: [%.2f, %.2f] (range: %.2f)\n", actual_min_x, actual_max_x, actual_max_x - actual_min_x))
  cat(sprintf("    Y: [%.2f, %.2f] (range: %.2f)\n", actual_min_y, actual_max_y, actual_max_y - actual_min_y))

  # Check if viewBox contains all content
  vb_min_x <- canvas_offset[1]
  vb_max_x <- canvas_offset[1] + canvas_size[1]
  vb_min_y <- canvas_offset[2]
  vb_max_y <- canvas_offset[2] + canvas_size[2]

  cat(sprintf("  ViewBox bounds:\n"))
  cat(sprintf("    X: [%.2f, %.2f]\n", vb_min_x, vb_max_x))
  cat(sprintf("    Y: [%.2f, %.2f]\n", vb_min_y, vb_max_y))

  # Check clipping
  clipped <- FALSE
  if (actual_min_x < vb_min_x) {
    cat(sprintf("  ⚠️ CLIPPED: Left edge (%.2f < %.2f)\n", actual_min_x, vb_min_x))
    clipped <- TRUE
  }
  if (actual_max_x > vb_max_x) {
    cat(sprintf("  ⚠️ CLIPPED: Right edge (%.2f > %.2f)\n", actual_max_x, vb_max_x))
    clipped <- TRUE
  }
  if (actual_min_y < vb_min_y) {
    cat(sprintf("  ⚠️ CLIPPED: Top edge (%.2f < %.2f)\n", actual_min_y, vb_min_y))
    clipped <- TRUE
  }
  if (actual_max_y > vb_max_y) {
    cat(sprintf("  ⚠️ CLIPPED: Bottom edge (%.2f > %.2f)\n", actual_max_y, vb_max_y))
    clipped <- TRUE
  }

  if (!clipped) {
    cat("  ✓ ViewBox contains all content\n")
  }

  cat("\n")
}

# Also test with offset > 0 (separated mode)
cat("\n")
cat("=" , rep("=", 70), "\n", sep = "")
cat("Testing separated mode (offset=15) with warp+trunc\n")
cat("=" , rep("=", 70), "\n\n", sep = "")

pieces_result <- generate_pieces_internal(
  type = "hexagonal",
  seed = seed,
  grid = c(rings),
  size = c(diameter),
  tabsize = 20,
  jitter = 4,
  do_warp = TRUE,
  do_trunc = TRUE
)

positioned <- apply_piece_positioning(pieces_result, offset = 15)

canvas_size <- positioned$canvas_size
canvas_offset <- positioned$canvas_offset

cat(sprintf("  canvas_size: %.2f x %.2f\n", canvas_size[1], canvas_size[2]))
cat(sprintf("  canvas_offset: (%.2f, %.2f)\n", canvas_offset[1], canvas_offset[2]))

# Calculate actual bounds from piece paths
all_x <- c()
all_y <- c()

for (piece in positioned$pieces) {
  path <- piece$path
  numbers <- as.numeric(unlist(regmatches(path, gregexpr("-?[0-9]+\\.?[0-9]*", path))))
  numbers <- numbers[!is.na(numbers)]

  if (length(numbers) >= 2) {
    x_coords <- numbers[seq(1, length(numbers), by = 2)]
    y_coords <- numbers[seq(2, length(numbers), by = 2)]
    all_x <- c(all_x, x_coords)
    all_y <- c(all_y, y_coords)
  }
}

actual_min_x <- min(all_x)
actual_max_x <- max(all_x)
actual_min_y <- min(all_y)
actual_max_y <- max(all_y)

cat(sprintf("  Actual path bounds:\n"))
cat(sprintf("    X: [%.2f, %.2f] (range: %.2f)\n", actual_min_x, actual_max_x, actual_max_x - actual_min_x))
cat(sprintf("    Y: [%.2f, %.2f] (range: %.2f)\n", actual_min_y, actual_max_y, actual_max_y - actual_min_y))

vb_min_x <- canvas_offset[1]
vb_max_x <- canvas_offset[1] + canvas_size[1]
vb_min_y <- canvas_offset[2]
vb_max_y <- canvas_offset[2] + canvas_size[2]

cat(sprintf("  ViewBox bounds:\n"))
cat(sprintf("    X: [%.2f, %.2f]\n", vb_min_x, vb_max_x))
cat(sprintf("    Y: [%.2f, %.2f]\n", vb_min_y, vb_max_y))

clipped <- FALSE
if (actual_min_x < vb_min_x) {
  cat(sprintf("  ⚠️ CLIPPED: Left edge (%.2f < %.2f)\n", actual_min_x, vb_min_x))
  clipped <- TRUE
}
if (actual_max_x > vb_max_x) {
  cat(sprintf("  ⚠️ CLIPPED: Right edge (%.2f > %.2f)\n", actual_max_x, vb_max_x))
  clipped <- TRUE
}
if (actual_min_y < vb_min_y) {
  cat(sprintf("  ⚠️ CLIPPED: Top edge (%.2f < %.2f)\n", actual_min_y, vb_min_y))
  clipped <- TRUE
}
if (actual_max_y > vb_max_y) {
  cat(sprintf("  ⚠️ CLIPPED: Bottom edge (%.2f > %.2f)\n", actual_max_y, vb_max_y))
  clipped <- TRUE
}

if (!clipped) {
  cat("  ✓ ViewBox contains all content\n")
}

cat("\n")
cat("=" , rep("=", 70), "\n", sep = "")
cat("Debug complete.\n")
