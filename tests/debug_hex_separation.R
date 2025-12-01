#!/usr/bin/env Rscript
# Debug hexagonal separation positioning
# Check for duplicate vertices or lines connecting to inner circles

cat("=== Debugging Hexagonal Separation ===\n\n")

# Source dependencies
source("R/logging.R")
source("R/config_utils.R")
source("R/hexagonal_puzzle.R")
source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")
source("R/unified_piece_generation.R")
source("R/piece_positioning.R")

# Generate a simple 3-ring hexagonal puzzle
rings <- 3
seed <- 42
diameter <- 200

cat("Generating hexagonal puzzle with", rings, "rings\n\n")

# Generate pieces internally (compact positions)
pieces_result <- generate_pieces_internal(
  type = "hexagonal",
  seed = seed,
  grid = c(rings),
  size = c(diameter),
  tabsize = 27,
  jitter = 5,
  do_warp = FALSE,
  do_trunc = FALSE
)

cat("Generated", length(pieces_result$pieces), "pieces\n\n")

# Check piece centers at compact position
cat("=== Compact Centers ===\n")
for (i in 1:min(7, length(pieces_result$pieces))) {
  p <- pieces_result$pieces[[i]]
  cat(sprintf("Piece %s: center (%.2f, %.2f)\n", p$id, p$center[1], p$center[2]))
}

# Apply separation
offset <- 20
positioned <- apply_piece_positioning(pieces_result, offset = offset)

cat("\n=== Separated Centers (offset =", offset, "mm) ===\n")
for (i in 1:min(7, length(positioned$pieces))) {
  p <- positioned$pieces[[i]]
  cat(sprintf("Piece %s: center (%.2f, %.2f)\n", p$id, p$center[1], p$center[2]))
}

# Check for duplicate coordinates in paths
cat("\n=== Checking for Duplicate/Shared Starting Points ===\n")

# Extract all M (move) commands from paths
extract_move_points <- function(path) {
  # Find all "M x y" patterns
  matches <- regmatches(path, gregexpr("M [0-9.-]+ [0-9.-]+", path, perl = TRUE))[[1]]
  points <- list()
  for (m in matches) {
    parts <- as.numeric(unlist(strsplit(gsub("M ", "", m), " ")))
    if (length(parts) == 2) {
      points[[length(points) + 1]] <- parts
    }
  }
  return(points)
}

all_start_points <- list()
for (i in 1:length(positioned$pieces)) {
  p <- positioned$pieces[[i]]
  points <- extract_move_points(p$path)
  for (pt in points) {
    key <- sprintf("%.1f,%.1f", pt[1], pt[2])
    if (is.null(all_start_points[[key]])) {
      all_start_points[[key]] <- c()
    }
    all_start_points[[key]] <- c(all_start_points[[key]], p$id)
  }
}

# Find any shared starting points
cat("\nStarting points shared by multiple pieces:\n")
shared_count <- 0
for (key in names(all_start_points)) {
  if (length(all_start_points[[key]]) > 1) {
    cat(sprintf("  Point %s: %s\n", key, paste(all_start_points[[key]], collapse = ", ")))
    shared_count <- shared_count + 1
  }
}
if (shared_count == 0) {
  cat("  None found - each piece starts at a unique point\n")
}

# Check piece 1 (center) path in detail
cat("\n=== Center Piece Path Analysis ===\n")
center_piece <- positioned$pieces[[1]]
cat("Piece ID:", center_piece$id, "\n")
cat("Center:", sprintf("(%.2f, %.2f)\n", center_piece$center[1], center_piece$center[2]))
cat("Path (first 500 chars):\n")
cat(substr(center_piece$path, 1, 500), "\n...\n")

# Extract all coordinate pairs from the path
extract_all_coords <- function(path) {
  # Remove commands and split by space
  coords_str <- gsub("[MLCAZ]", " ", path)
  numbers <- as.numeric(unlist(strsplit(trimws(coords_str), "\\s+")))
  numbers <- numbers[!is.na(numbers)]

  # Pair up as x,y
  if (length(numbers) %% 2 != 0) {
    numbers <- numbers[-length(numbers)]
  }

  x_coords <- numbers[seq(1, length(numbers), 2)]
  y_coords <- numbers[seq(2, length(numbers), 2)]

  return(list(x = x_coords, y = y_coords))
}

center_coords <- extract_all_coords(center_piece$path)
cat("\nCenter piece coordinate range:\n")
cat(sprintf("  X: [%.2f, %.2f]\n", min(center_coords$x), max(center_coords$x)))
cat(sprintf("  Y: [%.2f, %.2f]\n", min(center_coords$y), max(center_coords$y)))

# Check if any coordinates are at (0,0) or very close to origin
near_origin <- sum(abs(center_coords$x) < 1 & abs(center_coords$y) < 1)
cat(sprintf("  Coords near origin (|x|<1 && |y|<1): %d\n", near_origin))

# Check an outer piece
cat("\n=== Outer Piece (last one) Path Analysis ===\n")
outer_piece <- positioned$pieces[[length(positioned$pieces)]]
cat("Piece ID:", outer_piece$id, "\n")
cat("Center:", sprintf("(%.2f, %.2f)\n", outer_piece$center[1], outer_piece$center[2]))

outer_coords <- extract_all_coords(outer_piece$path)
cat("\nOuter piece coordinate range:\n")
cat(sprintf("  X: [%.2f, %.2f]\n", min(outer_coords$x), max(outer_coords$x)))
cat(sprintf("  Y: [%.2f, %.2f]\n", min(outer_coords$y), max(outer_coords$y)))

# Check if any coordinates are near the center
dist_from_center <- sqrt(outer_coords$x^2 + outer_coords$y^2)
cat(sprintf("  Min distance from origin: %.2f\n", min(dist_from_center)))
cat(sprintf("  Max distance from origin: %.2f\n", max(dist_from_center)))

# If there are small distances, that might indicate wrong coordinates
if (min(dist_from_center) < 10) {
  cat("  WARNING: Some coordinates are very close to origin!\n")
  idx <- which(dist_from_center < 10)
  cat("  Suspicious coords:\n")
  for (i in head(idx, 5)) {
    cat(sprintf("    (%.2f, %.2f) dist=%.2f\n", outer_coords$x[i], outer_coords$y[i], dist_from_center[i]))
  }
}

# Check for L 0 0 or L commands to origin
cat("\n=== Checking for Lines to Origin ===\n")
for (i in 1:length(positioned$pieces)) {
  p <- positioned$pieces[[i]]
  # Check for L commands near origin
  if (grepl("L 0\\.00 0\\.00|L -?0\\.[0-9]+ -?0\\.[0-9]+", p$path)) {
    cat(sprintf("WARNING: Piece %s has L command near origin!\n", p$id))
  }
  # Check for any coordinate pair (0, 0) or near it
  coords <- extract_all_coords(p$path)
  near_zero_x <- which(abs(coords$x) < 2)
  near_zero_y <- which(abs(coords$y) < 2)
  near_zero <- intersect(near_zero_x, near_zero_y)
  if (length(near_zero) > 0 && i > 1) {  # Skip center piece
    cat(sprintf("Piece %s has coords near origin: ", p$id))
    for (idx in near_zero) {
      cat(sprintf("(%.2f, %.2f) ", coords$x[idx], coords$y[idx]))
    }
    cat("\n")
  }
}

# Look at a middle ring piece in detail
cat("\n=== Middle Ring Piece (piece 8) Path Analysis ===\n")
mid_piece <- positioned$pieces[[8]]
cat("Piece ID:", mid_piece$id, "\n")
cat("Center:", sprintf("(%.2f, %.2f)\n", mid_piece$center[1], mid_piece$center[2]))
cat("Full path:\n")
cat(mid_piece$path, "\n")

cat("\n=== Debug Complete ===\n")
