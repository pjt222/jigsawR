# Debug a single outer piece to see its path coordinates

cat("=" , rep("=", 70), "\n", sep = "")
cat("Debug: Single Outer Piece Path Analysis\n")
cat("=" , rep("=", 70), "\n\n", sep = "")

source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")

rings <- 3
diameter <- 240
seed <- 1234

# Generate pieces with warp+trunc
pieces <- generate_hex_pieces_with_edge_map(
  rings = rings,
  seed = seed,
  diameter = diameter,
  tabsize = 20,
  jitter = 4,
  separated = FALSE,
  separation_factor = 1.0,
  do_warp = TRUE,
  do_trunc = TRUE
)

# Look at piece 8 (an outer piece)
piece <- pieces[[8]]

cat("Piece 8 (outer ring):\n")
cat(sprintf("  Ring: %d\n", piece$ring))
cat(sprintf("  Center: (%.2f, %.2f)\n", piece$center_x, piece$center_y))
cat("\nFull path:\n")
cat(piece$path, "\n\n")

# Parse the path to understand coordinates
numbers <- as.numeric(unlist(regmatches(piece$path, gregexpr("-?[0-9]+\\.?[0-9]*", piece$path))))
numbers <- numbers[!is.na(numbers)]

cat(sprintf("Total numbers in path: %d\n", length(numbers)))

x_coords <- numbers[seq(1, length(numbers), by = 2)]
y_coords <- numbers[seq(2, length(numbers), by = 2)]

cat(sprintf("X range: [%.2f, %.2f] (span: %.2f)\n", min(x_coords), max(x_coords), max(x_coords) - min(x_coords)))
cat(sprintf("Y range: [%.2f, %.2f] (span: %.2f)\n", min(y_coords), max(y_coords), max(y_coords) - min(y_coords)))

# Compare with center piece
center <- pieces[[1]]
cat("\n\nPiece 1 (center):\n")
cat(sprintf("  Ring: %d\n", center$ring))
cat(sprintf("  Center: (%.2f, %.2f)\n", center$center_x, center$center_y))

center_numbers <- as.numeric(unlist(regmatches(center$path, gregexpr("-?[0-9]+\\.?[0-9]*", center$path))))
center_numbers <- center_numbers[!is.na(center_numbers)]

cx <- center_numbers[seq(1, length(center_numbers), by = 2)]
cy <- center_numbers[seq(2, length(center_numbers), by = 2)]

cat(sprintf("X range: [%.2f, %.2f] (span: %.2f)\n", min(cx), max(cx), max(cx) - min(cx)))
cat(sprintf("Y range: [%.2f, %.2f] (span: %.2f)\n", min(cy), max(cy), max(cy) - min(cy)))
