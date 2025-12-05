# Detailed 7-ring hexagonal puzzle debug
# Looking for hardcoded values and inversion issues

setwd("D:/dev/p/jigsawR")

library(devtools)
load_all()

cat("==========================================================\n")
cat("DETAILED 7-RING PUZZLE ANALYSIS\n")
cat("==========================================================\n\n")

rings <- 7
diameter <- 400
seed <- 42

num_pieces <- 3 * rings * (rings - 1) + 1
piece_radius <- diameter / (rings * 4)
expected_puzzle_radius <- diameter / 2

cat("Parameters:\n")
cat("  rings:", rings, "\n")
cat("  diameter:", diameter, "\n")
cat("  num_pieces:", num_pieces, "\n")
cat("  piece_radius:", piece_radius, "\n")
cat("  expected_puzzle_radius:", expected_puzzle_radius, "\n\n")

# Collect all vertices to analyze
cat("Collecting all vertices...\n")
all_vertices <- list()
vertex_count <- 0

for (id in 1:num_pieces) {
  axial <- map_piece_id_to_axial(id, rings)
  cart <- axial_to_cartesian(axial$q, axial$r, piece_radius)

  for (v in 0:5) {
    vertex_angle <- v * pi / 3
    vx <- cart$x + piece_radius * cos(vertex_angle)
    vy <- cart$y + piece_radius * sin(vertex_angle)

    vertex_count <- vertex_count + 1
    all_vertices[[vertex_count]] <- list(
      piece_id = id,
      vertex = v,
      ring = axial$ring,
      x = vx,
      y = vy,
      dist = sqrt(vx^2 + vy^2)
    )
  }
}

cat("Total vertices:", vertex_count, "\n\n")

# Find boundary vertices (those at max distance from center)
# These should be at the outermost ring
cat("Analyzing boundary vertices:\n")
cat("------------------------------------------\n")

# Get unique vertex positions
unique_vertices <- list()
vertex_keys <- c()

for (v in all_vertices) {
  key <- sprintf("%.4f,%.4f", v$x, v$y)
  if (!(key %in% vertex_keys)) {
    vertex_keys <- c(vertex_keys, key)
    unique_vertices[[length(unique_vertices) + 1]] <- v
  }
}

cat("Unique vertices:", length(unique_vertices), "\n")

# Find max distance
max_dist <- max(sapply(unique_vertices, function(v) v$dist))
min_dist <- min(sapply(unique_vertices, function(v) v$dist))

cat("Distance range:", min_dist, "to", max_dist, "\n")
cat("Expected outer radius (vertices at corners):", piece_radius * (rings - 1) * sqrt(3) + piece_radius, "\n\n")

# Check vertices at different angles
cat("Checking vertices at cardinal directions:\n")
cat("------------------------------------------\n")

# Find vertices closest to 0, 90, 180, 270 degrees
for (angle_deg in c(0, 30, 60, 90, 120, 150, 180)) {
  angle_rad <- angle_deg * pi / 180

  # Find vertex closest to this angle and at max distance
  best_match <- NULL
  best_dist <- 0

  for (v in unique_vertices) {
    if (v$dist > max_dist * 0.9) {  # Only consider outer vertices
      v_angle <- atan2(v$y, v$x)
      if (v_angle < 0) v_angle <- v_angle + 2 * pi

      # Check angle difference
      angle_diff <- abs(v_angle - angle_rad)
      if (angle_diff > pi) angle_diff <- 2 * pi - angle_diff

      if (angle_diff < 0.1 && v$dist > best_dist) {
        best_dist <- v$dist
        best_match <- v
      }
    }
  }

  if (!is.null(best_match)) {
    cat(sprintf("Angle %3d deg: (%.2f, %.2f) dist=%.2f\n",
                angle_deg, best_match$x, best_match$y, best_match$dist))
  }
}

# Now test warp transformation on all boundary vertices
cat("\n\nWarp transformation analysis:\n")
cat("------------------------------------------\n")

boundary_vertices <- unique_vertices[sapply(unique_vertices, function(v) v$dist > max_dist * 0.9)]
cat("Boundary vertices (within 90% of max):", length(boundary_vertices), "\n\n")

# Check for inversions after warp
cat("Checking for inversions...\n")
inversion_count <- 0

for (v in boundary_vertices) {
  warped <- apply_hex_warp(v$x, v$y)
  warped_dist <- sqrt(warped$x^2 + warped$y^2)

  # Check if direction changed (which would indicate inversion)
  dot_product <- v$x * warped$x + v$y * warped$y

  if (dot_product < 0) {
    inversion_count <- inversion_count + 1
    cat(sprintf("INVERSION at (%.2f, %.2f): warped to (%.2f, %.2f)\n",
                v$x, v$y, warped$x, warped$y))
  }
}

if (inversion_count == 0) {
  cat("No inversions detected in warp transformation\n")
} else {
  cat(sprintf("\nWARNING: %d inversions detected!\n", inversion_count))
}

# Check the warp formula for edge cases
cat("\n\nWarp formula edge case analysis:\n")
cat("------------------------------------------\n")

# The leftmost vertex of a 7-ring puzzle
leftmost_x <- -max_dist
leftmost_y <- 0

cat("Testing leftmost point: (", leftmost_x, ", 0)\n")

# Manual calculation
angl <- atan2(0, leftmost_x) + pi  # = pi + pi = 2*pi (or 0)
cat("  atan2(0, ", leftmost_x, ") + pi =", angl, "(", angl * 180 / pi, "deg)\n")

angl60 <- angl %% (pi / 3)
cat("  angl %% (pi/3) =", angl60, "(", angl60 * 180 / pi, "deg)\n")

angl30 <- abs((pi / 6) - angl60)
cat("  abs(30deg - angl60) =", angl30, "(", angl30 * 180 / pi, "deg)\n")

l <- sqrt(0.75) / cos(angl30)
cat("  l = sqrt(0.75) / cos(angl30) =", l, "\n")

cat("  Warped x = x / l =", leftmost_x / l, "\n")

# Compare with actual function
warped <- apply_hex_warp(leftmost_x, 0)
cat("  apply_hex_warp result: (", warped$x, ", ", warped$y, ")\n")

# Test rightmost point
cat("\nTesting rightmost point: (", max_dist, ", 0)\n")
warped_right <- apply_hex_warp(max_dist, 0)
cat("  apply_hex_warp result: (", warped_right$x, ", ", warped_right$y, ")\n")

# Check circle_radius calculation in edge generation
cat("\n\nCircle radius calculation check:\n")
cat("------------------------------------------\n")

# Generate edge map and check the circle_radius
edge_data <- generate_hex_edge_map(
  rings = rings,
  seed = seed,
  diameter = diameter,
  do_warp = TRUE,
  do_trunc = TRUE,
  do_circular_border = TRUE
)

# Look at border edges to see what radius is being used
border_edges <- edge_data$piece_edge_map[grep("border", sapply(edge_data$piece_edge_map, function(e) e$type))]
cat("Number of border edges:", length(border_edges), "\n")

if (length(border_edges) > 0) {
  # Parse the first border edge arc command
  first_border <- border_edges[[1]]
  cat("\nFirst border edge:\n")
  cat("  forward:", first_border$forward, "\n")

  # Extract radius from arc command
  if (grepl("^A", first_border$forward)) {
    parts <- strsplit(first_border$forward, " ")[[1]]
    arc_rx <- as.numeric(parts[2])
    arc_ry <- as.numeric(parts[3])
    cat("  Arc radii: rx=", arc_rx, ", ry=", arc_ry, "\n")
    cat("  Expected diameter/2 =", expected_puzzle_radius, "\n")
  }
}

# Now generate a 3-ring puzzle to compare
cat("\n\n3-RING COMPARISON:\n")
cat("==================\n")

rings3 <- 3
diameter3 <- 240
piece_radius3 <- diameter3 / (rings3 * 4)

edge_data3 <- generate_hex_edge_map(
  rings = rings3,
  seed = seed,
  diameter = diameter3,
  do_warp = TRUE,
  do_trunc = TRUE,
  do_circular_border = TRUE
)

border_edges3 <- edge_data3$piece_edge_map[grep("border", sapply(edge_data3$piece_edge_map, function(e) e$type))]
cat("3-ring border edges:", length(border_edges3), "\n")

if (length(border_edges3) > 0) {
  first_border3 <- border_edges3[[1]]
  cat("First 3-ring border edge forward:", first_border3$forward, "\n")

  if (grepl("^A", first_border3$forward)) {
    parts <- strsplit(first_border3$forward, " ")[[1]]
    arc_rx <- as.numeric(parts[2])
    cat("3-ring arc radius:", arc_rx, "\n")
    cat("Expected 3-ring radius (diameter/2):", diameter3/2, "\n")
  }
}

cat("\n\nDone.\n")
