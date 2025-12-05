# Debug 7-ring hexagonal puzzle issues
# Looking for hardcoded values affecting larger puzzles

library(devtools)
load_all()

sep_line <- paste(rep("=", 60), collapse = "")
dash_line <- paste(rep("-", 40), collapse = "")

cat(sep_line, "\n")
cat("DEBUGGING 7-RING HEXAGONAL PUZZLE\n")
cat(sep_line, "\n\n")

rings <- 7
diameter <- 400
seed <- 42

# Calculate expected values
num_pieces <- 3 * rings * (rings - 1) + 1
piece_radius <- diameter / (rings * 4)
expected_puzzle_radius <- diameter / 2

cat("Parameters:\n")
cat(sprintf("  rings: %d\n", rings))
cat(sprintf("  diameter: %.2f\n", diameter))
cat(sprintf("  num_pieces: %d\n", num_pieces))
cat(sprintf("  piece_radius: %.4f\n", piece_radius))
cat(sprintf("  expected_puzzle_radius: %.2f\n\n", expected_puzzle_radius))

# Check piece positions
cat("Piece positions analysis:\n")
cat(dash_line, "\n")

# Find the extremal pieces (leftmost and rightmost)
all_positions <- list()
for (piece_id in 1:num_pieces) {
  axial <- map_piece_id_to_axial(piece_id, rings)
  cart <- axial_to_cartesian(axial$q, axial$r, piece_radius)
  all_positions[[piece_id]] <- list(
    id = piece_id,
    q = axial$q,
    r = axial$r,
    ring = axial$ring,
    x = cart$x,
    y = cart$y,
    dist = sqrt(cart$x^2 + cart$y^2)
  )
}

# Convert to data frame for analysis
df <- do.call(rbind, lapply(all_positions, function(p) {
  data.frame(id = p$id, q = p$q, r = p$r, ring = p$ring,
             x = p$x, y = p$y, dist = p$dist)
}))

# Find extremal pieces
leftmost <- df[which.min(df$x), ]
rightmost <- df[which.max(df$x), ]
topmost <- df[which.max(df$y), ]
bottommost <- df[which.min(df$y), ]
furthest <- df[which.max(df$dist), ]

cat("\nExtremal piece centers:\n")
cat(sprintf("  Leftmost:  piece %d (q=%d, r=%d) at (%.2f, %.2f) dist=%.2f\n",
            leftmost$id, leftmost$q, leftmost$r, leftmost$x, leftmost$y, leftmost$dist))
cat(sprintf("  Rightmost: piece %d (q=%d, r=%d) at (%.2f, %.2f) dist=%.2f\n",
            rightmost$id, rightmost$q, rightmost$r, rightmost$x, rightmost$y, rightmost$dist))
cat(sprintf("  Topmost:   piece %d (q=%d, r=%d) at (%.2f, %.2f) dist=%.2f\n",
            topmost$id, topmost$q, topmost$r, topmost$x, topmost$y, topmost$dist))
cat(sprintf("  Bottommost: piece %d (q=%d, r=%d) at (%.2f, %.2f) dist=%.2f\n",
            bottommost$id, bottommost$q, bottommost$r, bottommost$x, bottommost$y, bottommost$dist))
cat(sprintf("  Furthest:  piece %d (q=%d, r=%d) at (%.2f, %.2f) dist=%.2f\n",
            furthest$id, furthest$q, furthest$r, furthest$x, furthest$y, furthest$dist))

# Calculate actual boundary extent
# For each outer ring piece, calculate vertex positions
cat("\n\nBoundary vertex analysis:\n")
cat(dash_line, "\n")

outer_ring_pieces <- df[df$ring == rings - 1, ]
cat(sprintf("Outer ring (ring %d) has %d pieces\n", rings - 1, nrow(outer_ring_pieces)))

# Calculate all vertices for outer ring pieces
all_boundary_vertices <- list()
base_offset <- 0  # Flat-top hexagon

for (i in 1:nrow(outer_ring_pieces)) {
  p <- outer_ring_pieces[i, ]
  for (v in 0:5) {
    vertex_angle <- v * pi / 3 + base_offset
    vx <- p$x + piece_radius * cos(vertex_angle)
    vy <- p$y + piece_radius * sin(vertex_angle)
    all_boundary_vertices[[length(all_boundary_vertices) + 1]] <- list(
      piece_id = p$id,
      vertex = v,
      x = vx,
      y = vy,
      dist = sqrt(vx^2 + vy^2)
    )
  }
}

# Find extremal vertices
vdf <- do.call(rbind, lapply(all_boundary_vertices, function(v) {
  data.frame(piece_id = v$piece_id, vertex = v$vertex,
             x = v$x, y = v$y, dist = v$dist)
}))

cat(sprintf("\nVertex distance range: %.2f to %.2f\n", min(vdf$dist), max(vdf$dist)))
cat(sprintf("Expected puzzle radius: %.2f\n", expected_puzzle_radius))

# Check leftmost and rightmost vertices
leftmost_v <- vdf[which.min(vdf$x), ]
rightmost_v <- vdf[which.max(vdf$x), ]
cat(sprintf("\nLeftmost vertex: piece %d, vertex %d at (%.2f, %.2f)\n",
            leftmost_v$piece_id, leftmost_v$vertex, leftmost_v$x, leftmost_v$y))
cat(sprintf("Rightmost vertex: piece %d, vertex %d at (%.2f, %.2f)\n",
            rightmost_v$piece_id, rightmost_v$vertex, rightmost_v$x, rightmost_v$y))

# Now test warp transformation on these vertices
cat("\n\nWarp transformation analysis:\n")
cat(dash_line, "\n")

# Test warp on extremal vertices
test_vertices <- list(
  list(name = "Leftmost", x = leftmost_v$x, y = leftmost_v$y),
  list(name = "Rightmost", x = rightmost_v$x, y = rightmost_v$y),
  list(name = "Top", x = vdf[which.max(vdf$y), ]$x, y = vdf[which.max(vdf$y), ]$y),
  list(name = "Bottom", x = vdf[which.min(vdf$y), ]$x, y = vdf[which.min(vdf$y), ]$y)
)

for (tv in test_vertices) {
  warped <- apply_hex_warp(tv$x, tv$y)
  orig_dist <- sqrt(tv$x^2 + tv$y^2)
  warped_dist <- sqrt(warped$x^2 + warped$y^2)

  cat(sprintf("\n%s vertex:\n", tv$name))
  cat(sprintf("  Original: (%.2f, %.2f) dist=%.2f\n", tv$x, tv$y, orig_dist))
  cat(sprintf("  Warped:   (%.2f, %.2f) dist=%.2f\n", warped$x, warped$y, warped_dist))
  cat(sprintf("  Ratio:    %.4f (dist_warped / dist_orig)\n", warped_dist / orig_dist))
}

# Check if there are any hardcoded 3-ring values in the warp calculation
cat("\n\nChecking for hardcoded values:\n")
cat(dash_line, "\n")

# The warp formula uses sqrt(0.75) which is intrinsic to hexagonal geometry
# Let's verify the formula with explicit values
cat("\napply_hex_warp formula check:\n")
cat("Formula: l = sqrt(0.75) / cos(angl30)\n")
cat("Warped: (x/l, y/l)\n\n")

# Test at different angles to verify the transformation
test_angles <- c(0, 30, 60, 90) * pi / 180
test_radius <- 100

for (ang in test_angles) {
  x <- test_radius * cos(ang)
  y <- test_radius * sin(ang)

  # Apply the warp formula manually
  angl <- atan2(y, x) + pi
  angl60 <- angl %% (pi / 3)
  angl30 <- abs((pi / 6) - angl60)
  l <- sqrt(0.75) / cos(angl30)

  warped <- apply_hex_warp(x, y)

  cat(sprintf("Angle %.0f°: l=%.4f, orig_dist=%.2f, warped_dist=%.2f\n",
              ang * 180 / pi, l, test_radius, sqrt(warped$x^2 + warped$y^2)))
}

# Now generate the actual puzzle and check for issues
cat("\n\n")
cat(sep_line, "\n")
cat("GENERATING 7-RING PUZZLE WITH do_warp=TRUE\n")
cat(sep_line, "\n\n")

# Generate edge map to see actual transformations
edge_data <- generate_hex_edge_map(
  rings = rings,
  seed = seed,
  diameter = diameter,
  do_warp = TRUE,
  do_trunc = TRUE,
  do_circular_border = TRUE
)

cat(sprintf("Generated %d unique edges\n\n", edge_data$num_edges))

# Generate the complete puzzle
result <- generate_puzzle(
  type = "hexagonal",
  grid = c(rings),
  size = c(diameter),
  seed = seed,
  offset = 0,
  do_warp = TRUE,
  do_trunc = TRUE
)

cat("\nPuzzle generated successfully!\n")
cat(sprintf("Canvas size: %.2f x %.2f\n", result$canvas_size[1], result$canvas_size[2]))

# Save for visual inspection
output_file <- "output/debug_7ring_warp.svg"
writeLines(result$svg_content, output_file)
cat(sprintf("\nSaved to: %s\n", output_file))

cat("\n")
cat(sep_line, "\n")
cat("ANALYSIS COMPLETE\n")
cat(sep_line, "\n")
