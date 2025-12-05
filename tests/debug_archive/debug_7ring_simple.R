# Simple 7-ring hexagonal puzzle debug

# Set working directory
setwd("D:/dev/p/jigsawR")

library(devtools)
load_all()

cat("Testing 7-ring puzzle generation\n")
cat("================================\n\n")

rings <- 7
diameter <- 400
seed <- 42

# Calculate values
num_pieces <- 3 * rings * (rings - 1) + 1
piece_radius <- diameter / (rings * 4)

cat("Parameters:\n")
cat("  rings:", rings, "\n")
cat("  diameter:", diameter, "\n")
cat("  num_pieces:", num_pieces, "\n")
cat("  piece_radius:", piece_radius, "\n\n")

# Check a few key pieces
cat("Checking extremal pieces...\n")

# Leftmost piece should be at q = -(rings-1), r = 0
leftmost_axial <- map_piece_id_to_axial(1, rings)
cat("Center piece: q=", leftmost_axial$q, ", r=", leftmost_axial$r, "\n")

# Find the actual leftmost piece
cat("\nSearching for extremal positions...\n")
min_x <- Inf
max_x <- -Inf
leftmost_id <- 0
rightmost_id <- 0

for (id in 1:num_pieces) {
  axial <- map_piece_id_to_axial(id, rings)
  cart <- axial_to_cartesian(axial$q, axial$r, piece_radius)

  if (cart$x < min_x) {
    min_x <- cart$x
    leftmost_id <- id
  }
  if (cart$x > max_x) {
    max_x <- cart$x
    rightmost_id <- id
  }
}

cat("Leftmost piece:", leftmost_id, "at x =", min_x, "\n")
cat("Rightmost piece:", rightmost_id, "at x =", max_x, "\n")

# Get their axial coordinates
left_axial <- map_piece_id_to_axial(leftmost_id, rings)
right_axial <- map_piece_id_to_axial(rightmost_id, rings)

cat("\nLeftmost: q=", left_axial$q, ", r=", left_axial$r, ", ring=", left_axial$ring, "\n")
cat("Rightmost: q=", right_axial$q, ", r=", right_axial$r, ", ring=", right_axial$ring, "\n")

# Now test warp on these positions
cat("\n\nWarp transformation check:\n")
cat("---------------------------\n")

# Calculate vertex positions for leftmost piece
left_cart <- axial_to_cartesian(left_axial$q, left_axial$r, piece_radius)
right_cart <- axial_to_cartesian(right_axial$q, right_axial$r, piece_radius)

# Get the leftmost vertex of the leftmost piece
left_vertex_x <- left_cart$x - piece_radius  # vertex 3 at 180 degrees
left_vertex_y <- left_cart$y

cat("Leftmost vertex: (", left_vertex_x, ", ", left_vertex_y, ")\n")
cat("Distance from origin:", sqrt(left_vertex_x^2 + left_vertex_y^2), "\n")

# Apply warp
warped <- apply_hex_warp(left_vertex_x, left_vertex_y)
cat("After warp: (", warped$x, ", ", warped$y, ")\n")
cat("Warped distance:", sqrt(warped$x^2 + warped$y^2), "\n")

# Generate the puzzle
cat("\n\nGenerating puzzle...\n")
result <- tryCatch({
  generate_puzzle(
    type = "hexagonal",
    grid = c(rings),
    size = c(diameter),
    seed = seed,
    offset = 0,
    do_warp = TRUE,
    do_trunc = TRUE
  )
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
  NULL
})

if (!is.null(result)) {
  cat("Success! Canvas size:", result$canvas_size[1], "x", result$canvas_size[2], "\n")

  output_file <- "output/debug_7ring_warp.svg"
  writeLines(result$svg_content, output_file)
  cat("Saved to:", output_file, "\n")
} else {
  cat("Failed to generate puzzle\n")
}

cat("\nDone.\n")
