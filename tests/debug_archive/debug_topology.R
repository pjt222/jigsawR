# Debug topology - understand why vertex -76.9,0 is only used by piece 36

source("R/logging.R")
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  if (!grepl("scripts_archive", f)) source(f)
}

rings <- 7
piece_radius <- 400 / (4 * rings - 2)

# Look at all pieces that should be near vertex (-76.92, 0)
# This is the vertex 3 of piece 36

target_x <- -76.92
target_y <- 0

cat("=== Looking for pieces near (", target_x, ",", target_y, ") ===\n\n")

num_pieces <- 3 * rings * (rings - 1) + 1

for (piece_id in 1:num_pieces) {
  axial_coords <- map_piece_id_to_axial(piece_id, rings)
  cart_coords <- axial_to_cartesian(
    q = axial_coords$q,
    r = axial_coords$r,
    hex_size = piece_radius
  )
  
  # Check if center is within 2*piece_radius of target
  dist <- sqrt((cart_coords$x - target_x)^2 + (cart_coords$y - target_y)^2)
  
  if (dist < 2.5 * piece_radius) {
    cat("Piece", piece_id, ": center at (", round(cart_coords$x, 2), ",", round(cart_coords$y, 2), ")")
    cat(", ring", axial_coords$ring, "\n")
    
    # Check vertices
    for (i in 0:5) {
      vx <- cart_coords$x + piece_radius * cos(i * pi / 3)
      vy <- cart_coords$y + piece_radius * sin(i * pi / 3)
      vdist <- sqrt((vx - target_x)^2 + (vy - target_y)^2)
      if (vdist < 1) {
        cat("  -> Vertex", i+1, "at (", round(vx, 2), ",", round(vy, 2), ") MATCHES target!\n")
      }
    }
  }
}

# Also check what pieces are in ring 3 and 4 near the negative x-axis
cat("\n=== Pieces on negative x-axis (y near 0) ===\n")
for (piece_id in 1:num_pieces) {
  axial_coords <- map_piece_id_to_axial(piece_id, rings)
  cart_coords <- axial_to_cartesian(
    q = axial_coords$q,
    r = axial_coords$r,
    hex_size = piece_radius
  )
  
  if (abs(cart_coords$y) < 20 && cart_coords$x < -30) {
    cat("Piece", piece_id, ": center (", round(cart_coords$x, 2), ",", round(cart_coords$y, 2), ")")
    cat(", ring", axial_coords$ring, ", axial (q=", axial_coords$q, ", r=", axial_coords$r, ")\n")
  }
}

# Check specifically piece 60 (mentioned in the problem list)
cat("\n=== Piece 60 details ===\n")
p60_axial <- map_piece_id_to_axial(60, rings)
p60_cart <- axial_to_cartesian(p60_axial$q, p60_axial$r, piece_radius)
cat("Piece 60: center (", round(p60_cart$x, 2), ",", round(p60_cart$y, 2), ")")
cat(", ring", p60_axial$ring, ", axial (q=", p60_axial$q, ", r=", p60_axial$r, ")\n")
cat("Vertices:\n")
for (i in 0:5) {
  vx <- p60_cart$x + piece_radius * cos(i * pi / 3)
  vy <- p60_cart$y + piece_radius * sin(i * pi / 3)
  cat("  V", i+1, ": (", round(vx, 2), ",", round(vy, 2), ")\n")
}
