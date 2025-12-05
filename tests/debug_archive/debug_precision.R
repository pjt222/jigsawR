# Debug floating point precision issue

source("R/logging.R")
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  if (!grepl("scripts_archive", f)) source(f)
}

rings <- 7
piece_radius <- 400 / (4 * rings - 2)

cat("Piece radius:", piece_radius, "\n\n")

# Get exact vertices for pieces 36, 37, 60 at vertex (-76.92, 0)
pieces_to_check <- c(36, 37, 60)
vertex_indices <- c(3, 5, 1)  # Which vertex in each piece

for (idx in 1:3) {
  pid <- pieces_to_check[idx]
  vidx <- vertex_indices[idx]
  
  axial_coords <- map_piece_id_to_axial(pid, rings)
  cart_coords <- axial_to_cartesian(
    q = axial_coords$q,
    r = axial_coords$r,
    hex_size = piece_radius
  )
  
  i <- vidx - 1  # 0-indexed
  vx <- cart_coords$x + piece_radius * cos(i * pi / 3)
  vy <- cart_coords$y + piece_radius * sin(i * pi / 3)
  
  cat("Piece", pid, "vertex", vidx, ":\n")
  cat("  Center: (", sprintf("%.10f", cart_coords$x), ",", sprintf("%.10f", cart_coords$y), ")\n")
  cat("  Vertex: (", sprintf("%.10f", vx), ",", sprintf("%.10f", vy), ")\n")
  cat("  Key (%.1f): ", sprintf("%.1f,%.1f", vx, vy), "\n\n")
}

# Also check vertex 2 of piece 36 and vertex 6 of piece 37
cat("=== Vertex at approximately (-61.54, 0) ===\n")
for (pid in c(36, 37)) {
  vidx <- if (pid == 36) 2 else 6
  
  axial_coords <- map_piece_id_to_axial(pid, rings)
  cart_coords <- axial_to_cartesian(
    q = axial_coords$q,
    r = axial_coords$r,
    hex_size = piece_radius
  )
  
  i <- vidx - 1
  vx <- cart_coords$x + piece_radius * cos(i * pi / 3)
  vy <- cart_coords$y + piece_radius * sin(i * pi / 3)
  
  cat("Piece", pid, "vertex", vidx, ":\n")
  cat("  Vertex: (", sprintf("%.10f", vx), ",", sprintf("%.10f", vy), ")\n")
  cat("  Key (%.1f): ", sprintf("%.1f,%.1f", vx, vy), "\n\n")
}

# Now check what piece 19 has at vertex near (-61.54, 0)
cat("=== Piece 19 vertices ===\n")
pid <- 19
axial_coords <- map_piece_id_to_axial(pid, rings)
cart_coords <- axial_to_cartesian(
  q = axial_coords$q,
  r = axial_coords$r,
  hex_size = piece_radius
)
cat("Piece 19 center: (", sprintf("%.10f", cart_coords$x), ",", sprintf("%.10f", cart_coords$y), ")\n")
for (i in 0:5) {
  vx <- cart_coords$x + piece_radius * cos(i * pi / 3)
  vy <- cart_coords$y + piece_radius * sin(i * pi / 3)
  cat("  V", i+1, ": (", sprintf("%.10f", vx), ",", sprintf("%.10f", vy), ")")
  cat(" Key: ", sprintf("%.1f,%.1f", vx, vy), "\n")
}
