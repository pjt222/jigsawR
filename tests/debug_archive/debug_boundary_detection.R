# Debug: Why are piece 36's vertices being marked as boundary vertices?

source("R/logging.R")
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  if (!grepl("scripts_archive", f)) source(f)
}

rings <- 7
num_pieces <- 3 * rings * (rings - 1) + 1
piece_radius <- 400 / (4 * rings - 2)

# Calculate original vertices
piece_vertices_original <- list()

for (piece_id in 1:num_pieces) {
  axial_coords <- map_piece_id_to_axial(piece_id, rings)
  cart_coords <- axial_to_cartesian(
    q = axial_coords$q,
    r = axial_coords$r,
    hex_size = piece_radius
  )

  vertices <- list()
  for (i in 0:5) {
    vertex_angle <- i * pi / 3
    vx <- cart_coords$x + piece_radius * cos(vertex_angle)
    vy <- cart_coords$y + piece_radius * sin(vertex_angle)
    vertices[[i + 1]] <- c(vx, vy)
  }
  piece_vertices_original[[piece_id]] <- vertices
}

# Build vertex sharing map
vertex_sharing <- list()

for (piece_id in 1:num_pieces) {
  for (i in 1:6) {
    v <- piece_vertices_original[[piece_id]][[i]]
    v_key <- sprintf("%.1f,%.1f", v[1], v[2])

    if (is.null(vertex_sharing[[v_key]])) {
      vertex_sharing[[v_key]] <- list(pieces = c(), coords = v)
    }
    vertex_sharing[[v_key]]$pieces <- c(vertex_sharing[[v_key]]$pieces, piece_id)
  }
}

# Find boundary vertices (shared by < 3 pieces)
boundary_vertex_keys <- c()
for (v_key in names(vertex_sharing)) {
  if (length(unique(vertex_sharing[[v_key]]$pieces)) < 3) {
    boundary_vertex_keys <- c(boundary_vertex_keys, v_key)
  }
}

cat("Total boundary vertices:", length(boundary_vertex_keys), "\n\n")

# Check piece 36's vertices
cat("=== Piece 36 vertex analysis ===\n")
piece_id <- 36
info <- map_piece_id_to_axial(piece_id, rings)
cat("Piece 36 is in ring", info$ring, ", position", info$position, "\n\n")

for (i in 1:6) {
  v <- piece_vertices_original[[piece_id]][[i]]
  v_key <- sprintf("%.1f,%.1f", v[1], v[2])
  sharing_count <- length(unique(vertex_sharing[[v_key]]$pieces))
  is_boundary <- v_key %in% boundary_vertex_keys

  cat("Vertex", i, ": (", round(v[1], 2), ",", round(v[2], 2), ")\n")
  cat("  Key:", v_key, "\n")
  cat("  Shared by", sharing_count, "pieces:", paste(unique(vertex_sharing[[v_key]]$pieces), collapse=", "), "\n")
  cat("  Is boundary?", is_boundary, "\n\n")
}

# Check distances
cat("\n=== Distance check ===\n")
max_dist <- 0
for (piece_id in 1:num_pieces) {
  for (i in 1:6) {
    v <- piece_vertices_original[[piece_id]][[i]]
    d <- sqrt(v[1]^2 + v[2]^2)
    max_dist <- max(max_dist, d)
  }
}
cat("Max vertex distance from origin:", round(max_dist, 2), "\n")

for (i in 1:6) {
  v <- piece_vertices_original[[36]][[i]]
  d <- sqrt(v[1]^2 + v[2]^2)
  cat("Vertex", i, "distance:", round(d, 2), "\n")
}
