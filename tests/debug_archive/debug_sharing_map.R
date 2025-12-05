# Debug: Trace through the actual vertex sharing map creation

source("R/hexagonal_edge_generation_fixed.R")
source("R/hexagonal_topology.R")

rings <- 7
diameter <- 400
piece_radius <- diameter / (4 * rings - 2)
num_pieces <- 3 * rings * (rings - 1) + 1

cat("Using make_vertex_key for keys\n\n")

# Calculate original vertices (same as in generate_hex_edge_map)
piece_vertices_original <- list()
base_offset <- 0

for (piece_id in 1:num_pieces) {
  axial_coords <- map_piece_id_to_axial(piece_id, rings)
  cart_coords <- axial_to_cartesian(
    q = axial_coords$q,
    r = axial_coords$r,
    hex_size = piece_radius
  )
  
  vertices <- list()
  for (i in 0:5) {
    vertex_angle <- i * pi / 3 + base_offset
    vx <- cart_coords$x + piece_radius * cos(vertex_angle)
    vy <- cart_coords$y + piece_radius * sin(vertex_angle)
    vertices[[i + 1]] <- c(vx, vy)
  }
  piece_vertices_original[[piece_id]] <- vertices
}

# Build vertex sharing map with make_vertex_key
vertex_sharing <- list()

for (piece_id in 1:num_pieces) {
  for (i in 1:6) {
    v <- piece_vertices_original[[piece_id]][[i]]
    v_key <- make_vertex_key(v[1], v[2])
    
    if (is.null(vertex_sharing[[v_key]])) {
      vertex_sharing[[v_key]] <- list(pieces = c(), coords = v)
    }
    vertex_sharing[[v_key]]$pieces <- c(vertex_sharing[[v_key]]$pieces, piece_id)
  }
}

# Check for the specific vertex (-76.92, 0) that should be shared by 3 pieces
target_key <- make_vertex_key(-76.92, 0)
cat("Target key:", target_key, "\n")
cat("Pieces sharing this key:", paste(unique(vertex_sharing[[target_key]]$pieces), collapse=", "), "\n\n")

# Also check with slightly different values due to floating point
cat("All keys containing -76.9:\n")
for (k in names(vertex_sharing)) {
  if (grepl("-76.9", k)) {
    cat("  ", k, ": pieces", paste(unique(vertex_sharing[[k]]$pieces), collapse=", "), "\n")
  }
}

# Check piece 36 vertex 3 specifically
v36_3 <- piece_vertices_original[[36]][[3]]
k36_3 <- make_vertex_key(v36_3[1], v36_3[2])
cat("\nPiece 36 vertex 3: (", v36_3[1], ",", v36_3[2], ")\n")
cat("Key:", k36_3, "\n")
cat("Sharing:", paste(unique(vertex_sharing[[k36_3]]$pieces), collapse=", "), "\n")

# Check piece 37 vertex 5
v37_5 <- piece_vertices_original[[37]][[5]]
k37_5 <- make_vertex_key(v37_5[1], v37_5[2])
cat("\nPiece 37 vertex 5: (", v37_5[1], ",", v37_5[2], ")\n")
cat("Key:", k37_5, "\n")
cat("Keys equal?", k36_3 == k37_5, "\n")

# Check piece 60 vertex 1
v60_1 <- piece_vertices_original[[60]][[1]]
k60_1 <- make_vertex_key(v60_1[1], v60_1[2])
cat("\nPiece 60 vertex 1: (", v60_1[1], ",", v60_1[2], ")\n")
cat("Key:", k60_1, "\n")
cat("Keys equal to 36-3?", k36_3 == k60_1, "\n")
