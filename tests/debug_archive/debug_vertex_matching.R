# Debug vertex matching issue
# The problem: with do_trunc=TRUE, multiple boundary vertices get projected
# to the same location on the hexagon border, causing false edge matches

source("R/logging.R")
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  if (!grepl("scripts_archive", f)) source(f)
}

# Focus on the edge generation step
rings <- 7
seed <- 1234
diameter <- 400
tabsize <- 20
jitter <- 4
do_warp <- FALSE
do_trunc <- TRUE

num_pieces <- 3 * rings * (rings - 1) + 1
piece_radius <- diameter / (4 * rings - 2)

cat("=== Configuration ===\n")
cat("Rings:", rings, "\n")
cat("Pieces:", num_pieces, "\n")
cat("Piece radius:", round(piece_radius, 2), "\n")
cat("do_warp:", do_warp, "\n")
cat("do_trunc:", do_trunc, "\n\n")

# Step 1: Calculate ORIGINAL vertices for all pieces
cat("=== Step 1: Calculate original vertices ===\n")
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

# Step 2: Build vertex sharing map (original)
cat("=== Step 2: Build vertex sharing map ===\n")
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

cat("Unique vertex positions (original):", length(vertex_sharing), "\n")

# Step 3: Find boundary vertices
boundary_vertex_keys <- c()
max_boundary_dist <- 0

for (v_key in names(vertex_sharing)) {
  if (length(unique(vertex_sharing[[v_key]]$pieces)) < 3) {
    boundary_vertex_keys <- c(boundary_vertex_keys, v_key)
    v <- vertex_sharing[[v_key]]$coords
    dist <- sqrt(v[1]^2 + v[2]^2)
    max_boundary_dist <- max(max_boundary_dist, dist)
  }
}

cat("Boundary vertices (original):", length(boundary_vertex_keys), "\n")
cat("Max boundary distance:", round(max_boundary_dist, 2), "\n\n")

# Step 4: Apply hexagonal truncation (do_trunc without do_warp)
cat("=== Step 3: Apply hexagonal truncation ===\n")
piece_vertices <- piece_vertices_original  # Copy

# Apply truncation to boundary vertices only
for (v_key in boundary_vertex_keys) {
  v <- vertex_sharing[[v_key]]$coords
  transformed <- apply_hex_trunc(v[1], v[2], max_boundary_dist)

  # Update vertex in all pieces that share it
  for (piece_id in 1:num_pieces) {
    for (i in 1:6) {
      orig_v <- piece_vertices_original[[piece_id]][[i]]
      orig_key <- sprintf("%.1f,%.1f", orig_v[1], orig_v[2])

      if (orig_key == v_key) {
        piece_vertices[[piece_id]][[i]] <- c(transformed$x, transformed$y)
      }
    }
  }
}

# Step 5: Build NEW vertex sharing map AFTER transformation
cat("=== Step 4: Vertex sharing AFTER truncation ===\n")
vertex_sharing_new <- list()

for (piece_id in 1:num_pieces) {
  for (i in 1:6) {
    v <- piece_vertices[[piece_id]][[i]]
    v_key <- sprintf("%.1f,%.1f", v[1], v[2])

    if (is.null(vertex_sharing_new[[v_key]])) {
      vertex_sharing_new[[v_key]] <- list(pieces = c(), coords = v)
    }
    vertex_sharing_new[[v_key]]$pieces <- c(vertex_sharing_new[[v_key]]$pieces, piece_id)
  }
}

cat("Unique vertex positions (after truncation):", length(vertex_sharing_new), "\n\n")

# Step 6: Find vertices that are now shared by MORE pieces than before
cat("=== Step 5: Find problematic vertices (collision after truncation) ===\n")
problematic_vertices <- list()

for (v_key in names(vertex_sharing_new)) {
  pieces_now <- unique(vertex_sharing_new[[v_key]]$pieces)

  # Check how many pieces shared this position ORIGINALLY
  # This is tricky - we need to find original vertices that mapped here
  original_pieces <- c()
  for (orig_key in names(vertex_sharing)) {
    orig_v <- vertex_sharing[[orig_key]]$coords
    # Apply truncation to see where it goes
    if (orig_key %in% boundary_vertex_keys) {
      trans <- apply_hex_trunc(orig_v[1], orig_v[2], max_boundary_dist)
      trans_key <- sprintf("%.1f,%.1f", trans$x, trans$y)
    } else {
      trans_key <- orig_key
    }

    if (trans_key == v_key) {
      original_pieces <- c(original_pieces, vertex_sharing[[orig_key]]$pieces)
    }
  }

  # If more distinct original vertices now map to the same position
  if (length(pieces_now) > 3) {
    v <- vertex_sharing_new[[v_key]]$coords
    problematic_vertices[[v_key]] <- list(
      coords = v,
      pieces = pieces_now,
      count = length(pieces_now)
    )
  }
}

cat("Problematic vertices (>3 pieces sharing):", length(problematic_vertices), "\n\n")

if (length(problematic_vertices) > 0) {
  cat("Vertex positions with too many pieces:\n")
  for (v_key in names(problematic_vertices)) {
    p <- problematic_vertices[[v_key]]
    cat("  ", v_key, ": shared by pieces", paste(p$pieces, collapse=", "), "\n")
  }
}

# Step 7: Look specifically at piece 36 and its neighbors
cat("\n=== Step 6: Analyze piece 36 and neighbors ===\n")
piece_id <- 36

cat("Piece 36 vertices (after truncation):\n")
for (i in 1:6) {
  v <- piece_vertices[[piece_id]][[i]]
  orig_v <- piece_vertices_original[[piece_id]][[i]]
  cat("  Vertex", i, ": (", round(v[1], 2), ",", round(v[2], 2), ")")
  if (!all(abs(v - orig_v) < 0.01)) {
    cat(" <- TRANSFORMED from (", round(orig_v[1], 2), ",", round(orig_v[2], 2), ")")
  }
  cat("\n")
}

# Find which pieces share each vertex of piece 36
cat("\nVertex sharing for piece 36:\n")
for (i in 1:6) {
  v <- piece_vertices[[piece_id]][[i]]
  v_key <- sprintf("%.1f,%.1f", v[1], v[2])
  sharing_pieces <- unique(vertex_sharing_new[[v_key]]$pieces)
  cat("  Vertex", i, "(", v_key, "): shared by pieces", paste(sharing_pieces, collapse=", "), "\n")
}

# Look at edge 0 of piece 36 (first edge in path)
cat("\n=== Step 7: Analyze piece 36 edge 0 ===\n")
v1 <- piece_vertices[[piece_id]][[1]]
v2 <- piece_vertices[[piece_id]][[2]]
cat("Edge 0: from (", round(v1[1], 2), ",", round(v1[2], 2), ") to (",
    round(v2[1], 2), ",", round(v2[2], 2), ")\n")

# Find all pieces that share BOTH v1 AND v2
v1_key <- sprintf("%.1f,%.1f", v1[1], v1[2])
v2_key <- sprintf("%.1f,%.1f", v2[1], v2[2])

v1_pieces <- unique(vertex_sharing_new[[v1_key]]$pieces)
v2_pieces <- unique(vertex_sharing_new[[v2_key]]$pieces)
shared_both <- intersect(v1_pieces, v2_pieces)

cat("Pieces sharing v1:", paste(v1_pieces, collapse=", "), "\n")
cat("Pieces sharing v2:", paste(v2_pieces, collapse=", "), "\n")
cat("Pieces sharing BOTH:", paste(shared_both, collapse=", "), "\n")

if (length(shared_both) > 2) {
  cat("\n*** BUG: More than 2 pieces share this edge! ***\n")
  cat("This causes the edge-matching algorithm to match interior edges with boundary edges!\n")
}
