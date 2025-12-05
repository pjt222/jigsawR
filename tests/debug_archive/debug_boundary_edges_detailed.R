# Detailed debug of boundary edge detection in generate_hex_edge_map

cat("=" , rep("=", 70), "\n", sep = "")
cat("Debug: Boundary Edge Detection in generate_hex_edge_map\n")
cat("=" , rep("=", 70), "\n\n", sep = "")

source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")

rings <- 3
diameter <- 240
seed <- 1234
num_pieces <- 3 * rings * (rings - 1) + 1
piece_radius <- diameter / (rings * 4)
tabsize <- 20
jitter <- 4

# Replicate the code from generate_hex_edge_map
do_warp <- TRUE
do_trunc <- TRUE

# Step 1: Calculate vertices for all pieces (original coordinates)
piece_vertices_original <- list()
base_offset <- 0

for (piece_id in 1:num_pieces) {
  axial_coords <- map_piece_id_to_axial(piece_id, rings)
  hex_size <- piece_radius

  cart_coords <- axial_to_cartesian(
    q = axial_coords$q,
    r = axial_coords$r,
    hex_size = hex_size
  )
  center_x <- cart_coords$x
  center_y <- cart_coords$y

  vertices <- list()
  for (i in 0:5) {
    vertex_angle <- i * pi / 3 + base_offset
    vx <- center_x + piece_radius * cos(vertex_angle)
    vy <- center_y + piece_radius * sin(vertex_angle)
    vertices[[i + 1]] <- c(vx, vy)
  }
  piece_vertices_original[[piece_id]] <- vertices
}

cat(sprintf("Generated vertices for %d pieces\n\n", num_pieces))

# Copy for transformation
piece_vertices <- piece_vertices_original

# Pre-identified boundary edges
boundary_edge_keys <- c()

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

cat(sprintf("Found %d unique vertices\n", length(vertex_sharing)))

# Find boundary vertices and calculate max distance
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

cat(sprintf("Found %d boundary vertices\n\n", length(boundary_vertex_keys)))

# Pre-identify boundary edges BEFORE any vertex transformations
for (piece_id in 1:num_pieces) {
  for (side in 0:5) {
    v1 <- piece_vertices_original[[piece_id]][[side + 1]]
    v2 <- piece_vertices_original[[piece_id]][[(side + 1) %% 6 + 1]]
    v1_key <- sprintf("%.1f,%.1f", v1[1], v1[2])
    v2_key <- sprintf("%.1f,%.1f", v2[1], v2[2])

    # An edge is a boundary edge if BOTH its vertices are boundary vertices
    if (v1_key %in% boundary_vertex_keys && v2_key %in% boundary_vertex_keys) {
      # Also check that no other piece shares this exact edge
      v1_pieces <- unique(vertex_sharing[[v1_key]]$pieces)
      v2_pieces <- unique(vertex_sharing[[v2_key]]$pieces)
      shared_pieces <- intersect(v1_pieces, v2_pieces)

      # If only this piece uses both vertices, it's a boundary edge
      if (length(shared_pieces) == 1) {
        boundary_edge_keys <- c(boundary_edge_keys, sprintf("%d-%d", piece_id, side))
      }
    }
  }
}

cat(sprintf("Pre-identified %d boundary edges BEFORE transformations\n\n", length(boundary_edge_keys)))
cat("Boundary edge keys:\n")
cat(paste(boundary_edge_keys, collapse=", "), "\n\n")

# Now apply warp transformation
if (do_warp) {
  all_transformed <- list()

  for (v_key in names(vertex_sharing)) {
    v <- vertex_sharing[[v_key]]$coords
    transformed <- apply_hex_warp(v[1], v[2])
    all_transformed[[v_key]] <- c(transformed$x, transformed$y)
  }

  # Update all piece vertices with warped coordinates
  for (piece_id in 1:num_pieces) {
    for (i in 1:6) {
      v <- piece_vertices_original[[piece_id]][[i]]
      v_key <- sprintf("%.1f,%.1f", v[1], v[2])

      if (!is.null(all_transformed[[v_key]])) {
        piece_vertices[[piece_id]][[i]] <- all_transformed[[v_key]]
      }
    }
  }

  cat("Circular warp enabled - ALL vertices transformed\n\n")

  # When do_trunc is also enabled, project boundary vertices to circle
  if (do_trunc) {
    warped_boundary_dists <- c()
    for (v_key in boundary_vertex_keys) {
      warped_v <- all_transformed[[v_key]]
      if (!is.null(warped_v)) {
        dist <- sqrt(warped_v[1]^2 + warped_v[2]^2)
        warped_boundary_dists <- c(warped_boundary_dists, dist)
      }
    }

    computed_circle_radius <- mean(warped_boundary_dists)
    cat(sprintf("Computed circle radius: %.2f (avg of %d warped boundary distances)\n",
                computed_circle_radius, length(warped_boundary_dists)))

    for (v_key in boundary_vertex_keys) {
      warped_v <- all_transformed[[v_key]]
      if (is.null(warped_v)) next

      dist <- sqrt(warped_v[1]^2 + warped_v[2]^2)
      if (dist > 0) {
        projected <- c(
          warped_v[1] / dist * computed_circle_radius,
          warped_v[2] / dist * computed_circle_radius
        )

        # Update the vertex in all pieces that share it
        for (piece_id in 1:num_pieces) {
          for (i in 1:6) {
            orig_v <- piece_vertices_original[[piece_id]][[i]]
            orig_key <- sprintf("%.1f,%.1f", orig_v[1], orig_v[2])

            if (orig_key == v_key) {
              piece_vertices[[piece_id]][[i]] <- projected
            }
          }
        }
      }
    }

    cat(sprintf("Boundary vertices projected to circle radius %.2f\n\n", computed_circle_radius))
  }
}

# Now test accessing a boundary edge
cat("Testing boundary edge access:\n")
test_edge_key <- boundary_edge_keys[1]
cat(sprintf("  Test edge key: %s\n", test_edge_key))

# Parse piece_id and side from the key
parts <- strsplit(test_edge_key, "-")[[1]]
test_piece_id <- as.integer(parts[1])
test_side <- as.integer(parts[2])
cat(sprintf("  Parsed: piece_id=%d, side=%d\n", test_piece_id, test_side))

# Get the transformed vertices
v1 <- piece_vertices[[test_piece_id]][[test_side + 1]]
v2 <- piece_vertices[[test_piece_id]][[(test_side + 1) %% 6 + 1]]

cat(sprintf("  v1 = (%.2f, %.2f)\n", v1[1], v1[2]))
cat(sprintf("  v2 = (%.2f, %.2f)\n", v2[1], v2[2]))

# Test cross product
cross <- v1[1] * v2[2] - v1[2] * v2[1]
cat(sprintf("  cross product = %.2f\n", cross))

# Test sweep direction
sweep <- if (cross > 0) 1 else 0
cat(sprintf("  sweep = %d\n", sweep))

cat("\nAll boundary edges should work!\n")
