# Debug script to understand why 7-ring produces 3-ring sized circle radius

setwd("D:/dev/p/jigsawR")

library(devtools)
load_all()

cat("==========================================\n")
cat("DEBUG: Investigating circle radius scaling\n")
cat("==========================================\n\n")

# Test function for a specific ring count and diameter
test_scaling <- function(rings, diameter) {
  cat(sprintf("\n=== %d-ring puzzle (diameter = %d) ===\n", rings, diameter))

  # Calculate piece_radius (from hexagonal_edge_generation_fixed.R line 35)
  piece_radius <- diameter / (rings * 4)
  cat(sprintf("piece_radius = diameter / (rings * 4) = %d / (%d * 4) = %.3f\n",
              diameter, rings, piece_radius))

  num_pieces <- 3 * rings * (rings - 1) + 1
  cat(sprintf("num_pieces = 3 * %d * (%d - 1) + 1 = %d\n", rings, rings, num_pieces))

  # Calculate vertices for all pieces
  cat("\nCalculating piece vertices...\n")

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

  # Find boundary vertices
  boundary_vertex_keys <- c()
  for (v_key in names(vertex_sharing)) {
    if (length(unique(vertex_sharing[[v_key]]$pieces)) < 3) {
      boundary_vertex_keys <- c(boundary_vertex_keys, v_key)
    }
  }

  cat(sprintf("Total unique vertices: %d\n", length(names(vertex_sharing))))
  cat(sprintf("Boundary vertices: %d\n", length(boundary_vertex_keys)))

  # Calculate distances BEFORE warp
  boundary_dists_original <- c()
  for (v_key in boundary_vertex_keys) {
    v <- vertex_sharing[[v_key]]$coords
    dist <- sqrt(v[1]^2 + v[2]^2)
    boundary_dists_original <- c(boundary_dists_original, dist)
  }

  cat("\nOriginal boundary vertex distances (before warp):\n")
  cat(sprintf("  Min: %.3f\n", min(boundary_dists_original)))
  cat(sprintf("  Max: %.3f\n", max(boundary_dists_original)))
  cat(sprintf("  Mean: %.3f\n", mean(boundary_dists_original)))

  # Calculate distances AFTER warp
  warped_boundary_dists <- c()
  for (v_key in boundary_vertex_keys) {
    v <- vertex_sharing[[v_key]]$coords
    warped <- apply_hex_warp(v[1], v[2])
    dist <- sqrt(warped$x^2 + warped$y^2)
    warped_boundary_dists <- c(warped_boundary_dists, dist)
  }

  circle_radius <- mean(warped_boundary_dists)
  expected_radius <- diameter / 2

  cat("\nWarped boundary vertex distances:\n")
  cat(sprintf("  Min: %.3f\n", min(warped_boundary_dists)))
  cat(sprintf("  Max: %.3f\n", max(warped_boundary_dists)))
  cat(sprintf("  Mean (= circle_radius): %.3f\n", circle_radius))
  cat(sprintf("\nExpected radius (diameter/2): %.3f\n", expected_radius))
  cat(sprintf("Ratio actual/expected: %.3f\n", circle_radius / expected_radius))

  return(list(
    rings = rings,
    diameter = diameter,
    circle_radius = circle_radius,
    expected_radius = expected_radius,
    ratio = circle_radius / expected_radius
  ))
}

# Run tests
result3 <- test_scaling(3, 240)
result7 <- test_scaling(7, 400)

cat("\n\n==========================================\n")
cat("COMPARISON:\n")
cat(sprintf("3-ring: calculated radius = %.3f, expected = %.3f (ratio = %.3f)\n",
            result3$circle_radius, result3$expected_radius, result3$ratio))
cat(sprintf("7-ring: calculated radius = %.3f, expected = %.3f (ratio = %.3f)\n",
            result7$circle_radius, result7$expected_radius, result7$ratio))
cat("==========================================\n")
