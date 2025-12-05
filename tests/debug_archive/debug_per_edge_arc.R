# Debug: Calculate per-edge arc radius

cat("=" , rep("=", 70), "\n", sep = "")
cat("Testing: Per-edge arc radius calculation\n")
cat("=" , rep("=", 70), "\n\n", sep = "")

source("R/hexagonal_topology.R")

rings <- 3
diameter <- 240
piece_radius <- diameter / (rings * 4)
num_pieces <- 3 * rings * (rings - 1) + 1

# Apply warp to all vertices (no projection)
piece_vertices_warped <- list()

for (piece_id in 1:num_pieces) {
  axial <- map_piece_id_to_axial(piece_id, rings)
  cart <- axial_to_cartesian(axial$q, axial$r, piece_radius)

  vertices <- list()
  for (i in 0:5) {
    angle <- i * pi / 3
    vx <- cart$x + piece_radius * cos(angle)
    vy <- cart$y + piece_radius * sin(angle)

    # Apply warp
    angl <- atan2(vy, vx) + pi
    angl60 <- angl %% (pi / 3)
    angl30 <- abs((pi / 6) - angl60)
    l <- sqrt(0.75) / cos(angl30)

    vertices[[i + 1]] <- c(vx / l, vy / l)
  }
  piece_vertices_warped[[piece_id]] <- vertices
}

# Build vertex sharing map to identify boundary edges
vertex_sharing <- list()
for (piece_id in 1:num_pieces) {
  for (i in 1:6) {
    v <- piece_vertices_warped[[piece_id]][[i]]
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

# Analyze boundary edges
cat("Boundary edge analysis (warped vertices, no projection):\n\n")
cat(sprintf("%-12s %-10s %-10s %-12s %-12s\n",
            "Edge", "V1 Dist", "V2 Dist", "Avg Dist", "Arc Radius"))
cat(sprintf("%-12s %-10s %-10s %-12s %-12s\n",
            "----", "-------", "-------", "--------", "----------"))

for (piece_id in 1:num_pieces) {
  for (side in 0:5) {
    v1 <- piece_vertices_warped[[piece_id]][[side + 1]]
    v2 <- piece_vertices_warped[[piece_id]][[(side + 1) %% 6 + 1]]

    v1_key <- sprintf("%.1f,%.1f", v1[1], v1[2])
    v2_key <- sprintf("%.1f,%.1f", v2[1], v2[2])

    # Check if both vertices are boundary
    if (v1_key %in% boundary_vertex_keys && v2_key %in% boundary_vertex_keys) {
      v1_pieces <- unique(vertex_sharing[[v1_key]]$pieces)
      v2_pieces <- unique(vertex_sharing[[v2_key]]$pieces)
      shared_pieces <- intersect(v1_pieces, v2_pieces)

      if (length(shared_pieces) == 1) {
        # This is a boundary edge
        dist1 <- sqrt(v1[1]^2 + v1[2]^2)
        dist2 <- sqrt(v2[1]^2 + v2[2]^2)
        avg_dist <- (dist1 + dist2) / 2

        # For an arc centered at origin passing through both points,
        # the radius should be the average distance (approximately)
        # More precisely, for two points on a circle centered at origin,
        # the radius equals the distance from origin to each point
        # If they're at different distances, we can't have a perfect arc

        cat(sprintf("%-12s %-10.2f %-10.2f %-12.2f %-12.2f\n",
                    sprintf("%d-%d", piece_id, side), dist1, dist2, avg_dist, avg_dist))
      }
    }
  }
}

cat("\n")
cat("=" , rep("=", 70), "\n", sep = "")
cat("Key observation: For a true circular arc centered at origin,\n")
cat("both endpoints must be at the SAME distance from origin.\n")
cat("Since warped boundary vertices are at DIFFERENT distances,\n")
cat("we have two options:\n")
cat("  1. Use straight lines (L) instead of arcs (A)\n")
cat("  2. Project vertices to a consistent radius first (current approach)\n")
cat("=" , rep("=", 70), "\n")
