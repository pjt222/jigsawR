# Debug piece 27's edges - specifically the INNER edge issue
# Piece 27 (ring 3, 140°-160°) should have INNER edge segments
# connecting to BOTH piece 12 (120°-150°) AND piece 13 (150°-180°)

cat("=== Debug Piece 27 INNER Edge ===\n\n")

source("R/logging.R")
source("R/concentric_geometry.R")
source("R/hexagonal_bezier_generation.R")
source("R/concentric_edge_generation.R")

rings <- 7
diameter <- 500
seed <- 42
tabsize <- diameter * 0.10

# Generate edge map
cat("Generating edge map...\n")
edge_data <- generate_concentric_edge_map(
  rings = rings,
  seed = seed,
  diameter = diameter,
  tabsize = tabsize,
  jitter = 4,
  center_shape = "hexagon"
)

# Check what edges piece 27 has
cat("\n=== Piece 27 Edges ===\n")
piece27_edges <- edge_data$piece_edges[[27]]
cat(sprintf("Number of edges: %d\n\n", length(piece27_edges)))

for (i in seq_along(piece27_edges)) {
  edge <- piece27_edges[[i]]
  cat(sprintf("Edge %d: type = %s\n", i, edge$type))
  if (!is.null(edge$edge_ref)) {
    cat(sprintf("  edge_ref = %s\n", edge$edge_ref))
    cat(sprintf("  is_forward = %s\n", edge$is_forward))

    # Get the actual edge data
    edge_info <- edge_data$edge_map[[edge$edge_ref]]
    if (!is.null(edge_info)) {
      cat(sprintf("  start = (%.2f, %.2f)\n", edge_info$start[1], edge_info$start[2]))
      cat(sprintf("  end = (%.2f, %.2f)\n", edge_info$end[1], edge_info$end[2]))
      cat(sprintf("  piece1 = %d, piece2 = %d\n", edge_info$piece1, edge_info$piece2))
    }
  }
  if (!is.null(edge$neighbor)) {
    cat(sprintf("  neighbor = %d\n", edge$neighbor))
  }
  cat("\n")
}

# Check what inner neighbors piece 27 should have
cat("=== Inner Neighbors for Piece 27 ===\n")
inner_neighbors <- get_inner_neighbors(27, rings)
cat(sprintf("get_inner_neighbors(27, 7) = %s\n", paste(inner_neighbors, collapse=", ")))

# Expected angular ranges
piece27_info <- edge_data$piece_vertices[[27]]
cat(sprintf("\nPiece 27 angular range: %.1f° to %.1f°\n",
            piece27_info$start_angle * 180/pi,
            piece27_info$end_angle * 180/pi))

for (nid in inner_neighbors) {
  ninfo <- edge_data$piece_vertices[[nid]]
  cat(sprintf("Piece %d angular range: %.1f° to %.1f°\n",
              nid, ninfo$start_angle * 180/pi, ninfo$end_angle * 180/pi))
}

# Check if there are edges connecting 27 to both 12 and 13
cat("\n=== Edge Map Keys containing 27 ===\n")
for (key in names(edge_data$edge_map)) {
  if (grepl("27", key) || grepl("-27-", key)) {
    edge_info <- edge_data$edge_map[[key]]
    cat(sprintf("%s: piece1=%d, piece2=%d\n", key, edge_info$piece1, edge_info$piece2))
  }
}

# Check piece 12's edges to see if it properly adds to piece 27
cat("\n=== Piece 12 Edges ===\n")
piece12_edges <- edge_data$piece_edges[[12]]
cat(sprintf("Number of edges: %d\n", length(piece12_edges)))
for (i in seq_along(piece12_edges)) {
  edge <- piece12_edges[[i]]
  cat(sprintf("Edge %d: type = %s", i, edge$type))
  if (!is.null(edge$neighbor)) {
    cat(sprintf(", neighbor = %d", edge$neighbor))
  }
  cat("\n")
}

# Check piece 13's edges
cat("\n=== Piece 13 Edges ===\n")
piece13_edges <- edge_data$piece_edges[[13]]
cat(sprintf("Number of edges: %d\n", length(piece13_edges)))
for (i in seq_along(piece13_edges)) {
  edge <- piece13_edges[[i]]
  cat(sprintf("Edge %d: type = %s", i, edge$type))
  if (!is.null(edge$neighbor)) {
    cat(sprintf(", neighbor = %d", edge$neighbor))
  }
  cat("\n")
}

cat("\n=== Analysis Complete ===\n")
