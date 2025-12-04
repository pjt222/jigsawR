# Verify concentric edge topology
# Check that edges interlock properly between rings

cat("=== Verifying Concentric Edge Topology ===\n\n")

source("R/logging.R")
source("R/concentric_geometry.R")
source("R/hexagonal_bezier_generation.R")
source("R/concentric_edge_generation.R")

# Parameters
rings <- 3
diameter <- 240
seed <- 42

cat(sprintf("Testing with %d rings, %d mm diameter\n\n", rings, diameter))

# Generate edge map
edge_data <- generate_concentric_edge_map(
  rings = rings,
  seed = seed,
  diameter = diameter,
  tabsize = 20,
  jitter = 4,
  center_shape = "hexagon"
)

cat(sprintf("Total unique edges: %d\n", edge_data$num_edges))
cat(sprintf("Total pieces: %d\n\n", length(edge_data$piece_vertices)))

# Analyze edge sharing
cat("=== Edge Sharing Analysis ===\n\n")

# Check piece 2 (ring 1, position 0) - should have 2 outer segments
cat("Piece 2 (ring 1, position 0) edges:\n")
piece2_edges <- edge_data$piece_edges[[2]]
for (i in seq_along(piece2_edges)) {
  e <- piece2_edges[[i]]
  cat(sprintf("  Edge %d: type=%s", i, e$type))
  if (!is.null(e$edge_ref)) {
    cat(sprintf(", ref=%s", e$edge_ref))
  }
  if (!is.null(e$neighbor)) {
    cat(sprintf(", neighbor=%d", e$neighbor))
  }
  cat(sprintf(", is_forward=%s\n", ifelse(is.null(e$is_forward), "N/A", e$is_forward)))
}

cat("\n")

# Check pieces 8 and 9 (ring 2, positions 0 and 1) - should share inner edge with piece 2
cat("Piece 8 (ring 2, position 0) edges:\n")
piece8_edges <- edge_data$piece_edges[[8]]
for (i in seq_along(piece8_edges)) {
  e <- piece8_edges[[i]]
  cat(sprintf("  Edge %d: type=%s", i, e$type))
  if (!is.null(e$edge_ref)) {
    cat(sprintf(", ref=%s", e$edge_ref))
  }
  cat(sprintf(", is_forward=%s\n", ifelse(is.null(e$is_forward), "N/A", e$is_forward)))
}

cat("\n")

cat("Piece 9 (ring 2, position 1) edges:\n")
piece9_edges <- edge_data$piece_edges[[9]]
for (i in seq_along(piece9_edges)) {
  e <- piece9_edges[[i]]
  cat(sprintf("  Edge %d: type=%s", i, e$type))
  if (!is.null(e$edge_ref)) {
    cat(sprintf(", ref=%s", e$edge_ref))
  }
  cat(sprintf(", is_forward=%s\n", ifelse(is.null(e$is_forward), "N/A", e$is_forward)))
}

cat("\n")

# Verify that outer neighbors function works correctly
cat("=== Outer Neighbors Function Test ===\n\n")

for (piece_id in 2:7) {  # Ring 1 pieces
  neighbors <- get_outer_neighbors(piece_id, rings)
  cat(sprintf("Piece %d outer neighbors: %s\n", piece_id, paste(neighbors, collapse=", ")))
}

cat("\n")

# Count edges by type
cat("=== Edge Count by Type ===\n\n")

edge_types <- c()
for (key in names(edge_data$edge_map)) {
  if (grepl("-inner", key)) {
    edge_types <- c(edge_types, "inner")
  } else if (grepl("-circ", key)) {
    edge_types <- c(edge_types, "circumferential")
  } else if (grepl("-radial", key)) {
    edge_types <- c(edge_types, "radial")
  } else {
    edge_types <- c(edge_types, "center-ring1")
  }
}

cat(sprintf("Center-Ring1 edges: %d\n", sum(edge_types == "center-ring1")))
cat(sprintf("Circumferential edges: %d\n", sum(edge_types == "circumferential")))
cat(sprintf("Radial edges (ring1->ring2): %d\n", sum(edge_types == "radial")))
cat(sprintf("Inner edges: %d\n", sum(edge_types == "inner")))

cat("\n")

# Expected edge counts:
# - Center to ring 1: 6 edges (hexagon center has 6 edges to 6 ring 1 pieces)
# - Ring 1 circumferential: 6 edges (each ring 1 piece connects to 2 neighbors)
# - Ring 1 to ring 2 (radial): 12 edges (each of 6 ring1 pieces connects to 2 ring2 pieces)
# - Ring 2 circumferential: 12 edges
# Total expected: 6 + 6 + 12 + 12 = 36

cat("Expected edge counts:\n")
cat("  Center to ring 1: 6\n")
cat("  Ring 1 circumferential: 6\n")
cat("  Ring 1 to ring 2 (radial): 12\n")
cat("  Ring 2 circumferential: 12\n")
cat("  Total: 36\n\n")

# Verify path building for piece 2
cat("=== Path Verification for Piece 2 ===\n\n")

path2 <- build_concentric_piece_path(2, edge_data)
cat("Path for piece 2:\n")
cat(path2, "\n\n")

# Count bezier curves (C commands) in the path
c_count <- length(gregexpr("C ", path2)[[1]])
l_count <- length(gregexpr("L ", path2)[[1]])
if (l_count == 1 && !grepl("L ", path2)) l_count <- 0

cat(sprintf("Bezier curves (C): %d\n", c_count))
cat(sprintf("Line segments (L): %d\n", l_count))

cat("\n")

# Verify piece 8's inner edge matches piece 2's outer segment
cat("=== Edge Matching Verification ===\n\n")

# Find the edge that piece 2 and piece 8 share
for (key in names(edge_data$edge_map)) {
  edge <- edge_data$edge_map[[key]]
  if ((edge$piece1 == 2 && edge$piece2 == 8) || (edge$piece1 == 8 && edge$piece2 == 2)) {
    cat(sprintf("Found shared edge: %s\n", key))
    cat(sprintf("  Piece1: %d, Piece2: %d\n", edge$piece1, edge$piece2))
    cat(sprintf("  Start: (%.2f, %.2f)\n", edge$start[1], edge$start[2]))
    cat(sprintf("  End: (%.2f, %.2f)\n", edge$end[1], edge$end[2]))
    cat(sprintf("  Forward: %s...\n", substr(edge$forward, 1, 50)))
    cat(sprintf("  Reverse: %s...\n", substr(edge$reverse, 1, 50)))
    break
  }
}

cat("\n=== Verification Complete ===\n")
