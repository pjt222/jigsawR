# Verify edge interlocking between concentric ring pieces
# This test confirms that shared edges use the same bezier curve from both perspectives

cat("=== Verifying Edge Interlocking ===\n\n")

source("R/logging.R")
source("R/concentric_geometry.R")
source("R/hexagonal_bezier_generation.R")
source("R/concentric_edge_generation.R")

# Parameters
rings <- 3
diameter <- 240
seed <- 42

cat(sprintf("Testing with %d rings, %d mm diameter, seed %d\n\n", rings, diameter, seed))

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

# Test 1: Verify that center (piece 1) and ring 1 (pieces 2-7) share edges correctly
cat("=== Test 1: Center-Ring1 Edge Sharing ===\n")

for (neighbor_id in 2:7) {
  # Get center piece's reference to this edge
  center_edges <- edge_data$piece_edges[[1]]
  found_center_ref <- NULL
  for (e in center_edges) {
    if (!is.null(e$edge_ref) && grepl(sprintf("-%d$", neighbor_id), e$edge_ref)) {
      found_center_ref <- e
      break
    }
  }

  # Get neighbor's reference to this edge (should be INNER)
  neighbor_edges <- edge_data$piece_edges[[neighbor_id]]
  found_neighbor_ref <- NULL
  for (e in neighbor_edges) {
    if (e$type == "inner" && !is.null(e$edge_ref)) {
      found_neighbor_ref <- e
      break
    }
  }

  if (!is.null(found_center_ref) && !is.null(found_neighbor_ref)) {
    # Both should reference the same edge
    if (found_center_ref$edge_ref == found_neighbor_ref$edge_ref) {
      cat(sprintf("✓ Piece 1 <-> Piece %d: Edge '%s' shared correctly\n",
                  neighbor_id, found_center_ref$edge_ref))
      cat(sprintf("    Center: is_forward=%s, Neighbor: is_forward=%s\n",
                  found_center_ref$is_forward, found_neighbor_ref$is_forward))
    } else {
      cat(sprintf("✗ Piece 1 <-> Piece %d: Edge mismatch! Center='%s', Neighbor='%s'\n",
                  neighbor_id, found_center_ref$edge_ref, found_neighbor_ref$edge_ref))
    }
  } else {
    cat(sprintf("? Piece 1 <-> Piece %d: Could not find edge references\n", neighbor_id))
  }
}

cat("\n")

# Test 2: Verify that ring 1 (piece 2) and ring 2 (pieces 8,9) share radial edges
cat("=== Test 2: Ring1-Ring2 Radial Edge Sharing ===\n")

# Piece 2 should have outer_segment edges connecting to pieces 8 and 9
piece2_edges <- edge_data$piece_edges[[2]]
outer_segments <- piece2_edges[sapply(piece2_edges, function(e) e$type == "outer_segment")]

for (seg in outer_segments) {
  neighbor_id <- seg$neighbor
  edge_ref <- seg$edge_ref

  # Find corresponding INNER edge in neighbor
  neighbor_edges <- edge_data$piece_edges[[neighbor_id]]
  found_inner <- NULL
  for (e in neighbor_edges) {
    if (e$type == "inner" && !is.null(e$edge_ref) && e$edge_ref == edge_ref) {
      found_inner <- e
      break
    }
  }

  if (!is.null(found_inner)) {
    cat(sprintf("✓ Piece 2 <-> Piece %d: Edge '%s' shared correctly\n",
                neighbor_id, edge_ref))
    cat(sprintf("    Piece 2 (outer_segment): is_forward=%s\n", seg$is_forward))
    cat(sprintf("    Piece %d (inner): is_forward=%s\n", neighbor_id, found_inner$is_forward))

    # Verify forward/reverse are complementary
    if (seg$is_forward != found_inner$is_forward) {
      cat(sprintf("    Directions are complementary (expected)\n"))
    } else {
      cat(sprintf("    WARNING: Both pieces use same direction!\n"))
    }
  } else {
    cat(sprintf("✗ Piece 2 <-> Piece %d: Edge '%s' not found in neighbor!\n",
                neighbor_id, edge_ref))
  }
}

cat("\n")

# Test 3: Verify circumferential edges (same ring neighbors)
cat("=== Test 3: Circumferential Edge Sharing ===\n")

# Check piece 2 <-> piece 3 (ring 1 neighbors)
piece2_edges <- edge_data$piece_edges[[2]]
piece3_edges <- edge_data$piece_edges[[3]]

right_edge <- NULL
for (e in piece2_edges) {
  if (e$type == "right") {
    right_edge <- e
    break
  }
}

left_edge <- NULL
for (e in piece3_edges) {
  if (e$type == "left") {
    left_edge <- e
    break
  }
}

if (!is.null(right_edge) && !is.null(left_edge)) {
  if (right_edge$edge_ref == left_edge$edge_ref) {
    cat(sprintf("✓ Piece 2 <-> Piece 3: Edge '%s' shared correctly\n", right_edge$edge_ref))
    cat(sprintf("    Piece 2 (right): is_forward=%s\n", right_edge$is_forward))
    cat(sprintf("    Piece 3 (left): is_forward=%s\n", left_edge$is_forward))
  } else {
    cat(sprintf("✗ Piece 2 <-> Piece 3: Edge mismatch! '%s' vs '%s'\n",
                right_edge$edge_ref, left_edge$edge_ref))
  }
}

cat("\n")

# Test 4: Build paths and verify they are complete
cat("=== Test 4: Path Completeness ===\n")

for (piece_id in 1:min(7, length(edge_data$piece_vertices))) {
  path <- build_concentric_piece_path(piece_id, edge_data)

  # Count path commands
  m_count <- length(gregexpr("M ", path, fixed = TRUE)[[1]])
  if (!grepl("M ", path)) m_count <- 0

  c_count <- length(gregexpr("C ", path, fixed = TRUE)[[1]])
  if (!grepl("C ", path)) c_count <- 0

  l_count <- length(gregexpr("L ", path, fixed = TRUE)[[1]])
  if (!grepl("L ", path)) l_count <- 0

  z_count <- length(gregexpr("Z", path, fixed = TRUE)[[1]])
  if (!grepl("Z", path)) z_count <- 0

  # Path should start with M and end with Z
  starts_with_m <- grepl("^M ", path)
  ends_with_z <- grepl("Z\\s*$", path)

  if (starts_with_m && ends_with_z) {
    cat(sprintf("✓ Piece %d: Valid path (M=%d, C=%d, L=%d, Z=%d)\n",
                piece_id, m_count, c_count, l_count, z_count))
  } else {
    cat(sprintf("✗ Piece %d: Invalid path! starts_M=%s, ends_Z=%s\n",
                piece_id, starts_with_m, ends_with_z))
  }
}

cat("\n=== All Verification Tests Complete ===\n")
