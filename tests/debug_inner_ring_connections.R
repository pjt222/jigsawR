# Debug inner ring connections piece by piece
# Check that each piece's edges connect properly to neighbors

cat("=== Debugging Inner Ring Connections ===\n\n")

source("R/logging.R")
source("R/concentric_geometry.R")
source("R/hexagonal_bezier_generation.R")
source("R/concentric_edge_generation.R")

# Parameters - use 10% tabsize as default
rings <- 3
diameter <- 240
seed <- 42
piece_height <- get_concentric_piece_height(diameter, rings)
tabsize <- diameter * 0.10  # 10% of diameter

cat(sprintf("Rings: %d\n", rings))
cat(sprintf("Diameter: %d mm\n", diameter))
cat(sprintf("Piece height: %.2f mm\n", piece_height))
cat(sprintf("Tab size: %.2f mm (10%%)\n", tabsize))
cat(sprintf("Total pieces: %d\n\n", get_concentric_piece_count(rings)))

# Generate edge map
edge_data <- generate_concentric_edge_map(
  rings = rings,
  seed = seed,
  diameter = diameter,
  tabsize = tabsize,
  jitter = 4,
  center_shape = "hexagon"
)

cat(sprintf("Generated %d unique edges\n\n", edge_data$num_edges))

# Helper to describe edge
describe_edge <- function(edge_info) {
  if (is.null(edge_info$edge_ref)) {
    return(sprintf("type=%s (no ref)", edge_info$type))
  }
  sprintf("type=%s, ref=%s, forward=%s",
          edge_info$type,
          edge_info$edge_ref,
          ifelse(is.null(edge_info$is_forward), "N/A", edge_info$is_forward))
}

# Analyze each piece
cat("=== PIECE BY PIECE ANALYSIS ===\n\n")

for (piece_id in 1:get_concentric_piece_count(rings)) {
  piece_info <- edge_data$piece_vertices[[piece_id]]
  piece_edges <- edge_data$piece_edges[[piece_id]]

  cat(sprintf("--- Piece %d ---\n", piece_id))
  cat(sprintf("Type: %s\n", piece_info$type))

  if (piece_info$type == "hexagon") {
    cat("Ring: 0 (center)\n")
    cat(sprintf("Vertices: %d\n", length(piece_info$vertices)))

    # Show vertices
    for (i in seq_along(piece_info$vertices)) {
      v <- piece_info$vertices[[i]]
      cat(sprintf("  V%d: (%.2f, %.2f)\n", i, v[1], v[2]))
    }

    # Show edges
    cat(sprintf("Edges: %d\n", length(piece_edges)))
    for (i in seq_along(piece_edges)) {
      cat(sprintf("  E%d: %s\n", i, describe_edge(piece_edges[[i]])))
    }

    # Expected: 6 outer edges connecting to pieces 2-7
    cat("\nExpected connections:\n")
    for (neighbor_id in 2:7) {
      expected_key <- sprintf("E1-%d", neighbor_id)
      found <- any(sapply(piece_edges, function(e) !is.null(e$edge_ref) && e$edge_ref == expected_key))
      cat(sprintf("  -> Piece %d: %s\n", neighbor_id, ifelse(found, "FOUND", "MISSING")))
    }

  } else if (piece_info$type == "trapezoid") {
    cat(sprintf("Ring: %d, Position: %d\n", piece_info$ring, piece_info$position))

    # Show vertices
    cat("Vertices:\n")
    vertex_names <- c("inner-start", "inner-end", "outer-end", "outer-start")
    for (i in seq_along(piece_info$vertices)) {
      v <- piece_info$vertices[[i]]
      cat(sprintf("  V%d (%s): (%.2f, %.2f)\n", i, vertex_names[i], v[1], v[2]))
    }

    # Show edges by type
    cat(sprintf("Edges: %d\n", length(piece_edges)))

    inner_edges <- piece_edges[sapply(piece_edges, function(e) e$type == "inner")]
    right_edges <- piece_edges[sapply(piece_edges, function(e) e$type == "right")]
    outer_edges <- piece_edges[sapply(piece_edges, function(e) e$type %in% c("outer_segment", "border"))]
    left_edges <- piece_edges[sapply(piece_edges, function(e) e$type == "left")]

    cat(sprintf("  INNER edges: %d\n", length(inner_edges)))
    for (e in inner_edges) cat(sprintf("    %s\n", describe_edge(e)))

    cat(sprintf("  RIGHT edges: %d\n", length(right_edges)))
    for (e in right_edges) cat(sprintf("    %s\n", describe_edge(e)))

    cat(sprintf("  OUTER edges: %d\n", length(outer_edges)))
    for (e in outer_edges) {
      if (e$type == "border") {
        cat(sprintf("    type=border\n"))
      } else {
        cat(sprintf("    %s, neighbor=%d\n", describe_edge(e), e$neighbor))
      }
    }

    cat(sprintf("  LEFT edges: %d\n", length(left_edges)))
    for (e in left_edges) cat(sprintf("    %s\n", describe_edge(e)))

    # Check expected neighbors
    ring <- piece_info$ring
    position <- piece_info$position
    pieces_in_ring <- 6 * ring
    ring_start <- 3 * ring * (ring - 1) + 2

    cat("\nExpected neighbors:\n")

    # INNER neighbor
    if (ring == 1) {
      inner_neighbor <- 1  # center
      cat(sprintf("  INNER: Piece 1 (center)\n"))
    } else {
      pieces_in_inner_ring <- 6 * (ring - 1)
      inner_ring_start <- 3 * (ring - 1) * (ring - 2) + 2
      inner_pos <- floor(position * pieces_in_inner_ring / pieces_in_ring)
      inner_neighbor <- inner_ring_start + inner_pos
      cat(sprintf("  INNER: Piece %d (ring %d)\n", inner_neighbor, ring - 1))
    }

    # Check if inner edge exists
    has_inner <- length(inner_edges) > 0
    cat(sprintf("    -> %s\n", ifelse(has_inner, "FOUND", "MISSING!")))

    # RIGHT neighbor
    next_pos <- (position + 1) %% pieces_in_ring
    right_neighbor <- ring_start + next_pos
    cat(sprintf("  RIGHT: Piece %d\n", right_neighbor))
    has_right <- length(right_edges) > 0
    cat(sprintf("    -> %s\n", ifelse(has_right, "FOUND", "MISSING!")))

    # OUTER neighbors
    outer_neighbors <- get_outer_neighbors(piece_id, rings)
    if (length(outer_neighbors) == 0) {
      cat(sprintf("  OUTER: BOUNDARY\n"))
      has_outer <- any(sapply(outer_edges, function(e) e$type == "border"))
    } else {
      cat(sprintf("  OUTER: Pieces %s\n", paste(outer_neighbors, collapse=", ")))
      has_outer <- length(outer_edges) >= length(outer_neighbors)
    }
    cat(sprintf("    -> %s\n", ifelse(has_outer, "FOUND", "MISSING!")))

    # LEFT neighbor
    prev_pos <- (position - 1 + pieces_in_ring) %% pieces_in_ring
    left_neighbor <- ring_start + prev_pos
    cat(sprintf("  LEFT: Piece %d\n", left_neighbor))
    has_left <- length(left_edges) > 0
    cat(sprintf("    -> %s\n", ifelse(has_left, "FOUND", "MISSING!")))
  }

  cat("\n")
}

# Check path building for each piece
cat("=== PATH BUILDING CHECK ===\n\n")

for (piece_id in 1:get_concentric_piece_count(rings)) {
  path <- build_concentric_piece_path(piece_id, edge_data)

  # Check path structure
  starts_m <- grepl("^M ", path)
  ends_z <- grepl("Z\\s*$", path)

  # Count commands
  m_count <- length(gregexpr("M ", path, fixed=TRUE)[[1]])
  if (!grepl("M ", path)) m_count <- 0
  c_count <- length(gregexpr("C ", path, fixed=TRUE)[[1]])
  if (!grepl("C ", path)) c_count <- 0
  l_count <- length(gregexpr("L ", path, fixed=TRUE)[[1]])
  if (!grepl("L ", path)) l_count <- 0

  status <- if (starts_m && ends_z) "OK" else "ERROR"

  cat(sprintf("Piece %2d: %s (M=%d, C=%d, L=%d) - path length: %d\n",
              piece_id, status, m_count, c_count, l_count, nchar(path)))

  if (l_count > 0) {
    cat(sprintf("  WARNING: Has %d straight line segments (L) - may indicate missing edges\n", l_count))
  }
}

cat("\n=== EDGE REFERENCE CHECK ===\n\n")

# Check that all edge references in piece_edges actually exist in edge_map
missing_refs <- c()
for (piece_id in 1:get_concentric_piece_count(rings)) {
  piece_edges <- edge_data$piece_edges[[piece_id]]
  for (e in piece_edges) {
    if (!is.null(e$edge_ref)) {
      if (is.null(edge_data$edge_map[[e$edge_ref]])) {
        missing_refs <- c(missing_refs, sprintf("Piece %d: %s", piece_id, e$edge_ref))
      }
    }
  }
}

if (length(missing_refs) == 0) {
  cat("All edge references are valid!\n")
} else {
  cat("MISSING EDGE REFERENCES:\n")
  for (ref in missing_refs) {
    cat(sprintf("  %s\n", ref))
  }
}

cat("\n=== Debug Complete ===\n")
