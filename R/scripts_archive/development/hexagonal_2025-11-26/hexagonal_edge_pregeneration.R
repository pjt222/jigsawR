# Hexagonal Edge Pre-generation
# Pre-generate all edges for hexagonal puzzle to ensure complementarity

#' Generate all edges for hexagonal puzzle with complementarity
#'
#' Pre-generates all unique edges in the puzzle. Each edge is shared between
#' two adjacent pieces, with one using the forward path and the other using
#' the reverse path (complementary).
#'
#' @param rings Number of rings
#' @param seed Random seed for deterministic generation
#' @param diameter Puzzle diameter
#' @param tabsize Tab size percentage (10-30, default: 27)
#' @param jitter Jitter percentage (0-10, default: 5)
#' @return List of all edges, indexed by "piece_id-side" key
#'
#' @details
#' Each edge contains:
#' - forward: SVG path from v1 to v2
#' - reverse: SVG path from v2 to v1 (complementary)
#' - start: Starting vertex coordinates
#' - end: Ending vertex coordinates
#' - pieces: List of two piece IDs sharing this edge
#'
#' @export
generate_hex_edges_with_complementarity <- function(rings, seed, diameter, tabsize = 27, jitter = 5) {
  # Source dependencies
  if (!exists("map_piece_id_to_ring")) {
    source("R/hexagonal_topology.R")
  }
  if (!exists("get_hex_neighbor")) {
    source("R/hexagonal_neighbors.R")
  }
  if (!exists("generate_hex_bezier_edge")) {
    source("R/hexagonal_bezier_generation.R")
  }

  num_pieces <- 3 * rings * (rings - 1) + 1
  piece_radius <- diameter / (rings * 4)
  tab_params <- list(tabsize = tabsize, jitter = jitter)

  # Calculate vertices for each piece
  # We'll store vertices by piece_id
  piece_vertices <- list()

  for (piece_id in 1:num_pieces) {
    ring_info <- map_piece_id_to_ring(piece_id, rings)

    # Calculate piece center position (for separated layout, use connected = origin)
    # For edge generation, all pieces at origin (edges connect them)
    center_x <- 0
    center_y <- 0

    # Generate 6 vertices for flat-top hexagon
    base_offset <- pi / 6
    vertices <- list()
    for (i in 0:5) {
      angle <- i * pi / 3 + base_offset
      vx <- center_x + piece_radius * cos(angle)
      vy <- center_y + piece_radius * sin(angle)
      vertices[[i + 1]] <- c(vx, vy)
    }

    piece_vertices[[piece_id]] <- vertices
  }

  # Generate edges
  # We'll use "piece_id-side" as the key
  # Only generate each unique edge once (from lower piece_id)
  edges <- list()
  edge_counter <- 0

  for (piece_id in 1:num_pieces) {
    vertices <- piece_vertices[[piece_id]]

    for (side in 0:5) {
      neighbor_id <- get_hex_neighbor(piece_id, side, rings)

      # Key for this edge
      edge_key <- sprintf("%d-%d", piece_id, side)

      # Skip if already generated (from neighbor's perspective)
      if (!is.null(edges[[edge_key]])) {
        next
      }

      # Get edge endpoints
      v1 <- vertices[[side + 1]]
      v2 <- vertices[[(side + 1) %% 6 + 1]]

      if (is.na(neighbor_id)) {
        # Border edge - straight line
        edges[[edge_key]] <- list(
          forward = sprintf("L %.2f %.2f", v2[1], v2[2]),
          reverse = sprintf("L %.2f %.2f", v1[1], v1[2]),
          start = v1,
          end = v2,
          type = "border",
          piece_id = piece_id,
          side = side,
          neighbor_id = NA
        )
      } else {
        # Internal edge - generate bezier curve
        # Use deterministic edge_id based on both pieces
        # Use min and max to ensure same edge_id regardless of direction
        min_id <- min(piece_id, neighbor_id)
        max_id <- max(piece_id, neighbor_id)
        edge_id <- min_id * 1000 + max_id + side

        # Generate bezier edge
        bezier_edge <- generate_hex_bezier_edge(
          v1 = v1,
          v2 = v2,
          seed = seed,
          edge_id = edge_id,
          tab_params = tab_params
        )

        # Store edge with both keys (this piece and neighbor piece)
        edges[[edge_key]] <- list(
          forward = bezier_edge$forward,
          reverse = bezier_edge$reverse,
          start = v1,
          end = v2,
          type = "tab",
          piece_id = piece_id,
          side = side,
          neighbor_id = neighbor_id
        )

        # Also store reverse reference (neighbor's perspective)
        # Find which side of neighbor connects back to this piece
        for (neighbor_side in 0:5) {
          neighbor_neighbor <- get_hex_neighbor(neighbor_id, neighbor_side, rings)
          if (!is.na(neighbor_neighbor) && neighbor_neighbor == piece_id) {
            neighbor_edge_key <- sprintf("%d-%d", neighbor_id, neighbor_side)
            edges[[neighbor_edge_key]] <- list(
              forward = bezier_edge$reverse,  # Swap forward/reverse
              reverse = bezier_edge$forward,
              start = v2,
              end = v1,
              type = "tab",
              piece_id = neighbor_id,
              side = neighbor_side,
              neighbor_id = piece_id
            )
            break
          }
        }

        edge_counter <- edge_counter + 1
      }
    }
  }

  return(edges)
}
