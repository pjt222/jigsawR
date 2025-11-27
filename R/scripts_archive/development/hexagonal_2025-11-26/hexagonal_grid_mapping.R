# Hexagonal Grid Coordinate Mapping
# Maps logical piece IDs to hexagonal grid coordinates

#' Map piece ID to hexagonal grid coordinates
#'
#' The hexagonal puzzle uses a different coordinate system than our
#' ring-based topology. This function bridges the two systems.
#'
#' @param piece_id Logical piece ID (1 to num_pieces)
#' @param rings Number of rings in the puzzle
#' @return Named list with xi, yi coordinates in hexagonal grid
#'
#' @details
#' The hexagonal grid uses an offset coordinate system where:
#' - yi ranges from -(2n-3) to (2n-3) in steps of 2
#' - xi range depends on yi (narrower at extremes)
#' - Center is not at (0,0) in this system
#'
#' Our ring-based topology:
#' - Piece 1: center (ring 0)
#' - Pieces 2-7: ring 1 (6 pieces)
#' - Pieces 8-19: ring 2 (12 pieces)
#'
#' This function maps between the two systems.
#'
#' @examples
#' # Center piece
#' coords <- map_piece_to_hex_grid(1, rings = 3)
#' # Returns: list(xi = 0, yi = 0) or similar
#'
#' @export
map_piece_to_hex_grid <- function(piece_id, rings) {
  # Source topology if needed
  if (!exists("map_piece_id_to_ring")) {
    source("R/hexagonal_topology.R")
  }

  # Get ring-based topology
  ring_info <- map_piece_id_to_ring(piece_id, rings)

  # Convert ring-based to hexagonal grid coordinates
  # This is a simplified approximation - the actual mapping
  # depends on how hexagonal pieces are arranged in the grid

  if (ring_info$ring == 0) {
    # Center piece - at origin in both systems
    return(list(xi = 0, yi = 0))
  }

  # For other rings, use angular position to determine grid coords
  # Hexagonal grid has 6-fold symmetry
  angle <- ring_info$angle
  ring <- ring_info$ring

  # Approximate hexagonal grid position
  # This is a placeholder - actual mapping is more complex
  r <- ring * 2  # Scale factor
  xi <- round(r * cos(angle))
  yi <- round(r * sin(angle) * 2 / sqrt(3))  # Hex grid has different y-spacing

  return(list(xi = xi, yi = yi))
}

#' Alternative approach: Generate pieces using direct bezier math
#'
#' Instead of mapping to the hex_gentab coordinate system,
#' we can generate hexagonal pieces directly using bezier curves.
#'
#' This is simpler because:
#' 1. All pieces have same orientation (honeycomb)
#' 2. Can reuse rectangular bezier logic with 6 edges instead of 4
#' 3. Don't need to understand hex_gentab's coordinate system
#'
#' @keywords internal
generate_hex_piece_direct <- function(piece_id, rings, seed, params) {
  # Source dependencies
  if (!exists("map_piece_id_to_ring")) {
    source("R/hexagonal_topology.R")
  }
  if (!exists("init_jigsaw")) {
    source("R/rectangular_puzzle.R")
  }

  # Get piece position
  ring_info <- map_piece_id_to_ring(piece_id, rings)
  position <- calculate_hex_piece_position(piece_id, rings,
                                           base_spacing = params$base_spacing)

  # Classify piece
  piece_type <- if (ring_info$ring == 0) {
    "center"
  } else if (ring_info$ring == rings - 1) {
    "edge"
  } else {
    "inner"
  }

  # Initialize jigsaw environment for tab parameters
  init_jigsaw(seed = seed,
             tabsize = params$tabsize,
             jitter = params$jitter,
             width = params$diameter,
             height = params$diameter,
             xn = 2, yn = 2)  # Dummy grid for tab parameters

  # Generate 6 edges for hexagon
  edges <- list()
  piece_radius <- params$diameter / (rings * 4)

  for (side in 0:5) {
    # Calculate edge endpoints (hexagon vertices)
    # Remember: flat-top orientation (base offset π/6 built into vertices)
    base_offset <- pi / 6
    angle1 <- side * pi / 3 + base_offset
    angle2 <- (side + 1) * pi / 3 + base_offset

    v1_x <- piece_radius * cos(angle1)
    v1_y <- piece_radius * sin(angle1)
    v2_x <- piece_radius * cos(angle2)
    v2_y <- piece_radius * sin(angle2)

    # Determine if this edge is:
    # - Shared with another piece (needs tab)
    # - Border edge (straight)
    is_border <- (piece_type == "edge" && is_border_side_hex(side, ring_info))

    if (is_border) {
      # Straight edge
      edges[[side + 1]] <- list(
        forward = sprintf("L %.2f %.2f", v2_x, v2_y),
        reverse = sprintf("L %.2f %.2f", v1_x, v1_y),
        type = "border"
      )
    } else {
      # Generate bezier edge with tab
      # Use edge_id based on piece_id and side for determinism
      edge_id <- piece_id * 10 + side

      edge_bezier <- generate_hex_edge_with_tab(
        v1 = c(v1_x, v1_y),
        v2 = c(v2_x, v2_y),
        seed = seed,
        edge_id = edge_id,
        params = params
      )

      edges[[side + 1]] <- edge_bezier
    }
  }

  # Assemble piece path
  path <- sprintf("M %.2f %.2f", position$x + edges[[1]]$start[1],
                                 position$y + edges[[1]]$start[2])

  for (i in 1:6) {
    path <- paste0(path, " ", edges[[i]]$forward)
  }

  path <- paste0(path, " Z")

  return(list(
    id = piece_id,
    path = path,
    center_x = position$x,
    center_y = position$y,
    edges = edges,
    type = piece_type
  ))
}

#' Determine if a hexagon side is on the border
#'
#' @param side Side number (0-5)
#' @param ring_info Ring information from map_piece_id_to_ring
#' @return Logical TRUE if this side is on the puzzle border
#'
#' @keywords internal
is_border_side_hex <- function(side, ring_info) {
  # For outer ring pieces, some sides are borders
  # This is a simplified check - actual logic depends on position
  # For now, return FALSE (all edges have tabs)
  return(FALSE)
}

#' Generate hexagonal edge with tab using bezier curves
#'
#' Generates a bezier curve with tab for one hexagon edge.
#' Similar to rectangular edge generation but for hexagon.
#'
#' @param v1 Start vertex c(x, y)
#' @param v2 End vertex c(x, y)
#' @param seed Random seed
#' @param edge_id Edge identifier for deterministic tabs
#' @param params Generation parameters
#' @return List with forward/reverse paths and control points
#'
#' @keywords internal
generate_hex_edge_with_tab <- function(v1, v2, seed, edge_id, params) {
  # This will use similar logic to rectangular puzzle edge generation
  # but adapted for hexagonal edges

  # For now, return straight edge as placeholder
  # Full implementation will add bezier curves with tabs
  return(list(
    start = v1,
    end = v2,
    forward = sprintf("L %.2f %.2f", v2[1], v2[2]),
    reverse = sprintf("L %.2f %.2f", v1[1], v1[2]),
    type = "tab"
  ))
}
