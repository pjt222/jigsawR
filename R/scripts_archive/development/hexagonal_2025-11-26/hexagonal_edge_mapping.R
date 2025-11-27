# Hexagonal Edge Mapping
# Functions for calculating shared edge IDs between adjacent hexagonal pieces

#' Calculate shared edge ID for hexagonal pieces
#'
#' In a hexagonal grid, adjacent pieces share edges. This function calculates
#' a deterministic edge ID based on the positions of the two pieces sharing the edge.
#' The edge ID must be the same whether calculated from piece A or piece B.
#'
#' @param piece_id Piece ID
#' @param side Side number (0-5, where 0 is right, going counterclockwise)
#' @param rings Number of rings in puzzle
#' @return Edge ID (deterministic hash of the two piece IDs)
#'
#' @details
#' Hexagonal sides (flat-top orientation):
#' - Side 0: Right (0°)
#' - Side 1: Upper-right (60°)
#' - Side 2: Upper-left (120°)
#' - Side 3: Left (180°)
#' - Side 4: Lower-left (240°)
#' - Side 5: Lower-right (300°)
#'
#' @export
calculate_hex_edge_id <- function(piece_id, side, rings) {
  # Source topology if needed
  if (!exists("map_piece_id_to_ring")) {
    source("R/hexagonal_topology.R")
  }

  # Get neighbor piece ID for this side
  neighbor_id <- get_hex_neighbor(piece_id, side, rings)

  if (is.na(neighbor_id)) {
    # Border edge - use piece_id and side
    return(piece_id * 10 + side)
  }

  # Shared edge - create deterministic ID from both pieces
  # Use min/max to ensure same ID regardless of which piece calculates it
  min_id <- min(piece_id, neighbor_id)
  max_id <- max(piece_id, neighbor_id)

  # Create unique edge ID
  edge_id <- min_id * 1000 + max_id

  return(edge_id)
}

#' Get neighboring piece ID for a given side
#'
#' @param piece_id Piece ID
#' @param side Side number (0-5)
#' @param rings Number of rings in puzzle
#' @return Neighbor piece ID, or NA if border edge
#'
#' @details
#' For hexagonal honeycomb grid, neighbors depend on:
#' - Ring number
#' - Position within ring
#' - Which side (0-5)
#'
#' @keywords internal
get_hex_neighbor <- function(piece_id, side, rings) {
  # Source topology if needed
  if (!exists("map_piece_id_to_ring")) {
    source("R/hexagonal_topology.R")
  }

  ring_info <- map_piece_id_to_ring(piece_id, rings)

  # Center piece (ring 0)
  if (ring_info$ring == 0) {
    # Center connects to all 6 pieces in ring 1
    # Side 0 connects to piece 2, side 1 to piece 3, etc.
    return(2 + side)
  }

  # For other rings, use geometric approach
  # This is simplified - full implementation would need detailed grid math

  # For now, return NA for non-center pieces (will improve)
  # This means each piece generates its own edges (not complementary yet)
  return(NA)
}

#' Calculate comprehensive neighbor mapping for hexagonal grid
#'
#' @param rings Number of rings
#' @return Data frame with piece_id, side, neighbor_id for all pieces
#'
#' @keywords internal
build_hex_neighbor_map <- function(rings) {
  num_pieces <- 3 * rings * (rings - 1) + 1

  result <- list()

  for (piece_id in 1:num_pieces) {
    for (side in 0:5) {
      neighbor_id <- get_hex_neighbor(piece_id, side, rings)

      result[[length(result) + 1]] <- list(
        piece_id = piece_id,
        side = side,
        neighbor_id = neighbor_id
      )
    }
  }

  do.call(rbind, lapply(result, as.data.frame))
}
