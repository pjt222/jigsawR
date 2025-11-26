# Hexagonal Neighbor Mapping
# Functions to determine which pieces are adjacent in hexagonal grid

#' Get neighbor piece ID for a given side of a hexagonal piece
#'
#' In a hexagonal honeycomb grid, each piece has up to 6 neighbors.
#' This function returns the piece ID of the neighbor on a specific side.
#'
#' @param piece_id Piece ID (1 to num_pieces)
#' @param side Side number (0-5, where 0 is right, counterclockwise)
#' @param rings Number of rings in puzzle
#' @return Neighbor piece ID, or NA if border edge
#'
#' @details
#' Hexagonal sides for flat-top orientation:
#' - Side 0: Right (0°)
#' - Side 1: Upper-right (60°)
#' - Side 2: Upper-left (120°)
#' - Side 3: Left (180°)
#' - Side 4: Lower-left (240°)
#' - Side 5: Lower-right (300°)
#'
#' @export
get_hex_neighbor <- function(piece_id, side, rings) {
  # Source topology if needed
  if (!exists("map_piece_id_to_ring")) {
    source("R/hexagonal_topology.R")
  }

  ring_info <- map_piece_id_to_ring(piece_id, rings)
  num_pieces <- 3 * rings * (rings - 1) + 1

  # CENTER PIECE (ring 0)
  if (ring_info$ring == 0) {
    # Center connects to all 6 pieces in ring 1 (pieces 2-7)
    # Side 0 → piece 2, side 1 → piece 3, etc.
    return(2 + side)
  }

  # OUTER RING (ring = rings - 1)
  if (ring_info$ring == rings - 1) {
    # Outer ring pieces have some border edges (no neighbors)
    neighbor <- get_inner_ring_neighbor(piece_id, side, ring_info, rings)
    if (is.na(neighbor)) {
      # This is a border edge
      return(NA)
    }
    return(neighbor)
  }

  # INNER RINGS (1 to rings-2)
  # Each piece has:
  # - 1 neighbor toward inner ring
  # - 2 neighbors in same ring (adjacent pieces)
  # - 3 neighbors toward outer ring

  neighbor <- get_inner_ring_neighbor(piece_id, side, ring_info, rings)
  return(neighbor)
}

#' Helper function to calculate neighbors for non-center pieces
#'
#' @keywords internal
get_inner_ring_neighbor <- function(piece_id, side, ring_info, rings) {
  ring <- ring_info$ring
  position <- ring_info$position
  pieces_in_ring <- 6 * ring

  # Calculate first piece ID in this ring
  # Ring 0: piece 1
  # Ring 1: starts at 2 (1 + 1)
  # Ring 2: starts at 8 (1 + 1 + 6)
  # Ring r: starts at 1 + 1 + sum(6*(1:(r-1)))
  if (ring == 0) {
    first_in_ring <- 1
  } else if (ring == 1) {
    first_in_ring <- 2
  } else {
    first_in_ring <- 1 + 1 + sum(6 * (1:(ring - 1)))
  }

  # For ring 1, pattern is simpler
  if (ring == 1) {
    # Ring 1 has 6 pieces (IDs 2-7)
    # Each piece connects to:
    # - Side 3 (left): center (piece 1)
    # - Side 2 (upper-left): previous piece in ring
    # - Side 4 (lower-left): next piece in ring
    # - Sides 0, 1, 5: ring 2 pieces (if exists)

    if (side == 3) {
      # Inner neighbor: center
      return(1)
    }

    if (side == 2) {
      # Previous in ring (counterclockwise)
      prev_pos <- (position - 1 + pieces_in_ring) %% pieces_in_ring
      return(first_in_ring + prev_pos)
    }

    if (side == 4) {
      # Next in ring (clockwise)
      next_pos <- (position + 1) %% pieces_in_ring
      return(first_in_ring + next_pos)
    }

    # Sides 0, 1, 5: outer ring
    if (rings == 2) {
      # Ring 1 is the outermost - these are border edges
      return(NA)
    }

    # Calculate outer ring neighbor
    # Each ring 1 piece connects to 3 ring 2 pieces
    first_in_ring_2 <- 8
    pieces_in_ring_2 <- 12

    # Mapping: piece 2 (pos 0) connects to pieces 8, 9, 19
    # Each ring-1 piece maps to 3 consecutive pieces in ring-2
    # But with offset based on position
    base_outer_pos <- position * 2

    if (side == 0) {
      # Right side - middle of 3 connections
      outer_pos <- (base_outer_pos + 1) %% pieces_in_ring_2
      return(first_in_ring_2 + outer_pos)
    } else if (side == 1) {
      # Upper-right - first of 3 connections
      outer_pos <- base_outer_pos %% pieces_in_ring_2
      return(first_in_ring_2 + outer_pos)
    } else if (side == 5) {
      # Lower-right - last of 3 connections
      outer_pos <- (base_outer_pos + 2) %% pieces_in_ring_2
      return(first_in_ring_2 + outer_pos)
    }
  }

  # For rings > 1 (ring 2 and beyond)
  # General pattern for any ring:
  # - 3 sides connect to inner ring
  # - 2 sides connect to same ring (adjacent pieces)
  # - 1 side is border (for outermost ring) or connects to outer ring

  # Same-ring neighbors (sides 2 and 4 always connect to adjacent pieces)
  if (side == 2) {
    # Previous in ring (counterclockwise)
    prev_pos <- (position - 1 + pieces_in_ring) %% pieces_in_ring
    return(first_in_ring + prev_pos)
  }

  if (side == 4) {
    # Next in ring (clockwise)
    next_pos <- (position + 1) %% pieces_in_ring
    return(first_in_ring + next_pos)
  }

  # Inner ring neighbors (sides 3, and two others depend on position)
  # Each ring has 6*ring pieces, and each piece in inner ring connects to 2 pieces in this ring
  # So this ring has 2x as many pieces as inner ring

  pieces_in_inner_ring <- 6 * (ring - 1)
  first_in_inner_ring <- 1 + sum(6 * (1:(ring - 2)))
  if (ring == 2) first_in_inner_ring <- 2

  # Position mapping: each inner ring piece connects to 2 outer ring pieces
  # So position 0,1 → inner 0; position 2,3 → inner 1, etc.
  inner_position <- floor(position / 2)
  inner_piece <- first_in_inner_ring + inner_position

  # Inner ring connections: use reverse lookup for symmetry
  # Check all pieces in inner ring to see which one connects to us
  opposite_side <- function(s) { (s + 3) %% 6 }

  for (test_inner_id in first_in_inner_ring:(first_in_inner_ring + pieces_in_inner_ring - 1)) {
    # Check all sides of the inner piece
    for (test_side in 0:5) {
      test_neighbor <- get_hex_neighbor(test_inner_id, test_side, rings)
      if (!is.na(test_neighbor) && test_neighbor == piece_id) {
        # Found! This inner piece connects to us on test_side
        # We connect back on the opposite side
        if (side == opposite_side(test_side)) {
          return(test_inner_id)
        }
      }
    }
  }

  # Side 0: outer ring or border
  if (side == 0) {
    if (ring == rings - 1) {
      # Outermost ring - border edge
      return(NA)
    } else {
      # Connect to outer ring
      # TODO: Implement outer ring connections
      return(NA)
    }
  }

  # Shouldn't reach here
  return(NA)
}

#' Build complete neighbor map for all pieces
#'
#' @param rings Number of rings
#' @return Data frame with piece_id, side, neighbor_id
#'
#' @export
build_hex_neighbor_map <- function(rings) {
  num_pieces <- 3 * rings * (rings - 1) + 1

  result <- data.frame(
    piece_id = integer(),
    side = integer(),
    neighbor_id = integer(),
    stringsAsFactors = FALSE
  )

  for (piece_id in 1:num_pieces) {
    for (side in 0:5) {
      neighbor_id <- get_hex_neighbor(piece_id, side, rings)

      result <- rbind(result, data.frame(
        piece_id = piece_id,
        side = side,
        neighbor_id = if (is.na(neighbor_id)) NA_integer_ else neighbor_id,
        stringsAsFactors = FALSE
      ))
    }
  }

  return(result)
}
