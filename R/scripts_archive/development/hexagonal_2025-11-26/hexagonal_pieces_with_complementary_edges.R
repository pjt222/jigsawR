# Hexagonal Puzzle Pieces with Complementary Edges
# Updated implementation using pre-generated edges for proper complementarity

#' Generate hexagonal puzzle pieces with complementary edges
#'
#' This function generates all pieces for a hexagonal puzzle using pre-generated
#' edges to ensure adjacent pieces fit together perfectly.
#'
#' @param rings Number of rings
#' @param seed Random seed
#' @param diameter Puzzle diameter (default: 240)
#' @param tabsize Tab size percentage (default: 27)
#' @param jitter Jitter percentage (default: 5)
#' @param separated Generate separated layout (default: TRUE)
#' @param base_spacing Base spacing for separation (default: NULL, auto-calculated)
#' @param separation_factor Separation multiplier (default: 1.0)
#' @return List of piece objects with complementary edges
#'
#' @export
generate_hex_pieces_with_edges <- function(rings, seed, diameter = 240,
                                           tabsize = 27, jitter = 5,
                                           separated = TRUE,
                                           base_spacing = NULL,
                                           separation_factor = 1.0) {
  # Source dependencies
  if (!exists("map_piece_id_to_ring")) {
    source("R/hexagonal_topology.R")
  }
  if (!exists("generate_hex_edges_with_complementarity")) {
    source("R/hexagonal_edge_pregeneration.R")
  }

  # Pre-generate all edges
  cat("Pre-generating all edges for complementarity...\n")
  edges <- generate_hex_edges_with_complementarity(
    rings = rings,
    seed = seed,
    diameter = diameter,
    tabsize = tabsize,
    jitter = jitter
  )
  cat(sprintf("Generated %d edge entries\n", length(edges)))

  # Calculate spacing
  if (separated && is.null(base_spacing)) {
    base_spacing <- diameter / (rings * 2)
  }

  # Generate all pieces
  num_pieces <- 3 * rings * (rings - 1) + 1
  pieces <- list()

  for (piece_id in 1:num_pieces) {
    pieces[[piece_id]] <- build_hex_piece_from_edges(
      piece_id = piece_id,
      rings = rings,
      edges = edges,
      separated = separated,
      base_spacing = base_spacing,
      separation_factor = separation_factor
    )
  }

  return(pieces)
}

#' Build a single hexagonal piece from pre-generated edges
#'
#' @param piece_id Piece ID
#' @param rings Number of rings
#' @param edges Pre-generated edge list
#' @param separated Use separated layout
#' @param base_spacing Base spacing
#' @param separation_factor Separation multiplier
#' @return Piece object
#'
#' @keywords internal
build_hex_piece_from_edges <- function(piece_id, rings, edges,
                                        separated, base_spacing, separation_factor) {
  # Source dependencies
  if (!exists("map_piece_id_to_ring")) {
    source("R/hexagonal_topology.R")
  }

  # Get topology information
  ring_info <- map_piece_id_to_ring(piece_id, rings)

  # Calculate piece position
  if (separated) {
    position <- calculate_hex_piece_position(
      piece_id = piece_id,
      rings = rings,
      base_spacing = base_spacing,
      separation_factor = separation_factor
    )
  } else {
    position <- list(x = 0, y = 0)
  }

  # Classify piece type
  piece_type <- if (ring_info$ring == 0) {
    "center"
  } else if (ring_info$ring == rings - 1) {
    "edge"
  } else {
    "inner"
  }

  # Get starting vertex (will be adjusted by position)
  # For now, use the first edge's start point
  first_edge_key <- sprintf("%d-0", piece_id)
  first_edge <- edges[[first_edge_key]]
  start_point <- first_edge$start

  # Build piece path by assembling 6 edges
  path <- sprintf("M %.2f %.2f ", position$x + start_point[1], position$y + start_point[2])

  # Add all 6 edges in order
  for (side in 0:5) {
    edge_key <- sprintf("%d-%d", piece_id, side)
    edge <- edges[[edge_key]]

    # Use forward path (the edge is already oriented correctly for this piece)
    path <- paste0(path, edge$forward, " ")
  }

  # Close path
  path <- paste0(path, "Z")

  return(list(
    id = piece_id,
    ring = ring_info$ring,
    position_in_ring = ring_info$position,
    center_x = position$x,
    center_y = position$y,
    path = path,
    type = piece_type
  ))
}
