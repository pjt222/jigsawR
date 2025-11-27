# Hexagonal Edge Generation with Rotation Support
# Direct generation approach for hexagonal puzzle pieces

#' Generate hexagonal puzzle piece with rotation
#'
#' Creates a single hexagonal puzzle piece using direct generation approach.
#' Similar to rectangular puzzle but adapted for hexagonal grid topology.
#'
#' @param piece_id Piece ID (1 to num_pieces)
#' @param rings Total rings in the puzzle
#' @param seed Random seed for reproducibility
#' @param diameter Puzzle diameter in mm
#' @param tabsize Tab size percentage (default: 27)
#' @param jitter Jitter percentage (default: 5)
#' @param base_spacing Base spacing for separated pieces (optional)
#' @param separation_factor Separation multiplier (default: 1.0)
#' @return List with piece path, position, rotation, and metadata
#'
#' @details
#' This function implements the Hybrid Direct Generation approach:
#' 1. Uses topology utilities to determine piece position and rotation
#' 2. Generates edges directly (not extracted from monolithic SVG)
#' 3. Applies rotation during generation (not after)
#' 4. Ensures complementary edges between adjacent pieces
#'
#' @examples
#' # Generate center piece
#' piece1 <- generate_hex_piece(1, rings = 3, seed = 42, diameter = 240)
#'
#' # Generate outer piece with separation
#' piece5 <- generate_hex_piece(5, rings = 3, seed = 42, diameter = 240,
#'                              base_spacing = 60, separation_factor = 1.5)
#'
#' @export
generate_hex_piece <- function(piece_id, rings, seed, diameter = 240,
                               tabsize = 27, jitter = 5,
                               base_spacing = NULL, separation_factor = 1.0) {

  # Source dependencies if needed
  if (!exists("map_piece_id_to_ring")) {
    source("R/hexagonal_topology.R")
  }
  if (!exists("rotate_point")) {
    source("R/rotation_utils.R")
  }
  if (!exists("init_hex_jigsaw")) {
    source("R/hexagonal_puzzle.R")
  }

  # Initialize hexagonal puzzle environment
  init_hex_jigsaw(seed = seed, rings = rings, diameter = diameter,
                  tabsize = tabsize, jitter = jitter)

  # Get topology information
  ring_info <- map_piece_id_to_ring(piece_id, rings)

  # Calculate position (for separated layout)
  if (!is.null(base_spacing)) {
    position <- calculate_hex_piece_position(
      piece_id = piece_id,
      rings = rings,
      base_spacing = base_spacing,
      separation_factor = separation_factor
    )
  } else {
    # Connected layout - position from puzzle grid
    # This will be implemented based on hexagonal grid coordinate system
    position <- hex_piece_grid_position(piece_id, rings, diameter)
  }

  # Classify piece type
  piece_type <- classify_hex_piece(ring_info$ring, ring_info$position, rings)

  # Generate edges (6 edges for hexagonal piece)
  # For now, create placeholder - full implementation will follow
  edges <- generate_hex_piece_edges_placeholder(
    piece_id = piece_id,
    rings = rings,
    piece_type = piece_type,
    rotation = ring_info$angle,
    diameter = diameter
  )

  # Assemble piece path
  piece_path <- assemble_hex_piece_path_placeholder(edges, position)

  return(list(
    id = piece_id,
    ring = ring_info$ring,
    position_in_ring = ring_info$position,
    rotation = ring_info$angle,
    rotation_degrees = ring_info$angle * 180 / pi,
    center_x = position$x,
    center_y = position$y,
    path = piece_path,
    type = piece_type,
    edges = edges
  ))
}

#' Classify hexagonal piece type
#'
#' Determines if piece is center, inner, or edge piece.
#'
#' @param ring Ring number (0 = center)
#' @param position Position in ring
#' @param total_rings Total rings in puzzle
#' @return Character: "center", "inner", or "edge"
#'
#' @export
classify_hex_piece <- function(ring, position, total_rings) {
  if (ring == 0) {
    return("center")
  }
  if (ring == total_rings - 1) {
    return("edge")
  }
  return("inner")
}

#' Calculate hexagonal piece position in connected grid
#'
#' @param piece_id Piece ID
#' @param rings Number of rings
#' @param diameter Puzzle diameter
#' @return Named list with x, y coordinates
#'
#' @keywords internal
hex_piece_grid_position <- function(piece_id, rings, diameter) {
  # For connected puzzle, pieces are positioned based on hexagonal grid
  # This is a simplified calculation - full implementation will use
  # the coordinate transformation from hexagonal_puzzle.R

  ring_info <- map_piece_id_to_ring(piece_id, rings)

  if (ring_info$ring == 0) {
    # Center piece
    return(list(x = 0, y = 0))
  }

  # For other pieces, use polar coordinates
  radius <- (ring_info$ring / rings) * (diameter / 2)
  x <- radius * cos(ring_info$angle)
  y <- radius * sin(ring_info$angle)

  return(list(x = x, y = y))
}

#' Generate hexagonal piece edges (placeholder)
#'
#' Placeholder implementation that creates simple hexagon edges.
#' Full implementation will generate proper bezier curves with tabs.
#'
#' @param piece_id Piece ID
#' @param rings Number of rings
#' @param piece_type Type of piece
#' @param rotation Rotation angle in radians
#' @param diameter Puzzle diameter
#' @return List with 6 edges
#'
#' @keywords internal
generate_hex_piece_edges_placeholder <- function(piece_id, rings, piece_type,
                                                 rotation, diameter) {
  # Calculate approximate piece size
  piece_radius <- diameter / (rings * 4)

  # Generate 6 edges for hexagon (with rotation)
  edges <- list()
  for (side in 0:5) {
    # Calculate edge endpoints (regular hexagon vertices)
    angle1 <- side * pi / 3 + rotation
    angle2 <- (side + 1) * pi / 3 + rotation

    x1 <- piece_radius * cos(angle1)
    y1 <- piece_radius * sin(angle1)
    x2 <- piece_radius * cos(angle2)
    y2 <- piece_radius * sin(angle2)

    # For now, create simple straight line
    # Full implementation will use bezier curves with tabs
    edges[[side + 1]] <- list(
      start = c(x1, y1),
      end = c(x2, y2),
      forward = sprintf("L %.2f %.2f", x2, y2),
      reverse = sprintf("L %.2f %.2f", x1, y1),
      side = side,
      rotation = rotation
    )
  }

  return(edges)
}

#' Assemble hexagonal piece path (placeholder)
#'
#' @param edges List of 6 edges
#' @param position Piece center position
#' @return SVG path string
#'
#' @keywords internal
assemble_hex_piece_path_placeholder <- function(edges, position) {
  # Start path at first edge start point
  path <- sprintf("M %.2f %.2f",
                  position$x + edges[[1]]$start[1],
                  position$y + edges[[1]]$start[2])

  # Add all edges
  for (i in 1:6) {
    edge_path <- edges[[i]]$forward
    # Translate coordinates to piece position
    path <- paste0(path, " ", edge_path)
  }

  # Close path
  path <- paste0(path, " Z")

  return(path)
}

#' Generate multiple hexagonal pieces
#'
#' Convenience function to generate all pieces in a puzzle.
#'
#' @param rings Number of rings
#' @param seed Random seed
#' @param diameter Puzzle diameter
#' @param tabsize Tab size percentage
#' @param jitter Jitter percentage
#' @param separated Generate separated layout (default: FALSE)
#' @param base_spacing Base spacing for separation (default: NULL)
#' @param separation_factor Separation multiplier (default: 1.0)
#' @return List of piece objects
#'
#' @examples
#' # Generate connected puzzle
#' pieces <- generate_all_hex_pieces(rings = 3, seed = 42)
#'
#' # Generate separated puzzle
#' pieces <- generate_all_hex_pieces(rings = 3, seed = 42,
#'                                   separated = TRUE,
#'                                   base_spacing = 60)
#'
#' @export
generate_all_hex_pieces <- function(rings, seed, diameter = 240,
                                    tabsize = 27, jitter = 5,
                                    separated = FALSE,
                                    base_spacing = NULL,
                                    separation_factor = 1.0) {

  num_pieces <- 3 * rings * (rings - 1) + 1

  # Calculate default base spacing if separated
  if (separated && is.null(base_spacing)) {
    base_spacing <- diameter / (rings * 2)
  }

  pieces <- list()
  for (i in 1:num_pieces) {
    pieces[[i]] <- generate_hex_piece(
      piece_id = i,
      rings = rings,
      seed = seed,
      diameter = diameter,
      tabsize = tabsize,
      jitter = jitter,
      base_spacing = if (separated) base_spacing else NULL,
      separation_factor = separation_factor
    )
  }

  return(pieces)
}
