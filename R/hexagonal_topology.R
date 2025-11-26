# Hexagonal Puzzle Topology Utilities
# Functions for mapping between piece IDs and hexagonal grid coordinates

#' Map piece ID to hexagonal axial coordinates
#'
#' Converts sequential piece IDs (1, 2, 3, ...) to axial hex grid coordinates (q, r).
#' Uses a honeycomb structure with center piece at (0,0) and concentric rings.
#'
#' Piece numbering:
#' - Piece 1: Center (ring 0)
#' - Pieces 2-7: Ring 1 (6 pieces, starting from East and going counter-clockwise)
#' - Pieces 8-19: Ring 2 (12 pieces)
#' - etc.
#'
#' @param piece_id Integer piece ID (1 to num_pieces)
#' @param rings Number of rings in the puzzle
#' @return Named list with q, r (axial coordinates), ring, and position
#' @examples
#' # Get coordinates for piece 5 in a 3-ring puzzle
#' info <- map_piece_id_to_axial(piece_id = 5, rings = 3)
#' # Returns: list(q = ..., r = ..., ring = 1, position = 3)
#'
#' # Map all pieces in a puzzle
#' num_pieces <- 3 * 3 * (3 - 1) + 1  # 19 pieces for 3 rings
#' all_info <- lapply(1:num_pieces, function(i) {
#'   map_piece_id_to_axial(i, rings = 3)
#' })
#' @export
map_piece_id_to_axial <- function(piece_id, rings) {
  num_pieces <- 3 * rings * (rings - 1) + 1

  if (piece_id < 1 || piece_id > num_pieces) {
    stop(sprintf("Piece ID %d out of range [1, %d] for %d rings",
                 piece_id, num_pieces, rings))
  }

  # Piece 1 is the center
  if (piece_id == 1) {
    return(list(q = 0, r = 0, ring = 0, position = 0))
  }

  # Determine which ring this piece is in
  cumulative <- 1  # Start with center piece
  for (ring in 1:(rings - 1)) {
    pieces_in_ring <- 6 * ring
    if (piece_id <= cumulative + pieces_in_ring) {
      # This piece is in this ring
      position_in_ring <- piece_id - cumulative - 1  # 0-indexed position

      # Convert position to axial coordinates using hex ring spiral
      # Based on Red Blob Games hex ring algorithm:
      # 1. Start at SW corner: (-ring, ring) in axial coordinates
      # 2. For each of 6 directions, output current position then move
      # 3. Each direction is traversed for `ring` steps

      # The 6 axial direction vectors
      directions <- list(
        c(1, 0),   # 0: E
        c(1, -1),  # 1: NE
        c(0, -1),  # 2: NW
        c(-1, 0),  # 3: W
        c(-1, 1),  # 4: SW
        c(0, 1)    # 5: SE
      )

      # Start at SW corner (direction 4 from center)
      q <- -ring
      r <- ring

      # The hex_ring algorithm: for each direction, output current hex then step
      # Position 0 is the starting position (no movement)
      if (position_in_ring > 0) {
        # Calculate how many complete edges we've passed
        positions_remaining <- position_in_ring

        for (dir_idx in 1:6) {
          if (positions_remaining <= ring) {
            # We're on this edge
            dir <- directions[[dir_idx]]
            q <- q + positions_remaining * dir[1]
            r <- r + positions_remaining * dir[2]
            break
          }
          # Complete this entire edge
          dir <- directions[[dir_idx]]
          q <- q + ring * dir[1]
          r <- r + ring * dir[2]
          positions_remaining <- positions_remaining - ring
        }
      }

      return(list(
        q = q,
        r = r,
        ring = ring,
        position = position_in_ring
      ))
    }
    cumulative <- cumulative + pieces_in_ring
  }

  # Should not reach here
  stop(sprintf("Failed to map piece ID %d", piece_id))
}

#' Map piece ID to hexagonal ring and position (DEPRECATED)
#'
#' This function uses polar coordinates which don't create proper hexagonal tiling.
#' Use map_piece_id_to_axial() instead.
#'
#' @param piece_id Integer piece ID (1 to num_pieces)
#' @param rings Number of rings in the puzzle
#' @return Named list with ring (0 to rings-1), position (0 to 6*ring-1), and angle (radians)
#' @keywords internal
map_piece_id_to_ring <- function(piece_id, rings) {
  # Get axial coordinates
  axial <- map_piece_id_to_axial(piece_id, rings)

  # Return ring and position info
  return(list(
    ring = axial$ring,
    position = axial$position,
    angle = (axial$position / max(1, 6 * axial$ring)) * 2 * pi
  ))
}

#' Convert hexagonal axial coordinates to cartesian coordinates
#'
#' Transforms axial hex grid coordinates (q, r) to cartesian (x, y) coordinates.
#' This creates a proper hexagonal lattice where adjacent hexagons share vertices.
#'
#' The conversion uses the flat-top hexagon orientation:
#' - x = size * (3/2 * q)
#' - y = size * (sqrt(3)/2 * q + sqrt(3) * r)
#'
#' @param q Axial q coordinate
#' @param r Axial r coordinate
#' @param hex_size Size of a hexagon (distance from center to vertex)
#' @return Named list with x, y cartesian coordinates
#' @details
#' For flat-top hexagons with pointy vertices on left/right:
#' - Hexagon width = 2 * hex_size
#' - Hexagon height = sqrt(3) * hex_size
#' - Horizontal spacing = 3/2 * hex_size
#' - Vertical spacing = sqrt(3) * hex_size
#'
#' The center hexagon (q=0, r=0) is at (0, 0).
#' @examples
#' # Center hexagon
#' center <- axial_to_cartesian(q = 0, r = 0, hex_size = 10)
#' # Returns: list(x = 0, y = 0)
#'
#' # Hexagon to the right (q=1, r=0)
#' right <- axial_to_cartesian(q = 1, r = 0, hex_size = 10)
#' # Returns: list(x = 15, y = 0)
#'
#' # Hexagon below-right (q=1, r=-1)
#' below_right <- axial_to_cartesian(q = 1, r = -1, hex_size = 10)
#' # Returns: list(x = 15, y = -17.32)
#' @export
axial_to_cartesian <- function(q, r, hex_size) {
  x <- hex_size * (3/2 * q)
  y <- hex_size * (sqrt(3)/2 * q + sqrt(3) * r)

  return(list(x = x, y = y))
}

#' Convert hexagonal ring position to cartesian coordinates (DEPRECATED)
#'
#' This function uses polar coordinates which don't create proper hexagonal tiling.
#' Use axial_to_cartesian() instead.
#'
#' @param ring Ring number (0 for center, 1+ for outer rings)
#' @param angle Angular position in radians (0 = right, pi/2 = up, etc.)
#' @param ring_spacing Distance between rings (default: 1.0)
#' @return Named list with x, y cartesian coordinates
#' @keywords internal
hex_ring_to_cartesian <- function(ring, angle, ring_spacing = 1.0) {
  # Center piece is at origin
  if (ring == 0) {
    return(list(x = 0, y = 0))
  }

  # Polar to cartesian conversion
  radius <- ring * ring_spacing
  x <- radius * cos(angle)
  y <- radius * sin(angle)

  return(list(x = x, y = y))
}

#' Calculate position for a hexagonal puzzle piece
#'
#' Determines the (x, y) position for a piece in separated layout using
#' proper hexagonal lattice coordinates. This ensures adjacent pieces
#' share vertices and maintain hexagonal tiling structure.
#'
#' @param piece_id Integer piece ID
#' @param rings Number of rings in the puzzle
#' @param piece_radius Radius of each hexagonal piece (distance from center to vertex)
#' @param base_spacing (Deprecated) Old parameter name for piece_radius, kept for backward compatibility
#' @param separation_factor Multiplier for separation distance (default: 1.0)
#' @return Named list with x, y coordinates for piece center
#' @details
#' The function:
#' 1. Maps piece_id to axial coordinates (q, r)
#' 2. Converts axial to cartesian (x, y) using hexagonal lattice formulas
#' 3. Applies separation_factor to scale the distance from center
#'
#' This creates a proper hexagonal tiling where:
#' - Adjacent pieces share exactly 2 vertices
#' - All pieces maintain hexagonal shape orientation
#' - Center piece is at (0, 0)
#'
#' @examples
#' # Position for piece 1 (center) with 20mm piece radius
#' pos1 <- calculate_hex_piece_position(
#'   piece_id = 1, rings = 3, piece_radius = 20
#' )
#' # Returns: list(x = 0, y = 0)
#'
#' # Position for piece 5 with 1.5x separation
#' pos5 <- calculate_hex_piece_position(
#'   piece_id = 5, rings = 3, piece_radius = 20, separation_factor = 1.5
#' )
#' @export
calculate_hex_piece_position <- function(piece_id, rings, piece_radius = NULL,
                                         base_spacing = NULL,
                                         separation_factor = 1.0) {
  # Handle backward compatibility: base_spacing was the old parameter name
  if (is.null(piece_radius) && !is.null(base_spacing)) {
    piece_radius <- base_spacing
  }

  if (is.null(piece_radius)) {
    stop("Either 'piece_radius' or 'base_spacing' must be provided")
  }

  # Step 1: Map piece ID to axial coordinates
  axial_coords <- map_piece_id_to_axial(piece_id, rings)

  # Step 2: Convert to cartesian coordinates using hexagonal lattice
  # For a compact hexagonal grid, pieces touch when hex_size = piece_radius
  # The spacing between hex centers depends on piece_radius
  cart_coords <- axial_to_cartesian(
    q = axial_coords$q,
    r = axial_coords$r,
    hex_size = piece_radius
  )

  # Step 3: Apply separation factor
  # Scale coordinates to create separation between pieces
  x <- cart_coords$x * separation_factor
  y <- cart_coords$y * separation_factor

  return(list(
    x = x,
    y = y,
    q = axial_coords$q,
    r = axial_coords$r,
    ring = axial_coords$ring
  ))
}
