# Hexagonal Puzzle Topology Utilities
# Functions for mapping between piece IDs and hexagonal grid coordinates

#' Map piece ID to hexagonal ring and position
#'
#' Converts sequential piece IDs (1, 2, 3, ...) to hexagonal ring structure.
#' Hexagonal puzzles have a center piece (ring 0) surrounded by concentric rings.
#' Ring n contains 6n pieces arranged in a hexagonal pattern.
#'
#' @param piece_id Integer piece ID (1 to num_pieces)
#' @param rings Number of rings in the puzzle
#' @return Named list with ring (0 to rings-1), position (0 to 6*ring-1), and angle (radians)
#' @examples
#' # Get ring info for piece 5 in a 3-ring puzzle
#' info <- map_piece_id_to_ring(piece_id = 5, rings = 3)
#' # Returns: list(ring = 1, position = 3, angle = ...)
#'
#' # Map all pieces in a puzzle
#' num_pieces <- 3 * 3 * (3 - 1) + 1  # 19 pieces for 3 rings
#' all_info <- lapply(1:num_pieces, function(i) {
#'   map_piece_id_to_ring(i, rings = 3)
#' })
#' @export
map_piece_id_to_ring <- function(piece_id, rings) {
  num_pieces <- 3 * rings * (rings - 1) + 1

  if (piece_id < 1 || piece_id > num_pieces) {
    stop(sprintf("Piece ID %d out of range [1, %d] for %d rings",
                 piece_id, num_pieces, rings))
  }

  # Piece 1 is the center
  if (piece_id == 1) {
    return(list(ring = 0, position = 0, angle = 0))
  }

  # For other pieces, determine which ring
  cumulative <- 1  # Start with center piece
  for (r in 1:(rings - 1)) {
    pieces_in_ring <- 6 * r
    if (piece_id <= cumulative + pieces_in_ring) {
      # This piece is in ring r
      position_in_ring <- piece_id - cumulative - 1  # 0-indexed position
      angle <- (position_in_ring / pieces_in_ring) * 2 * pi

      return(list(ring = r, position = position_in_ring, angle = angle))
    }
    cumulative <- cumulative + pieces_in_ring
  }

  # Should not reach here
  stop(sprintf("Failed to map piece ID %d", piece_id))
}

#' Convert hexagonal ring position to cartesian coordinates
#'
#' Transforms ring number and angular position to cartesian (x, y) coordinates.
#' The center piece is at (0, 0). Pieces in each ring are evenly distributed
#' in a circular arrangement.
#'
#' @param ring Ring number (0 for center, 1+ for outer rings)
#' @param angle Angular position in radians (0 = right, pi/2 = up, etc.)
#' @param ring_spacing Distance between rings (default: 1.0)
#' @return Named list with x, y cartesian coordinates
#' @details
#' Uses polar-to-cartesian conversion:
#' - Center (ring 0): x = 0, y = 0
#' - Other rings: x = radius * cos(angle), y = radius * sin(angle)
#' - radius = ring * ring_spacing
#'
#' Note: Y-axis is inverted in SVG (positive Y is down), but we use
#' standard math convention here (positive Y is up). SVG transformations
#' are applied later if needed.
#' @examples
#' # Center hexagon
#' center <- hex_ring_to_cartesian(ring = 0, angle = 0, ring_spacing = 10)
#' # Returns: list(x = 0, y = 0)
#'
#' # First ring, position 0 (rightmost)
#' right <- hex_ring_to_cartesian(ring = 1, angle = 0, ring_spacing = 10)
#' # Returns: list(x = 10, y = 0)
#'
#' # First ring, position 60 degrees (upper-right)
#' upper_right <- hex_ring_to_cartesian(ring = 1, angle = pi/3, ring_spacing = 10)
#' # Returns: list(x = 5, y = 8.66)
#' @export
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
#' Determines the (x, y) position for a piece in separated layout.
#' Uses topology-based positioning with center at (0, 0). Combines
#' piece ID mapping and cartesian conversion in one convenient function.
#'
#' @param piece_id Integer piece ID
#' @param rings Number of rings in the puzzle
#' @param base_spacing Base spacing between rings
#' @param separation_factor Multiplier for separation distance (default: 1.0)
#' @return Named list with x, y coordinates for piece center
#' @details
#' The function:
#' 1. Maps piece_id to ring number and angular position
#' 2. Converts ring/angle to cartesian (x, y) using polar coordinates
#' 3. Applies separation_factor to scale the distance from center
#'
#' Since the center piece is at (0, 0), the cartesian coordinates naturally
#' represent direction vectors from the origin.
#' @examples
#' # Position for piece 1 (center) with 50mm spacing
#' pos1 <- calculate_hex_piece_position(
#'   piece_id = 1, rings = 3, base_spacing = 50
#' )
#' # Returns: list(x = 0, y = 0)
#'
#' # Position for piece 5 with 1.5x separation
#' pos5 <- calculate_hex_piece_position(
#'   piece_id = 5, rings = 3, base_spacing = 50, separation_factor = 1.5
#' )
#' @export
calculate_hex_piece_position <- function(piece_id, rings, base_spacing,
                                         separation_factor = 1.0) {
  # Step 1: Map piece ID to ring and angular position
  ring_info <- map_piece_id_to_ring(piece_id, rings)

  # Step 2: Convert to cartesian coordinates
  # Use base_spacing as the distance between rings
  cart_coords <- hex_ring_to_cartesian(
    ring = ring_info$ring,
    angle = ring_info$angle,
    ring_spacing = base_spacing
  )

  # Step 3: Apply separation factor
  # These coordinates ARE direction vectors from (0,0)
  # Scale by separation_factor to increase/decrease spacing
  x <- cart_coords$x * separation_factor
  y <- cart_coords$y * separation_factor

  return(list(x = x, y = y))
}
