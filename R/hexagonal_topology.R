# Hexagonal Puzzle Topology Utilities
# Functions for mapping between piece IDs and hexagonal grid coordinates
# Also includes warp transformation for circular puzzle boundaries

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

#' Apply circular warp transformation to a point
#'
#' Transforms a point from hexagonal boundary space to circular boundary space.
#' Points on the hexagonal boundary are mapped to points on a circle of the same
#' "radius" (distance from center along the hexagonal boundary direction).
#'
#' This is the inverse of what the original hex_warp does - it normalizes
#' the radial distance so that hexagonal boundary becomes circular.
#'
#' @param x X coordinate (relative to puzzle center at origin)
#' @param y Y coordinate (relative to puzzle center at origin)
#' @return Named list with warped x, y coordinates
#' @details
#' The transformation matches the original hex_warp() from hexagonal_puzzle.R:
#' 1. Calculates the angle from center to point (with +pi offset)
#' 2. Determines the scale factor l at that angle
#' 3. DIVIDES coordinates by l to push edge midpoints outward
#'
#' For a point at angle θ, the scale factor is:
#'   l = sqrt(0.75) / cos(|30° - ((θ+180°) mod 60°)|)
#'
#' The warped coordinates are: (x/l, y/l)
#'
#' This maps a hexagonal grid onto a circle by:
#' - Keeping corners at same distance (l=1 at corners)
#' - Pushing edge midpoints outward (l<1 means divide increases distance)
#'
#' @examples
#' # At 0 degrees (hexagon edge midpoint): no change
#' edge_mid <- apply_hex_warp(50, 0)  # Returns same point
#'
#' # At 30 degrees (between corner and edge): pushed outward
#' between <- apply_hex_warp(43.3, 25)
#' @export
apply_hex_warp <- function(x, y) {

  # Handle origin case
  if (x == 0 && y == 0) {
    return(list(x = 0, y = 0))
  }

  # EXACT formula from hexagonal_puzzle.R hex_warp function:
  # angl <- atan2(vec$y, vec$x) + pi
  # angl60 <- angl %% (pi / 3)
  # angl30 <- abs((pi / 6) - angl60)
  # l <- sqrt(0.75) / cos(angl30)
  # return(list(x = vec$x / l, y = vec$y / l))

  angl <- atan2(y, x) + pi
  angl60 <- angl %% (pi / 3)
  angl30 <- abs((pi / 6) - angl60)
  l <- sqrt(0.75) / cos(angl30)

  # DIVIDE by l (not multiply!) to match original behavior
  # This pushes edge midpoints outward to create circular shape
  return(list(
    x = x / l,
    y = y / l
  ))
}

#' Apply hexagonal truncation to boundary vertex
#'
#' Projects a boundary vertex onto a regular hexagon boundary.
#' Creates a clean hexagonal outline matching the complete mode do_trunc behavior.
#'
#' For a pointy-top hexagon (matching complete mode):
#' - Corners (vertices) at 0, 60, 120, 180, 240, 300 degrees
#' - Edge midpoints at 30, 90, 150, 210, 270, 330 degrees
#'
#' The hexagon has corner distance = target_radius and
#' edge midpoint distance = target_radius * sqrt(0.75).
#'
#' @param x X coordinate of the vertex
#' @param y Y coordinate of the vertex
#' @param target_radius The radius of the truncation hexagon (distance to corners)
#' @return List with x and y coordinates projected onto hexagon boundary
#' @export
apply_hex_trunc <- function(x, y, target_radius) {

  # Handle origin case
  if (x == 0 && y == 0) {
    return(list(x = 0, y = 0))
  }

  # Require target_radius
  if (is.null(target_radius)) {
    stop("target_radius is required for apply_hex_trunc")
  }

  # Current distance from center
  current_dist <- sqrt(x^2 + y^2)

  # Calculate angle from center (in radians)
  # NO offset - we want a pointy-top hexagon with corners at 0, 60, 120...
  angl <- atan2(y, x)

  # Normalize to positive angle [0, 2*pi)
  if (angl < 0) angl <- angl + 2 * pi

  # Find the 60-degree sector and offset within it
  # For pointy-top hex: corners at 0, 60, 120... so sector boundaries at 30, 90, 150...
  angl60 <- angl %% (pi / 3)  # Position within 60-degree sector
  angl30 <- abs((pi / 6) - angl60)  # Distance from sector midpoint (edge midpoint direction)

  # Calculate hexagon boundary distance at this angle
  # For a regular hexagon with corners at distance R:
  # - At corners (angl30 = 30°): hex_dist = R
  # - At edge midpoints (angl30 = 0°): hex_dist = R * sqrt(0.75)
  #
  # Formula: hex_dist = R * sqrt(0.75) / cos(angl30)
  # At 0° offset from corner: angl30 = pi/6, cos(pi/6) = sqrt(0.75), so hex_dist = R
  # At 30° offset (edge midpoint): angl30 = 0, cos(0) = 1, so hex_dist = R * sqrt(0.75)
  hex_boundary_dist <- target_radius * sqrt(0.75) / cos(angl30)

  # Scale the current point to lie on the hexagon boundary
  scale <- hex_boundary_dist / current_dist

  return(list(
    x = x * scale,
    y = y * scale
  ))
}

#' Check if a vertex is on the outer boundary of the puzzle
#'
#' Determines if a vertex position is on the outermost ring boundary.
#' Used to identify which vertices need warp transformation.
#'
#' @param x X coordinate
#' @param y Y coordinate
#' @param puzzle_radius The outer radius of the puzzle (diameter/2)
#' @param tolerance Tolerance for floating point comparison (default: 0.01)
#' @return TRUE if vertex is on outer boundary, FALSE otherwise
#' @export
is_boundary_vertex <- function(x, y, puzzle_radius, tolerance = 0.01) {
  # Calculate distance from center

  dist <- sqrt(x^2 + y^2)

  # For a hexagonal puzzle, boundary vertices are at distance ~ puzzle_radius

  # Account for the hexagonal shape (corners are further than edges)
  # Maximum distance is at corners: puzzle_radius
  # Minimum distance is at edge midpoints: puzzle_radius * sqrt(0.75)

  min_boundary_dist <- puzzle_radius * sqrt(0.75) - tolerance
  max_boundary_dist <- puzzle_radius + tolerance


  return(dist >= min_boundary_dist && dist <= max_boundary_dist)
}

#' Generate SVG arc command for a circular border edge
#'
#' Creates an SVG arc path segment between two points on a circle.
#'
#' @param x1 Start X coordinate
#' @param y1 Start Y coordinate
#' @param x2 End X coordinate
#' @param y2 End Y coordinate
#' @param radius Circle radius
#' @param large_arc Use large arc (1) or small arc (0), default 0
#' @param sweep Sweep direction: 1 for clockwise, 0 for counter-clockwise
#' @return SVG arc command string "A rx ry rotation large-arc sweep x y"
#' @export
svg_arc_command <- function(x1, y1, x2, y2, radius, large_arc = 0, sweep = 1) {
  # SVG arc: A rx ry x-axis-rotation large-arc-flag sweep-flag x y
  # For a circle: rx = ry = radius, rotation = 0
  sprintf("A %.2f %.2f 0 %d %d %.2f %.2f",
          radius, radius, large_arc, sweep, x2, y2)
}
