# Concentric Ring Geometry for Hexagonal Puzzles
#
# Provides geometry calculations for concentric ring mode where:
# - Pieces have constant radial height
# - Pieces get wider toward the outside (trapezoidal shape)
# - Center piece can be hexagon or circle

#' Calculate the number of pieces for a concentric ring puzzle
#'
#' @param rings Number of rings (including center)
#' @return Total number of pieces
#' @export
get_concentric_piece_count <- function(rings) {
  # Same formula as hexagonal: 1 + 6 + 12 + 18 + ... = 3*n*(n-1) + 1
  3 * rings * (rings - 1) + 1
}

#' Calculate piece height (radial extent) for concentric mode
#'
#' @param diameter Puzzle diameter
#' @param rings Number of rings
#' @return Piece height in same units as diameter
#' @export
get_concentric_piece_height <- function(diameter, rings) {

  diameter / (2 * rings)
}

#' Map piece ID to ring and position within ring
#'
#' @param piece_id Piece ID (1-based)
#' @param rings Total number of rings
#' @return List with ring (0-based) and position (0-based within ring)
#' @export
map_concentric_piece_id <- function(piece_id, rings) {
  if (piece_id == 1) {
    return(list(ring = 0, position = 0, pieces_in_ring = 1))
  }

  # Find which ring this piece is in
  cumulative <- 1
  for (ring in 1:(rings - 1)) {
    pieces_in_ring <- 6 * ring
    if (piece_id <= cumulative + pieces_in_ring) {
      position <- piece_id - cumulative - 1
      return(list(ring = ring, position = position, pieces_in_ring = pieces_in_ring))
    }
    cumulative <- cumulative + pieces_in_ring
  }

  stop(sprintf("piece_id %d out of range for %d rings", piece_id, rings))
}

#' Calculate vertices for a concentric piece
#'
#' @param piece_id Piece ID (1-based)
#' @param rings Total number of rings
#' @param diameter Puzzle diameter
#' @param center_shape "hexagon" or "circle" (only affects center piece)
#' @return List with type ("hexagon", "circle", or "trapezoid") and vertices
#' @export
calculate_concentric_vertices <- function(piece_id, rings, diameter,
                                          center_shape = "hexagon") {
  piece_height <- get_concentric_piece_height(diameter, rings)
  info <- map_concentric_piece_id(piece_id, rings)

  if (info$ring == 0) {
    # Center piece
    if (center_shape == "circle") {
      # Circle: return center and radius (special handling in path generation)
      return(list(
        type = "circle",
        center = c(0, 0),
        radius = piece_height,
        piece_id = piece_id,
        ring = 0,
        position = 0
      ))
    } else {
      # Hexagon: 6 vertices
      vertices <- list()
      for (i in 0:5) {
        angle <- i * pi / 3
        vertices[[i + 1]] <- c(
          piece_height * cos(angle),
          piece_height * sin(angle)
        )
      }
      return(list(
        type = "hexagon",
        vertices = vertices,
        center = c(0, 0),
        piece_id = piece_id,
        ring = 0,
        position = 0
      ))
    }
  }

  # Outer rings - trapezoid with 4 vertices
  ring <- info$ring
  position <- info$position
  pieces_in_ring <- info$pieces_in_ring
  arc_angle <- 2 * pi / pieces_in_ring

  # Start and end angles for this piece
  start_angle <- position * arc_angle
  end_angle <- (position + 1) * arc_angle

  inner_radius <- ring * piece_height
  outer_radius <- (ring + 1) * piece_height

  # 4 vertices in clockwise order (for SVG path):
  # V1: inner-start (bottom-right of piece when viewed radially)
  # V2: inner-end (bottom-left)
  # V3: outer-end (top-left)
  # V4: outer-start (top-right)
  vertices <- list(
    c(inner_radius * cos(start_angle), inner_radius * sin(start_angle)),
    c(inner_radius * cos(end_angle), inner_radius * sin(end_angle)),
    c(outer_radius * cos(end_angle), outer_radius * sin(end_angle)),
    c(outer_radius * cos(start_angle), outer_radius * sin(start_angle))
  )

  # Calculate center for positioning
  mid_radius <- (inner_radius + outer_radius) / 2
  mid_angle <- (start_angle + end_angle) / 2
  center <- c(mid_radius * cos(mid_angle), mid_radius * sin(mid_angle))

  return(list(
    type = "trapezoid",
    vertices = vertices,
    center = center,
    piece_id = piece_id,
    ring = ring,
    position = position,
    inner_radius = inner_radius,
    outer_radius = outer_radius,
    start_angle = start_angle,
    end_angle = end_angle
  ))
}

#' Get neighbor information for a concentric piece edge
#'
#' @param piece_id Piece ID
#' @param edge_index Edge index (1=inner, 2=right, 3=outer, 4=left)
#' @param rings Total rings
#' @return List with neighbor_id (NA if boundary) and neighbor_edge
#' @export
get_concentric_neighbor <- function(piece_id, edge_index, rings) {
  info <- map_concentric_piece_id(piece_id, rings)
  ring <- info$ring
  position <- info$position
  pieces_in_ring <- info$pieces_in_ring

  # Center piece (ring 0) is special

  if (ring == 0) {
    # Center piece has 6 edges connecting to ring 1
    # Each edge connects to one piece in ring 1
    if (edge_index >= 1 && edge_index <= 6) {
      neighbor_id <- 1 + edge_index  # Pieces 2-7 are ring 1
      return(list(neighbor_id = neighbor_id, neighbor_edge = 1, is_boundary = FALSE))
    }
    return(list(neighbor_id = NA, neighbor_edge = NA, is_boundary = TRUE))
  }

  # Edge indices for trapezoid:
  # 1 = INNER (connects to inner ring)
  # 2 = RIGHT (connects to next piece in same ring)
  # 3 = OUTER (connects to outer ring or boundary)
  # 4 = LEFT (connects to previous piece in same ring)

  if (edge_index == 1) {
    # INNER edge - connects to inner ring
    if (ring == 1) {
      # Connects to center piece
      # Determine which edge of center hexagon
      center_edge <- position + 1
      return(list(neighbor_id = 1, neighbor_edge = center_edge, is_boundary = FALSE))
    } else {
      # Connects to piece in ring-1
      # Ring r has 6*r pieces, ring r-1 has 6*(r-1) pieces
      # Need to map position in ring r to position in ring r-1
      inner_pieces <- 6 * (ring - 1)
      # Each inner piece connects to multiple outer pieces
      # Position mapping: inner_pos = floor(position * inner_pieces / pieces_in_ring)
      inner_pos <- floor(position * inner_pieces / pieces_in_ring)
      inner_piece_start <- 3 * (ring - 1) * (ring - 2) + 2  # First piece of inner ring
      neighbor_id <- inner_piece_start + inner_pos
      return(list(neighbor_id = neighbor_id, neighbor_edge = 3, is_boundary = FALSE))
    }
  }

  if (edge_index == 2) {
    # RIGHT edge - connects to next piece in same ring (circumferential)
    next_pos <- (position + 1) %% pieces_in_ring
    ring_start <- 3 * ring * (ring - 1) + 2
    neighbor_id <- ring_start + next_pos
    return(list(neighbor_id = neighbor_id, neighbor_edge = 4, is_boundary = FALSE))
  }

  if (edge_index == 3) {
    # OUTER edge - connects to outer ring or is boundary
    if (ring == rings - 1) {
      # Outermost ring - boundary
      return(list(neighbor_id = NA, neighbor_edge = NA, is_boundary = TRUE))
    } else {
      # Connects to piece in ring+1
      outer_pieces <- 6 * (ring + 1)
      # Position mapping: find which outer piece(s) this inner edge connects to
      # For simplicity, connect to the corresponding position ratio
      outer_pos <- floor(position * outer_pieces / pieces_in_ring)
      outer_piece_start <- 3 * (ring + 1) * ring + 2  # First piece of outer ring
      neighbor_id <- outer_piece_start + outer_pos
      return(list(neighbor_id = neighbor_id, neighbor_edge = 1, is_boundary = FALSE))
    }
  }

  if (edge_index == 4) {
    # LEFT edge - connects to previous piece in same ring (circumferential)
    prev_pos <- (position - 1 + pieces_in_ring) %% pieces_in_ring
    ring_start <- 3 * ring * (ring - 1) + 2
    neighbor_id <- ring_start + prev_pos
    return(list(neighbor_id = neighbor_id, neighbor_edge = 2, is_boundary = FALSE))
  }

  return(list(neighbor_id = NA, neighbor_edge = NA, is_boundary = TRUE))
}

#' Get all vertices for all pieces in a concentric puzzle
#'
#' @param rings Number of rings
#' @param diameter Puzzle diameter
#' @param center_shape "hexagon" or "circle"
#' @return List of piece vertex data
#' @export
get_all_concentric_vertices <- function(rings, diameter, center_shape = "hexagon") {
  num_pieces <- get_concentric_piece_count(rings)
  pieces <- list()

  for (piece_id in 1:num_pieces) {
    pieces[[piece_id]] <- calculate_concentric_vertices(
      piece_id, rings, diameter, center_shape
    )
  }

  return(pieces)
}
