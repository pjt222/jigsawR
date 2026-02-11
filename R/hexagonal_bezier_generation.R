# Hexagonal Bezier Curve Generation
# Direct generation approach for hexagonal puzzle pieces with tabs

#' Generate hexagonal edge with bezier curve and tab
#'
#' Creates a bezier curve with tab between two hexagon vertices.
#' Adapted from rectangular edge generation for hexagonal geometry.
#'
#' @param v1 Start vertex c(x, y)
#' @param v2 End vertex c(x, y)
#' @param seed Random seed
#' @param edge_id Unique edge identifier for deterministic tabs
#' @param tab_params List with tabsize and jitter parameters
#' @param min_tab_size Minimum absolute tab size in mm (default: NULL for no limit).
#'   Prevents tabs from becoming too small on short edges.
#' @param max_tab_size Maximum absolute tab size in mm (default: NULL for no limit).
#'   Prevents tabs from becoming too large on long edges.
#' @return List with forward and reverse SVG paths
#'
#' @details
#' Uses the same bezier curve pattern as rectangular puzzles:
#' - 3 cubic bezier curves (9 control points total)
#' - Tab in the middle section
#' - Deterministic based on seed and edge_id
#'
#' Tab size constraints use the formula: tab_height = 3 * t * edge_length
#' If min_tab_size would require a tab wider than 70% of the edge,
#' a straight line is returned instead.
#'
#' @examples
#' \dontrun{
#' v1 <- c(0, 0)
#' v2 <- c(10, 0)
#' edge <- generate_hex_bezier_edge(v1, v2, seed = 42, edge_id = 1,
#'                                   tab_params = list(tabsize = 6, jitter = 5))
#'
#' # With constraints
#' edge <- generate_hex_bezier_edge(v1, v2, seed = 42, edge_id = 1,
#'                                   tab_params = list(tabsize = 6, jitter = 5),
#'                                   min_tab_size = 2, max_tab_size = 8)
#' }
generate_hex_bezier_edge <- function(v1, v2, seed, edge_id,
                                      tab_params = list(tabsize = 6, jitter = 5),
                                      min_tab_size = NULL,
                                      max_tab_size = NULL) {

  # Calculate edge vector and length
  dx <- v2[1] - v1[1]
  dy <- v2[2] - v1[2]
  edge_length <- sqrt(dx^2 + dy^2)

  # Unit vectors along edge (tangent) and perpendicular (normal)
  tangent <- c(dx / edge_length, dy / edge_length)
  normal <- c(-tangent[2], tangent[1])  # Rotate 90° counterclockwise

  # Tab parameters (adapted from rectangular puzzle)
  tabsize <- tab_params$tabsize / 100  # Convert percentage to fraction
  jitter <- tab_params$jitter / 100

  # Generate 6 random values using unified batch RNG (Rcpp with R fallback)
  # Uses seed + edge_id for determinism - same edge always gets same tab shape
  rng_vals <- uniform_batch(seed + edge_id, 6)

  # Random tab parameters (same pattern as rectangular)
  t <- tabsize * (0.8 + 0.4 * rng_vals[1])    # Tab size
  a <- jitter * (rng_vals[2] - 0.5)            # Start jitter
  b <- jitter * (rng_vals[3] - 0.5)            # Tab position jitter
  c <- jitter * (rng_vals[4] - 0.5)            # Tab offset jitter
  d <- jitter * (rng_vals[5] - 0.5)            # Tab width jitter
  e <- jitter * (rng_vals[6] - 0.5)            # End jitter


  # Apply min/max tab size constraints
  # Tab height formula: tab_height = 3 * t * edge_length
  tab_height <- 3.0 * t * edge_length

  if (!is.null(min_tab_size) && tab_height < min_tab_size) {
    # Tab would be too small - scale up
    t <- min_tab_size / (3.0 * edge_length)

    # Check if this would make the tab too wide for the edge
    # Tab spans from 0.5 - 2t to 0.5 + 2t, so total width is 4t
    # It should fit within roughly 0.1 to 0.9 of the edge (leaving margins)
    if (4.0 * t > 0.7) {
      # Edge is too short for even a minimum tab - use straight line
      return(list(
        forward = sprintf("L %.2f %.2f", v2[1], v2[2]),
        reverse = sprintf("L %.2f %.2f", v1[1], v1[2]),
        start = v1,
        end = v2,
        type = "straight_constrained"
      ))
    }
  }

  if (!is.null(max_tab_size) && tab_height > max_tab_size) {
    # Tab would be too large - scale down
    t <- max_tab_size / (3.0 * edge_length)
  }

  # Helper function: position along edge (0 to 1)
  l <- function(frac) {
    v1 + tangent * (frac * edge_length)
  }

  # Helper function: perpendicular offset
  w <- function(offset) {
    normal * (offset * edge_length)
  }

  # Generate 9 control points (same pattern as rectangular)
  p1 <- l(0.2) + w(a)
  p2 <- l(0.5 + b + d) + w(-t + c)
  p3 <- l(0.5 - t + b) + w(t + c)
  p4 <- l(0.5 - 2.0 * t + b - d) + w(3.0 * t + c)
  p5 <- l(0.5 + 2.0 * t + b - d) + w(3.0 * t + c)
  p6 <- l(0.5 + t + b) + w(t + c)
  p7 <- l(0.5 + b + d) + w(-t + c)
  p8 <- l(0.8) + w(e)
  p9 <- l(1.0) + w(0.0)  # End point (should equal v2)

  # Build forward path (v1 to v2) - 3 cubic bezier curves
  forward <- sprintf("C %.2f %.2f %.2f %.2f %.2f %.2f ", p1[1], p1[2], p2[1], p2[2], p3[1], p3[2])
  forward <- paste0(forward, sprintf("C %.2f %.2f %.2f %.2f %.2f %.2f ", p4[1], p4[2], p5[1], p5[2], p6[1], p6[2]))
  forward <- paste0(forward, sprintf("C %.2f %.2f %.2f %.2f %.2f %.2f", p7[1], p7[2], p8[1], p8[2], p9[1], p9[2]))

  # Build reverse path (v2 to v1) - same curves reversed
  # For bezier P0 → P3 with control points P1, P2
  # Reverse is P3 → P0 with control points P2, P1
  reverse <- sprintf("C %.2f %.2f %.2f %.2f %.2f %.2f ", p8[1], p8[2], p7[1], p7[2], p6[1], p6[2])
  reverse <- paste0(reverse, sprintf("C %.2f %.2f %.2f %.2f %.2f %.2f ", p5[1], p5[2], p4[1], p4[2], p3[1], p3[2]))
  reverse <- paste0(reverse, sprintf("C %.2f %.2f %.2f %.2f %.2f %.2f", p2[1], p2[2], p1[1], p1[2], v1[1], v1[2]))

  return(list(
    forward = forward,
    reverse = reverse,
    start = v1,
    end = v2,
    type = "tab"
  ))
}

#' Generate complete hexagonal piece with bezier curves
#'
#' Creates a hexagonal puzzle piece with 6 edges, each having bezier curves with tabs.
#' All pieces use honeycomb orientation (flat-top, no rotation).
#'
#' @param piece_id Piece ID (1 to num_pieces)
#' @param rings Number of rings in puzzle
#' @param seed Random seed for deterministic generation
#' @param diameter Puzzle diameter in mm
#' @param tabsize Tab size percentage (10-30, default: 27)
#' @param jitter Jitter percentage (0-10, default: 5)
#' @param separated Generate separated layout (default: TRUE)
#' @param base_spacing Base spacing for separation (default: NULL, auto-calculated)
#' @param separation_factor Separation multiplier (default: 1.0)
#' @return List with piece SVG path and metadata
#'
#' @examples
#' \dontrun{
#' # Generate center piece
#' piece1 <- generate_hex_piece_bezier(1, rings = 3, seed = 42, diameter = 240)
#'
#' # Generate separated outer piece
#' piece8 <- generate_hex_piece_bezier(8, rings = 3, seed = 42, diameter = 240,
#'                                      separated = TRUE, base_spacing = 60)
#' }
generate_hex_piece_bezier <- function(piece_id, rings, seed, diameter = 240,
                                       tabsize = 6, jitter = 5,
                                       separated = TRUE,
                                       base_spacing = NULL,
                                       separation_factor = 1.0) {

  # Get topology information
  ring_info <- map_piece_id_to_ring(piece_id, rings)

  # Calculate piece position
  if (separated) {
    if (is.null(base_spacing)) {
      base_spacing <- diameter / (rings * 2)
    }
    position <- calculate_hex_piece_position(
      piece_id = piece_id,
      rings = rings,
      base_spacing = base_spacing,
      separation_factor = separation_factor
    )
  } else {
    # Connected layout - all at origin (edges connect them)
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

  # Calculate hexagon radius (approximate piece size)
  # Correct formula: diameter / (4 * rings - 2)
  piece_radius <- diameter / (4 * rings - 2)

  # Tab parameters
  tab_params <- list(tabsize = tabsize, jitter = jitter)

  # Generate 6 vertices for flat-top hexagon
  # Base offset π/6 for flat-top orientation
  base_offset <- pi / 6
  vertices <- list()
  for (i in 0:5) {
    angle <- i * pi / 3 + base_offset
    vx <- piece_radius * cos(angle)
    vy <- piece_radius * sin(angle)
    vertices[[i + 1]] <- c(vx, vy)
  }

  # Generate 6 edges with bezier curves
  edges <- list()
  for (side in 0:5) {
    v1 <- vertices[[side + 1]]
    v2 <- vertices[[(side + 1) %% 6 + 1]]  # Next vertex (wrap around)

    # Edge ID for deterministic tab generation
    # Use piece_id and side to ensure same edges get same tabs
    edge_id <- piece_id * 10 + side

    # Check if this is a border edge (no tab)
    is_border <- FALSE  # For now, all edges have tabs
    # TODO: Implement border detection for outer ring pieces

    if (is_border) {
      # Straight edge
      edges[[side + 1]] <- list(
        forward = sprintf("L %.2f %.2f", v2[1], v2[2]),
        reverse = sprintf("L %.2f %.2f", v1[1], v1[2]),
        start = v1,
        end = v2,
        type = "border"
      )
    } else {
      # Bezier edge with tab
      edges[[side + 1]] <- generate_hex_bezier_edge(
        v1 = v1,
        v2 = v2,
        seed = seed,
        edge_id = edge_id,
        tab_params = tab_params
      )
    }
  }

  # Assemble piece path
  # Start at first vertex (absolute coordinates with position offset)
  path <- sprintf("M %.2f %.2f ",
                  position$x + vertices[[1]][1],
                  position$y + vertices[[1]][2])

  # Add all 6 edges
  for (i in 1:6) {
    path <- paste0(path, edges[[i]]$forward, " ")
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
    type = piece_type,
    edges = edges
  ))
}

#' Generate all hexagonal pieces with bezier curves
#'
#' Convenience function to generate all pieces in a puzzle.
#'
#' @param rings Number of rings
#' @param seed Random seed
#' @param diameter Puzzle diameter (default: 240)
#' @param tabsize Tab size percentage (default: 27)
#' @param jitter Jitter percentage (default: 5)
#' @param separated Generate separated layout (default: TRUE)
#' @param base_spacing Base spacing for separation (default: NULL)
#' @param separation_factor Separation multiplier (default: 1.0)
#' @return List of piece objects
#'
#' @examples
#' \dontrun{
#' # Generate all pieces for 3-ring puzzle
#' pieces <- generate_all_hex_pieces_bezier(rings = 3, seed = 42)
#' }
generate_all_hex_pieces_bezier <- function(rings, seed, diameter = 240,
                                            tabsize = 6, jitter = 5,
                                            separated = TRUE,
                                            base_spacing = NULL,
                                            separation_factor = 1.0) {

  num_pieces <- 3 * rings * (rings - 1) + 1

  if (separated && is.null(base_spacing)) {
    base_spacing <- diameter / (rings * 2)
  }

  pieces <- list()
  for (i in 1:num_pieces) {
    pieces[[i]] <- generate_hex_piece_bezier(
      piece_id = i,
      rings = rings,
      seed = seed,
      diameter = diameter,
      tabsize = tabsize,
      jitter = jitter,
      separated = separated,
      base_spacing = base_spacing,
      separation_factor = separation_factor
    )
  }

  return(pieces)
}
