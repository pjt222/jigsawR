# Concentric Ring Edge Generation
#
# Generates edges with bezier tabs for concentric ring puzzles
#
# KEY INSIGHT: The number of pieces per ring increases as we go outward:
# - Ring 0: 1 piece (center)
# - Ring 1: 6 pieces (each spans 60°)
# - Ring 2: 12 pieces (each spans 30°)
# - Ring 3: 18 pieces (each spans 20°)
# etc.
#
# This means that:
# - Each ring r piece's OUTER edge connects to multiple ring r+1 pieces
# - Each ring r piece's INNER edge connects to one or more ring r-1 pieces
# - The edge must be SUBDIVIDED at the junction points to create proper interlocking tabs
#
# LIMITATION: Currently works correctly for 2-3 rings only.
# For 4+ rings, the ring boundaries don't align (e.g., ring 2 has 30° pieces,
# ring 3 has 20° pieces - their corners only align every 60°).
# This causes some edge mismatches in ring 2+ transitions.

#' Get all outer ring piece IDs that share an edge with an inner ring piece
#'
#' @param piece_id The inner piece ID
#' @param rings Total number of rings
#' @return Vector of outer piece IDs that share the OUTER edge
#' @keywords internal
get_outer_neighbors <- function(piece_id, rings) {
  info <- map_concentric_piece_id(piece_id, rings)
  ring <- info$ring
  position <- info$position

  # Outermost ring has no outer neighbors
  if (ring >= rings - 1) {
    return(integer(0))
  }

  # Calculate angular range of this piece
  if (ring == 0) {
    # Center piece - connects to all 6 ring 1 pieces via its 6 edges
    return(2:7)  # Pieces 2-7 are ring 1
  }

  pieces_in_ring <- 6 * ring
  pieces_in_outer_ring <- 6 * (ring + 1)

  # Ratio of outer to inner pieces
  ratio <- pieces_in_outer_ring / pieces_in_ring

  # Angular range of this piece
  arc_angle <- 2 * pi / pieces_in_ring
  start_angle <- position * arc_angle
  end_angle <- (position + 1) * arc_angle

  # Find outer pieces that overlap
  outer_arc_angle <- 2 * pi / pieces_in_outer_ring
  outer_ring_start <- 3 * (ring + 1) * ring + 2  # First piece ID in outer ring

  neighbors <- c()
  for (outer_pos in 0:(pieces_in_outer_ring - 1)) {
    outer_start <- outer_pos * outer_arc_angle
    outer_end <- (outer_pos + 1) * outer_arc_angle

    # Check for overlap
    if (outer_start < end_angle && outer_end > start_angle) {
      neighbors <- c(neighbors, outer_ring_start + outer_pos)
    }
  }

  return(neighbors)
}

#' Get ALL inner ring piece IDs that share the INNER edge
#'
#' An outer piece's inner edge can span multiple inner pieces when ring
#' boundaries don't align (e.g., ring 3 at 20° pieces, ring 2 at 30° pieces).
#'
#' @param piece_id The outer piece ID
#' @param rings Total number of rings
#' @return Vector of inner piece IDs that overlap with this piece's inner edge
#' @keywords internal
get_inner_neighbors <- function(piece_id, rings) {
  info <- map_concentric_piece_id(piece_id, rings)
  ring <- info$ring
  position <- info$position

  # Ring 0 has no inner neighbors
  if (ring == 0) {
    return(integer(0))
  }

  # Ring 1 connects to center (center has 6 edges, one per ring 1 piece)
  if (ring == 1) {
    return(1)  # Center piece
  }

  # For rings > 1, find ALL inner pieces that overlap with this piece's angular range
  pieces_in_ring <- 6 * ring
  pieces_in_inner_ring <- 6 * (ring - 1)

  arc_angle <- 2 * pi / pieces_in_ring
  start_angle <- position * arc_angle
  end_angle <- (position + 1) * arc_angle

  inner_arc_angle <- 2 * pi / pieces_in_inner_ring
  inner_ring_start <- 3 * (ring - 1) * (ring - 2) + 2

  # Find all inner pieces that overlap with [start_angle, end_angle]
  neighbors <- c()
  for (inner_pos in 0:(pieces_in_inner_ring - 1)) {
    inner_start <- inner_pos * inner_arc_angle
    inner_end <- (inner_pos + 1) * inner_arc_angle

    # Check for overlap
    if (inner_start < end_angle && inner_end > start_angle) {
      neighbors <- c(neighbors, inner_ring_start + inner_pos)
    }
  }

  return(neighbors)
}

#' Get inner ring piece ID that contains the midpoint (legacy function)
#' @keywords internal
get_inner_neighbor <- function(piece_id, rings) {
  # For backwards compatibility, return the first neighbor
  neighbors <- get_inner_neighbors(piece_id, rings)
  if (length(neighbors) == 0) return(integer(0))
  return(neighbors[1])
}

#' Generate edge map for concentric ring puzzle
#'
#' Creates unique edges with bezier curves for all piece connections.
#' Handles the case where OUTER edges span multiple outer ring pieces.
#'
#' @param rings Number of rings
#' @param seed Random seed for tab generation
#' @param diameter Puzzle diameter
#' @param tabsize Tab size parameter
#' @param jitter Jitter parameter for tab variation
#' @param center_shape "hexagon" or "circle"
#' @param do_circular_border If TRUE, use arc commands for perfect circular boundary
#' @param boundary_facing Direction the circular arc faces: "outward" (convex, away from center)
#'   or "inward" (concave, toward center). Only applies when do_circular_border = TRUE.
#' @return List with edge_map, piece_edges (list of edges per piece), and piece_vertices
#' @export
generate_concentric_edge_map <- function(rings, seed, diameter,
                                          tabsize = 27, jitter = 5,
                                          center_shape = "hexagon",
                                          do_circular_border = FALSE,
                                          boundary_facing = "outward") {
  # Source dependencies
  if (!exists("generate_hex_bezier_edge")) {
    source("R/hexagonal_bezier_generation.R")
  }

  num_pieces <- get_concentric_piece_count(rings)
  piece_height <- get_concentric_piece_height(diameter, rings)
  tab_params <- list(tabsize = tabsize, jitter = jitter)

  # Calculate circle radius for circular border option
  circle_radius <- diameter / 2

  # Calculate sweep flags based on boundary_facing direction

  # SVG arc: A rx ry x-rotation large-arc-flag sweep-flag x y
  # sweep-flag = 1: clockwise, sweep-flag = 0: counter-clockwise
  # For outward-facing (convex): forward uses sweep=0, reverse uses sweep=1
  # For inward-facing (concave): forward uses sweep=1, reverse uses sweep=0
  if (boundary_facing == "outward") {
    sweep_forward <- 0
    sweep_reverse <- 1
  } else {
    sweep_forward <- 1
    sweep_reverse <- 0
  }

  # Get all piece vertices
  all_vertices <- get_all_concentric_vertices(rings, diameter, center_shape)

  # Storage for unique edges and per-piece edge lists
  edge_map <- list()
  piece_edges <- vector("list", num_pieces)  # List of edges for each piece
  edge_counter <- 0

  # Initialize piece edge lists
  for (i in 1:num_pieces) {
    piece_edges[[i]] <- list()
  }

  # Process each piece and generate its edges
  for (piece_id in 1:num_pieces) {
    piece_info <- all_vertices[[piece_id]]

    if (piece_info$type == "circle") {
      # Circle center - will be handled specially during path building
      # Its border is the outer edge connecting to ring 1 pieces
      outer_neighbors <- get_outer_neighbors(piece_id, rings)

      # Circle connects to 6 pieces, generate 6 edge segments
      # Each segment is an arc of the circle
      r <- piece_info$radius
      for (i in seq_along(outer_neighbors)) {
        neighbor_id <- outer_neighbors[i]
        neighbor_info <- all_vertices[[neighbor_id]]

        # Get the inner edge vertices of the neighbor
        v1 <- neighbor_info$vertices[[1]]  # inner-start
        v2 <- neighbor_info$vertices[[2]]  # inner-end

        # Unique key for this edge
        unique_key <- sprintf("E%d-%d", min(piece_id, neighbor_id), max(piece_id, neighbor_id))

        if (is.null(edge_map[[unique_key]])) {
          edge_counter <- edge_counter + 1
          edge_seed <- seed + piece_id * 1000 + neighbor_id

          # Generate bezier edge from neighbor's perspective (inner edge)
          bezier <- generate_hex_bezier_edge(
            v1 = v1,
            v2 = v2,
            seed = edge_seed,
            edge_id = edge_counter,
            tab_params = tab_params
          )

          edge_map[[unique_key]] <- list(
            id = edge_counter,
            piece1 = piece_id,
            piece2 = neighbor_id,
            forward = bezier$forward,   # v1 -> v2
            reverse = bezier$reverse,   # v2 -> v1
            start = v1,
            end = v2
          )
        }

        # Add to both pieces' edge lists
        # Both circle and neighbor traverse the edge in the SAME direction (v1->v2)
        # This is counter-clockwise for both pieces:
        # - Circle: going around its boundary counter-clockwise
        # - Neighbor: going along its inner edge from V1 to V2
        piece_edges[[piece_id]] <- c(piece_edges[[piece_id]], list(list(
          edge_ref = unique_key,
          is_forward = TRUE,  # Circle uses forward (v1 to v2, counter-clockwise)
          type = "radial"
        )))

        piece_edges[[neighbor_id]] <- c(piece_edges[[neighbor_id]], list(list(
          edge_ref = unique_key,
          is_forward = TRUE,  # Neighbor uses forward (v1 to v2 for INNER)
          type = "inner"
        )))
      }

    } else if (piece_info$type == "hexagon") {
      # Hexagon center - 6 edges connecting to ring 1
      vertices <- piece_info$vertices

      for (edge_idx in 1:6) {
        v1 <- vertices[[edge_idx]]
        v2 <- vertices[[(edge_idx %% 6) + 1]]

        # This edge connects to ring 1 piece at position (edge_idx - 1)
        neighbor_id <- 1 + edge_idx  # Pieces 2-7 are ring 1

        unique_key <- sprintf("E%d-%d", piece_id, neighbor_id)

        if (is.null(edge_map[[unique_key]])) {
          edge_counter <- edge_counter + 1
          edge_seed <- seed + piece_id * 1000 + neighbor_id

          bezier <- generate_hex_bezier_edge(
            v1 = v1,
            v2 = v2,
            seed = edge_seed,
            edge_id = edge_counter,
            tab_params = tab_params
          )

          edge_map[[unique_key]] <- list(
            id = edge_counter,
            piece1 = piece_id,
            piece2 = neighbor_id,
            forward = bezier$forward,
            reverse = bezier$reverse,
            start = v1,
            end = v2
          )
        }

        # Add to both pieces' edge lists
        piece_edges[[piece_id]] <- c(piece_edges[[piece_id]], list(list(
          edge_ref = unique_key,
          is_forward = TRUE,
          type = "outer",
          vertex_order = c(edge_idx, (edge_idx %% 6) + 1)
        )))

        # Ring 1 piece's INNER edge V1->V2 is in SAME direction as center's outer edge
        # So we use is_forward = TRUE (not FALSE!)
        piece_edges[[neighbor_id]] <- c(piece_edges[[neighbor_id]], list(list(
          edge_ref = unique_key,
          is_forward = TRUE,  # Same direction as center's edge
          type = "inner"
        )))
      }

    } else {
      # Trapezoid piece - has INNER, RIGHT, OUTER, LEFT edges
      # Key insight: INNER and OUTER may connect to multiple pieces!

      vertices <- piece_info$vertices
      ring <- piece_info$ring
      position <- piece_info$position

      # V1: inner-start, V2: inner-end, V3: outer-end, V4: outer-start
      v1 <- vertices[[1]]
      v2 <- vertices[[2]]
      v3 <- vertices[[3]]
      v4 <- vertices[[4]]

      # --- INNER EDGE (v1 -> v2) ---
      # NOTE: The INNER edge is already generated by the inner piece as its OUTER edge
      # (with -radial key). We just need to reference it here, not generate a new edge.
      # The edge reference will be added by the inner piece when processing OUTER segments.
      # We don't add anything here since the inner piece already adds this to our edge list
      # in the OUTER processing section (lines ~436-440).

      # --- RIGHT EDGE (v2 -> v3) ---
      # Connects to next piece in same ring
      pieces_in_ring <- 6 * ring
      next_pos <- (position + 1) %% pieces_in_ring
      ring_start <- 3 * ring * (ring - 1) + 2
      right_neighbor <- ring_start + next_pos

      unique_key <- sprintf("E%d-%d-circ", min(piece_id, right_neighbor), max(piece_id, right_neighbor))

      if (is.null(edge_map[[unique_key]])) {
        edge_counter <- edge_counter + 1
        edge_seed <- seed + piece_id * 1000 + right_neighbor + 2

        bezier <- generate_hex_bezier_edge(
          v1 = v2,
          v2 = v3,
          seed = edge_seed,
          edge_id = edge_counter,
          tab_params = tab_params
        )

        edge_map[[unique_key]] <- list(
          id = edge_counter,
          piece1 = piece_id,
          piece2 = right_neighbor,
          forward = bezier$forward,
          reverse = bezier$reverse,
          start = v2,
          end = v3
        )
      }

      piece_edges[[piece_id]] <- c(piece_edges[[piece_id]], list(list(
        edge_ref = unique_key,
        is_forward = (piece_id < right_neighbor),
        type = "right",
        start_vertex = 2,
        end_vertex = 3
      )))

      # --- OUTER EDGE (v3 -> v4, actually v4 -> v3 going clockwise) ---
      # This is the tricky one - may connect to MULTIPLE outer ring pieces!
      outer_neighbors <- get_outer_neighbors(piece_id, rings)

      if (length(outer_neighbors) == 0) {
        # Boundary - straight line or circular arc
        if (do_circular_border) {
          # Use arc commands for perfect circular border
          # SVG arc: A rx ry x-rotation large-arc-flag sweep-flag x y
          # rx = ry = circle_radius (perfect circle)
          # x-rotation = 0 (no rotation)
          # large-arc-flag = 0 (small arc, each segment < 180 degrees)
          # sweep-flag determined by boundary_facing parameter (set above)
          piece_edges[[piece_id]] <- c(piece_edges[[piece_id]], list(list(
            type = "border",
            path_forward = sprintf("A %.2f %.2f 0 0 %d %.2f %.2f",
                                   circle_radius, circle_radius, sweep_forward, v4[1], v4[2]),
            path_reverse = sprintf("A %.2f %.2f 0 0 %d %.2f %.2f",
                                   circle_radius, circle_radius, sweep_reverse, v3[1], v3[2]),
            start_vertex = 3,
            end_vertex = 4
          )))
        } else {
          # Straight line (original behavior)
          piece_edges[[piece_id]] <- c(piece_edges[[piece_id]], list(list(
            type = "border",
            path_forward = sprintf("L %.2f %.2f", v4[1], v4[2]),
            path_reverse = sprintf("L %.2f %.2f", v3[1], v3[2]),
            start_vertex = 3,
            end_vertex = 4
          )))
        }
      } else {
        # Connect to outer ring pieces
        # We need to subdivide this edge at the junction points
        # IMPORTANT: Calculate the OVERLAP between this piece's OUTER edge
        # and each outer neighbor's INNER edge

        # This piece's OUTER edge spans [start_angle, end_angle] at outer_radius
        piece_outer_start <- piece_info$start_angle  # V4 angle
        piece_outer_end <- piece_info$end_angle      # V3 angle
        shared_radius <- piece_info$outer_radius     # Same as neighbor's inner_radius

        # Sort outer neighbors by DECREASING angle for V3->V4 direction
        # V3 (outer-end) has higher angle than V4 (outer-start)
        # So we traverse from high angle to low angle
        outer_with_angles <- lapply(outer_neighbors, function(nid) {
          ninfo <- all_vertices[[nid]]
          mid_angle <- (ninfo$start_angle + ninfo$end_angle) / 2
          list(id = nid, angle = mid_angle, start = ninfo$start_angle, end = ninfo$end_angle)
        })
        outer_sorted <- outer_with_angles[order(sapply(outer_with_angles, function(x) x$angle), decreasing = TRUE)]

        # Generate edges for each outer neighbor
        for (outer_data in outer_sorted) {
          neighbor_id <- outer_data$id
          neighbor_info <- all_vertices[[neighbor_id]]

          # Calculate the OVERLAP between this piece's OUTER and neighbor's INNER
          # This piece's OUTER: [piece_outer_start, piece_outer_end] at shared_radius
          # Neighbor's INNER: [neighbor_info$start_angle, neighbor_info$end_angle] at shared_radius
          overlap_start <- max(piece_outer_start, neighbor_info$start_angle)
          overlap_end <- min(piece_outer_end, neighbor_info$end_angle)

          # Calculate actual vertices at the OVERLAP points (not the neighbor's full edge)
          # nv1 = overlap_start point, nv2 = overlap_end point
          nv1 <- c(shared_radius * cos(overlap_start), shared_radius * sin(overlap_start))
          nv2 <- c(shared_radius * cos(overlap_end), shared_radius * sin(overlap_end))

          unique_key <- sprintf("E%d-%d-radial", min(piece_id, neighbor_id), max(piece_id, neighbor_id))

          if (is.null(edge_map[[unique_key]])) {
            edge_counter <- edge_counter + 1
            edge_seed <- seed + piece_id * 1000 + neighbor_id + 3

            # Generate edge from the overlap segment perspective
            bezier <- generate_hex_bezier_edge(
              v1 = nv1,
              v2 = nv2,
              seed = edge_seed,
              edge_id = edge_counter,
              tab_params = tab_params
            )

            edge_map[[unique_key]] <- list(
              id = edge_counter,
              piece1 = piece_id,
              piece2 = neighbor_id,
              forward = bezier$forward,   # nv1 -> nv2 (overlap_start to overlap_end)
              reverse = bezier$reverse,   # nv2 -> nv1 (overlap_end to overlap_start)
              start = nv1,
              end = nv2,
              overlap_start_angle = overlap_start,
              overlap_end_angle = overlap_end
            )
          }

          # Add edge segment to this piece's OUTER
          piece_edges[[piece_id]] <- c(piece_edges[[piece_id]], list(list(
            edge_ref = unique_key,
            is_forward = FALSE,  # Outer piece perspective: going from high angle to low
            type = "outer_segment",
            neighbor = neighbor_id
          )))

          # Add to neighbor's INNER (as inner_segment since it may be partial)
          piece_edges[[neighbor_id]] <- c(piece_edges[[neighbor_id]], list(list(
            edge_ref = unique_key,
            is_forward = TRUE,  # Inner piece perspective: going from low angle to high
            type = "inner_segment",
            neighbor = piece_id
          )))
        }
      }

      # --- LEFT EDGE (v4 -> v1) ---
      # Connects to previous piece in same ring
      prev_pos <- (position - 1 + pieces_in_ring) %% pieces_in_ring
      left_neighbor <- ring_start + prev_pos

      unique_key <- sprintf("E%d-%d-circ", min(piece_id, left_neighbor), max(piece_id, left_neighbor))

      if (is.null(edge_map[[unique_key]])) {
        edge_counter <- edge_counter + 1
        edge_seed <- seed + piece_id * 1000 + left_neighbor + 4

        bezier <- generate_hex_bezier_edge(
          v1 = v4,
          v2 = v1,
          seed = edge_seed,
          edge_id = edge_counter,
          tab_params = tab_params
        )

        edge_map[[unique_key]] <- list(
          id = edge_counter,
          piece1 = piece_id,
          piece2 = left_neighbor,
          forward = bezier$forward,
          reverse = bezier$reverse,
          start = v4,
          end = v1
        )
      }

      piece_edges[[piece_id]] <- c(piece_edges[[piece_id]], list(list(
        edge_ref = unique_key,
        is_forward = (piece_id < left_neighbor),
        type = "left",
        start_vertex = 4,
        end_vertex = 1
      )))
    }
  }

  return(list(
    edge_map = edge_map,
    piece_edges = piece_edges,
    piece_vertices = all_vertices,
    num_edges = edge_counter,
    rings = rings,
    diameter = diameter,
    piece_height = piece_height
  ))
}

#' Build SVG path for a concentric piece
#'
#' @param piece_id Piece ID
#' @param edge_data Edge map data from generate_concentric_edge_map
#' @return SVG path string
#' @export
build_concentric_piece_path <- function(piece_id, edge_data) {
  piece_info <- edge_data$piece_vertices[[piece_id]]
  piece_edge_list <- edge_data$piece_edges[[piece_id]]

  if (piece_info$type == "circle") {
    # Circle: collect all the radial edges and build path
    r <- piece_info$radius

    # Get all edges for this piece in order
    radial_edges <- piece_edge_list[sapply(piece_edge_list, function(e) e$type == "radial")]

    if (length(radial_edges) == 0) {
      # No edges yet, just draw circle
      return(sprintf("M %.2f 0 A %.2f %.2f 0 1 1 %.2f 0 A %.2f %.2f 0 1 1 %.2f 0 Z",
                     r, r, r, -r, r, r, r))
    }

    # Build path from radial edges (going around the circle)
    path_parts <- c()

    for (i in seq_along(radial_edges)) {
      edge_info <- radial_edges[[i]]
      edge <- edge_data$edge_map[[edge_info$edge_ref]]

      if (i == 1) {
        # Start at the beginning of first edge
        start_pt <- if (edge_info$is_forward) edge$start else edge$end
        path_parts <- c(path_parts, sprintf("M %.2f %.2f", start_pt[1], start_pt[2]))
      }

      # Add the edge (reverse direction for circle - going inward along neighbor's inner edge)
      if (edge_info$is_forward) {
        path_parts <- c(path_parts, edge$forward)
      } else {
        path_parts <- c(path_parts, edge$reverse)
      }
    }

    path_parts <- c(path_parts, "Z")
    return(paste(path_parts, collapse = " "))
  }

  if (piece_info$type == "hexagon") {
    # Hexagon center - 6 outer edges
    vertices <- piece_info$vertices

    path_parts <- c()
    v1 <- vertices[[1]]
    path_parts <- c(path_parts, sprintf("M %.2f %.2f", v1[1], v1[2]))

    # Get outer edges in order
    outer_edges <- piece_edge_list[sapply(piece_edge_list, function(e) e$type == "outer")]

    for (edge_info in outer_edges) {
      edge <- edge_data$edge_map[[edge_info$edge_ref]]

      if (edge_info$is_forward) {
        path_parts <- c(path_parts, edge$forward)
      } else {
        path_parts <- c(path_parts, edge$reverse)
      }
    }

    path_parts <- c(path_parts, "Z")
    return(paste(path_parts, collapse = " "))
  }

  # Trapezoid - 4 edges (but INNER and OUTER may be multiple segments)
  vertices <- piece_info$vertices
  v1 <- vertices[[1]]
  v2 <- vertices[[2]]

  path_parts <- c(sprintf("M %.2f %.2f", v1[1], v1[2]))

  # INNER edge (v1 -> v2) - may be single or multiple segments
  # "inner" type = single connection (ring 1 to center)
  # "inner_segment" type = partial connection when edge spans multiple inner pieces
  inner_edges <- piece_edge_list[sapply(piece_edge_list, function(e) e$type %in% c("inner", "inner_segment"))]

  if (length(inner_edges) > 0) {
    # Sort inner segments by angle (increasing) for V1->V2 direction
    # V1 (inner-start) has lower angle than V2 (inner-end)
    if (length(inner_edges) > 1) {
      inner_with_angles <- lapply(inner_edges, function(e) {
        edge <- edge_data$edge_map[[e$edge_ref]]
        # Use the start angle of the edge
        start_angle <- if (!is.null(edge$overlap_start_angle)) {
          edge$overlap_start_angle
        } else {
          atan2(edge$start[2], edge$start[1])
        }
        list(edge_info = e, angle = start_angle)
      })
      inner_edges <- lapply(inner_with_angles[order(sapply(inner_with_angles, function(x) x$angle))],
                            function(x) x$edge_info)
    }

    # Add each inner edge segment
    for (edge_info in inner_edges) {
      edge <- edge_data$edge_map[[edge_info$edge_ref]]
      if (!is.null(edge)) {
        if (edge_info$is_forward) {
          path_parts <- c(path_parts, edge$forward)
        } else {
          path_parts <- c(path_parts, edge$reverse)
        }
      }
    }
  } else {
    # No inner edge - go straight to v2
    path_parts <- c(path_parts, sprintf("L %.2f %.2f", v2[1], v2[2]))
  }

  # RIGHT edge (v2 -> v3)
  right_edges <- piece_edge_list[sapply(piece_edge_list, function(e) e$type == "right")]
  if (length(right_edges) > 0) {
    edge_info <- right_edges[[1]]
    edge <- edge_data$edge_map[[edge_info$edge_ref]]
    if (!is.null(edge)) {
      if (edge_info$is_forward) {
        path_parts <- c(path_parts, edge$forward)
      } else {
        path_parts <- c(path_parts, edge$reverse)
      }
    } else {
      v3 <- vertices[[3]]
      path_parts <- c(path_parts, sprintf("L %.2f %.2f", v3[1], v3[2]))
    }
  } else {
    v3 <- vertices[[3]]
    path_parts <- c(path_parts, sprintf("L %.2f %.2f", v3[1], v3[2]))
  }

  # OUTER edge (v3 -> v4) - may be multiple segments or border
  outer_edges <- piece_edge_list[sapply(piece_edge_list, function(e) e$type %in% c("outer_segment", "border"))]

  if (length(outer_edges) > 0) {
    # Sort outer segments by neighbor angle (to ensure correct order)
    for (edge_info in outer_edges) {
      if (edge_info$type == "border") {
        path_parts <- c(path_parts, edge_info$path_forward)
      } else {
        edge <- edge_data$edge_map[[edge_info$edge_ref]]
        if (!is.null(edge)) {
          if (edge_info$is_forward) {
            path_parts <- c(path_parts, edge$forward)
          } else {
            path_parts <- c(path_parts, edge$reverse)
          }
        }
      }
    }
  } else {
    # Fallback - straight line to v4
    v4 <- vertices[[4]]
    path_parts <- c(path_parts, sprintf("L %.2f %.2f", v4[1], v4[2]))
  }

  # LEFT edge (v4 -> v1)
  left_edges <- piece_edge_list[sapply(piece_edge_list, function(e) e$type == "left")]
  if (length(left_edges) > 0) {
    edge_info <- left_edges[[1]]
    edge <- edge_data$edge_map[[edge_info$edge_ref]]
    if (!is.null(edge)) {
      if (edge_info$is_forward) {
        path_parts <- c(path_parts, edge$forward)
      } else {
        path_parts <- c(path_parts, edge$reverse)
      }
    } else {
      path_parts <- c(path_parts, sprintf("L %.2f %.2f", v1[1], v1[2]))
    }
  } else {
    path_parts <- c(path_parts, sprintf("L %.2f %.2f", v1[1], v1[2]))
  }

  path_parts <- c(path_parts, "Z")
  return(paste(path_parts, collapse = " "))
}

#' Generate all concentric pieces with paths
#'
#' @param rings Number of rings
#' @param seed Random seed
#' @param diameter Puzzle diameter
#' @param tabsize Tab size parameter
#' @param jitter Jitter parameter
#' @param center_shape "hexagon" or "circle"
#' @param do_circular_border If TRUE, use arc commands for perfect circular boundary
#' @param boundary_facing Direction the circular arc faces: "outward" or "inward"
#' @return List with pieces and metadata
#' @export
generate_concentric_pieces <- function(rings, seed, diameter,
                                        tabsize = 27, jitter = 5,
                                        center_shape = "hexagon",
                                        do_circular_border = FALSE,
                                        boundary_facing = "outward") {
  cat("Creating concentric edge mapping...\n")
  edge_data <- generate_concentric_edge_map(
    rings = rings,
    seed = seed,
    diameter = diameter,
    tabsize = tabsize,
    jitter = jitter,
    center_shape = center_shape,
    do_circular_border = do_circular_border,
    boundary_facing = boundary_facing
  )
  cat(sprintf("Generated %d unique edges\n", edge_data$num_edges))

  num_pieces <- get_concentric_piece_count(rings)
  pieces <- list()

  for (piece_id in 1:num_pieces) {
    piece_info <- edge_data$piece_vertices[[piece_id]]
    path <- build_concentric_piece_path(piece_id, edge_data)

    pieces[[piece_id]] <- list(
      id = piece_id,
      path = path,
      center_x = piece_info$center[1],
      center_y = piece_info$center[2],
      ring = piece_info$ring,
      position = piece_info$position,
      type = piece_info$type
    )
  }

  return(list(
    pieces = pieces,
    piece_height = edge_data$piece_height,
    rings = rings,
    diameter = diameter,
    num_pieces = num_pieces,
    center_shape = center_shape
  ))
}
