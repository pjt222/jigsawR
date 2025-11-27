# Fixed Hexagonal Edge Generation
# Proper edge mapping with unique edge IDs

#' Generate all edges with proper unique identifiers
#'
#' Creates a proper edge mapping where each shared edge has exactly one ID,
#' and both adjacent pieces reference the same edge (one forward, one reverse).
#'
#' @param rings Number of rings
#' @param seed Random seed
#' @param diameter Puzzle diameter
#' @param tabsize Tab size percentage
#' @param jitter Jitter percentage
#' @param do_warp Apply circular warp transformation to border edges (default: FALSE)
#' @return List with edge_map (unique edges) and piece_edges (piece-to-edge mapping)
#'
#' @export
generate_hex_edge_map <- function(rings, seed, diameter, tabsize = 27, jitter = 5,
                                  do_warp = FALSE) {
  # Source dependencies
  if (!exists("map_piece_id_to_ring")) {
    source("R/hexagonal_topology.R")
  }
  if (!exists("get_hex_neighbor")) {
    source("R/hexagonal_neighbors.R")
  }
  if (!exists("generate_hex_bezier_edge")) {
    source("R/hexagonal_bezier_generation.R")
  }

  num_pieces <- 3 * rings * (rings - 1) + 1
  piece_radius <- diameter / (rings * 4)
  tab_params <- list(tabsize = tabsize, jitter = jitter)

  # Step 1: Calculate vertices for all pieces (original coordinates)
  piece_vertices_original <- list()
  base_offset <- 0  # Flat-top hexagon: vertices at 0°, 60°, 120°, 180°, 240°, 300°

  for (piece_id in 1:num_pieces) {
    axial_coords <- map_piece_id_to_axial(piece_id, rings)
    hex_size <- piece_radius

    cart_coords <- axial_to_cartesian(
      q = axial_coords$q,
      r = axial_coords$r,
      hex_size = hex_size
    )
    center_x <- cart_coords$x
    center_y <- cart_coords$y

    vertices <- list()
    for (i in 0:5) {
      vertex_angle <- i * pi / 3 + base_offset
      vx <- center_x + piece_radius * cos(vertex_angle)
      vy <- center_y + piece_radius * sin(vertex_angle)
      vertices[[i + 1]] <- c(vx, vy)
    }
    piece_vertices_original[[piece_id]] <- vertices
  }

  # Step 1b: If warping, identify boundary vertices and create warped vertex list
  # A vertex is on the boundary if no other piece shares it (or only border edges touch it)
  piece_vertices <- piece_vertices_original  # Start with original

  if (do_warp) {
    # Build a map of vertex -> pieces that share it
    vertex_sharing <- list()
    tol <- 0.01

    for (piece_id in 1:num_pieces) {
      for (i in 1:6) {
        v <- piece_vertices_original[[piece_id]][[i]]
        v_key <- sprintf("%.1f,%.1f", v[1], v[2])

        if (is.null(vertex_sharing[[v_key]])) {
          vertex_sharing[[v_key]] <- list(pieces = c(), coords = v)
        }
        vertex_sharing[[v_key]]$pieces <- c(vertex_sharing[[v_key]]$pieces, piece_id)
      }
    }

    # A boundary vertex is shared by fewer than 3 pieces (in a hex grid, interior vertices touch 3 hexes)
    boundary_vertices <- list()
    for (v_key in names(vertex_sharing)) {
      if (length(unique(vertex_sharing[[v_key]]$pieces)) < 3) {
        v <- vertex_sharing[[v_key]]$coords
        warped <- apply_hex_warp(v[1], v[2])
        boundary_vertices[[v_key]] <- c(warped$x, warped$y)
      }
    }

    # Update piece_vertices with warped boundary vertices
    for (piece_id in 1:num_pieces) {
      for (i in 1:6) {
        v <- piece_vertices_original[[piece_id]][[i]]
        v_key <- sprintf("%.1f,%.1f", v[1], v[2])

        if (!is.null(boundary_vertices[[v_key]])) {
          piece_vertices[[piece_id]][[i]] <- boundary_vertices[[v_key]]
        }
      }
    }
  }

  # Step 2: Create unique edge mapping
  edge_map <- list()
  edge_counter <- 0

  # Map from "piece_id-side" to edge info
  piece_edge_map <- list()

  for (piece_id in 1:num_pieces) {
    vertices <- piece_vertices[[piece_id]]

    for (side in 0:5) {
      # Get edge endpoints (two consecutive vertices)
      v1 <- vertices[[side + 1]]
      v2 <- vertices[[(side + 1) %% 6 + 1]]

      # Find neighbor by checking which other piece shares these two vertices
      neighbor_id <- NA
      neighbor_side <- NA

      for (test_id in 1:num_pieces) {
        if (test_id == piece_id) next  # Skip self

        test_vertices <- piece_vertices[[test_id]]

        # Check if this piece shares both v1 and v2 (in either order)
        for (test_side in 0:5) {
          test_v1 <- test_vertices[[test_side + 1]]
          test_v2 <- test_vertices[[(test_side + 1) %% 6 + 1]]

          # Check if vertices match (within tolerance for floating point)
          tol <- 0.01
          if ((all(abs(v1 - test_v2) < tol) && all(abs(v2 - test_v1) < tol)) ||
              (all(abs(v1 - test_v1) < tol) && all(abs(v2 - test_v2) < tol))) {
            neighbor_id <- test_id
            neighbor_side <- test_side
            break
          }
        }
        if (!is.na(neighbor_id)) break
      }

      if (is.na(neighbor_id)) {
        # Border edge - no neighbor, this is on the puzzle boundary
        edge_key <- sprintf("%d-%d", piece_id, side)

        if (do_warp) {
          # v1 and v2 are already warped (done in Step 1b)
          # Use arc command to follow the circular boundary
          warp_radius <- (sqrt(v1[1]^2 + v1[2]^2) + sqrt(v2[1]^2 + v2[2]^2)) / 2

          # Determine sweep direction based on vertex order
          # Cross product determines if we're going clockwise or counter-clockwise
          cross <- v1[1] * v2[2] - v1[2] * v2[1]
          sweep <- if (cross > 0) 1 else 0

          # Create arc command for border edge
          forward_arc <- sprintf("A %.2f %.2f 0 0 %d %.2f %.2f",
                                 warp_radius, warp_radius, sweep,
                                 v2[1], v2[2])
          reverse_arc <- sprintf("A %.2f %.2f 0 0 %d %.2f %.2f",
                                 warp_radius, warp_radius, 1 - sweep,
                                 v1[1], v1[2])

          piece_edge_map[[edge_key]] <- list(
            type = "border",
            forward = forward_arc,
            reverse = reverse_arc,
            start = v1,
            end = v2,
            is_forward = TRUE,
            warped = TRUE
          )
        } else {
          # No warp - straight line
          piece_edge_map[[edge_key]] <- list(
            type = "border",
            forward = sprintf("L %.2f %.2f", v2[1], v2[2]),
            reverse = sprintf("L %.2f %.2f", v1[1], v1[2]),
            start = v1,
            end = v2,
            is_forward = TRUE,
            warped = FALSE
          )
        }
      } else {
        # Internal edge - check if already generated
        pieces <- sort(c(piece_id, neighbor_id))
        unique_edge_key <- sprintf("E%d-%d", pieces[1], pieces[2])

        if (is.null(edge_map[[unique_edge_key]])) {
          # First time seeing this edge - generate it
          edge_counter <- edge_counter + 1

          # Use deterministic seed based on sorted piece IDs
          edge_seed <- seed + pieces[1] * 1000 + pieces[2]

          # Generate bezier curve
          bezier <- generate_hex_bezier_edge(
            v1 = v1,
            v2 = v2,
            seed = edge_seed,
            edge_id = edge_counter,
            tab_params = tab_params
          )

          # Store the unique edge
          edge_map[[unique_edge_key]] <- list(
            id = edge_counter,
            piece1 = piece_id,
            piece2 = neighbor_id,
            forward = bezier$forward,
            reverse = bezier$reverse,
            start = v1,
            end = v2
          )

          # Map this piece's side to the edge (forward direction)
          piece_key <- sprintf("%d-%d", piece_id, side)
          piece_edge_map[[piece_key]] <- list(
            type = "internal",
            edge_key = unique_edge_key,
            is_forward = TRUE,
            forward = bezier$forward,
            reverse = bezier$reverse,
            start = v1,
            end = v2
          )
        } else {
          # Edge already generated - use reverse direction
          edge <- edge_map[[unique_edge_key]]
          piece_key <- sprintf("%d-%d", piece_id, side)
          piece_edge_map[[piece_key]] <- list(
            type = "internal",
            edge_key = unique_edge_key,
            is_forward = FALSE,
            forward = edge$reverse,  # Swap!
            reverse = edge$forward,  # Swap!
            start = v1,
            end = v2
          )
        }
      }
    }
  }

  return(list(
    edge_map = edge_map,
    piece_edge_map = piece_edge_map,
    num_edges = edge_counter
  ))
}

#' Generate hexagonal pieces using proper edge mapping
#'
#' @param rings Number of rings
#' @param seed Random seed
#' @param diameter Puzzle diameter
#' @param tabsize Tab size percentage
#' @param jitter Jitter percentage
#' @param separated Use separated layout
#' @param base_spacing Base spacing for separation
#' @param separation_factor Separation multiplier
#' @param do_warp Apply circular warp transformation to border edges (default: FALSE)
#' @return List of piece objects
#'
#' @export
generate_hex_pieces_with_edge_map <- function(rings, seed, diameter = 240,
                                               tabsize = 27, jitter = 5,
                                               separated = TRUE,
                                               base_spacing = NULL,
                                               separation_factor = 1.0,
                                               do_warp = FALSE) {
  # Source dependencies
  if (!exists("map_piece_id_to_ring")) {
    source("R/hexagonal_topology.R")
  }

  # Generate edge mapping
  cat("Creating edge mapping...\n")
  edge_data <- generate_hex_edge_map(rings, seed, diameter, tabsize, jitter, do_warp)
  cat(sprintf("Generated %d unique edges\n", edge_data$num_edges))
  if (do_warp) {
    cat("Circular warp enabled - border edges will be arcs\n")
  }

  # Calculate spacing
  if (separated && is.null(base_spacing)) {
    base_spacing <- diameter / (rings * 2)
  }

  # Generate pieces
  num_pieces <- 3 * rings * (rings - 1) + 1
  pieces <- list()

  for (piece_id in 1:num_pieces) {
    # Get topology and position
    ring_info <- map_piece_id_to_ring(piece_id, rings)

    # Edges are generated at COMPACT lattice positions
    # We need to calculate the OFFSET from compact to desired position

    # Get compact position (where edges actually are)
    piece_radius <- diameter / (rings * 4)
    compact_pos <- calculate_hex_piece_position(
      piece_id = piece_id,
      rings = rings,
      piece_radius = piece_radius,
      separation_factor = 1.0  # No separation
    )

    if (separated) {
      # Get separated position (where we want the piece)
      # Note: Use piece_radius (not base_spacing) to maintain correct scale
      # base_spacing is the spacing parameter, but calculate_hex_piece_position
      # expects piece_radius for coordinate calculations
      separated_pos <- calculate_hex_piece_position(
        piece_id = piece_id,
        rings = rings,
        piece_radius = piece_radius,
        separation_factor = separation_factor
      )
      # Calculate the offset needed to move from compact to separated
      offset <- list(
        x = separated_pos$x - compact_pos$x,
        y = separated_pos$y - compact_pos$y
      )
      # Store both offset and absolute position
      position <- offset
      absolute_center <- separated_pos
    } else {
      # No offset needed (edges already at compact positions)
      position <- list(x = 0, y = 0)
      absolute_center <- compact_pos
    }

    # Build piece path from edges
    path_parts <- c()

    # Start at first vertex
    first_edge_key <- sprintf("%d-0", piece_id)
    first_edge <- edge_data$piece_edge_map[[first_edge_key]]
    path_parts <- c(sprintf("M %.2f %.2f",
                            position$x + first_edge$start[1],
                            position$y + first_edge$start[2]))

    # Helper function to offset coordinates in a path segment
    offset_path_coords <- function(path_segment, offset_x, offset_y) {
      # Get the command type
      cmd <- substr(trimws(path_segment), 1, 1)

      if (cmd == "A") {
        # Arc command: A rx ry x-axis-rotation large-arc-flag sweep-flag x y
        # Only the last two values (x, y) are coordinates to offset
        # Extract all numbers
        numbers <- as.numeric(unlist(strsplit(path_segment, "[A ]+")))
        numbers <- numbers[!is.na(numbers)]

        if (length(numbers) >= 7) {
          # rx, ry, rotation, large-arc, sweep, x, y
          rx <- numbers[1]
          ry <- numbers[2]
          rotation <- numbers[3]
          large_arc <- numbers[4]
          sweep <- numbers[5]
          x <- numbers[6] + offset_x
          y <- numbers[7] + offset_y

          return(sprintf("A %.2f %.2f %.0f %d %d %.2f %.2f",
                         rx, ry, rotation, large_arc, sweep, x, y))
        } else {
          # Malformed arc, return as-is
          return(path_segment)
        }
      } else {
        # L or C commands - all values are coordinates
        numbers <- as.numeric(unlist(strsplit(path_segment, "[CLM ]+")))
        numbers <- numbers[!is.na(numbers)]

        # Offset x coordinates (odd indices) and y coordinates (even indices)
        for (i in seq_along(numbers)) {
          if (i %% 2 == 1) {
            numbers[i] <- numbers[i] + offset_x  # x coordinate
          } else {
            numbers[i] <- numbers[i] + offset_y  # y coordinate
          }
        }

        # Rebuild path segment
        coords <- sprintf("%.2f", numbers)
        return(paste(cmd, paste(coords, collapse = " ")))
      }
    }

    # Add all 6 edges with position offset applied
    for (side in 0:5) {
      edge_key <- sprintf("%d-%d", piece_id, side)
      edge <- edge_data$piece_edge_map[[edge_key]]

      # Apply position offset to edge coordinates
      offset_edge <- offset_path_coords(edge$forward, position$x, position$y)
      path_parts <- c(path_parts, offset_edge)
    }

    # Close path
    path_parts <- c(path_parts, "Z")
    path <- paste(path_parts, collapse = " ")

    # Classify piece type
    piece_type <- if (ring_info$ring == 0) {
      "center"
    } else if (ring_info$ring == rings - 1) {
      "edge"
    } else {
      "inner"
    }

    pieces[[piece_id]] <- list(
      id = piece_id,
      ring = ring_info$ring,
      position_in_ring = ring_info$position,
      center_x = absolute_center$x,
      center_y = absolute_center$y,
      path = path,
      type = piece_type
    )
  }

  return(pieces)
}
