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
#' @param do_trunc Truncate boundary to clean geometric shape (default: FALSE)
#' @return List with edge_map (unique edges) and piece_edges (piece-to-edge mapping)
#'
#' @export
generate_hex_edge_map <- function(rings, seed, diameter, tabsize = 27, jitter = 5,
                                  do_warp = FALSE, do_trunc = FALSE) {
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

  # Step 1b: Handle vertex transformations based on do_warp and do_trunc
  #
  # Complete mode semantics (from hexagonal_puzzle.R):
  # - do_warp=TRUE: Applies hex_warp() to ALL coordinates, mapping hexagonal grid to circle
  # - do_trunc=TRUE: Clips boundary to clean shape (circle if warped, hexagon if not)
  #
  # The warp transformation applies to EVERY vertex (internal and boundary).
  # The truncation only affects boundary vertices.
  piece_vertices <- piece_vertices_original  # Start with original

  if (do_warp || do_trunc) {
    # Build a map of vertex -> pieces that share it
    vertex_sharing <- list()

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

    # Find boundary vertices and calculate max distance for truncation
    boundary_vertex_keys <- c()
    max_boundary_dist <- 0

    for (v_key in names(vertex_sharing)) {
      if (length(unique(vertex_sharing[[v_key]]$pieces)) < 3) {
        boundary_vertex_keys <- c(boundary_vertex_keys, v_key)
        v <- vertex_sharing[[v_key]]$coords
        dist <- sqrt(v[1]^2 + v[2]^2)
        max_boundary_dist <- max(max_boundary_dist, dist)
      }
    }

    # Step 1: If do_warp, apply warp to ALL vertices (not just boundary)
    # This matches complete mode where hex_process_r applies warp to every coordinate
    if (do_warp) {
      all_transformed <- list()

      for (v_key in names(vertex_sharing)) {
        v <- vertex_sharing[[v_key]]$coords
        # apply_hex_warp now uses division (matches original hex_warp)
        transformed <- apply_hex_warp(v[1], v[2])
        all_transformed[[v_key]] <- c(transformed$x, transformed$y)
      }

      # Update all piece vertices with warped coordinates
      for (piece_id in 1:num_pieces) {
        for (i in 1:6) {
          v <- piece_vertices_original[[piece_id]][[i]]
          v_key <- sprintf("%.1f,%.1f", v[1], v[2])

          if (!is.null(all_transformed[[v_key]])) {
            piece_vertices[[piece_id]][[i]] <- all_transformed[[v_key]]
          }
        }
      }

      cat("Circular warp enabled - ALL vertices transformed\n")

      # Step 1b: If also do_trunc, project boundary vertices onto the target circle
      # This ensures boundary arcs connect smoothly on the circle
      if (do_trunc) {
        circle_radius <- diameter / 2

        for (v_key in boundary_vertex_keys) {
          # Get the already-warped vertex position
          warped_v <- all_transformed[[v_key]]
          if (is.null(warped_v)) next

          # Project onto circle: normalize to unit vector, then scale to circle_radius
          dist <- sqrt(warped_v[1]^2 + warped_v[2]^2)
          if (dist > 0) {
            projected <- c(
              warped_v[1] / dist * circle_radius,
              warped_v[2] / dist * circle_radius
            )

            # Update the vertex in all pieces that share it
            for (piece_id in 1:num_pieces) {
              for (i in 1:6) {
                orig_v <- piece_vertices_original[[piece_id]][[i]]
                orig_key <- sprintf("%.1f,%.1f", orig_v[1], orig_v[2])

                if (orig_key == v_key) {
                  piece_vertices[[piece_id]][[i]] <- projected
                }
              }
            }
          }
        }

        cat(sprintf("Boundary vertices projected to circle radius %.2f\n", circle_radius))
      }
    }

    # Step 2: If do_trunc (but not do_warp), apply hexagonal truncation to boundary only
    # Note: If do_warp is already applied, boundaries get handled differently (arcs for border edges)
    if (do_trunc && !do_warp) {
      # Hexagonal truncation: project boundary vertices onto regular hexagon
      for (v_key in boundary_vertex_keys) {
        v <- vertex_sharing[[v_key]]$coords
        transformed <- apply_hex_trunc(v[1], v[2], max_boundary_dist)

        # Update only the boundary vertex
        for (piece_id in 1:num_pieces) {
          for (i in 1:6) {
            orig_v <- piece_vertices_original[[piece_id]][[i]]
            orig_key <- sprintf("%.1f,%.1f", orig_v[1], orig_v[2])

            if (orig_key == v_key) {
              piece_vertices[[piece_id]][[i]] <- c(transformed$x, transformed$y)
            }
          }
        }
      }

      cat(sprintf("Hexagonal truncation enabled - boundary at radius %.2f\n", max_boundary_dist))
    }

    # Log if neither warp nor trunc is applied (shouldn't reach here due to outer if)
    if (!do_warp && !do_trunc) {
      cat("No transformation (zigzag boundary)\n")
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

        # Match complete mode semantics: arcs only when BOTH do_trunc AND do_warp
        if (do_trunc && do_warp) {
          # For circular puzzle with do_warp+do_trunc, the boundary is a circle
          # with radius = diameter/2 (matching complete mode)
          # Note: vertices are warped, so we use the target circular radius
          circle_radius <- diameter / 2

          # Determine sweep direction based on vertex order
          # Cross product determines if we're going clockwise or counter-clockwise
          cross <- v1[1] * v2[2] - v1[2] * v2[1]
          sweep <- if (cross > 0) 1 else 0

          # Create arc command for border edge
          # Using the consistent circle_radius for all border arcs
          forward_arc <- sprintf("A %.2f %.2f 0 0 %d %.2f %.2f",
                                 circle_radius, circle_radius, sweep,
                                 v2[1], v2[2])
          reverse_arc <- sprintf("A %.2f %.2f 0 0 %d %.2f %.2f",
                                 circle_radius, circle_radius, 1 - sweep,
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
          # No warp (do_trunc only or neither) - straight line
          # Vertices may still be transformed (truncated hexagon) but edges are straight
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
#' @param do_trunc Truncate boundary to clean geometric shape (default: FALSE)
#' @return List of piece objects
#'
#' @export
generate_hex_pieces_with_edge_map <- function(rings, seed, diameter = 240,
                                               tabsize = 27, jitter = 5,
                                               separated = TRUE,
                                               base_spacing = NULL,
                                               separation_factor = 1.0,
                                               do_warp = FALSE,
                                               do_trunc = FALSE) {
  # Source dependencies
  if (!exists("map_piece_id_to_ring")) {
    source("R/hexagonal_topology.R")
  }

  # Generate edge mapping
  cat("Creating edge mapping...\n")
  edge_data <- generate_hex_edge_map(rings, seed, diameter, tabsize, jitter, do_warp, do_trunc)
  cat(sprintf("Generated %d unique edges\n", edge_data$num_edges))

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
    # Handles multiple commands in sequence (e.g., "C ... C ... C ...")
    offset_path_coords <- function(path_segment, offset_x, offset_y) {
      # Split by command letters, keeping the delimiters
      # This handles paths like "C 1 2 3 4 5 6 C 7 8 9 10 11 12"
      tokens <- unlist(strsplit(path_segment, "(?=[CLMA])", perl = TRUE))
      tokens <- tokens[nchar(trimws(tokens)) > 0]

      result_parts <- c()

      for (token in tokens) {
        token <- trimws(token)
        if (nchar(token) == 0) next

        cmd <- substr(token, 1, 1)
        rest <- substr(token, 2, nchar(token))

        if (cmd == "A") {
          # Arc command: A rx ry x-axis-rotation large-arc-flag sweep-flag x y
          # Only the last two values (x, y) are coordinates to offset
          numbers <- as.numeric(unlist(strsplit(rest, "\\s+")))
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

            result_parts <- c(result_parts,
              sprintf("A %.2f %.2f %.0f %d %d %.2f %.2f",
                      rx, ry, rotation, large_arc, sweep, x, y))
          } else {
            # Malformed arc, keep as-is
            result_parts <- c(result_parts, token)
          }
        } else if (cmd == "L") {
          # Line command: L x y
          numbers <- as.numeric(unlist(strsplit(rest, "\\s+")))
          numbers <- numbers[!is.na(numbers)]

          if (length(numbers) >= 2) {
            x <- numbers[1] + offset_x
            y <- numbers[2] + offset_y
            result_parts <- c(result_parts, sprintf("L %.2f %.2f", x, y))
          } else {
            result_parts <- c(result_parts, token)
          }
        } else if (cmd == "C") {
          # Cubic bezier: C x1 y1 x2 y2 x3 y3
          numbers <- as.numeric(unlist(strsplit(rest, "\\s+")))
          numbers <- numbers[!is.na(numbers)]

          if (length(numbers) >= 6) {
            x1 <- numbers[1] + offset_x
            y1 <- numbers[2] + offset_y
            x2 <- numbers[3] + offset_x
            y2 <- numbers[4] + offset_y
            x3 <- numbers[5] + offset_x
            y3 <- numbers[6] + offset_y
            result_parts <- c(result_parts,
              sprintf("C %.2f %.2f %.2f %.2f %.2f %.2f", x1, y1, x2, y2, x3, y3))
          } else {
            result_parts <- c(result_parts, token)
          }
        } else if (cmd == "M") {
          # Move command: M x y
          numbers <- as.numeric(unlist(strsplit(rest, "\\s+")))
          numbers <- numbers[!is.na(numbers)]

          if (length(numbers) >= 2) {
            x <- numbers[1] + offset_x
            y <- numbers[2] + offset_y
            result_parts <- c(result_parts, sprintf("M %.2f %.2f", x, y))
          } else {
            result_parts <- c(result_parts, token)
          }
        } else {
          # Unknown command, keep as-is
          result_parts <- c(result_parts, token)
        }
      }

      return(paste(result_parts, collapse = " "))
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
