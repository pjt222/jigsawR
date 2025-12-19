# Fixed Hexagonal Edge Generation
# Proper edge mapping with unique edge IDs

#' Create a vertex key string from coordinates
#'
#' Normalizes coordinates to avoid floating point comparison issues.
#' - Rounds to 1 decimal place to handle tiny floating point errors
#' - Converts -0.0 to 0.0 for consistent string keys
#'
#' @param x X coordinate
#' @param y Y coordinate
#' @return String key in format "x.x,y.y"
#' @keywords internal
make_vertex_key <- function(x, y) {
  # Round to 1 decimal place to handle floating point precision issues
  # (e.g., -1.776357e-15 should become 0.0, not -0.0)
  x_rounded <- round(x, 1)
  y_rounded <- round(y, 1)

  # Add 0.0 to normalize -0.0 to 0.0
  sprintf("%.1f,%.1f", x_rounded + 0.0, y_rounded + 0.0)
}

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
#' @param do_circular_border Use perfect circular arc borders (requires do_warp=TRUE)
#' @return List with edge_map (unique edges) and piece_edges (piece-to-edge mapping)
#'
#' @export
generate_hex_edge_map <- function(rings, seed, diameter, tabsize = 27, jitter = 5,
                                  do_warp = FALSE, do_trunc = FALSE,
                                  do_circular_border = FALSE) {
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
  # Correct formula: diameter / (4 * rings - 2)
  # This ensures that after warp transformation, boundary vertices reach diameter/2
  # The old formula diameter / (rings * 4) produced coordinates that were too small
  piece_radius <- diameter / (4 * rings - 2)
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

  # Pre-identified boundary edges (computed before transformations to avoid matching issues)
  # We always compute these based on original topology
  boundary_edge_keys <- c()

  # Build a map of vertex -> pieces that share it (always needed for boundary detection)
  vertex_sharing <- list()

  for (piece_id in 1:num_pieces) {
    for (i in 1:6) {
      v <- piece_vertices_original[[piece_id]][[i]]
      v_key <- make_vertex_key(v[1], v[2])

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

  # Pre-identify boundary edges BEFORE any vertex transformations
  # A boundary edge has BOTH vertices as boundary vertices AND is only used by one piece
  for (piece_id in 1:num_pieces) {
    for (side in 0:5) {
      v1 <- piece_vertices_original[[piece_id]][[side + 1]]
      v2 <- piece_vertices_original[[piece_id]][[(side + 1) %% 6 + 1]]
      v1_key <- make_vertex_key(v1[1], v1[2])
      v2_key <- make_vertex_key(v2[1], v2[2])

      # An edge is a boundary edge if BOTH its vertices are boundary vertices
      if (v1_key %in% boundary_vertex_keys && v2_key %in% boundary_vertex_keys) {
        # Also check that no other piece shares this exact edge
        v1_pieces <- unique(vertex_sharing[[v1_key]]$pieces)
        v2_pieces <- unique(vertex_sharing[[v2_key]]$pieces)
        shared_pieces <- intersect(v1_pieces, v2_pieces)

        # If only this piece uses both vertices, it's a boundary edge
        if (length(shared_pieces) == 1) {
          boundary_edge_keys <- c(boundary_edge_keys, sprintf("%d-%d", piece_id, side))
        }
      }
    }
  }

  # Circle radius for circular border (computed if needed)
  circle_radius <- NULL

  # Apply vertex transformations if needed
  if (do_warp || do_trunc) {

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
          v_key <- make_vertex_key(v[1], v[2])

          if (!is.null(all_transformed[[v_key]])) {
            piece_vertices[[piece_id]][[i]] <- all_transformed[[v_key]]
          }
        }
      }

      cat("Circular warp enabled - ALL vertices transformed\n")

      # If do_trunc is enabled, project boundary vertices to circle radius
      # This gives a clean circular outline. The difference is:
      # - do_trunc only: Projects to circle, uses straight lines (L) for border edges
      # - do_trunc + do_circular_border: Projects to circle, uses arc commands (A) for border edges
      if (do_trunc) {
        # Use the target diameter/2 as the circle radius
        # With the corrected piece_radius formula (diameter / (4*rings - 2)),
        # the warped boundary vertices are already at approximately this distance,
        # so projection causes minimal distortion.
        circle_radius <- diameter / 2
        cat(sprintf("Truncation enabled - projecting boundary to radius %.2f\n", circle_radius))

        # Project only boundary vertices to circle radius
        for (piece_id in 1:num_pieces) {
          for (i in 1:6) {
            orig_v <- piece_vertices_original[[piece_id]][[i]]
            orig_key <- make_vertex_key(orig_v[1], orig_v[2])

            if (orig_key %in% boundary_vertex_keys) {
              # Get current warped position
              current_v <- piece_vertices[[piece_id]][[i]]
              current_dist <- sqrt(current_v[1]^2 + current_v[2]^2)

              if (current_dist > 0) {
                # Project to circle radius
                scale <- circle_radius / current_dist
                piece_vertices[[piece_id]][[i]] <- c(
                  current_v[1] * scale,
                  current_v[2] * scale
                )
              }
            }
          }
        }
      } else {
        # Note: We do NOT project boundary vertices to a circle radius.
        # Projecting causes outer pieces to stretch (vertices get moved).
        # Instead, we keep vertices at their natural warped positions.
        # Border edges will use straight lines (L) connecting these positions,
        # which preserves consistent piece sizes across all rings.
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
            orig_key <- make_vertex_key(orig_v[1], orig_v[2])

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

      # Check if this is a pre-identified boundary edge
      # This is crucial when vertices have been transformed (warp+trunc can make
      # non-adjacent vertices appear to match due to circle projection)
      edge_key_check <- sprintf("%d-%d", piece_id, side)
      is_boundary_edge <- edge_key_check %in% boundary_edge_keys

      # Find neighbor (only if not a boundary edge)
      # NOTE: We use brute-force vertex matching here because the geometry side
      # (based on vertex order) doesn't directly correspond to the topology side
      # (based on axial coordinates). The adjacency matrix uses topology sides,
      # so we can't use it directly here without complex geo-topo conversion.
      # See Insights #26-27, #43 for details on the geo-topo mapping complexity.
      neighbor_id <- NA
      neighbor_side <- NA

      if (!is_boundary_edge) {
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
      }

      if (is_boundary_edge || is.na(neighbor_id)) {
        # Border edge - no neighbor, this is on the puzzle boundary
        edge_key <- sprintf("%d-%d", piece_id, side)

        if (do_circular_border && do_warp && !is.null(circle_radius)) {
          # Use arc commands for perfect circular border
          # SVG arc: A rx ry x-axis-rotation large-arc-flag sweep-flag x y
          # For a circular arc: rx = ry = circle_radius
          # x-axis-rotation = 0
          # large-arc-flag = 0 (small arc, < 180°)
          # sweep-flag = 1 (clockwise)
          piece_edge_map[[edge_key]] <- list(
            type = "border",
            forward = sprintf("A %.2f %.2f 0 0 1 %.2f %.2f",
                              circle_radius, circle_radius, v2[1], v2[2]),
            reverse = sprintf("A %.2f %.2f 0 0 0 %.2f %.2f",
                              circle_radius, circle_radius, v1[1], v1[2]),
            start = v1,
            end = v2,
            is_forward = TRUE,
            warped = TRUE,
            circular_border = TRUE
          )
        } else {
          # Use straight lines for borders
          # This preserves consistent piece sizes by keeping vertices at their
          # natural positions (warped or original) without any projection.
          #
          # When do_warp is enabled, the warp transformation already creates
          # a circular shape - we don't need arcs to smooth it further.
          # Using arcs would require projecting vertices to a common radius,
          # which causes piece size distortion (outer pieces stretch).
          piece_edge_map[[edge_key]] <- list(
            type = "border",
            forward = sprintf("L %.2f %.2f", v2[1], v2[2]),
            reverse = sprintf("L %.2f %.2f", v1[1], v1[2]),
            start = v1,
            end = v2,
            is_forward = TRUE,
            warped = do_warp  # Track if warp was applied for reference
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
#' @param do_circular_border Use perfect circular arc borders (requires do_warp=TRUE)
#' @return List of piece objects
#'
#' @export
generate_hex_pieces_with_edge_map <- function(rings, seed, diameter = 240,
                                               tabsize = 27, jitter = 5,
                                               separated = TRUE,
                                               base_spacing = NULL,
                                               separation_factor = 1.0,
                                               do_warp = FALSE,
                                               do_trunc = FALSE,
                                               do_circular_border = FALSE) {
  # Source dependencies
  if (!exists("map_piece_id_to_ring")) {
    source("R/hexagonal_topology.R")
  }

  # Generate edge mapping
  cat("Creating edge mapping...\n")
  edge_data <- generate_hex_edge_map(rings, seed, diameter, tabsize, jitter,
                                      do_warp, do_trunc, do_circular_border)
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
    # Correct formula: diameter / (4 * rings - 2)
    piece_radius <- diameter / (4 * rings - 2)
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
