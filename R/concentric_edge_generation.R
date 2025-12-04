# Concentric Ring Edge Generation
#
# Generates edges with bezier tabs for concentric ring puzzles

#' Generate edge map for concentric ring puzzle
#'
#' Creates unique edges with bezier curves for all piece connections.
#'
#' @param rings Number of rings
#' @param seed Random seed for tab generation
#' @param diameter Puzzle diameter
#' @param tabsize Tab size parameter
#' @param jitter Jitter parameter for tab variation
#' @param center_shape "hexagon" or "circle"
#' @return List with edge_map, piece_edges, and piece_vertices
#' @export
generate_concentric_edge_map <- function(rings, seed, diameter,
                                          tabsize = 27, jitter = 5,
                                          center_shape = "hexagon") {
  # Source dependencies
  if (!exists("generate_hex_bezier_edge")) {
    source("R/hexagonal_bezier_generation.R")
  }

  num_pieces <- get_concentric_piece_count(rings)
  piece_height <- get_concentric_piece_height(diameter, rings)
  tab_params <- list(tabsize = tabsize, jitter = jitter)

  # Get all piece vertices
  all_vertices <- get_all_concentric_vertices(rings, diameter, center_shape)

  # Edge storage
  edge_map <- list()
  piece_edge_map <- list()
  edge_counter <- 0

  # Process each piece

  for (piece_id in 1:num_pieces) {
    piece_info <- all_vertices[[piece_id]]

    if (piece_info$type == "circle") {
      # Circle center piece - connects to 6 pieces in ring 1
      # Generate 6 edges (arcs of the circle connecting to trapezoids)
      for (edge_idx in 1:6) {
        edge_key <- sprintf("%d-%d", piece_id, edge_idx)
        neighbor <- get_concentric_neighbor(piece_id, edge_idx, rings)

        if (!neighbor$is_boundary) {
          # Get the neighbor's inner edge vertices
          neighbor_info <- all_vertices[[neighbor$neighbor_id]]
          # The inner edge of trapezoid is V1 -> V2
          v1 <- neighbor_info$vertices[[1]]
          v2 <- neighbor_info$vertices[[2]]

          # Create unique edge key
          pieces_sorted <- sort(c(piece_id, neighbor$neighbor_id))
          unique_key <- sprintf("E%d-%d-%d", pieces_sorted[1], pieces_sorted[2], edge_idx)

          if (is.null(edge_map[[unique_key]])) {
            edge_counter <- edge_counter + 1
            edge_seed <- seed + pieces_sorted[1] * 1000 + pieces_sorted[2] * 10 + edge_idx

            # Generate bezier edge
            bezier <- generate_hex_bezier_edge(
              v1 = v2,  # Reverse order for circle's perspective
              v2 = v1,
              seed = edge_seed,
              edge_id = edge_counter,
              tab_params = tab_params
            )

            edge_map[[unique_key]] <- list(
              id = edge_counter,
              piece1 = piece_id,
              piece2 = neighbor$neighbor_id,
              forward = bezier$forward,
              reverse = bezier$reverse,
              start = v2,
              end = v1
            )
          }

          # Store reference for this piece
          is_primary <- (piece_id == pieces_sorted[1])
          piece_edge_map[[edge_key]] <- list(
            type = "internal",
            edge_ref = unique_key,
            is_forward = is_primary,
            start = if (is_primary) v2 else v1,
            end = if (is_primary) v1 else v2
          )
        }
      }

    } else if (piece_info$type == "hexagon") {
      # Hexagon center piece - 6 edges connecting to ring 1
      vertices <- piece_info$vertices

      for (edge_idx in 1:6) {
        v1 <- vertices[[edge_idx]]
        v2 <- vertices[[(edge_idx %% 6) + 1]]
        edge_key <- sprintf("%d-%d", piece_id, edge_idx)

        neighbor <- get_concentric_neighbor(piece_id, edge_idx, rings)

        if (!neighbor$is_boundary) {
          pieces_sorted <- sort(c(piece_id, neighbor$neighbor_id))
          unique_key <- sprintf("E%d-%d", pieces_sorted[1], pieces_sorted[2])

          if (is.null(edge_map[[unique_key]])) {
            edge_counter <- edge_counter + 1
            edge_seed <- seed + pieces_sorted[1] * 1000 + pieces_sorted[2]

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
              piece2 = neighbor$neighbor_id,
              forward = bezier$forward,
              reverse = bezier$reverse,
              start = v1,
              end = v2
            )
          }

          is_primary <- (piece_id == pieces_sorted[1])
          piece_edge_map[[edge_key]] <- list(
            type = "internal",
            edge_ref = unique_key,
            is_forward = is_primary,
            start = if (is_primary) v1 else v2,
            end = if (is_primary) v2 else v1
          )
        } else {
          # Boundary edge (shouldn't happen for center piece)
          piece_edge_map[[edge_key]] <- list(
            type = "border",
            forward = sprintf("L %.2f %.2f", v2[1], v2[2]),
            reverse = sprintf("L %.2f %.2f", v1[1], v1[2]),
            start = v1,
            end = v2,
            is_forward = TRUE
          )
        }
      }

    } else {
      # Trapezoid piece - 4 edges
      vertices <- piece_info$vertices

      for (edge_idx in 1:4) {
        v1 <- vertices[[edge_idx]]
        v2 <- vertices[[(edge_idx %% 4) + 1]]
        edge_key <- sprintf("%d-%d", piece_id, edge_idx)

        neighbor <- get_concentric_neighbor(piece_id, edge_idx, rings)

        if (!neighbor$is_boundary) {
          # Internal edge with tab
          pieces_sorted <- sort(c(piece_id, neighbor$neighbor_id))
          unique_key <- sprintf("E%d-%d-%d", pieces_sorted[1], pieces_sorted[2], edge_idx)

          if (is.null(edge_map[[unique_key]])) {
            edge_counter <- edge_counter + 1
            edge_seed <- seed + pieces_sorted[1] * 1000 + pieces_sorted[2] * 10 + edge_idx

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
              piece2 = neighbor$neighbor_id,
              forward = bezier$forward,
              reverse = bezier$reverse,
              start = v1,
              end = v2
            )
          }

          is_primary <- (piece_id == pieces_sorted[1])
          piece_edge_map[[edge_key]] <- list(
            type = "internal",
            edge_ref = unique_key,
            is_forward = is_primary,
            start = if (is_primary) v1 else v2,
            end = if (is_primary) v2 else v1
          )
        } else {
          # Boundary edge - straight line
          piece_edge_map[[edge_key]] <- list(
            type = "border",
            forward = sprintf("L %.2f %.2f", v2[1], v2[2]),
            reverse = sprintf("L %.2f %.2f", v1[1], v1[2]),
            start = v1,
            end = v2,
            is_forward = TRUE
          )
        }
      }
    }
  }

  return(list(
    edge_map = edge_map,
    piece_edge_map = piece_edge_map,
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

  if (piece_info$type == "circle") {
    # Circle: use arc commands
    r <- piece_info$radius
    # Full circle using two arcs
    path <- sprintf("M %.2f 0 A %.2f %.2f 0 1 1 %.2f 0 A %.2f %.2f 0 1 1 %.2f 0 Z",
                    r, r, r, -r, r, r, r)
    return(path)
  }

  if (piece_info$type == "hexagon") {
    # Hexagon center piece - 6 edges
    num_edges <- 6
    vertices <- piece_info$vertices
  } else {
    # Trapezoid - 4 edges
    num_edges <- 4
    vertices <- piece_info$vertices
  }

  # Build path from edges
  path_parts <- c()

  # Start at first vertex
  v1 <- vertices[[1]]
  path_parts <- c(path_parts, sprintf("M %.2f %.2f", v1[1], v1[2]))

  # Add each edge
  for (edge_idx in 1:num_edges) {
    edge_key <- sprintf("%d-%d", piece_id, edge_idx)
    edge_info <- edge_data$piece_edge_map[[edge_key]]

    if (is.null(edge_info)) {
      # Fallback to straight line
      v2 <- vertices[[(edge_idx %% num_edges) + 1]]
      path_parts <- c(path_parts, sprintf("L %.2f %.2f", v2[1], v2[2]))
    } else if (edge_info$type == "border") {
      path_parts <- c(path_parts, edge_info$forward)
    } else {
      # Internal edge - get from edge map
      edge_ref <- edge_info$edge_ref
      edge <- edge_data$edge_map[[edge_ref]]

      if (edge_info$is_forward) {
        path_parts <- c(path_parts, edge$forward)
      } else {
        path_parts <- c(path_parts, edge$reverse)
      }
    }
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
#' @return List of piece objects with paths
#' @export
generate_concentric_pieces <- function(rings, seed, diameter,
                                        tabsize = 27, jitter = 5,
                                        center_shape = "hexagon") {
  cat("Creating concentric edge mapping...\n")
  edge_data <- generate_concentric_edge_map(
    rings = rings,
    seed = seed,
    diameter = diameter,
    tabsize = tabsize,
    jitter = jitter,
    center_shape = center_shape
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
