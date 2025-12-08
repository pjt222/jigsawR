# Unified Adjacency API for jigsawR
#
# Provides consistent neighbor detection across all puzzle types:
# - Rectangular (grid-based)
# - Hexagonal (ring-based, 6 sides)
# - Concentric (ring-based, 4 edges)
#
# Also includes fusion group validation and input parsing.

# ============================================================================
# MAIN DISPATCH FUNCTION
# ============================================================================

#' Get neighbors of a piece
#'
#' Returns all neighbors of a piece in any puzzle type. This is the main
#' entry point for adjacency queries.
#'
#' @param piece_id Piece identifier. For rectangular: integer (1-based index) or
#'   string ("piece_0_0"). For hexagonal/concentric: integer (1-based).
#' @param puzzle_result Output from \code{generate_puzzle()} containing pieces
#'   and parameters.
#' @param include_boundary If TRUE, include boundary edges (with neighbor_id = NA).
#'   Default TRUE.
#'
#' @return Data frame with columns:
#'   \describe{
#'     \item{direction}{Direction name (N/E/S/W for rectangular, 0-5 for hexagonal,
#'       INNER/RIGHT/OUTER/LEFT for concentric)}
#'     \item{neighbor_id}{Neighbor piece ID (integer), or NA if boundary edge}
#'     \item{is_boundary}{TRUE if this is a boundary edge (no neighbor)}
#'   }
#'
#' @examples
#' \dontrun{
#' result <- generate_puzzle(type = "rectangular", grid = c(3, 3), seed = 42)
#' neighbors <- get_piece_neighbors(5, result)  # Center piece has 4 neighbors
#' }
#'
#' @export
get_piece_neighbors <- function(piece_id, puzzle_result, include_boundary = TRUE) {
  type <- puzzle_result$type %||% puzzle_result$parameters$type

  if (is.null(type)) {
    stop("Cannot determine puzzle type from puzzle_result")
  }

  if (type == "rectangular") {
    return(get_rect_neighbors(piece_id, puzzle_result, include_boundary))
  } else if (type == "hexagonal") {
    return(get_hex_neighbors_unified(piece_id, puzzle_result, include_boundary))
  } else if (type == "concentric") {
    return(get_concentric_neighbors_unified(piece_id, puzzle_result, include_boundary))
  } else {
    stop(sprintf("Unknown puzzle type: %s", type))
  }
}

#' Check if two pieces are adjacent
#'
#' @param piece_id_a First piece ID (integer)
#' @param piece_id_b Second piece ID (integer)
#' @param puzzle_result Output from \code{generate_puzzle()}
#'
#' @return TRUE if pieces share an edge, FALSE otherwise
#'
#' @export
are_pieces_adjacent <- function(piece_id_a, piece_id_b, puzzle_result) {
  # Normalize to integers
  id_a <- normalize_piece_id(piece_id_a, puzzle_result)
  id_b <- normalize_piece_id(piece_id_b, puzzle_result)

  # Get neighbors of piece A

neighbors <- get_piece_neighbors(id_a, puzzle_result, include_boundary = FALSE)

  # Check if piece B is among them
  return(id_b %in% neighbors$neighbor_id)
}

#' Validate a fusion group (all pieces must be connected)
#'
#' Checks that all pieces in a fusion group form a connected component,
#' meaning each piece is reachable from any other piece through adjacency.
#'
#' @param piece_ids Vector of piece IDs to validate
#' @param puzzle_result Output from \code{generate_puzzle()}
#'
#' @return List with:
#'   \describe{
#'     \item{valid}{TRUE if group is connected, FALSE otherwise}
#'     \item{message}{Explanation if invalid}
#'     \item{components}{If invalid, list of disconnected components}
#'   }
#'
#' @export
validate_fusion_group <- function(piece_ids, puzzle_result) {
  if (length(piece_ids) < 2) {
    return(list(valid = TRUE, message = "Single piece or empty group"))
  }

  # Normalize all IDs to integers
  ids <- sapply(piece_ids, function(id) normalize_piece_id(id, puzzle_result))

  # Build adjacency within the group using BFS/DFS
  visited <- rep(FALSE, length(ids))
  names(visited) <- as.character(ids)

  # Start BFS from first piece
  queue <- ids[1]
  visited[as.character(ids[1])] <- TRUE

  while (length(queue) > 0) {
    current <- queue[1]
    queue <- queue[-1]

    # Get neighbors of current piece
    neighbors <- get_piece_neighbors(current, puzzle_result, include_boundary = FALSE)

    # Check which neighbors are in our group and unvisited
    for (neighbor_id in neighbors$neighbor_id) {
      if (!is.na(neighbor_id) && neighbor_id %in% ids) {
        neighbor_key <- as.character(neighbor_id)
        if (!visited[neighbor_key]) {
          visited[neighbor_key] <- TRUE
          queue <- c(queue, neighbor_id)
        }
      }
    }
  }

  # Check if all pieces were visited
  if (all(visited)) {
    return(list(valid = TRUE, message = "Group is connected"))
  } else {
    unvisited <- ids[!visited]
    return(list(
      valid = FALSE,
      message = sprintf("Pieces %s are not connected to the rest of the group",
                        paste(unvisited, collapse = ", ")),
      disconnected = unvisited
    ))
  }
}

# ============================================================================
# RECTANGULAR NEIGHBORS (NEW IMPLEMENTATION)
# ============================================================================

#' Get neighbors for a rectangular piece
#'
#' @param piece_id Piece ID (integer 1-based, or string "piece_xi_yi")
#' @param puzzle_result Output from generate_puzzle()
#' @param include_boundary Include boundary edges in result
#'
#' @return Data frame with direction, neighbor_id, is_boundary columns
#'
#' @keywords internal
get_rect_neighbors <- function(piece_id, puzzle_result, include_boundary = TRUE) {
  # Get grid dimensions
  grid <- puzzle_result$parameters$grid
  if (is.null(grid)) {
    stop("Cannot find grid dimensions in puzzle_result")
  }

  yn <- grid[1]  # rows
  xn <- grid[2]  # columns

  # Parse piece_id to get grid position
  pos <- parse_rect_piece_id(piece_id, xn, yn)
  xi <- pos$xi
  yi <- pos$yi

  # Build neighbors data frame
  neighbors <- data.frame(
    direction = character(),
    neighbor_id = integer(),
    is_boundary = logical(),
    stringsAsFactors = FALSE
  )

  # Check all 4 directions (N, E, S, W)
  directions <- list(
    list(name = "N", dx = 0, dy = -1),
    list(name = "E", dx = 1, dy = 0),
    list(name = "S", dx = 0, dy = 1),
    list(name = "W", dx = -1, dy = 0)
  )

  for (dir in directions) {
    nx <- xi + dir$dx
    ny <- yi + dir$dy

    is_boundary <- nx < 0 || nx >= xn || ny < 0 || ny >= yn

    if (!is_boundary) {
      # Calculate neighbor's piece index (1-based)
      neighbor_idx <- ny * xn + nx + 1
    } else {
      neighbor_idx <- NA_integer_
    }

    if (include_boundary || !is_boundary) {
      neighbors <- rbind(neighbors, data.frame(
        direction = dir$name,
        neighbor_id = neighbor_idx,
        is_boundary = is_boundary,
        stringsAsFactors = FALSE
      ))
    }
  }

  return(neighbors)
}

#' Parse rectangular piece ID to grid position
#'
#' @param piece_id Integer (1-based) or string ("piece_xi_yi")
#' @param xn Number of columns
#' @param yn Number of rows
#'
#' @return List with xi (0-based column) and yi (0-based row)
#'
#' @keywords internal
parse_rect_piece_id <- function(piece_id, xn, yn) {
  if (is.character(piece_id)) {
    # Parse "piece_xi_yi" format
    if (grepl("^piece_\\d+_\\d+$", piece_id)) {
      parts <- strsplit(piece_id, "_")[[1]]
      xi <- as.integer(parts[2])
      yi <- as.integer(parts[3])
    } else {
      stop(sprintf("Invalid rectangular piece ID format: %s", piece_id))
    }
  } else {
    # Convert 1-based index to grid position
    idx <- as.integer(piece_id) - 1
    yi <- idx %/% xn
    xi <- idx %% xn
  }

  # Validate
  if (xi < 0 || xi >= xn || yi < 0 || yi >= yn) {
    stop(sprintf("Piece position (%d, %d) out of bounds for grid %dx%d",
                 xi, yi, xn, yn))
  }

  return(list(xi = xi, yi = yi))
}

#' Convert rectangular grid position to piece index
#'
#' @param xi Column (0-based)
#' @param yi Row (0-based)
#' @param xn Number of columns
#'
#' @return 1-based piece index
#'
#' @keywords internal
rect_pos_to_index <- function(xi, yi, xn) {
  yi * xn + xi + 1
}

# ============================================================================
# HEXAGONAL NEIGHBORS (WRAPPER)
# ============================================================================

#' Get neighbors for a hexagonal piece (unified interface)
#'
#' Wraps the existing get_hex_neighbor() function to provide consistent output.
#'
#' @param piece_id Piece ID (integer 1-based)
#' @param puzzle_result Output from generate_puzzle()
#' @param include_boundary Include boundary edges in result
#'
#' @return Data frame with direction, neighbor_id, is_boundary columns
#'
#' @keywords internal
get_hex_neighbors_unified <- function(piece_id, puzzle_result, include_boundary = TRUE) {
  # Get rings from parameters
  rings <- puzzle_result$parameters$grid[1]
  if (is.null(rings)) {
    stop("Cannot find rings in puzzle_result parameters")
  }

  # Normalize piece_id
  id <- normalize_piece_id(piece_id, puzzle_result)

  # Build neighbors data frame
  neighbors <- data.frame(
    direction = character(),
    neighbor_id = integer(),
    is_boundary = logical(),
    stringsAsFactors = FALSE
  )

  # Check all 6 sides
  for (side in 0:5) {
    neighbor_id <- get_hex_neighbor(id, side, rings)
    is_boundary <- is.na(neighbor_id)

    if (include_boundary || !is_boundary) {
      neighbors <- rbind(neighbors, data.frame(
        direction = as.character(side),
        neighbor_id = if (is_boundary) NA_integer_ else as.integer(neighbor_id),
        is_boundary = is_boundary,
        stringsAsFactors = FALSE
      ))
    }
  }

  return(neighbors)
}

# ============================================================================
# CONCENTRIC NEIGHBORS (WRAPPER)
# ============================================================================

#' Get neighbors for a concentric piece (unified interface)
#'
#' Wraps get_concentric_neighbor() to provide consistent output.
#'
#' @param piece_id Piece ID (integer 1-based)
#' @param puzzle_result Output from generate_puzzle()
#' @param include_boundary Include boundary edges in result
#'
#' @return Data frame with direction, neighbor_id, is_boundary columns
#'
#' @keywords internal
get_concentric_neighbors_unified <- function(piece_id, puzzle_result, include_boundary = TRUE) {
  # Get rings from parameters
  rings <- puzzle_result$parameters$grid[1]
  if (is.null(rings)) {
    stop("Cannot find rings in puzzle_result parameters")
  }

  # Normalize piece_id
  id <- normalize_piece_id(piece_id, puzzle_result)

  # Get piece info to determine edge count
  info <- map_concentric_piece_id(id, rings)

  # Build neighbors data frame
  neighbors <- data.frame(
    direction = character(),
    neighbor_id = integer(),
    is_boundary = logical(),
    stringsAsFactors = FALSE
  )

  # Edge directions depend on piece type
  if (info$ring == 0) {
    # Center piece has 6 edges
    edge_names <- as.character(1:6)
    edges <- 1:6
  } else {
    # Trapezoid pieces have 4 edges
    edge_names <- c("INNER", "RIGHT", "OUTER", "LEFT")
    edges <- 1:4
  }

  for (i in seq_along(edges)) {
    result <- get_concentric_neighbor(id, edges[i], rings)
    is_boundary <- result$is_boundary

    if (include_boundary || !is_boundary) {
      neighbors <- rbind(neighbors, data.frame(
        direction = edge_names[i],
        neighbor_id = if (is_boundary) NA_integer_ else as.integer(result$neighbor_id),
        is_boundary = is_boundary,
        stringsAsFactors = FALSE
      ))
    }
  }

  return(neighbors)
}

# ============================================================================
# FUSION INPUT PARSING
# ============================================================================

#' Parse fusion group input string
#'
#' Converts string input format to list of integer vectors.
#'
#' @param input String like "1,2" or "(1,2),(7,8,9)" or list
#' @param puzzle_result Puzzle result for ID normalization
#'
#' @return List of integer vectors, each representing a fusion group
#'
#' @examples
#' \dontrun{
#' parse_fusion_input("1,2")             # -> list(c(1, 2))
#' parse_fusion_input("(1,2),(7,8,9)")   # -> list(c(1, 2), c(7, 8, 9))
#' parse_fusion_input(list(c(1, 2)))     # -> list(c(1, 2)) (pass-through)
#' }
#'
#' @export
parse_fusion_input <- function(input, puzzle_result = NULL) {
  if (is.null(input) || length(input) == 0) {
    return(list())
  }

  # If already a list, normalize and return
  if (is.list(input)) {
    return(lapply(input, function(group) {
      as.integer(group)
    }))
  }

  # Parse string format
  if (!is.character(input)) {
    stop("Fusion input must be a string or list")
  }

  input <- trimws(input)

  if (nchar(input) == 0) {
    return(list())
  }

  # Check format: "(1,2),(3,4)" or "1,2"
  if (grepl("^\\(", input)) {
    # Multiple groups: "(1,2),(3,4,5)"
    # Extract each group
    groups <- regmatches(input, gregexpr("\\([^)]+\\)", input))[[1]]
    result <- lapply(groups, function(g) {
      # Remove parentheses and split
      inner <- gsub("[()]", "", g)
      as.integer(strsplit(inner, ",")[[1]])
    })
  } else {
    # Single group: "1,2,3"
    result <- list(as.integer(strsplit(input, ",")[[1]]))
  }

  return(result)
}

#' Validate all fusion groups
#'
#' Checks that all fusion groups are valid (connected, no overlaps).
#'
#' @param fusion_groups List of integer vectors (from parse_fusion_input)
#' @param puzzle_result Output from generate_puzzle()
#'
#' @return List with valid (TRUE/FALSE), message, and details
#'
#' @export
validate_all_fusion_groups <- function(fusion_groups, puzzle_result) {
  if (is.null(fusion_groups) || length(fusion_groups) == 0) {
    return(list(valid = TRUE, message = "No fusion groups"))
  }

  # Check for overlapping pieces
  all_pieces <- unlist(fusion_groups)
  if (length(all_pieces) != length(unique(all_pieces))) {
    dups <- all_pieces[duplicated(all_pieces)]
    return(list(
      valid = FALSE,
      message = sprintf("Pieces appear in multiple groups: %s", paste(dups, collapse = ", "))
    ))
  }

  # Check each group is connected
  for (i in seq_along(fusion_groups)) {
    group <- fusion_groups[[i]]
    result <- validate_fusion_group(group, puzzle_result)
    if (!result$valid) {
      return(list(
        valid = FALSE,
        message = sprintf("Group %d: %s", i, result$message)
      ))
    }
  }

  return(list(valid = TRUE, message = "All fusion groups are valid"))
}

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Normalize piece ID to integer
#'
#' Converts various piece ID formats to consistent integer representation.
#'
#' @param piece_id Piece ID in any format
#' @param puzzle_result Puzzle result for context
#'
#' @return Integer piece ID (1-based)
#'
#' @keywords internal
normalize_piece_id <- function(piece_id, puzzle_result) {
  type <- puzzle_result$type %||% puzzle_result$parameters$type

  if (is.numeric(piece_id)) {
    return(as.integer(piece_id))
  }

  if (!is.character(piece_id)) {
    stop(sprintf("Invalid piece_id type: %s", typeof(piece_id)))
  }

  # Parse string format
  if (type == "rectangular") {
    # "piece_xi_yi" -> index
    if (grepl("^piece_\\d+_\\d+$", piece_id)) {
      parts <- strsplit(piece_id, "_")[[1]]
      xi <- as.integer(parts[2])
      yi <- as.integer(parts[3])
      grid <- puzzle_result$parameters$grid
      xn <- grid[2]
      return(yi * xn + xi + 1)
    }
  }

  # "piece_N" -> N
  if (grepl("^piece_\\d+$", piece_id)) {
    return(as.integer(sub("piece_", "", piece_id)))
  }

  # Try direct integer parse
  if (grepl("^\\d+$", piece_id)) {
    return(as.integer(piece_id))
  }

  stop(sprintf("Cannot parse piece ID: %s", piece_id))
}

#' Get total piece count from puzzle result
#'
#' @param puzzle_result Output from generate_puzzle()
#'
#' @return Integer piece count
#'
#' @keywords internal
get_piece_count <- function(puzzle_result) {
  length(puzzle_result$pieces)
}

#' Null-coalescing operator
#'
#' @keywords internal
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

# ============================================================================
# EDGE FUSION MECHANISM
# ============================================================================

#' Create a standardized edge key
#'
#' Edge keys uniquely identify an edge by the piece ID and direction.
#' Format: "piece_id-direction" (e.g., "1-E", "5-3", "2-INNER")
#'
#' @param piece_id Piece ID (integer)
#' @param direction Direction string (N/E/S/W, 0-5, or INNER/RIGHT/OUTER/LEFT)
#'
#' @return String edge key
#'
#' @keywords internal
make_edge_key <- function(piece_id, direction) {
  sprintf("%d-%s", as.integer(piece_id), as.character(direction))
}

#' Get the complementary edge key
#'
#' Given an edge key, returns the key for the same edge from the neighbor's
#' perspective. For example, piece 1's east edge is piece 2's west edge.
#'
#' @param edge_key Edge key string
#' @param puzzle_result Puzzle result for adjacency lookup
#'
#' @return Complementary edge key, or NA if boundary edge
#'
#' @keywords internal
get_complementary_edge_key <- function(edge_key, puzzle_result) {
  # Parse edge key
  parts <- strsplit(edge_key, "-")[[1]]
  piece_id <- as.integer(parts[1])
  direction <- parts[2]

  type <- puzzle_result$type %||% puzzle_result$parameters$type

  # Get the neighbor and opposite direction
  if (type == "rectangular") {
    opposite <- switch(direction,
      "N" = "S", "S" = "N", "E" = "W", "W" = "E",
      NA
    )
    neighbors <- get_piece_neighbors(piece_id, puzzle_result, include_boundary = FALSE)
    neighbor_row <- neighbors[neighbors$direction == direction, ]
    if (nrow(neighbor_row) == 0 || is.na(neighbor_row$neighbor_id)) {
      return(NA)
    }
    return(make_edge_key(neighbor_row$neighbor_id, opposite))

  } else if (type == "hexagonal") {
    # Hexagonal: opposite side is (side + 3) mod 6
    side <- as.integer(direction)
    opposite <- (side + 3) %% 6
    neighbors <- get_piece_neighbors(piece_id, puzzle_result, include_boundary = FALSE)
    neighbor_row <- neighbors[neighbors$direction == as.character(side), ]
    if (nrow(neighbor_row) == 0 || is.na(neighbor_row$neighbor_id)) {
      return(NA)
    }
    return(make_edge_key(neighbor_row$neighbor_id, opposite))

  } else if (type == "concentric") {
    # Concentric: INNER<->OUTER (for radial), LEFT<->RIGHT (for circumferential)
    opposite <- switch(direction,
      "INNER" = "OUTER", "OUTER" = "INNER",
      "LEFT" = "RIGHT", "RIGHT" = "LEFT",
      NA
    )
    # For center piece edges (1-6), need special handling
    if (grepl("^[1-6]$", direction)) {
      # Center piece edge connects to ring 1 piece's INNER edge
      opposite <- "INNER"
    }
    neighbors <- get_piece_neighbors(piece_id, puzzle_result, include_boundary = FALSE)
    neighbor_row <- neighbors[neighbors$direction == direction, ]
    if (nrow(neighbor_row) == 0 || is.na(neighbor_row$neighbor_id)) {
      return(NA)
    }
    return(make_edge_key(neighbor_row$neighbor_id, opposite))
  }

  return(NA)
}

#' Compute fused edges from fusion groups
#'
#' Converts fusion groups to a set of edge keys that should be treated as
#' "fused" (internal edges within meta-pieces).
#'
#' @param fusion_groups List of integer vectors (piece IDs to fuse)
#' @param puzzle_result Output from generate_puzzle()
#'
#' @return List with:
#'   \describe{
#'     \item{fused_edges}{Character vector of edge keys to fuse}
#'     \item{edge_to_group}{Named list mapping edge keys to fusion group index}
#'     \item{piece_to_group}{Named list mapping piece IDs to fusion group index}
#'   }
#'
#' @export
compute_fused_edges <- function(fusion_groups, puzzle_result) {
  if (is.null(fusion_groups) || length(fusion_groups) == 0) {
    return(list(
      fused_edges = character(),
      edge_to_group = list(),
      piece_to_group = list()
    ))
  }

  fused_edges <- character()
  edge_to_group <- list()
  piece_to_group <- list()

  for (group_idx in seq_along(fusion_groups)) {
    group <- fusion_groups[[group_idx]]

    # Map pieces to this group
    for (piece_id in group) {
      piece_to_group[[as.character(piece_id)]] <- group_idx
    }

    # Find all internal edges (edges between pieces in this group)
    for (piece_id in group) {
      neighbors <- get_piece_neighbors(piece_id, puzzle_result, include_boundary = FALSE)

      for (i in seq_len(nrow(neighbors))) {
        neighbor_id <- neighbors$neighbor_id[i]
        direction <- neighbors$direction[i]

        # If neighbor is also in this group, this is an internal edge
        if (neighbor_id %in% group) {
          edge_key <- make_edge_key(piece_id, direction)

          if (!(edge_key %in% fused_edges)) {
            fused_edges <- c(fused_edges, edge_key)
            edge_to_group[[edge_key]] <- group_idx

            # Also add the complementary edge
            comp_key <- get_complementary_edge_key(edge_key, puzzle_result)
            if (!is.na(comp_key) && !(comp_key %in% fused_edges)) {
              fused_edges <- c(fused_edges, comp_key)
              edge_to_group[[comp_key]] <- group_idx
            }
          }
        }
      }
    }
  }

  return(list(
    fused_edges = fused_edges,
    edge_to_group = edge_to_group,
    piece_to_group = piece_to_group
  ))
}

#' Check if an edge is fused
#'
#' @param piece_id Piece ID
#' @param direction Edge direction
#' @param fused_edge_data Output from compute_fused_edges()
#'
#' @return TRUE if edge is fused (internal to a meta-piece), FALSE otherwise
#'
#' @export
is_edge_fused <- function(piece_id, direction, fused_edge_data) {
  if (is.null(fused_edge_data) || length(fused_edge_data$fused_edges) == 0) {
    return(FALSE)
  }

  edge_key <- make_edge_key(piece_id, direction)
  return(edge_key %in% fused_edge_data$fused_edges)
}

#' Get fusion group for a piece
#'
#' @param piece_id Piece ID
#' @param fused_edge_data Output from compute_fused_edges()
#'
#' @return Fusion group index (integer), or NA if piece is not in any group
#'
#' @export
get_piece_fusion_group <- function(piece_id, fused_edge_data) {
  if (is.null(fused_edge_data) || length(fused_edge_data$piece_to_group) == 0) {
    return(NA)
  }

  key <- as.character(piece_id)
  if (key %in% names(fused_edge_data$piece_to_group)) {
    return(fused_edge_data$piece_to_group[[key]])
  }
  return(NA)
}

#' Get all pieces in a fusion group
#'
#' @param group_idx Fusion group index
#' @param fusion_groups Original fusion groups list
#'
#' @return Vector of piece IDs in the group
#'
#' @export
get_fusion_group_pieces <- function(group_idx, fusion_groups) {
  if (is.null(fusion_groups) || group_idx < 1 || group_idx > length(fusion_groups)) {
    return(integer())
  }
  return(fusion_groups[[group_idx]])
}

#' Get internal edges of a fusion group
#'
#' Returns all edges that are internal to a fusion group (edges between
#' pieces within the same group).
#'
#' @param group_idx Fusion group index
#' @param fused_edge_data Output from compute_fused_edges()
#'
#' @return Character vector of edge keys
#'
#' @export
get_group_internal_edges <- function(group_idx, fused_edge_data) {
  if (is.null(fused_edge_data)) {
    return(character())
  }

  edges <- character()
  for (edge_key in names(fused_edge_data$edge_to_group)) {
    if (fused_edge_data$edge_to_group[[edge_key]] == group_idx) {
      edges <- c(edges, edge_key)
    }
  }
  return(edges)
}

#' Get boundary edges of a fusion group
#'
#' Returns all edges that form the outer boundary of a fusion group
#' (edges that connect to pieces outside the group or to the puzzle boundary).
#'
#' @param fusion_group Vector of piece IDs in the group
#' @param puzzle_result Output from generate_puzzle()
#' @param fused_edge_data Output from compute_fused_edges()
#'
#' @return Data frame with piece_id, direction, neighbor_id (NA if puzzle boundary)
#'
#' @export
get_group_boundary_edges <- function(fusion_group, puzzle_result, fused_edge_data) {
  boundary_edges <- data.frame(
    piece_id = integer(),
    direction = character(),
    neighbor_id = integer(),
    is_puzzle_boundary = logical(),
    stringsAsFactors = FALSE
  )

  for (piece_id in fusion_group) {
    neighbors <- get_piece_neighbors(piece_id, puzzle_result, include_boundary = TRUE)

    for (i in seq_len(nrow(neighbors))) {
      direction <- neighbors$direction[i]
      neighbor_id <- neighbors$neighbor_id[i]
      is_boundary <- neighbors$is_boundary[i]

      # Check if this edge is NOT fused (i.e., it's a boundary of the group)
      if (!is_edge_fused(piece_id, direction, fused_edge_data)) {
        boundary_edges <- rbind(boundary_edges, data.frame(
          piece_id = piece_id,
          direction = direction,
          neighbor_id = if (is.na(neighbor_id)) NA_integer_ else neighbor_id,
          is_puzzle_boundary = is_boundary,
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  return(boundary_edges)
}


# =============================================================================
# Concentric Puzzle Fusion Support
# =============================================================================

#' Get all neighbors for a concentric puzzle piece
#'
#' Returns neighbor information for all edges of a concentric piece.
#' Center piece (ring 0) has 6 edges, trapezoid pieces have 4 edges.
#'
#' @param piece_id Piece ID (1-based)
#' @param rings Number of rings in puzzle
#' @return Data frame with direction, neighbor_id, is_boundary
#' @export
get_concentric_neighbors <- function(piece_id, rings) {
  info <- map_concentric_piece_id(piece_id, rings)
  ring <- info$ring

  if (ring == 0) {
    # Center piece has 6 edges
    edges <- 1:6
    direction_names <- as.character(edges)
  } else {
    # Trapezoid pieces have 4 edges: INNER, RIGHT, OUTER, LEFT
    edges <- 1:4
    direction_names <- c("INNER", "RIGHT", "OUTER", "LEFT")
  }

  neighbors <- data.frame(
    direction = character(),
    neighbor_id = integer(),
    is_boundary = logical(),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(edges)) {
    edge_idx <- edges[i]
    neighbor_info <- get_concentric_neighbor(piece_id, edge_idx, rings)

    neighbors <- rbind(neighbors, data.frame(
      direction = direction_names[i],
      neighbor_id = if (is.na(neighbor_info$neighbor_id)) NA_integer_ else as.integer(neighbor_info$neighbor_id),
      is_boundary = neighbor_info$is_boundary,
      stringsAsFactors = FALSE
    ))
  }

  return(neighbors)
}


#' Get complementary direction for concentric puzzle
#'
#' Returns the opposite edge direction. For trapezoid pieces:
#' INNER <-> OUTER, LEFT <-> RIGHT.
#' For center piece edges (1-6), returns the opposite edge of the neighbor.
#'
#' @param direction Edge direction (INNER/RIGHT/OUTER/LEFT or 1-6 for center)
#' @return Opposite direction
#' @export
get_concentric_complementary_direction <- function(direction) {
  # Trapezoid edges
  if (direction == "INNER") return("OUTER")
  if (direction == "OUTER") return("INNER")
  if (direction == "LEFT") return("RIGHT")
  if (direction == "RIGHT") return("LEFT")

  # Center piece edges (1-6) - neighbor's INNER edge connects back
  if (direction %in% as.character(1:6)) {
    return("INNER")
  }

  return(NA_character_)
}


#' Compute fused edges for concentric puzzles
#'
#' Determines which edges should be fused (internal to meta-pieces) based on
#' fusion groups. An edge is fused if both pieces it connects are in the same
#' fusion group.
#'
#' @param fusion_groups List of piece ID vectors, where each vector represents
#'   pieces that should be fused together into a meta-piece
#' @param puzzle_result Output from generate_puzzle() containing piece data
#' @return List with:
#'   - fused_edges: Character vector of edge keys that are internal
#'   - edge_to_group: Named list mapping edge keys to group indices
#'   - piece_to_group: Named list mapping piece IDs to group indices
#' @export
compute_concentric_fused_edges <- function(fusion_groups, puzzle_result) {
  if (is.null(fusion_groups) || length(fusion_groups) == 0) {
    return(NULL)
  }

  rings <- puzzle_result$parameters$rings

  fused_edges <- character()
  edge_to_group <- list()
  piece_to_group <- list()

  # Build piece-to-group mapping

  for (group_idx in seq_along(fusion_groups)) {
    group <- fusion_groups[[group_idx]]
    for (piece_id in group) {
      piece_to_group[[as.character(piece_id)]] <- group_idx
    }
  }

  # Find edges between pieces in the same group
  for (group_idx in seq_along(fusion_groups)) {
    group <- fusion_groups[[group_idx]]

    for (piece_id in group) {
      # Get all neighbors for this piece
      neighbors <- get_concentric_neighbors(piece_id, rings)

      for (i in seq_len(nrow(neighbors))) {
        direction <- neighbors$direction[i]
        neighbor_id <- neighbors$neighbor_id[i]
        is_boundary <- neighbors$is_boundary[i]

        # Skip boundary edges
        if (is_boundary || is.na(neighbor_id)) {
          next
        }

        # Check if neighbor is in the same group
        if (neighbor_id %in% group) {
          # This edge is fused
          edge_key <- make_edge_key(piece_id, direction)
          if (!(edge_key %in% fused_edges)) {
            fused_edges <- c(fused_edges, edge_key)
            edge_to_group[[edge_key]] <- group_idx
          }

          # Also add the complementary edge from neighbor's perspective
          comp_direction <- get_concentric_complementary_direction(direction)
          if (!is.na(comp_direction)) {
            comp_edge_key <- make_edge_key(neighbor_id, comp_direction)
            if (!(comp_edge_key %in% fused_edges)) {
              fused_edges <- c(fused_edges, comp_edge_key)
              edge_to_group[[comp_edge_key]] <- group_idx
            }
          }
        }
      }
    }
  }

  return(list(
    fused_edges = fused_edges,
    edge_to_group = edge_to_group,
    piece_to_group = piece_to_group
  ))
}


# =============================================================================
# Hexagonal Puzzle Fusion Support
# =============================================================================

#' Get all neighbors for a hexagonal puzzle piece
#'
#' Returns neighbor information for all 6 sides of a hexagonal piece.
#'
#' @param piece_id Piece ID (1-based)
#' @param rings Number of rings in puzzle
#' @return Data frame with side (0-5), neighbor_id, is_boundary
#' @export
get_hex_neighbors_for_fusion <- function(piece_id, rings) {
  neighbors <- data.frame(
    direction = character(),
    neighbor_id = integer(),
    is_boundary = logical(),
    stringsAsFactors = FALSE
  )

  for (side in 0:5) {
    neighbor_id <- get_hex_neighbor(piece_id, side, rings)

    neighbors <- rbind(neighbors, data.frame(
      direction = as.character(side),
      neighbor_id = if (is.na(neighbor_id)) NA_integer_ else as.integer(neighbor_id),
      is_boundary = is.na(neighbor_id),
      stringsAsFactors = FALSE
    ))
  }

  return(neighbors)
}


#' Get complementary side for hexagonal puzzle
#'
#' Returns the opposite side number. For hexagons, the opposite of side n
#' is (n + 3) mod 6.
#'
#' @param side Side number (0-5) as character or integer
#' @return Opposite side number as character
#' @export
get_hex_complementary_side <- function(side) {
  side_num <- as.integer(side)
  opposite <- (side_num + 3) %% 6
  return(as.character(opposite))
}


#' Compute fused edges for hexagonal puzzles
#'
#' Determines which edges should be fused (internal to meta-pieces) based on
#' fusion groups. An edge is fused if both pieces it connects are in the same
#' fusion group.
#'
#' @param fusion_groups List of piece ID vectors, where each vector represents
#'   pieces that should be fused together into a meta-piece
#' @param puzzle_result Output from generate_puzzle() containing piece data
#' @return List with:
#'   - fused_edges: Character vector of edge keys that are internal
#'   - edge_to_group: Named list mapping edge keys to group indices
#'   - piece_to_group: Named list mapping piece IDs to group indices
#' @export
compute_hex_fused_edges <- function(fusion_groups, puzzle_result) {
  if (is.null(fusion_groups) || length(fusion_groups) == 0) {
    return(NULL)
  }

  rings <- puzzle_result$parameters$rings

  fused_edges <- character()
  edge_to_group <- list()
  piece_to_group <- list()

  # Build piece-to-group mapping
  for (group_idx in seq_along(fusion_groups)) {
    group <- fusion_groups[[group_idx]]
    for (piece_id in group) {
      piece_to_group[[as.character(piece_id)]] <- group_idx
    }
  }

  # Find edges between pieces in the same group
  for (group_idx in seq_along(fusion_groups)) {
    group <- fusion_groups[[group_idx]]

    for (piece_id in group) {
      # Get all neighbors for this piece
      neighbors <- get_hex_neighbors_for_fusion(piece_id, rings)

      for (i in seq_len(nrow(neighbors))) {
        direction <- neighbors$direction[i]
        neighbor_id <- neighbors$neighbor_id[i]
        is_boundary <- neighbors$is_boundary[i]

        # Skip boundary edges
        if (is_boundary || is.na(neighbor_id)) {
          next
        }

        # Check if neighbor is in the same group
        if (neighbor_id %in% group) {
          # This edge is fused
          edge_key <- make_edge_key(piece_id, direction)
          if (!(edge_key %in% fused_edges)) {
            fused_edges <- c(fused_edges, edge_key)
            edge_to_group[[edge_key]] <- group_idx
          }

          # Also add the complementary edge from neighbor's perspective
          comp_direction <- get_hex_complementary_side(direction)
          comp_edge_key <- make_edge_key(neighbor_id, comp_direction)
          if (!(comp_edge_key %in% fused_edges)) {
            fused_edges <- c(fused_edges, comp_edge_key)
            edge_to_group[[comp_edge_key]] <- group_idx
          }
        }
      }
    }
  }

  return(list(
    fused_edges = fused_edges,
    edge_to_group = edge_to_group,
    piece_to_group = piece_to_group
  ))
}
