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
  } else if (type %in% c("voronoi", "random", "snic")) {
    return(get_tessellation_neighbors(piece_id, puzzle_result, include_boundary))
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
validate_fusion_group <- function(piece_ids, puzzle_result) {
  if (length(piece_ids) < 2) {
    return(list(valid = TRUE, message = "Single piece or empty group"))
  }

  # Normalize all IDs to integers
  ids <- sapply(piece_ids, function(id) normalize_piece_id(id, puzzle_result))

  # Use hash set for O(1) group membership lookup instead of O(n) vector search
  ids_set <- new.env(hash = TRUE, parent = emptyenv())
  for (id in ids) {
    ids_set[[as.character(id)]] <- TRUE
  }

  # Build adjacency within the group using BFS/DFS
  visited <- rep(FALSE, length(ids))
  names(visited) <- as.character(ids)

  # Start BFS from first piece - use list for O(1) queue operations
  queue_list <- list(ids[1])
  queue_head <- 1L
  visited[as.character(ids[1])] <- TRUE

  while (queue_head <= length(queue_list)) {
    current <- queue_list[[queue_head]]
    queue_head <- queue_head + 1L

    # Get neighbors of current piece
    neighbors <- get_piece_neighbors(current, puzzle_result, include_boundary = FALSE)

    # Check which neighbors are in our group and unvisited
    for (neighbor_id in neighbors$neighbor_id) {
      if (!is.na(neighbor_id)) {
        neighbor_key <- as.character(neighbor_id)
        # O(1) hash lookup instead of O(n) vector search
        if (exists(neighbor_key, envir = ids_set, inherits = FALSE) && !visited[neighbor_key]) {
          visited[neighbor_key] <- TRUE
          # O(1) list append instead of O(n) vector copy
          queue_list[[length(queue_list) + 1L]] <- neighbor_id
        }
      }
    }
  }

  # Check if all pieces were visited
  if (all(visited)) {
    return(list(valid = TRUE, message = "Group is connected"))
  } else {
    unvisited <- ids[!visited]

    # For random/voronoi puzzles, provide helpful info about actual neighbors
    puzzle_type <- puzzle_result$parameters$type
    neighbor_hints <- NULL

    if (puzzle_type %in% c("random", "voronoi") && length(unvisited) <= 3) {
      # Get actual neighbors for disconnected pieces to help the user
      hints <- lapply(unvisited, function(piece_id) {
        tryCatch({
          neighbors <- get_piece_neighbors(piece_id, puzzle_result, include_boundary = FALSE)
          if (nrow(neighbors) > 0) {
            neighbor_ids <- sort(neighbors$neighbor_id)
            sprintf("Piece %d neighbors: %s", piece_id, paste(neighbor_ids, collapse = ", "))
          } else {
            NULL
          }
        }, error = function(e) NULL)
      })
      neighbor_hints <- unlist(hints[!sapply(hints, is.null)])
    }

    # Build message with hints
    base_msg <- sprintf("Pieces %s are not connected to the rest of the group",
                        paste(unvisited, collapse = ", "))

    if (!is.null(neighbor_hints) && length(neighbor_hints) > 0) {
      full_msg <- paste0(base_msg, ". ", paste(neighbor_hints, collapse = ". "))
    } else {
      full_msg <- base_msg
    }

    return(list(
      valid = FALSE,
      message = full_msg,
      disconnected = unvisited,
      neighbor_hints = neighbor_hints
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

  # Pre-allocate vectors for all 4 directions (optimized - no rbind)
  dir_names <- c("N", "E", "S", "W")
  dx <- c(0L, 1L, 0L, -1L)
  dy <- c(-1L, 0L, 1L, 0L)

  # Vectorized neighbor computation
  nx <- xi + dx
  ny <- yi + dy
  is_boundary <- nx < 0L | nx >= xn | ny < 0L | ny >= yn
  neighbor_idx <- ifelse(is_boundary, NA_integer_, ny * xn + nx + 1L)

  # Build result data frame with pre-computed vectors
  if (include_boundary) {
    neighbors <- data.frame(
      direction = dir_names,
      neighbor_id = neighbor_idx,
      is_boundary = is_boundary,
      stringsAsFactors = FALSE
    )
  } else {
    keep <- !is_boundary
    neighbors <- data.frame(
      direction = dir_names[keep],
      neighbor_id = neighbor_idx[keep],
      is_boundary = is_boundary[keep],
      stringsAsFactors = FALSE
    )
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

  # Use axial-based topology matrix for consistent neighbor ordering

  # This aligns with get_hex_neighbors_for_fusion() and .HEX_DIRECTIONS
  # Side 0=E, 1=NE, 2=NW, 3=W, 4=SW, 5=SE (geometric compass directions)
  topo_matrix <- get_hex_topo_neighbor_matrix(rings)
  neighbor_ids <- topo_matrix[id, ]

  is_boundary <- is.na(neighbor_ids)
  dir_names <- as.character(0:5)

  # Build result data frame with pre-computed vectors
  if (include_boundary) {
    neighbors <- data.frame(
      direction = dir_names,
      neighbor_id = neighbor_ids,
      is_boundary = is_boundary,
      stringsAsFactors = FALSE
    )
  } else {
    keep <- !is_boundary
    neighbors <- data.frame(
      direction = dir_names[keep],
      neighbor_id = neighbor_ids[keep],
      is_boundary = is_boundary[keep],
      stringsAsFactors = FALSE
    )
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

  # Edge directions depend on piece type (optimized - no rbind)
  if (info$ring == 0) {
    # Center piece has 6 edges
    edge_names <- as.character(1:6)
    edges <- 1:6
  } else {
    # Trapezoid pieces have 4 edges
    edge_names <- c("INNER", "RIGHT", "OUTER", "LEFT")
    edges <- 1:4
  }

  # Vectorized neighbor lookup
  n_edges <- length(edges)
  neighbor_ids <- integer(n_edges)
  is_boundary <- logical(n_edges)

  for (i in seq_along(edges)) {
    result <- get_concentric_neighbor(id, edges[i], rings)
    is_boundary[i] <- result$is_boundary
    neighbor_ids[i] <- if (result$is_boundary) NA_integer_ else as.integer(result$neighbor_id)
  }

  # Build result data frame with pre-computed vectors
  if (include_boundary) {
    neighbors <- data.frame(
      direction = edge_names,
      neighbor_id = neighbor_ids,
      is_boundary = is_boundary,
      stringsAsFactors = FALSE
    )
  } else {
    keep <- !is_boundary
    neighbors <- data.frame(
      direction = edge_names[keep],
      neighbor_id = neighbor_ids[keep],
      is_boundary = is_boundary[keep],
      stringsAsFactors = FALSE
    )
  }

  return(neighbors)
}

# ============================================================================
# TESSELLATION NEIGHBORS (voronoi, random, snic)
# ============================================================================

#' Get neighbors for tessellation-based puzzle types
#'
#' Shared implementation for voronoi, random, and snic puzzle types.
#' Queries stored adjacency data to find neighbors and compute compass
#' directions based on edge midpoints relative to piece centers.
#'
#' @param piece_id Piece/cell ID
#' @param puzzle_result Puzzle result containing adjacency data and pieces
#' @param include_boundary Whether to include boundary edges
#' @return Data frame with direction, neighbor_id, is_boundary columns
#'
#' @keywords internal
get_tessellation_neighbors <- function(piece_id, puzzle_result, include_boundary = TRUE) {
  adjacency <- puzzle_result$adjacency

  if (is.null(adjacency)) {
    return(data.frame(
      direction = character(),
      neighbor_id = integer(),
      is_boundary = logical()
    ))
  }

  # Find all adjacencies involving this piece
  cell_adj <- adjacency[adjacency$cell_a == piece_id | adjacency$cell_b == piece_id, ]

  if (nrow(cell_adj) == 0) {
    return(data.frame(
      direction = character(),
      neighbor_id = integer(),
      is_boundary = logical()
    ))
  }

  neighbors <- lapply(seq_len(nrow(cell_adj)), function(i) {
    row <- cell_adj[i, ]
    neighbor_id <- if (row$cell_a == piece_id) row$cell_b else row$cell_a
    is_boundary <- neighbor_id < 0

    if (!include_boundary && is_boundary) {
      return(NULL)
    }

    # Calculate direction based on edge midpoint relative to piece center
    edge_mid <- c((row$v1_x + row$v2_x) / 2, (row$v1_y + row$v2_y) / 2)
    piece_center <- puzzle_result$pieces[[piece_id]]$center
    dir_vec <- edge_mid - piece_center

    # Convert to compass direction
    angle <- atan2(dir_vec[2], dir_vec[1]) * 180 / pi
    direction <- angle_to_direction(angle)

    data.frame(
      direction = direction,
      neighbor_id = if (is_boundary) NA_integer_ else neighbor_id,
      is_boundary = is_boundary,
      stringsAsFactors = FALSE
    )
  })

  neighbors <- neighbors[!sapply(neighbors, is.null)]

  if (length(neighbors) == 0) {
    return(data.frame(
      direction = character(),
      neighbor_id = integer(),
      is_boundary = logical()
    ))
  }

  do.call(rbind, neighbors)
}

# ============================================================================
# FUSION INPUT PARSING
# ============================================================================

#' Merge overlapping fusion groups
#'
#' Uses Union-Find algorithm to merge fusion groups that share common pieces.
#' For example, (7,8) and (5,8) share piece 8, so they merge into (5,7,8).
#'
#' @param fusion_groups List of integer vectors representing fusion groups
#'
#' @return List of merged integer vectors (sorted, deduplicated)
#'
#' @examples
#' \dontrun{
#' merge_fusion_groups(list(c(7,8), c(5,8)))    # -> list(c(5,7,8))
#' merge_fusion_groups(list(c(1,2), c(2,3)))    # -> list(c(1,2,3))
#' merge_fusion_groups(list(c(1,2), c(3,4)))    # -> list(c(1,2), c(3,4))
#' }
#'
merge_fusion_groups <- function(fusion_groups) {
  if (is.null(fusion_groups) || length(fusion_groups) == 0) {
    return(list())
  }

  # Filter empty groups and groups with < 2 pieces
  fusion_groups <- lapply(fusion_groups, function(g) {
    g <- unique(as.integer(g[!is.na(g)]))
    g[g > 0]  # Only positive integers
  })
  fusion_groups <- fusion_groups[sapply(fusion_groups, length) >= 2]

  if (length(fusion_groups) == 0) {
    return(list())
  }

  # If only one group, just return it sorted
  if (length(fusion_groups) == 1) {
    return(list(sort(fusion_groups[[1]])))
  }

  # Collect all unique piece IDs
  all_pieces <- unique(unlist(fusion_groups))

  # Union-Find data structure
  # parent[piece_id] = parent piece ID (initially self)
  parent <- setNames(all_pieces, all_pieces)

  # Find with path compression
  find_root <- function(x) {
    if (parent[[as.character(x)]] != x) {
      parent[[as.character(x)]] <<- find_root(parent[[as.character(x)]])
    }
    return(parent[[as.character(x)]])
  }

  # Union two pieces into same group
  union_pieces <- function(a, b) {
    root_a <- find_root(a)
    root_b <- find_root(b)
    if (root_a != root_b) {
      # Union by smaller root (for deterministic results)
      if (root_a < root_b) {
        parent[[as.character(root_b)]] <<- root_a
      } else {
        parent[[as.character(root_a)]] <<- root_b
      }
    }
  }

  # Union all pieces within each fusion group
  for (group in fusion_groups) {
    if (length(group) >= 2) {
      first_piece <- group[1]
      for (piece in group[-1]) {
        union_pieces(first_piece, piece)
      }
    }
  }

  # Group pieces by their root
  piece_to_root <- sapply(all_pieces, find_root)
  roots <- unique(piece_to_root)

  # Build merged groups
  merged_groups <- lapply(roots, function(root) {
    pieces <- all_pieces[piece_to_root == root]
    sort(pieces)
  })

  # Sort groups by their first (smallest) piece ID
  merged_groups <- merged_groups[order(sapply(merged_groups, function(g) g[1]))]

  # Log if merging occurred
  if (length(merged_groups) < length(fusion_groups)) {
    original_count <- length(fusion_groups)
    merged_count <- length(merged_groups)
    log_info("Merged {original_count} overlapping fusion groups into {merged_count} groups")
  }

  return(merged_groups)
}

#' Parse fusion group input string
#'
#' Converts string input format to list of integer vectors.
#' Automatically merges overlapping groups (e.g., (7,8),(5,8) becomes (5,7,8)).
#'
#' @param input String like "1,2" or "(1,2),(7,8,9)" or list
#' @param puzzle_result Puzzle result for ID normalization (unused, for future)
#' @param auto_merge If TRUE (default), automatically merge overlapping groups
#'
#' @return List of integer vectors, each representing a fusion group
#'
#' @examples
#' \dontrun{
#' parse_fusion_input("1,2")             # -> list(c(1, 2))
#' parse_fusion_input("(1,2),(7,8,9)")   # -> list(c(1, 2), c(7, 8, 9))
#' parse_fusion_input("(7,8),(5,8)")     # -> list(c(5, 7, 8)) (auto-merged)
#' parse_fusion_input(list(c(1, 2)))     # -> list(c(1, 2)) (pass-through)
#' }
#'
#' @export
parse_fusion_input <- function(input, puzzle_result = NULL, auto_merge = TRUE) {
  if (is.null(input) || length(input) == 0) {
    return(list())
  }

  # If already a list, normalize
  if (is.list(input)) {
    result <- lapply(input, function(group) {
      as.integer(group)
    })
  } else if (is.character(input)) {
    # Parse string format
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
  } else {
    stop("Fusion input must be a string or list")
  }

  # Automatically merge overlapping groups
  if (isTRUE(auto_merge) && length(result) > 0) {
    result <- merge_fusion_groups(result)
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

# Internal helper: get piece count from result object
# Note: the exported get_piece_count(type, grid) is in unified_piece_generation.R
get_piece_count_from_result <- function(puzzle_result) {
  length(puzzle_result$pieces)
}

# Null-coalescing operator
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
    # Hexagonal: find which side of the NEIGHBOR points back to this piece
    # We can't use simple (side + 3) mod 6 because the hexagonal grid topology
    # doesn't guarantee opposite sides are exactly 3 apart in numbering
    side <- as.integer(direction)
    neighbors <- get_piece_neighbors(piece_id, puzzle_result, include_boundary = FALSE)
    neighbor_row <- neighbors[neighbors$direction == as.character(side), ]
    if (nrow(neighbor_row) == 0 || is.na(neighbor_row$neighbor_id)) {
      return(NA)
    }
    neighbor_id <- neighbor_row$neighbor_id

    # Look up which side of the neighbor points back to us
    neighbor_neighbors <- get_piece_neighbors(neighbor_id, puzzle_result, include_boundary = FALSE)
    back_row <- neighbor_neighbors[neighbor_neighbors$neighbor_id == piece_id, ]
    if (nrow(back_row) == 0) {
      return(NA)
    }
    opposite <- back_row$direction[1]
    return(make_edge_key(neighbor_id, opposite))

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
compute_fused_edges <- function(fusion_groups, puzzle_result) {
  if (is.null(fusion_groups) || length(fusion_groups) == 0) {
    return(list(
      fused_edges = character(),
      fused_edges_set = new.env(hash = TRUE, parent = emptyenv()),
      edge_to_group = list(),
      piece_to_group = list()
    ))
  }

  # Get puzzle type to handle type-specific neighbor relationships
  type <- puzzle_result$type %||% puzzle_result$parameters$type

  # Use environment as hash set for O(1) lookups instead of O(n) vector search
  fused_edges_set <- new.env(hash = TRUE, parent = emptyenv())
  fused_edges_list <- list()  # Accumulate in list, convert once at end
  edge_to_group <- list()
  piece_to_group <- list()

  for (group_idx in seq_along(fusion_groups)) {
    group <- fusion_groups[[group_idx]]

    # Create hash set for group membership (O(1) lookup instead of O(n))
    group_set <- new.env(hash = TRUE, parent = emptyenv())
    for (piece_id in group) {
      group_set[[as.character(piece_id)]] <- TRUE
      piece_to_group[[as.character(piece_id)]] <- group_idx
    }

    # Find all internal edges (edges between pieces in this group)
    for (piece_id in group) {
      neighbors <- get_piece_neighbors(piece_id, puzzle_result, include_boundary = FALSE)

      for (i in seq_len(nrow(neighbors))) {
        neighbor_id <- neighbors$neighbor_id[i]
        direction <- neighbors$direction[i]

        # O(1) hash lookup instead of O(n) vector search
        if (exists(as.character(neighbor_id), envir = group_set, inherits = FALSE)) {
          edge_key <- make_edge_key(piece_id, direction)

          # O(1) hash lookup instead of O(n) vector search
          if (!exists(edge_key, envir = fused_edges_set, inherits = FALSE)) {
            fused_edges_list[[length(fused_edges_list) + 1]] <- edge_key
            fused_edges_set[[edge_key]] <- TRUE
            edge_to_group[[edge_key]] <- group_idx

            # Also add the complementary edge
            comp_key <- get_complementary_edge_key(edge_key, puzzle_result)
            if (!is.na(comp_key) && !exists(comp_key, envir = fused_edges_set, inherits = FALSE)) {
              fused_edges_list[[length(fused_edges_list) + 1]] <- comp_key
              fused_edges_set[[comp_key]] <- TRUE
              edge_to_group[[comp_key]] <- group_idx
            }
          }
        }
      }

      # CONCENTRIC SPECIAL CASE: Handle many-to-one OUTER edge relationships
      # An inner ring piece's OUTER edge may touch MULTIPLE outer ring pieces.
      # The standard get_piece_neighbors() only returns one neighbor per direction,
      # so we need to check ALL outer neighbors.
      if (type == "concentric") {
        rings <- puzzle_result$parameters$grid[1] %||% puzzle_result$parameters$rings
        if (!is.null(rings)) {
          all_outer_neighbors <- get_all_concentric_outer_neighbors(piece_id, rings)
          for (outer_neighbor_id in all_outer_neighbors) {
            if (exists(as.character(outer_neighbor_id), envir = group_set, inherits = FALSE)) {
              # Mark the OUTER edge as fused
              edge_key <- make_edge_key(piece_id, "OUTER")
              if (!exists(edge_key, envir = fused_edges_set, inherits = FALSE)) {
                fused_edges_list[[length(fused_edges_list) + 1]] <- edge_key
                fused_edges_set[[edge_key]] <- TRUE
                edge_to_group[[edge_key]] <- group_idx
              }
              # Mark the specific neighbor's INNER edge as fused
              # This is the correct complementary edge for THIS neighbor
              comp_key <- make_edge_key(outer_neighbor_id, "INNER")
              if (!exists(comp_key, envir = fused_edges_set, inherits = FALSE)) {
                fused_edges_list[[length(fused_edges_list) + 1]] <- comp_key
                fused_edges_set[[comp_key]] <- TRUE
                edge_to_group[[comp_key]] <- group_idx
              }
            }
          }
        }
      }
    }
  }

  # Convert list to vector once at end (O(n) instead of O(n²))
  fused_edges <- unlist(fused_edges_list)
  if (is.null(fused_edges)) fused_edges <- character()

  return(list(
    fused_edges = fused_edges,
    fused_edges_set = fused_edges_set,  # Include hash set for O(1) lookups
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
is_edge_fused <- function(piece_id, direction, fused_edge_data) {
  if (is.null(fused_edge_data)) {
    return(FALSE)
  }

  edge_key <- make_edge_key(piece_id, direction)

  # Use O(1) hash set lookup if available, fall back to O(n) vector search
  if (!is.null(fused_edge_data$fused_edges_set)) {
    return(exists(edge_key, envir = fused_edge_data$fused_edges_set, inherits = FALSE))
  }

  # Fallback for backwards compatibility
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
get_group_boundary_edges <- function(fusion_group, puzzle_result, fused_edge_data) {
  # Pre-collect all boundary edges using lists (optimized - no rbind)
  results <- vector("list", length(fusion_group))

  for (idx in seq_along(fusion_group)) {
    piece_id <- fusion_group[idx]
    neighbors <- get_piece_neighbors(piece_id, puzzle_result, include_boundary = TRUE)

    # Find non-fused edges for this piece
    is_fused <- vapply(seq_len(nrow(neighbors)), function(i) {
      is_edge_fused(piece_id, neighbors$direction[i], fused_edge_data)
    }, logical(1))

    keep <- !is_fused
    if (any(keep)) {
      results[[idx]] <- data.frame(
        piece_id = rep(piece_id, sum(keep)),
        direction = neighbors$direction[keep],
        neighbor_id = ifelse(is.na(neighbors$neighbor_id[keep]),
                             NA_integer_, neighbors$neighbor_id[keep]),
        is_puzzle_boundary = neighbors$is_boundary[keep],
        stringsAsFactors = FALSE
      )
    }
  }

  # Combine all results efficiently using do.call(rbind, ...)
  results <- results[!vapply(results, is.null, logical(1))]
  if (length(results) == 0) {
    return(data.frame(
      piece_id = integer(),
      direction = character(),
      neighbor_id = integer(),
      is_puzzle_boundary = logical(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, results)
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

  # Pre-allocate vectors (optimized - no rbind)
  n_edges <- length(edges)
  neighbor_ids <- integer(n_edges)
  is_boundary <- logical(n_edges)

  for (i in seq_along(edges)) {
    neighbor_info <- get_concentric_neighbor(piece_id, edges[i], rings)
    neighbor_ids[i] <- if (is.na(neighbor_info$neighbor_id)) NA_integer_ else as.integer(neighbor_info$neighbor_id)
    is_boundary[i] <- neighbor_info$is_boundary
  }

  data.frame(
    direction = direction_names,
    neighbor_id = neighbor_ids,
    is_boundary = is_boundary,
    stringsAsFactors = FALSE
  )
}


#' Get complementary direction for concentric puzzle
#'
#' Returns the opposite edge direction. For trapezoid pieces:
#' INNER <-> OUTER, LEFT <-> RIGHT.
#' For center piece edges (1-6), returns the opposite edge of the neighbor.
#'
#' @param direction Edge direction (INNER/RIGHT/OUTER/LEFT or 1-6 for center)
#' @return Opposite direction
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
#' NOTE: This handles the many-to-one relationship where an inner ring piece's
#' OUTER edge may touch MULTIPLE outer ring pieces' INNER edges. For example,
#' in a 3-ring puzzle, piece 4's OUTER edge touches both pieces 12 and 13.
#'
#' @param fusion_groups List of piece ID vectors, where each vector represents
#'   pieces that should be fused together into a meta-piece
#' @param puzzle_result Output from generate_puzzle() containing piece data
#' @return List with:
#'   - fused_edges: Character vector of edge keys that are internal
#'   - edge_to_group: Named list mapping edge keys to group indices
#'   - piece_to_group: Named list mapping piece IDs to group indices
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
      # Get standard neighbors for this piece
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
        }
      }

      # CRITICAL: Handle many-to-one relationship for OUTER edges
      # An inner ring piece's OUTER edge may touch MULTIPLE outer ring pieces.
      # The standard get_concentric_neighbors() only returns one neighbor,
      # so we need to check ALL outer neighbors for the OUTER direction.
      all_outer_neighbors <- get_all_concentric_outer_neighbors(piece_id, rings)
      for (outer_neighbor_id in all_outer_neighbors) {
        if (outer_neighbor_id %in% group) {
          # Mark the OUTER edge as fused
          edge_key <- make_edge_key(piece_id, "OUTER")
          if (!(edge_key %in% fused_edges)) {
            fused_edges <- c(fused_edges, edge_key)
            edge_to_group[[edge_key]] <- group_idx
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
# Hexagonal Puzzle Fusion Support - O(1) Axial Coordinate Neighbor Lookup
# =============================================================================

# Module-level cache for fast neighbor lookups
# Uses axial coordinates for O(1) neighbor computation
.hex_neighbor_cache <- new.env(hash = TRUE, parent = emptyenv())

# The 6 axial direction vectors for hexagonal neighbors
# Each direction adds (dq, dr) to the current axial coordinates
# Side numbering: 0=E, 1=NE, 2=NW, 3=W, 4=SW, 5=SE
.HEX_DIRECTIONS <- list(
  c(1, 0),   # Side 0: East
  c(1, -1),  # Side 1: Northeast
  c(0, -1),  # Side 2: Northwest
  c(-1, 0),  # Side 3: West
  c(-1, 1),  # Side 4: Southwest
  c(0, 1)    # Side 5: Southeast
)

#' Build axial-to-piece-id lookup table for O(1) reverse mapping
#'
#' Creates a hash table mapping "(q,r)" strings to piece IDs.
#' This enables O(1) lookup when computing neighbors.
#'
#' @param rings Number of rings in puzzle
#' @return Environment (hash table) mapping "q,r" to piece_id
#' @keywords internal
build_axial_lookup <- function(rings) {
  num_pieces <- 3 * rings * (rings - 1) + 1
  lookup <- new.env(hash = TRUE, parent = emptyenv())

  for (piece_id in 1:num_pieces) {
    axial <- map_piece_id_to_axial(piece_id, rings)
    key <- paste0(axial$q, ",", axial$r)
    lookup[[key]] <- piece_id
  }

  return(lookup)
}

#' Get or build cached neighbor data for hexagonal puzzles
#'
#' Builds both the axial lookup table and pre-computed neighbor matrix
#' using O(n) time with direct axial coordinate arithmetic.
#'
#' @param rings Number of rings in puzzle
#' @return List with neighbor_matrix and axial_lookup
#' @keywords internal
get_hex_neighbor_data <- function(rings) {
  cache_key <- as.character(rings)

  # Check cache first
  if (exists(cache_key, envir = .hex_neighbor_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .hex_neighbor_cache, inherits = FALSE))
  }

  num_pieces <- 3 * rings * (rings - 1) + 1

  # Step 1: Build axial coordinate lookup (O(n))
  axial_lookup <- build_axial_lookup(rings)

  # Step 2: Pre-compute all axial coordinates (O(n))
  piece_axial <- vector("list", num_pieces)
  for (piece_id in 1:num_pieces) {
    piece_axial[[piece_id]] <- map_piece_id_to_axial(piece_id, rings)
  }

  # Step 3: Build neighbor matrix using O(1) axial arithmetic (O(n))
  neighbor_matrix <- matrix(NA_integer_, nrow = num_pieces, ncol = 6)

  for (piece_id in 1:num_pieces) {
    axial <- piece_axial[[piece_id]]
    q <- axial$q
    r <- axial$r

    for (side in 0:5) {
      dir <- .HEX_DIRECTIONS[[side + 1]]
      neighbor_q <- q + dir[1]
      neighbor_r <- r + dir[2]

      # O(1) lookup in hash table
      neighbor_key <- paste0(neighbor_q, ",", neighbor_r)
      if (exists(neighbor_key, envir = axial_lookup, inherits = FALSE)) {
        neighbor_matrix[piece_id, side + 1] <- axial_lookup[[neighbor_key]]
      }
      # NA if no neighbor (boundary edge)
    }
  }

  # Cache the result
  cache_data <- list(
    neighbor_matrix = neighbor_matrix,
    axial_lookup = axial_lookup,
    piece_axial = piece_axial
  )
  assign(cache_key, cache_data, envir = .hex_neighbor_cache)

  return(cache_data)
}

#' Clear hexagonal neighbor cache
#'
clear_hex_neighbor_cache <- function() {
  rm(list = ls(envir = .hex_neighbor_cache), envir = .hex_neighbor_cache)
}

# Keep old function name for backwards compatibility
clear_hex_topo_neighbor_cache <- clear_hex_neighbor_cache

#' Get cached neighbor matrix (legacy interface)
#'
#' @param rings Number of rings in puzzle
#' @return Matrix where matrix[piece_id, side+1] = neighbor_id (or NA)
#' @keywords internal
get_hex_topo_neighbor_matrix <- function(rings) {
  cache_data <- get_hex_neighbor_data(rings)
  return(cache_data$neighbor_matrix)
}

#' Get all neighbors for a hexagonal puzzle piece
#'
#' Returns neighbor information for all 6 sides of a hexagonal piece.
#' Uses cached topology matrix for O(1) lookups after initial O(n*ring^2) build.
#'
#' @param piece_id Piece ID (1-based)
#' @param rings Number of rings in puzzle
#' @return Data frame with side (0-5), neighbor_id, is_boundary
get_hex_neighbors_for_fusion <- function(piece_id, rings) {
  # Use cached topology matrix for O(1) lookup
  topo_matrix <- get_hex_topo_neighbor_matrix(rings)

  # Pre-allocate data frame (no rbind needed)
  neighbor_ids <- topo_matrix[piece_id, ]

  neighbors <- data.frame(
    direction = as.character(0:5),
    neighbor_id = neighbor_ids,
    is_boundary = is.na(neighbor_ids),
    stringsAsFactors = FALSE
  )

  return(neighbors)
}


#' Get complementary side for hexagonal puzzle
#'
#' Returns the opposite side number. For hexagons, the opposite of side n
#' is (n + 3) mod 6.
#'
#' @param side Side number (0-5) as character or integer
#' @return Opposite side number as character
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

          # Don't blindly add complementary edge - the neighbor will add its own
          # when it processes its edges. This avoids bugs from asymmetric adjacency.
          # The symmetric edge will be added when we process the neighbor piece.
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
