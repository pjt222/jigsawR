# Hexagonal Adjacency Cache
# Pre-computed adjacency matrix for O(1) neighbor lookups
# Eliminates O(ring^2) recursive lookups in get_hex_neighbor()

# Module-level cache environment
.hex_adjacency_cache <- new.env(hash = TRUE, parent = emptyenv())

#' Build or retrieve cached hexagonal adjacency matrix
#'
#' Creates a pre-computed matrix of all neighbor relationships for a hexagonal
#' puzzle. Uses axial coordinate math for O(n) construction instead of O(n*ring^2)
#' recursive lookups.
#'
#' @param rings Number of rings in puzzle
#' @return Matrix where adj_matrix[piece_id, side+1] = neighbor_id (or NA for boundary)
get_hex_adjacency_matrix <- function(rings) {
  cache_key <- as.character(rings)


  # Check cache first
  if (exists(cache_key, envir = .hex_adjacency_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .hex_adjacency_cache, inherits = FALSE))
  }

  # Build adjacency matrix using axial coordinate math
  num_pieces <- 3 * rings * (rings - 1) + 1

  # Matrix: rows = piece_id (1 to num_pieces), cols = side (1-6 for sides 0-5)
  # Value = neighbor piece_id or NA for boundary
  adj_matrix <- matrix(NA_integer_, nrow = num_pieces, ncol = 6)

  # Build coordinate-to-piece lookup table (hash map for O(1) lookup)
  coord_to_piece <- new.env(hash = TRUE, parent = emptyenv())
  piece_coords <- vector("list", num_pieces)

  # First pass: map all pieces to coordinates
  for (piece_id in 1:num_pieces) {
    axial <- map_piece_id_to_axial(piece_id, rings)
    coord_key <- paste(axial$q, axial$r, sep = ",")
    coord_to_piece[[coord_key]] <- piece_id
    piece_coords[[piece_id]] <- c(axial$q, axial$r)
  }

  # Axial direction vectors for flat-top hexagons
  # Side 0 = E, 1 = NE, 2 = NW, 3 = W, 4 = SW, 5 = SE
  directions <- list(
    c(1, 0),    # Side 0: E
    c(1, -1),   # Side 1: NE
    c(0, -1),   # Side 2: NW
    c(-1, 0),   # Side 3: W
    c(-1, 1),   # Side 4: SW
    c(0, 1)     # Side 5: SE
  )

  # Second pass: for each piece, compute neighbors using coordinate math
  for (piece_id in 1:num_pieces) {
    coords <- piece_coords[[piece_id]]
    q <- coords[1]
    r <- coords[2]

    for (side in 0:5) {
      dir <- directions[[side + 1]]
      neighbor_q <- q + dir[1]
      neighbor_r <- r + dir[2]
      neighbor_key <- paste(neighbor_q, neighbor_r, sep = ",")

      # Look up neighbor in coordinate table
      if (exists(neighbor_key, envir = coord_to_piece, inherits = FALSE)) {
        adj_matrix[piece_id, side + 1] <- coord_to_piece[[neighbor_key]]
      }
      # If not found, it's a boundary edge (NA already set)
    }
  }

  # Cache the result
  assign(cache_key, adj_matrix, envir = .hex_adjacency_cache)

  return(adj_matrix)
}

#' Fast hexagonal neighbor lookup using cached adjacency matrix
#'
#' O(1) lookup after initial O(n) matrix construction.
#'
#' @param piece_id Piece ID (1 to num_pieces)
#' @param side Side number (0-5)
#' @param rings Number of rings in puzzle
#' @return Neighbor piece ID or NA if boundary
get_hex_neighbor_fast <- function(piece_id, side, rings) {
  adj_matrix <- get_hex_adjacency_matrix(rings)
  return(adj_matrix[piece_id, side + 1])
}

#' Get all neighbors for a piece using cached matrix
#'
#' Returns pre-allocated data frame with all 6 neighbors.
#' Much faster than iterative rbind approach.
#'
#' @param piece_id Piece ID (1 to num_pieces)
#' @param rings Number of rings in puzzle
#' @return Data frame with direction, neighbor_id, is_boundary
get_hex_neighbors_fast <- function(piece_id, rings) {
  adj_matrix <- get_hex_adjacency_matrix(rings)

  # Pre-allocate data frame (no rbind needed)
  neighbors <- data.frame(
    direction = as.character(0:5),
    neighbor_id = adj_matrix[piece_id, ],
    is_boundary = is.na(adj_matrix[piece_id, ]),
    stringsAsFactors = FALSE
  )

  return(neighbors)
}

#' Clear hexagonal adjacency cache
#'
#' Useful for testing or when memory is constrained.
#'
clear_hex_adjacency_cache <- function() {
  rm(list = ls(envir = .hex_adjacency_cache), envir = .hex_adjacency_cache)
}

#' Optimized fused edge computation using cached adjacency
#'
#' Uses hash sets for O(1) group membership and edge existence checks.
#' Uses the original get_hex_neighbors_for_fusion for topology-correct sides.
#' Uses list accumulation instead of vector c() for O(n) instead of O(n^2).
#'
#' IMPORTANT: Uses original get_hex_neighbor() for side mapping to maintain
#' compatibility with is_edge_fused() and the existing edge key format.
#' The main speedup comes from hash sets for membership/existence checks
#' and list accumulation instead of vector concatenation.
#'
#' @param fusion_groups List of piece ID vectors
#' @param rings Number of rings in puzzle
#' @return List with fused_edges, edge_to_group, piece_to_group
compute_hex_fused_edges_fast <- function(fusion_groups, rings) {
  if (is.null(fusion_groups) || length(fusion_groups) == 0) {
    return(NULL)
  }

  # Build piece-to-group hash map (O(1) lookup)
  piece_to_group <- new.env(hash = TRUE, parent = emptyenv())
  for (group_idx in seq_along(fusion_groups)) {
    group <- fusion_groups[[group_idx]]
    for (piece_id in group) {
      piece_to_group[[as.character(piece_id)]] <- group_idx
    }
  }

  # Build group membership hash sets (O(1) membership test)
  group_sets <- lapply(fusion_groups, function(group) {
    group_set <- new.env(hash = TRUE, parent = emptyenv())
    for (piece_id in group) {
      group_set[[as.character(piece_id)]] <- TRUE
    }
    group_set
  })

  # Hash set for edge deduplication (O(1) existence check)
  edge_set <- new.env(hash = TRUE, parent = emptyenv())

  # List accumulation for edges (O(1) append, O(n) final unlist)
  fused_edges_list <- list()
  edge_to_group <- list()

  # Process all pieces in all groups
  for (group_idx in seq_along(fusion_groups)) {
    group <- fusion_groups[[group_idx]]
    group_set <- group_sets[[group_idx]]

    for (piece_id in group) {
      # Use original function for topology-correct side mapping
      neighbors <- get_hex_neighbors_for_fusion(piece_id, rings)

      for (i in seq_len(nrow(neighbors))) {
        direction <- neighbors$direction[i]
        neighbor_id <- neighbors$neighbor_id[i]
        is_boundary <- neighbors$is_boundary[i]

        # Skip boundary edges
        if (is_boundary || is.na(neighbor_id)) next

        # Check if neighbor is in same group (O(1) hash lookup instead of O(n) %in%)
        if (exists(as.character(neighbor_id), envir = group_set, inherits = FALSE)) {
          # This edge is fused - create edge key using standard format
          edge_key <- make_edge_key(piece_id, direction)

          # Check if already added (O(1) hash lookup instead of O(n) %in%)
          if (!exists(edge_key, envir = edge_set, inherits = FALSE)) {
            edge_set[[edge_key]] <- TRUE
            fused_edges_list[[length(fused_edges_list) + 1]] <- edge_key
            edge_to_group[[edge_key]] <- group_idx
          }
        }
      }
    }
  }

  # Convert list to named list for piece_to_group return
  piece_to_group_list <- as.list(piece_to_group)
  names(piece_to_group_list) <- ls(piece_to_group)

  return(list(
    fused_edges = unlist(fused_edges_list),
    edge_to_group = edge_to_group,
    piece_to_group = piece_to_group_list
  ))
}
