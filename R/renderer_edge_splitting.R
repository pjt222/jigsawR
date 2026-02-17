# Edge Splitting Functions for SVG Rendering
# Split from unified_renderer.R for maintainability
# Contains type-specific path splitting for rectangular, hexagonal, and concentric puzzles

#' Split rectangular piece path into individual edge paths
#'
#' Parses a closed rectangular piece path and extracts the 4 edge segments.
#' Uses corner detection to identify edge boundaries.
#'
#' @param path SVG path string for a rectangular piece
#' @param piece Piece object with path and grid position
#' @return List with N, E, S, W edge path strings (each starting with M)
#' @keywords internal
split_rect_path_into_edges <- function(path, piece = NULL) {
  # Use cached parsed_segments if available (Phase 2 optimization)
  segments <- if (!is.null(piece) && !is.null(piece$parsed_segments)) {
    piece$parsed_segments
  } else {
    parse_svg_path(path)
  }

  # Find the corners by tracking endpoints
  # First segment should be M (move to top-left)
  if (length(segments) == 0 || segments[[1]]$type != "M") {
    return(list(N = "", E = "", S = "", W = ""))
  }

  # Track current position and collect segments for each edge
  current_x <- segments[[1]]$x
  current_y <- segments[[1]]$y
  start_x <- current_x
  start_y <- current_y

  # For rectangular pieces, corners are when x or y changes direction
  # N: left to right (x increasing, y constant or slight variation)
  # E: top to bottom (y increasing, x constant or slight variation)
  # S: right to left (x decreasing, y constant or slight variation)
  # W: bottom to top (y decreasing, x constant or slight variation)

  edges <- list(N = list(), E = list(), S = list(), W = list())
  current_edge <- "N"  # Start with north edge

  for (i in 2:length(segments)) {
    seg <- segments[[i]]

    if (seg$type == "Z") {
      next
    }

    # Determine endpoint of this segment
    end_x <- seg$x
    end_y <- seg$y

    # Calculate direction from current position to segment endpoint
    dx <- end_x - current_x
    dy <- end_y - current_y

    # Detect edge transitions BEFORE adding segment
    # Check if this segment starts a new direction
    if (current_edge == "N" && abs(dy) > abs(dx) && dy > 0.1) {
      current_edge <- "E"
    } else if (current_edge == "E" && abs(dx) > abs(dy) && dx < -0.1) {
      current_edge <- "S"
    } else if (current_edge == "S" && abs(dy) > abs(dx) && dy < -0.1) {
      current_edge <- "W"
    }

    # Add segment to current edge (after transition check)
    edges[[current_edge]][[length(edges[[current_edge]]) + 1]] <- seg

    current_x <- end_x
    current_y <- end_y
  }

  # Convert segment lists to path strings using fast builder
  edge_paths <- list()
  prev_end <- c(start_x, start_y)

  for (edge_name in c("N", "E", "S", "W")) {
    segs <- edges[[edge_name]]
    if (length(segs) == 0) {
      edge_paths[[edge_name]] <- ""
      next
    }

    # Use optimized path string builder (avoids O(n²) string concatenation)
    edge_paths[[edge_name]] <- build_path_string_fast(segs, prev_end)

    # Update prev_end to last segment's endpoint
    if (length(segs) > 0) {
      last_seg <- segs[[length(segs)]]
      prev_end <- c(last_seg$x, last_seg$y)
    }
  }

  return(edge_paths)
}


#' Split concentric piece path into individual edge paths
#'
#' Parses a concentric piece path and extracts the edge segments.
#' Trapezoid pieces have 4 edges: INNER, RIGHT, OUTER, LEFT
#' Center piece (hexagon) has 6 edges: 1-6
#'
#' Uses vertex detection based on angle changes to correctly identify edge boundaries.
#'
#' @param path SVG path string for a concentric piece
#' @param piece Piece object with path and ring_pos
#' @return List with edge path strings keyed by edge name
#' @keywords internal
split_concentric_path_into_edges <- function(path, piece = NULL) {
  # Use cached parsed_segments if available (Phase 2 optimization)
  segments <- if (!is.null(piece) && !is.null(piece$parsed_segments)) {
    piece$parsed_segments
  } else {
    parse_svg_path(path)
  }

  if (length(segments) == 0 || segments[[1]]$type != "M") {
    if (!is.null(piece) && !is.null(piece$ring_pos) && piece$ring_pos$ring == 0) {
      return(list(`1` = "", `2` = "", `3` = "", `4` = "", `5` = "", `6` = ""))
    }
    return(list(INNER = "", RIGHT = "", OUTER = "", LEFT = ""))
  }

  # Determine if this is center piece (6 edges) or trapezoid (4 edges)
  is_center <- FALSE
  if (!is.null(piece) && !is.null(piece$ring_pos)) {
    is_center <- piece$ring_pos$ring == 0
  }

  start_x <- segments[[1]]$x
  start_y <- segments[[1]]$y

  # Extract content segments (excluding M and Z)
  content_segments <- list()
  for (i in 2:length(segments)) {
    if (segments[[i]]$type != "Z") {
      content_segments[[length(content_segments) + 1]] <- segments[[i]]
    }
  }

  if (is_center) {
    # Center piece: 6 edges with equal segments (typically 3 beziers each)
    edge_names <- as.character(1:6)
    n_edges <- 6
    segs_per_edge <- max(1, ceiling(length(content_segments) / n_edges))
    edges <- setNames(vector("list", n_edges), edge_names)
    for (e in edge_names) edges[[e]] <- list()

    for (i in seq_along(content_segments)) {
      edge_idx <- min(n_edges, ceiling(i / segs_per_edge))
      edge_name <- edge_names[edge_idx]
      edges[[edge_name]][[length(edges[[edge_name]]) + 1]] <- content_segments[[i]]
    }
  } else {
    # Trapezoid piece: 4 edges (INNER, RIGHT, OUTER, LEFT)
    # Use vertex coordinates to correctly identify edge boundaries
    # Path order: V1 -> INNER -> V2 -> RIGHT -> V3 -> OUTER -> V4 -> LEFT -> V1
    edge_names <- c("INNER", "RIGHT", "OUTER", "LEFT")
    edges <- setNames(vector("list", 4), edge_names)
    for (e in edge_names) edges[[e]] <- list()

    # Identify vertices by scanning path endpoint coordinates
    # We find vertices by looking for segment endpoints that mark "corners"
    # (significant direction changes in the path)
    #
    # For concentric trapezoids with bezier edges:
    # - Each edge has 3 bezier segments
    # - A vertex is where one edge ends and another begins
    # - The 4 vertices are visited in order: V1 -> INNER -> V2 -> RIGHT -> V3 -> OUTER -> V4 -> LEFT -> V1

    # Collect all segment endpoints
    all_endpoints <- list()
    for (seg in content_segments) {
      all_endpoints[[length(all_endpoints) + 1]] <- c(seg$x, seg$y)
    }

    # Find vertices by detecting "corners" where direction changes significantly
    # Or use the fact that each edge has a consistent segment count (3 beziers per internal edge)
    # For internal pieces: typically 3+3+3+3 = 12 bezier segments
    # For boundary pieces: 3+3+1(L)+3 = 10 segments

    # Strategy: The path visits V1 -> V2 -> V3 -> V4 -> V1
    # We find these by looking at the endpoints that repeat as both end-of-edge and start-of-edge
    # Simpler approach: count segments and divide into 4 groups

    # Check if there's an L segment - it marks the OUTER boundary
    l_indices <- which(sapply(content_segments, function(s) s$type == "L"))

    if (length(l_indices) > 0) {
      # Has boundary L segment - use it to identify edge splits
      l_idx <- l_indices[1]

      # OUTER is at l_idx, so:
      # Segments 1 to (l_idx-1) are INNER + RIGHT
      # Segment l_idx is OUTER
      # Segments (l_idx+1) to end are LEFT

      before_l <- seq_len(l_idx - 1)
      n_before <- length(before_l)

      # Split INNER and RIGHT evenly
      n_inner <- ceiling(n_before / 2)
      inner_indices <- before_l[seq_len(n_inner)]
      right_indices <- before_l[(n_inner + 1):n_before]

      outer_indices <- l_idx
      left_indices <- (l_idx + 1):length(content_segments)

      for (i in inner_indices) edges$INNER[[length(edges$INNER) + 1]] <- content_segments[[i]]
      for (i in right_indices) edges$RIGHT[[length(edges$RIGHT) + 1]] <- content_segments[[i]]
      for (i in outer_indices) edges$OUTER[[length(edges$OUTER) + 1]] <- content_segments[[i]]
      if (length(left_indices) > 0 && left_indices[1] <= length(content_segments)) {
        for (i in left_indices) edges$LEFT[[length(edges$LEFT) + 1]] <- content_segments[[i]]
      }

    } else {
      # No L segment - internal piece with all bezier edges
      # Total segments should be ~12-16 (3 beziers per edge, OUTER may have more)
      # Use V1 position to detect where we return to start
      n_segs <- length(content_segments)

      # For internal pieces, try to find V1 return point to identify LEFT edge boundary
      # V1 is at (start_x, start_y), path should return close to V1 at end of LEFT
      tolerance <- 1.0

      # Scan for where path endpoints match V1 (return point)
      # This marks the end of LEFT edge (should be near last segment)
      v1_return_idx <- NULL
      for (i in seq_along(all_endpoints)) {
        dist <- sqrt(sum((all_endpoints[[i]] - c(start_x, start_y))^2))
        if (dist < tolerance) {
          v1_return_idx <- i
          # Don't break - we want the LAST match (in case of multiple near-hits)
        }
      }

      if (!is.null(v1_return_idx) && v1_return_idx >= 10) {
        # Found V1 return point - use it to split
        # LEFT ends at v1_return_idx
        # Everything before that is INNER + RIGHT + OUTER
        left_start <- v1_return_idx - 2  # LEFT is typically 3 segments, starts 3 before return

        if (left_start > 6) {
          # Reasonable split: INNER (3) + RIGHT (3) + OUTER (variable) + LEFT (3)
          for (i in 1:3) edges$INNER[[length(edges$INNER) + 1]] <- content_segments[[i]]
          for (i in 4:6) edges$RIGHT[[length(edges$RIGHT) + 1]] <- content_segments[[i]]
          for (i in 7:(left_start - 1)) edges$OUTER[[length(edges$OUTER) + 1]] <- content_segments[[i]]
          for (i in left_start:n_segs) edges$LEFT[[length(edges$LEFT) + 1]] <- content_segments[[i]]
        } else {
          # Fallback to even distribution
          segs_per_edge <- max(1, ceiling(n_segs / 4))
          for (i in seq_along(content_segments)) {
            edge_idx <- min(4, ceiling(i / segs_per_edge))
            edge_name <- edge_names[edge_idx]
            edges[[edge_name]][[length(edges[[edge_name]]) + 1]] <- content_segments[[i]]
          }
        }
      } else {
        # Can't find V1 return - fall back to fixed split assuming 3-3-X-3 pattern
        if (n_segs >= 12) {
          # Standard internal piece: INNER=3, RIGHT=3, OUTER=variable, LEFT=3
          for (i in 1:3) edges$INNER[[length(edges$INNER) + 1]] <- content_segments[[i]]
          for (i in 4:6) edges$RIGHT[[length(edges$RIGHT) + 1]] <- content_segments[[i]]
          outer_end <- n_segs - 3
          for (i in 7:outer_end) edges$OUTER[[length(edges$OUTER) + 1]] <- content_segments[[i]]
          for (i in (outer_end + 1):n_segs) edges$LEFT[[length(edges$LEFT) + 1]] <- content_segments[[i]]
        } else {
          # Small segment count - distribute evenly
          segs_per_edge <- max(1, ceiling(n_segs / 4))
          for (i in seq_along(content_segments)) {
            edge_idx <- min(4, ceiling(i / segs_per_edge))
            edge_name <- edge_names[edge_idx]
            edges[[edge_name]][[length(edges[[edge_name]]) + 1]] <- content_segments[[i]]
          }
        }
      }
    }

  }

  # Convert segment lists to path strings using fast builder
  edge_paths <- list()
  prev_end <- c(start_x, start_y)

  for (edge_name in edge_names) {
    segs <- edges[[edge_name]]
    if (length(segs) == 0) {
      edge_paths[[edge_name]] <- ""
      next
    }

    # Use optimized path string builder (avoids O(n²) string concatenation)
    edge_paths[[edge_name]] <- build_path_string_fast(segs, prev_end)

    # Update prev_end to last segment's endpoint
    if (length(segs) > 0) {
      last_seg <- segs[[length(segs)]]
      prev_end <- c(last_seg$x, last_seg$y)
    }
  }

  return(edge_paths)
}


#' Find vertices (corners) in a path by detecting angle changes
#'
#' @param endpoints List of c(x, y) endpoint coordinates
#' @param n_edges Expected number of edges (4 for trapezoid, 6 for hexagon)
#' @return List of vertex indices in the endpoints list
#' @keywords internal
find_path_vertices <- function(endpoints, n_edges) {
  if (length(endpoints) < 3) {
    # Not enough points to detect vertices
    return(list())
  }

  # Calculate direction angles between consecutive points
  angles <- numeric(length(endpoints) - 1)
  for (i in 1:(length(endpoints) - 1)) {
    dx <- endpoints[[i + 1]][1] - endpoints[[i]][1]
    dy <- endpoints[[i + 1]][2] - endpoints[[i]][2]
    angles[i] <- atan2(dy, dx)
  }

  # Calculate angle changes
  angle_changes <- numeric(length(angles) - 1)
  for (i in 1:(length(angles) - 1)) {
    diff <- angles[i + 1] - angles[i]
    # Normalize to [-pi, pi]
    while (diff > pi) diff <- diff - 2 * pi
    while (diff < -pi) diff <- diff + 2 * pi
    angle_changes[i] <- abs(diff)
  }

  # Find the n_edges-1 largest angle changes (vertices between edges)
  # Add 1 to indices because angle_changes[i] corresponds to endpoint[i+1]
  if (length(angle_changes) == 0) {
    return(list())
  }

  # Get indices sorted by angle change magnitude (descending)
  sorted_indices <- order(angle_changes, decreasing = TRUE)

  # Take top n_edges - 1 vertices (we need n_edges-1 boundaries for n_edges edges)
  n_vertices <- min(n_edges - 1, length(sorted_indices))
  vertex_indices <- sort(sorted_indices[1:n_vertices] + 1)  # +1 to get endpoint index

  return(vertex_indices)
}


#' Assign path segments to edges based on vertex boundaries
#'
#' @param segments Parsed path segments
#' @param vertex_indices Indices of vertices in the path
#' @param edge_names Names of edges
#' @return Named list of segment lists per edge
#' @keywords internal
assign_segments_to_edges <- function(segments, vertex_indices, edge_names) {
  n_edges <- length(edge_names)
  edges <- setNames(vector("list", n_edges), edge_names)
  for (e in edge_names) edges[[e]] <- list()

  # Content segments (excluding M and Z)
  content_segments <- list()
  for (i in 2:length(segments)) {
    if (segments[[i]]$type != "Z") {
      content_segments[[length(content_segments) + 1]] <- segments[[i]]
    }
  }

  if (length(content_segments) == 0) {
    return(edges)
  }

  # If no vertices detected, distribute evenly as fallback
  if (length(vertex_indices) == 0) {
    segs_per_edge <- ceiling(length(content_segments) / n_edges)
    for (i in seq_along(content_segments)) {
      edge_idx <- min(n_edges, ceiling(i / segs_per_edge))
      edge_name <- edge_names[edge_idx]
      edges[[edge_name]][[length(edges[[edge_name]]) + 1]] <- content_segments[[i]]
    }
    return(edges)
  }

  # Assign segments based on vertex boundaries
  # vertex_indices are 1-based into the endpoint list
  # Segment i ends at endpoint i+1 (since endpoint 1 is the M start)
  current_edge <- 1
  vertex_ptr <- 1

  for (i in seq_along(content_segments)) {
    seg <- content_segments[[i]]
    edge_name <- edge_names[current_edge]
    edges[[edge_name]][[length(edges[[edge_name]]) + 1]] <- seg

    # Check if this segment ends at a vertex (transition to next edge)
    # Segment i ends at endpoint i+1
    seg_endpoint_idx <- i + 1
    if (vertex_ptr <= length(vertex_indices) && seg_endpoint_idx == vertex_indices[vertex_ptr]) {
      current_edge <- min(current_edge + 1, n_edges)
      vertex_ptr <- vertex_ptr + 1
    }
  }

  return(edges)
}


#' Split hexagonal piece path into individual edge paths
#'
#' Parses a hexagonal piece path and extracts the 6 edge segments (sides 0-5).
#' Uses vertex detection based on angle changes to correctly identify edge boundaries.
#'
#' @param path SVG path string for a hexagonal piece
#' @param piece Piece object with path and ring_pos
#' @return List with edge path strings keyed by side number ("0" through "5")
#' @keywords internal
split_hex_path_into_edges <- function(path, piece = NULL) {
  # Use cached parsed_segments if available (Phase 2 optimization)
  segments <- if (!is.null(piece) && !is.null(piece$parsed_segments)) {
    piece$parsed_segments
  } else {
    parse_svg_path(path)
  }

  if (length(segments) == 0 || segments[[1]]$type != "M") {
    return(list(`0` = "", `1` = "", `2` = "", `3` = "", `4` = "", `5` = ""))
  }

  # Hexagonal pieces have 6 sides numbered 0-5
  edge_names <- as.character(0:5)
  n_edges <- 6

  start_x <- segments[[1]]$x
  start_y <- segments[[1]]$y

  # Extract content segments (excluding M and Z)
  content_segments <- list()
  for (i in 2:length(segments)) {
    if (segments[[i]]$type != "Z") {
      content_segments[[length(content_segments) + 1]] <- segments[[i]]
    }
  }

  # For hexagonal pieces:
  # - Each edge typically has 3 bezier segments (one complete bezier tab)
  # - Total: 18 segments for internal pieces (6 edges x 3 beziers)
  # - Boundary edges may have L or A segments
  edges <- setNames(vector("list", n_edges), edge_names)
  for (e in edge_names) edges[[e]] <- list()

  n_segs <- length(content_segments)

  if (n_segs == 0) {
    # Empty path
    edge_paths <- list()
    for (edge_name in edge_names) edge_paths[[edge_name]] <- ""
    return(edge_paths)
  }

  # Find L and A segments - they often mark boundary edges
  special_indices <- which(sapply(content_segments, function(s) s$type %in% c("L", "A")))

  if (length(special_indices) > 0 && length(special_indices) <= 3) {
    # Boundary piece: L/A segments mark specific edges
    # Distribute beziers evenly, assigning special segments to their own edges
    bezier_count <- n_segs - length(special_indices)
    beziers_per_edge <- max(1, round(bezier_count / (n_edges - length(special_indices))))

    current_edge <- 1
    bezier_in_edge <- 0

    for (i in seq_along(content_segments)) {
      seg <- content_segments[[i]]
      edge_name <- edge_names[current_edge]

      if (seg$type %in% c("L", "A")) {
        # Special segment gets its own edge if it's at a boundary
        if (bezier_in_edge > 0) {
          # Move to next edge for this special segment
          current_edge <- min(n_edges, current_edge + 1)
          edge_name <- edge_names[current_edge]
        }
        edges[[edge_name]][[length(edges[[edge_name]]) + 1]] <- seg
        # Move to next edge after special segment
        current_edge <- min(n_edges, current_edge + 1)
        bezier_in_edge <- 0
      } else {
        # Bezier segment
        edges[[edge_name]][[length(edges[[edge_name]]) + 1]] <- seg
        bezier_in_edge <- bezier_in_edge + 1
        if (bezier_in_edge >= beziers_per_edge && current_edge < n_edges) {
          current_edge <- current_edge + 1
          bezier_in_edge <- 0
        }
      }
    }
  } else {
    # Internal piece or standard distribution: 3 segments per edge
    segs_per_edge <- max(1, ceiling(n_segs / n_edges))

    for (i in seq_along(content_segments)) {
      edge_idx <- min(n_edges, ceiling(i / segs_per_edge))
      edge_name <- edge_names[edge_idx]
      edges[[edge_name]][[length(edges[[edge_name]]) + 1]] <- content_segments[[i]]
    }
  }

  # Convert segment lists to path strings using fast builder
  edge_paths <- list()
  prev_end <- c(start_x, start_y)

  for (edge_name in edge_names) {
    segs <- edges[[edge_name]]
    if (length(segs) == 0) {
      edge_paths[[edge_name]] <- ""
      next
    }

    # Use optimized path string builder (avoids O(n²) string concatenation)
    edge_paths[[edge_name]] <- build_path_string_fast(segs, prev_end)

    # Update prev_end to last segment's endpoint
    if (length(segs) > 0) {
      last_seg <- segs[[length(segs)]]
      prev_end <- c(last_seg$x, last_seg$y)
    }
  }

  return(edge_paths)
}


#' Get edge paths for a piece based on its type
#'
#' Dispatches to the appropriate path splitting function based on piece type.
#'
#' @param piece Piece object with path, type, and position info
#' @return List with edge path strings keyed by edge name/number
#' @keywords internal
get_piece_edge_paths <- function(piece) {
  piece_type <- piece$type %||% "rectangular"

  if (piece_type == "concentric") {
    return(split_concentric_path_into_edges(piece$path, piece))
  } else if (piece_type == "hexagonal") {
    return(split_hex_path_into_edges(piece$path, piece))
  } else if (piece_type %in% c("voronoi", "random", "snic")) {
    # For voronoi/random/snic, use edge_segments keyed by neighbor_id
    # Returns list with neighbor_id keys -> path values
    if (is.null(piece$edge_segments) || length(piece$edge_segments) == 0) {
      return(list())
    }
    # Extract just the path from each segment
    edge_paths <- lapply(piece$edge_segments, function(seg) seg$path)
    return(edge_paths)
  } else {
    return(split_rect_path_into_edges(piece$path, piece))
  }
}


#' Get edge names for a piece based on its type
#'
#' Returns the edge names/numbers used for a piece type.
#'
#' @param piece Piece object with type and position info
#' @return Character vector of edge names
#' @keywords internal
get_piece_edge_names <- function(piece) {
  piece_type <- piece$type %||% "rectangular"

  if (piece_type == "concentric") {
    # Check if center piece (6 edges) or trapezoid (4 edges)
    if (!is.null(piece$ring_pos) && piece$ring_pos$ring == 0) {
      return(as.character(1:6))
    }
    return(c("INNER", "RIGHT", "OUTER", "LEFT"))
  } else if (piece_type == "hexagonal") {
    return(as.character(0:5))
  } else if (piece_type %in% c("voronoi", "random", "snic")) {
    # For voronoi/random/snic, edge names are neighbor IDs (as strings)
    if (is.null(piece$edge_segments) || length(piece$edge_segments) == 0) {
      return(character())
    }
    return(names(piece$edge_segments))
  } else {
    return(c("N", "E", "S", "W"))
  }
}
