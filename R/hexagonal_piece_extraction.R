# Hexagonal Puzzle Piece Extraction
# Real solution: Extract individual pieces from complete puzzle paths

#' Split SVG path by M commands to get individual segments
#'
#' @param svg_path Complete SVG path string
#' @return List of individual path segments with their data
split_path_by_move <- function(svg_path) {
  # Split by M commands while keeping the M
  # Pattern: M followed by coordinates, then other commands until next M
  pattern <- "M\\s*[^M]+"
  matches <- gregexpr(pattern, svg_path, perl = TRUE)
  segments_raw <- regmatches(svg_path, matches)[[1]]

  segments <- list()
  for (i in seq_along(segments_raw)) {
    seg_str <- trimws(segments_raw[i])

    # Parse to get start and end points
    parsed <- parse_svg_path(seg_str)

    if (length(parsed) == 0) next

    # Start point is from M command
    start_point <- c(parsed[[1]]$x, parsed[[1]]$y)

    # End point is from last command
    last_cmd <- parsed[[length(parsed)]]
    if (last_cmd$type == "Z") {
      # If closed, end point is start point
      end_point <- start_point
    } else {
      end_point <- c(last_cmd$x, last_cmd$y)
    }

    segments[[i]] <- list(
      path = seg_str,
      start = start_point,
      end = end_point,
      parsed = parsed
    )
  }

  return(segments)
}

#' Find segments that connect to a given endpoint
#'
#' @param point Target point c(x, y)
#' @param segments List of segments to search
#' @param tolerance Matching tolerance for coordinates
#' @return Indices of matching segments
find_connecting_segments <- function(point, segments, tolerance = 0.1) {
  matches <- integer(0)

  for (i in seq_along(segments)) {
    seg <- segments[[i]]

    # Check if segment starts or ends at this point
    if (points_match(point, seg$start, tolerance)) {
      matches <- c(matches, i)
    } else if (points_match(point, seg$end, tolerance)) {
      matches <- c(matches, i)
    }
  }

  return(matches)
}

#' Check if two points match within tolerance
#'
#' @param p1 Point 1 c(x, y)
#' @param p2 Point 2 c(x, y)
#' @param tolerance Matching tolerance
#' @return TRUE if points match
points_match <- function(p1, p2, tolerance = 0.1) {
  if (length(p1) != 2 || length(p2) != 2) return(FALSE)
  dist <- sqrt((p1[1] - p2[1])^2 + (p1[2] - p2[2])^2)
  return(dist < tolerance)
}

#' Build connectivity graph from path segments
#'
#' Creates an adjacency graph showing which segments connect at their endpoints.
#'
#' @param segments List of segment data (each has start, end, path)
#' @param tolerance Coordinate matching tolerance
#' @return List representing adjacency graph: segment_id -> list of connected segment_ids
build_segment_graph <- function(segments, tolerance = 0.1) {

  n_segments <- length(segments)
  graph <- vector("list", n_segments)

  # For each segment, find all other segments that share endpoints
  for (i in seq_along(segments)) {
    seg <- segments[[i]]
    connections <- integer(0)

    # Check both endpoints
    for (j in seq_along(segments)) {
      if (i == j) next  # Don't connect to self

      other_seg <- segments[[j]]

      # Check if segments share any endpoints
      if (points_match(seg$start, other_seg$start, tolerance) ||
          points_match(seg$start, other_seg$end, tolerance) ||
          points_match(seg$end, other_seg$start, tolerance) ||
          points_match(seg$end, other_seg$end, tolerance)) {
        connections <- c(connections, j)
      }
    }

    graph[[i]] <- list(
      segment_id = i,
      connections = connections,
      start = seg$start,
      end = seg$end
    )
  }

  return(graph)
}

#' Extract all unique endpoints from segments
#'
#' @param segments List of segments
#' @param tolerance Coordinate matching tolerance
#' @return Data frame of unique points with their segment connections
extract_unique_endpoints <- function(segments, tolerance = 0.1) {

  # Collect all endpoints
  all_points <- list()
  for (i in seq_along(segments)) {
    seg <- segments[[i]]
    all_points[[length(all_points) + 1]] <- list(
      x = seg$start[1],
      y = seg$start[2],
      segment_id = i,
      endpoint_type = "start"
    )
    all_points[[length(all_points) + 1]] <- list(
      x = seg$end[1],
      y = seg$end[2],
      segment_id = i,
      endpoint_type = "end"
    )
  }

  # Group points by proximity
  unique_points <- list()
  used <- rep(FALSE, length(all_points))

  for (i in seq_along(all_points)) {
    if (used[i]) next

    pt <- all_points[[i]]
    cluster <- list(pt)
    used[i] <- TRUE

    # Find all nearby points
    for (j in seq_along(all_points)) {
      if (used[j]) next

      other_pt <- all_points[[j]]
      if (points_match(c(pt$x, pt$y), c(other_pt$x, other_pt$y), tolerance)) {
        cluster[[length(cluster) + 1]] <- other_pt
        used[j] <- TRUE
      }
    }

    unique_points[[length(unique_points) + 1]] <- cluster
  }

  return(unique_points)
}

#' Extract individual hexagonal puzzle pieces from complete paths
#'
#' This is the core extraction algorithm that traces piece boundaries.
#'
#' @param horizontal_path Complete horizontal cuts path
#' @param vertical_path Complete vertical cuts path
#' @param border_path Complete border path
#' @param rings Number of rings
#' @return List of individual pieces with their paths
extract_hex_pieces_from_paths <- function(horizontal_path, vertical_path, border_path, rings) {

  cat("Extracting hexagonal pieces from complete paths...\n")

  # Step 1: Split all paths into segments
  h_segments <- split_path_by_move(horizontal_path)
  v_segments <- split_path_by_move(vertical_path)
  b_segments <- split_path_by_move(border_path)

  cat(sprintf("  Split into %d horizontal, %d vertical, %d border segments\n",
              length(h_segments), length(v_segments), length(b_segments)))

  # Step 2: Combine all segments
  all_segments <- c(h_segments, v_segments, b_segments)

  # Step 3: Build connectivity graph
  cat("  Building connectivity graph...\n")
  connectivity_graph <- build_segment_graph(all_segments, tolerance = 0.5)

  # Extract unique endpoints for analysis
  unique_points <- extract_unique_endpoints(all_segments, tolerance = 0.5)

  cat(sprintf("  Found %d segments with %d unique junction points\n",
              length(all_segments), length(unique_points)))

  # Verify connectivity
  connections_per_segment <- sapply(connectivity_graph, function(node) length(node$connections))
  cat(sprintf("  Connections per segment: min=%d, max=%d, mean=%.1f\n",
              min(connections_per_segment),
              max(connections_per_segment),
              mean(connections_per_segment)))

  expected_pieces <- 3 * rings * (rings - 1) + 1

  cat(sprintf("  Expected %d pieces\n", expected_pieces))
  cat("  Phase 1 Day 1: Connectivity graph built successfully\n")
  cat("  Next: Implement cycle detection and piece tracing\n")

  return(list(
    segments = list(
      horizontal = h_segments,
      vertical = v_segments,
      border = b_segments,
      all = all_segments
    ),
    connectivity_graph = connectivity_graph,
    unique_points = unique_points,
    expected_pieces = expected_pieces,
    status = "graph_built"
  ))
}

#' Generate separated hexagonal puzzle with REAL pieces (work in progress)
#'
#' @param rings Number of rings
#' @param seed Random seed
#' @param diameter Puzzle diameter
#' @param offset Separation distance
#' @param tabsize Tab size percentage
#' @param jitter Jitter percentage
#' @param do_warp Apply circular warping
#' @param do_trunc Truncate edge pieces
#' @param colors Piece colors
#' @param stroke_width Line width
#' @return SVG content with separated real pieces
generate_separated_hex_real <- function(rings = 3, seed = NULL,
                                       diameter = 240, offset = 10,
                                       tabsize = 27, jitter = 5,
                                       do_warp = FALSE, do_trunc = FALSE,
                                       colors = NULL, stroke_width = 1.5) {

  if (is.null(seed)) {
    seed <- as.integer(runif(1) * 10000)
  }

  cat(sprintf("Generating separated hexagonal puzzle (real pieces)...\n"))
  cat(sprintf("  Rings: %d, Seed: %d\n", rings, seed))

  # Generate complete puzzle
  puzzle <- generate_hex_jigsaw_svg(
    seed = seed,
    tabsize = tabsize,
    jitter = jitter,
    diameter = diameter,
    rings = rings,
    do_warp = do_warp,
    do_trunc = do_trunc
  )

  # Extract pieces
  extraction <- extract_hex_pieces_from_paths(
    puzzle$horizontal,
    puzzle$vertical,
    puzzle$border,
    rings
  )

  cat("\n")
  cat("CURRENT STATUS: Piece extraction in progress\n")
  cat("For now, returning complete puzzle\n")
  cat("Next step: Implement piece boundary tracing\n")

  # For now, return the complete puzzle
  # TODO: Replace with actual separated pieces
  return(puzzle$svg)
}
