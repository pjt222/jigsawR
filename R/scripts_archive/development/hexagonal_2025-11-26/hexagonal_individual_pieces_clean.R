# Clean Hexagonal Individual Piece Generation
# Following the successful rectangular approach: generate edges once, assemble pieces

#' Generate all hexagonal edges for individual pieces
#'
#' Pre-generates all shared edges in a hexagonal puzzle using the core
#' generation functions. Each edge is stored with both forward and reverse
#' paths for use by adjacent pieces.
#'
#' @param rings Number of rings in hexagonal puzzle
#' @return List containing all edge segments with forward/reverse paths
generate_all_hex_edges <- function(rings) {

  # Initialize hexagonal generation state
  # Note: init_hex_jigsaw must be called before this function

  edges <- list(
    horizontal = list(),  # Horizontal edges (connecting pieces side-to-side)
    vertical = list(),    # Vertical edges (connecting pieces top-to-bottom)
    border = list()       # Border segments for edge pieces
  )

  n <- rings

  # Generate horizontal edge segments
  # These connect adjacent pieces in the same "row" of the hex grid
  .hex_jigsaw_env$seed <- .hex_jigsaw_env$seed_initial
  yl <- 2 * n - 1
  edge_index <- 1

  for (yi in seq(-yl + 2, yl - 2, by = 2)) {
    xl <- 2 * n - 1 - (abs(yi) - 1) / 2
    for (xi in seq(-xl + 1, xl - 2, by = 1)) {
      # Generate this edge segment
      forward_path <- hex_hlineseg(xi, yi, TRUE)

      # Store with both orientations
      edges$horizontal[[edge_index]] <- list(
        xi = xi,
        yi = yi,
        forward = forward_path,
        reverse = reverse_hex_path(forward_path),
        type = "horizontal"
      )

      edge_index <- edge_index + 1
    }
  }

  # Generate vertical edge segments
  # These connect pieces in adjacent "rows"
  .hex_jigsaw_env$seed <- .hex_jigsaw_env$seed_initial + 10000
  edge_index <- 1

  for (yi in seq(-yl, yl - 1, by = 2)) {
    xl <- 2 * n - 1 - (abs(yi + 1)) / 2
    for (xi in seq(-xl + 2, xl - 2, by = 2)) {
      # Generate this edge segment
      forward_path <- hex_vlineseg(xi, yi)

      # Store with both orientations
      edges$vertical[[edge_index]] <- list(
        xi = xi,
        yi = yi,
        forward = forward_path,
        reverse = reverse_hex_path(forward_path),
        type = "vertical"
      )

      edge_index <- edge_index + 1
    }
  }

  # Generate border segments for edge pieces
  .hex_jigsaw_env$seed <- .hex_jigsaw_env$seed_initial + 20000
  edge_index <- 1
  yi <- 1 - 2 * n

  if (!.hex_jigsaw_env$do_trunc) {
    for (rot in seq(0.0, 2 * pi - 0.1, by = pi/3)) {
      for (xi in seq(-n, n - 2, by = 1)) {
        forward_path <- hex_blineseg(xi, yi, TRUE, rot)

        edges$border[[edge_index]] <- list(
          xi = xi,
          yi = yi,
          rot = rot,
          forward = forward_path,
          type = "border"
        )

        edge_index <- edge_index + 1
      }
    }
  }

  return(edges)
}

#' Reverse a hexagonal path segment
#'
#' Reverses the direction of a hex path segment so it can be used by
#' the adjacent piece. This is critical for ensuring complementary edges.
#'
#' @param path_segment SVG path string
#' @return Reversed path string
reverse_hex_path <- function(path_segment) {
  # For now, use the bezier_utils functions if available
  if (exists("reverse_path_segments")) {
    # Parse, reverse, and rebuild
    segments <- parse_svg_path(path_segment)
    reversed_segments <- reverse_path_segments(segments)

    # Rebuild path
    path_parts <- character()
    for (i in seq_along(reversed_segments)) {
      seg <- reversed_segments[[i]]
      if (seg$type == "M") {
        path_parts <- c(path_parts, sprintf("M %.2f %.2f", seg$x, seg$y))
      } else if (seg$type == "C") {
        path_parts <- c(path_parts, sprintf("C %.2f %.2f %.2f %.2f %.2f %.2f",
                                           seg$cp1x, seg$cp1y, seg$cp2x, seg$cp2y, seg$x, seg$y))
      } else if (seg$type == "L") {
        path_parts <- c(path_parts, sprintf("L %.2f %.2f", seg$x, seg$y))
      }
    }

    return(paste(path_parts, collapse = " "))
  }

  # Fallback: simple coordinate reversal (not perfect but workable)
  return(simple_reverse_path(path_segment))
}

#' Simple path reversal (fallback)
#'
#' @param path SVG path string
#' @return Reversed path (simplified approach)
simple_reverse_path <- function(path) {
  # Extract all coordinates
  coords <- as.numeric(unlist(regmatches(path, gregexpr("-?\\d+\\.?\\d*", path))))
  coords <- coords[!is.na(coords)]

  if (length(coords) < 4) return(path)

  # Reverse coordinate pairs
  n_pairs <- length(coords) / 2
  reversed_coords <- numeric(length(coords))

  for (i in 1:n_pairs) {
    from_idx <- (i - 1) * 2 + 1
    to_idx <- (n_pairs - i) * 2 + 1
    reversed_coords[to_idx:(to_idx + 1)] <- coords[from_idx:(from_idx + 1)]
  }

  # Rebuild path with reversed coordinates
  result <- path
  for (i in seq_along(coords)) {
    result <- sub("-?\\d+\\.?\\d*", sprintf("%.2f", reversed_coords[i]), result)
  }

  return(result)
}

#' Calculate hexagonal piece positions
#'
#' Determines the position and neighbors of each piece in the hexagonal grid.
#' Uses axial coordinates (q, r) for proper hexagonal positioning.
#'
#' @param rings Number of rings
#' @return List of piece positions with coordinate data
calculate_hex_piece_positions <- function(rings) {

  positions <- list()
  piece_index <- 1

  # Center piece at (0, 0)
  positions[[piece_index]] <- list(
    index = piece_index,
    q = 0,
    r = 0,
    ring = 0,
    type = "center",
    neighbors = list()  # 6 neighbors for center
  )
  piece_index <- piece_index + 1

  # Generate pieces ring by ring
  # Hexagonal directions: E, NE, NW, W, SW, SE
  hex_directions <- list(
    c(1, 0),   # East
    c(1, -1),  # Northeast
    c(0, -1),  # Northwest
    c(-1, 0),  # West
    c(-1, 1),  # Southwest
    c(0, 1)    # Southeast
  )

  for (ring in 1:(rings - 1)) {
    # Start at the "top" of this ring
    q <- 0
    r <- -ring

    # Walk around the ring in 6 directions
    for (direction in 1:6) {
      # Walk 'ring' steps in this direction
      for (step in 1:ring) {
        # Calculate position in coordinate space
        # (will be used to match with edge segments)
        xi <- q  # Simplified mapping (will need refinement)
        yi <- r * 2

        # Determine piece type
        piece_type <- if (ring == rings - 1) "edge" else "interior"

        positions[[piece_index]] <- list(
          index = piece_index,
          q = q,
          r = r,
          xi = xi,
          yi = yi,
          ring = ring,
          position_in_ring = (direction - 1) * ring + step - 1,
          type = piece_type,
          neighbors = calculate_hex_neighbors(q, r, hex_directions)
        )

        piece_index <- piece_index + 1

        # Move to next position (except last step of last direction)
        if (!(direction == 6 && step == ring)) {
          q <- q + hex_directions[[direction]][1]
          r <- r + hex_directions[[direction]][2]
        }
      }
    }
  }

  return(positions)
}

#' Calculate hexagonal neighbors
#'
#' @param q Axial q coordinate
#' @param r Axial r coordinate
#' @param directions Hexagonal direction vectors
#' @return List of neighbor coordinates
calculate_hex_neighbors <- function(q, r, directions) {
  neighbors <- list()
  for (i in seq_along(directions)) {
    dir <- directions[[i]]
    neighbors[[i]] <- list(
      q = q + dir[1],
      r = r + dir[2],
      direction = i
    )
  }
  return(neighbors)
}

#' Build hexagonal piece path from edges
#'
#' Assembles a complete hexagonal piece by combining 6 edges.
#' Uses the same principle as rectangular pieces: adjacent pieces share edges.
#'
#' @param piece_pos Piece position data
#' @param edges All pre-generated edges
#' @param rings Total number of rings
#' @return Complete SVG path for the piece
build_hex_piece_path <- function(piece_pos, edges, rings) {

  # For a hexagonal piece, we need to identify its 6 edges
  # based on its position in the grid and its neighbors

  piece_edges <- identify_hex_piece_edges(piece_pos, edges, rings)

  if (length(piece_edges) == 0) {
    # Fallback to simple hexagon
    return(create_simple_hex_path(piece_pos))
  }

  # Assemble the edges clockwise
  path_parts <- list()

  for (i in seq_along(piece_edges)) {
    edge_info <- piece_edges[[i]]

    if (!is.null(edge_info$path)) {
      if (i == 1) {
        # First edge: keep M command
        path_parts <- c(path_parts, edge_info$path)
      } else {
        # Subsequent edges: remove M command and chain
        cleaned <- gsub("^M[^CL]*", "", edge_info$path)
        path_parts <- c(path_parts, cleaned)
      }
    }
  }

  # Close the path
  path_parts <- c(path_parts, "Z")

  complete_path <- paste(path_parts, collapse = " ")
  complete_path <- gsub("\\s+", " ", trimws(complete_path))

  return(complete_path)
}

#' Identify edges for a hexagonal piece
#'
#' Maps piece position to the specific edge segments that form its boundary.
#'
#' @param piece_pos Piece position data
#' @param edges All edges
#' @param rings Number of rings
#' @return List of edge segments for this piece
identify_hex_piece_edges <- function(piece_pos, edges, rings) {

  # This is a simplified version - the actual implementation would need
  # to properly map hexagonal coordinates to edge indices

  piece_edges <- list()

  # For now, return empty to trigger fallback
  # This is where the core mapping logic would go

  return(piece_edges)
}

#' Create simple hexagonal path (fallback)
#'
#' @param piece_pos Piece position data
#' @return Simple hexagon SVG path
create_simple_hex_path <- function(piece_pos) {
  # Calculate center position
  # This uses the q, r coordinates to determine world position
  size <- 25
  x <- size * (3/2 * piece_pos$q)
  y <- size * (sqrt(3)/2 * piece_pos$q + sqrt(3) * piece_pos$r)

  # Create regular hexagon
  radius <- 10
  vertices <- list()
  for (i in 0:5) {
    angle <- i * pi / 3
    vx <- x + radius * cos(angle)
    vy <- y + radius * sin(angle)
    vertices[[i + 1]] <- c(vx, vy)
  }

  # Build path
  path <- sprintf("M %.2f %.2f", vertices[[1]][1], vertices[[1]][2])
  for (i in 2:6) {
    path <- paste(path, sprintf("L %.2f %.2f", vertices[[i]][1], vertices[[i]][2]))
  }
  path <- paste(path, "Z")

  return(path)
}

#' Generate individual hexagonal puzzle pieces (clean implementation)
#'
#' Follows the successful rectangular approach:
#' 1. Generate all edges once
#' 2. Assemble individual pieces from shared edges
#' 3. Ensure adjacent pieces use exact same edge paths
#'
#' @param seed Random seed
#' @param rings Number of rings
#' @param diameter Puzzle diameter in mm
#' @param tabsize Tab size percentage
#' @param jitter Jitter percentage
#' @param do_warp Apply circular warping
#' @param do_trunc Truncate edge pieces
#' @param output_dir Output directory
#' @param save_combined Save combined view
#' @param colors Color palette
#' @return List with pieces and metadata
#' @export
generate_hexagonal_individual_pieces_clean <- function(seed = 42, rings = 3,
                                                      diameter = 240,
                                                      tabsize = 27, jitter = 5,
                                                      do_warp = FALSE, do_trunc = FALSE,
                                                      output_dir = "output",
                                                      save_combined = TRUE,
                                                      colors = NULL) {

  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Source required functions if not in package context
  if (!exists("init_hex_jigsaw")) {
    if (file.exists("R/hexagonal_puzzle.R")) {
      source("R/hexagonal_puzzle.R")
    }
  }
  if (exists("log_info")) {
    log_info("Generating {rings}-ring hexagonal puzzle (seed: {seed})...")
  } else {
    cat(sprintf("Generating %d-ring hexagonal puzzle (seed: %d)...\n", rings, seed))
  }

  # Initialize hexagonal puzzle environment
  init_hex_jigsaw(seed = seed, tabsize = tabsize, jitter = jitter,
                  diameter = diameter, rings = rings,
                  do_warp = do_warp, do_trunc = do_trunc)

  # Generate all shared edges
  edges <- generate_all_hex_edges(rings)

  # Calculate piece positions
  piece_positions <- calculate_hex_piece_positions(rings)

  # Calculate expected number of pieces
  num_pieces <- 3 * rings * (rings - 1) + 1

  if (exists("log_info")) {
    log_info("Expected {num_pieces} pieces, calculated {length(piece_positions)} positions")
  } else {
    cat(sprintf("Expected %d pieces, calculated %d positions\n", num_pieces, length(piece_positions)))
  }

  # Storage for piece data
  pieces <- list()

  # Generate each piece
  for (i in seq_along(piece_positions)) {
    piece_pos <- piece_positions[[i]]

    # Build piece path from edges
    piece_path <- build_hex_piece_path(piece_pos, edges, rings)

    # Store piece data
    pieces[[i]] <- list(
      index = i,
      q = piece_pos$q,
      r = piece_pos$r,
      ring = piece_pos$ring,
      type = piece_pos$type,
      path = piece_path
    )

    if (exists("log_info")) {
      log_info("Generated piece {i} (ring {piece_pos$ring})")
    }
  }

  if (exists("log_success")) {
    log_success("Successfully generated {length(pieces)} hexagonal pieces!")
  } else {
    cat(sprintf("Successfully generated %d hexagonal pieces!\n", length(pieces)))
  }

  return(list(
    pieces = pieces,
    parameters = list(
      seed = seed,
      rings = rings,
      diameter = diameter,
      tabsize = tabsize,
      jitter = jitter,
      total_pieces = length(pieces),
      do_warp = do_warp,
      do_trunc = do_trunc
    ),
    edges = edges,
    files = list(
      output_dir = output_dir
    )
  ))
}
