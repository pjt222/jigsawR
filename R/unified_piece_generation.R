# Unified Piece Generation Module
# Part of Epic #32 - Unified Puzzle Generation Pipeline
# Always generates closed piece paths regardless of puzzle type

#' Generate all puzzle pieces as closed paths
#'
#' Core function that generates piece objects with closed SVG paths.
#' Works for rectangular, hexagonal, and concentric puzzles with consistent output structure.
#'
#' @param type Puzzle type: "rectangular", "hexagonal", or "concentric"
#' @param seed Random seed for reproducibility
#' @param grid For rectangular: c(rows, cols). For hexagonal/concentric: c(rings) or just rings
#' @param size For rectangular: c(width, height). For hexagonal/concentric: c(diameter) or just diameter
#' @param tabsize Tab size as percentage (10-40, default: 20)
#' @param jitter Jitter as percentage (0-15, default: 4)
#' @param do_warp Apply circular warp (hexagonal only, default: FALSE)
#' @param do_trunc Truncate boundary (hexagonal only, default: FALSE)
#' @param do_circular_border Use perfect circular arc borders (hexagonal: requires do_warp=TRUE; concentric: always available)
#' @param center_shape Center piece shape for concentric type: "hexagon" or "circle"
#' @param boundary_facing Direction the circular arc faces (concentric only): "outward" or "inward"
#' @return List with:
#'   - pieces: List of piece objects with id, path, center, grid_pos/ring_pos
#'   - canvas_size: c(width, height) for compact (offset=0) layout
#'   - type: "rectangular", "hexagonal", or "concentric"
#'   - parameters: Generation parameters used
#' @export
generate_pieces_internal <- function(type = "rectangular",
                                     seed = NULL,
                                     grid = c(2, 2),
                                     size = c(200, 200),
                                     tabsize = 20,
                                     jitter = 4,
                                     do_warp = FALSE,
                                     do_trunc = FALSE,
                                     do_circular_border = FALSE,
                                     center_shape = "hexagon",
                                     boundary_facing = "outward") {

  # Generate seed if not provided
  if (is.null(seed)) {
    seed <- as.integer(runif(1) * 10000)
  }

  # Dispatch to type-specific implementation
  if (type == "concentric") {
    return(generate_concentric_pieces_internal(
      seed = seed,
      rings = if (length(grid) == 1) grid else grid[1],
      diameter = if (length(size) == 1) size else size[1],
      tabsize = tabsize,
      jitter = jitter,
      center_shape = center_shape,
      do_circular_border = do_circular_border,
      boundary_facing = boundary_facing
    ))
  } else if (type == "hexagonal") {
    return(generate_hex_pieces_internal(
      seed = seed,
      rings = if (length(grid) == 1) grid else grid[1],
      diameter = if (length(size) == 1) size else size[1],
      tabsize = tabsize,
      jitter = jitter,
      do_warp = do_warp,
      do_trunc = do_trunc,
      do_circular_border = do_circular_border
    ))
  } else {
    return(generate_rect_pieces_internal(
      seed = seed,
      grid = grid,
      size = size,
      tabsize = tabsize,
      jitter = jitter
    ))
  }
}


#' Generate rectangular puzzle pieces internally
#'
#' @param seed Random seed
#' @param grid c(rows, cols)
#' @param size c(width, height) in mm
#' @param tabsize Tab size percentage
#' @param jitter Jitter percentage
#' @return Piece generation result
#' @keywords internal
generate_rect_pieces_internal <- function(seed, grid, size, tabsize, jitter) {

  yn <- grid[1]  # rows
  xn <- grid[2]  # cols
  width <- size[1]
  height <- size[2]

  # Generate puzzle structure using existing core function
  puzzle_structure <- generate_puzzle_core(
    seed = seed,
    grid = grid,
    size = size,
    tabsize = tabsize,
    jitter = jitter
  )

  piece_width <- puzzle_structure$piece_width
  piece_height <- puzzle_structure$piece_height

  # Generate all pieces

pieces <- list()
  piece_idx <- 1

  for (yi in 0:(yn - 1)) {
    for (xi in 0:(xn - 1)) {
      # Generate closed piece path using existing function
      piece_path <- generate_single_piece(xi, yi, puzzle_structure)

      # Calculate piece center
      center_x <- (xi + 0.5) * piece_width
      center_y <- (yi + 0.5) * piece_height

      # Create standardized piece object
      pieces[[piece_idx]] <- list(
        id = sprintf("piece_%d_%d", xi, yi),
        path = piece_path,
        center = c(center_x, center_y),
        grid_pos = c(xi = xi, yi = yi),
        type = "rectangular"
      )

      piece_idx <- piece_idx + 1
    }
  }

  return(list(
    pieces = pieces,
    canvas_size = c(width, height),
    type = "rectangular",
    parameters = list(
      seed = seed,
      grid = grid,
      size = size,
      tabsize = tabsize,
      jitter = jitter,
      piece_width = piece_width,
      piece_height = piece_height
    ),
    # Keep puzzle_structure for backward compatibility
    puzzle_structure = puzzle_structure
  ))
}


#' Generate hexagonal puzzle pieces internally
#'
#' @param seed Random seed
#' @param rings Number of rings
#' @param diameter Puzzle diameter in mm
#' @param tabsize Tab size percentage
#' @param jitter Jitter percentage
#' @param do_warp Apply circular warp
#' @param do_trunc Truncate boundary
#' @param do_circular_border Use perfect circular arc borders (requires do_warp=TRUE)
#' @return Piece generation result
#' @keywords internal
generate_hex_pieces_internal <- function(seed, rings, diameter, tabsize, jitter,
                                         do_warp = FALSE, do_trunc = FALSE,
                                         do_circular_border = FALSE) {

  # Use existing edge map generation
  # This generates all pieces with proper complementary edges
  hex_pieces <- generate_hex_pieces_with_edge_map(
    rings = rings,
    seed = seed,
    diameter = diameter,
    tabsize = tabsize,
    jitter = jitter,
    separated = FALSE,  # Compact positions (offset=0)
    separation_factor = 1.0,
    do_warp = do_warp,
    do_trunc = do_trunc,
    do_circular_border = do_circular_border
  )

  # Convert to standardized piece format
  pieces <- lapply(seq_along(hex_pieces), function(i) {
    hp <- hex_pieces[[i]]

    list(
      id = sprintf("piece_%d", hp$id),
      path = hp$path,
      center = c(hp$center_x, hp$center_y),
      ring_pos = list(ring = hp$ring, position = hp$position_in_ring),
      type = "hexagonal"
    )
  })

  # Correct formula: diameter / (4 * rings - 2)
  piece_radius <- diameter / (4 * rings - 2)

  # Calculate canvas size from actual piece path bounds
  # This is critical when warp/trunc are enabled, as pieces extend beyond centers
  all_path_x <- c()
  all_path_y <- c()

  for (piece in pieces) {
    # Extract all coordinates from the path
    path <- piece$path
    numbers <- as.numeric(unlist(regmatches(path, gregexpr("-?[0-9]+\\.?[0-9]*", path))))
    numbers <- numbers[!is.na(numbers)]

    if (length(numbers) >= 2) {
      # Coordinates alternate: x, y, x, y, ...
      x_coords <- numbers[seq(1, length(numbers), by = 2)]
      y_coords <- numbers[seq(2, length(numbers), by = 2)]
      all_path_x <- c(all_path_x, x_coords)
      all_path_y <- c(all_path_y, y_coords)
    }
  }

  # Calculate bounds from actual path coordinates
  if (length(all_path_x) > 0 && length(all_path_y) > 0) {
    path_min_x <- min(all_path_x)
    path_max_x <- max(all_path_x)
    path_min_y <- min(all_path_y)
    path_max_y <- max(all_path_y)
  } else {
    # Fallback to center-based calculation
    all_x <- sapply(pieces, function(p) p$center[1])
    all_y <- sapply(pieces, function(p) p$center[2])
    path_min_x <- min(all_x) - piece_radius
    path_max_x <- max(all_x) + piece_radius
    path_min_y <- min(all_y) - piece_radius
    path_max_y <- max(all_y) + piece_radius
  }

  # Add a small margin for stroke width and visual padding
  stroke_margin <- piece_radius * 0.15
  min_x <- path_min_x - stroke_margin
  max_x <- path_max_x + stroke_margin
  min_y <- path_min_y - stroke_margin
  max_y <- path_max_y + stroke_margin

  canvas_width <- max_x - min_x
  canvas_height <- max_y - min_y

  # Calculate number of pieces
  num_pieces <- 3 * rings * (rings - 1) + 1

  return(list(
    pieces = pieces,
    canvas_size = c(canvas_width, canvas_height),
    canvas_offset = c(min_x, min_y),  # ViewBox offset for hexagonal
    type = "hexagonal",
    parameters = list(
      seed = seed,
      rings = rings,
      diameter = diameter,
      tabsize = tabsize,
      jitter = jitter,
      do_warp = do_warp,
      do_trunc = do_trunc,
      do_circular_border = do_circular_border,
      piece_radius = piece_radius,
      num_pieces = num_pieces
    )
  ))
}


#' Generate concentric ring puzzle pieces internally
#'
#' Creates pieces with constant radial height and trapezoidal shapes.
#'
#' @param seed Random seed
#' @param rings Number of rings
#' @param diameter Puzzle diameter in mm
#' @param tabsize Tab size percentage
#' @param jitter Jitter percentage
#' @param center_shape "hexagon" or "circle" for center piece
#' @param do_circular_border If TRUE, use arc commands for perfect circular boundary
#' @param boundary_facing Direction the circular arc faces: "outward" or "inward"
#' @return Piece generation result
#' @keywords internal
generate_concentric_pieces_internal <- function(seed, rings, diameter, tabsize, jitter,
                                                 center_shape = "hexagon",
                                                 do_circular_border = FALSE,
                                                 boundary_facing = "outward") {
  # Source concentric modules if needed
  if (!exists("generate_concentric_pieces")) {
    source("R/concentric_geometry.R")
    source("R/concentric_edge_generation.R")
  }

  # Generate pieces using concentric edge generation
  concentric_result <- generate_concentric_pieces(
    rings = rings,
    seed = seed,
    diameter = diameter,
    tabsize = tabsize,
    jitter = jitter,
    center_shape = center_shape,
    do_circular_border = do_circular_border,
    boundary_facing = boundary_facing
  )

  # Extract pieces from result
  concentric_pieces <- concentric_result$pieces

  # Convert to standardized piece format
  pieces <- lapply(seq_along(concentric_pieces), function(i) {
    cp <- concentric_pieces[[i]]

    list(
      id = sprintf("piece_%d", cp$id),
      path = cp$path,
      center = c(cp$center_x, cp$center_y),
      ring_pos = list(ring = cp$ring, position = cp$position),
      type = "concentric"
    )
  })

  # Calculate piece height for this configuration
  piece_height <- get_concentric_piece_height(diameter, rings)

  # Calculate canvas size from actual piece path bounds
  all_path_x <- c()
  all_path_y <- c()

  for (piece in pieces) {
    path <- piece$path
    numbers <- as.numeric(unlist(regmatches(path, gregexpr("-?[0-9]+\\.?[0-9]*", path))))
    numbers <- numbers[!is.na(numbers)]

    if (length(numbers) >= 2) {
      x_coords <- numbers[seq(1, length(numbers), by = 2)]
      y_coords <- numbers[seq(2, length(numbers), by = 2)]
      all_path_x <- c(all_path_x, x_coords)
      all_path_y <- c(all_path_y, y_coords)
    }
  }

  # Calculate bounds from actual path coordinates
  if (length(all_path_x) > 0 && length(all_path_y) > 0) {
    path_min_x <- min(all_path_x)
    path_max_x <- max(all_path_x)
    path_min_y <- min(all_path_y)
    path_max_y <- max(all_path_y)
  } else {
    # Fallback to diameter-based calculation
    path_min_x <- -diameter / 2
    path_max_x <- diameter / 2
    path_min_y <- -diameter / 2
    path_max_y <- diameter / 2
  }

  # Add a small margin for stroke width
  stroke_margin <- piece_height * 0.15
  min_x <- path_min_x - stroke_margin
  max_x <- path_max_x + stroke_margin
  min_y <- path_min_y - stroke_margin
  max_y <- path_max_y + stroke_margin

  canvas_width <- max_x - min_x
  canvas_height <- max_y - min_y

  # Calculate number of pieces
  num_pieces <- get_concentric_piece_count(rings)

  return(list(
    pieces = pieces,
    canvas_size = c(canvas_width, canvas_height),
    canvas_offset = c(min_x, min_y),
    type = "concentric",
    parameters = list(
      seed = seed,
      rings = rings,
      diameter = diameter,
      tabsize = tabsize,
      jitter = jitter,
      center_shape = center_shape,
      do_circular_border = do_circular_border,
      boundary_facing = boundary_facing,
      piece_height = piece_height,
      num_pieces = num_pieces
    )
  ))
}


#' Get piece count for puzzle configuration
#'
#' @param type "rectangular", "hexagonal", or "concentric"
#' @param grid For rectangular: c(rows, cols). For hexagonal/concentric: c(rings) or rings
#' @return Number of pieces
#' @export
get_piece_count <- function(type, grid) {
  if (type == "hexagonal" || type == "concentric") {
    rings <- if (length(grid) == 1) grid else grid[1]
    return(3 * rings * (rings - 1) + 1)
  } else {
    return(grid[1] * grid[2])
  }
}


#' Validate piece has closed path
#'
#' Checks that a piece path is properly closed (ends with Z command).
#'
#' @param piece Piece object with path
#' @return TRUE if valid, FALSE otherwise
#' @keywords internal
validate_piece_path <- function(piece) {
  if (is.null(piece$path)) return(FALSE)

  # Check starts with M (move) and ends with Z (close)
  path <- trimws(piece$path)
  starts_ok <- grepl("^M\\s", path)
  ends_ok <- grepl("Z\\s*$", path)

  return(starts_ok && ends_ok)
}


#' Validate all pieces in generation result
#'
#' @param result Output from generate_pieces_internal()
#' @return TRUE if all valid, stops with error otherwise
#' @export
validate_pieces <- function(result) {
  if (is.null(result$pieces) || length(result$pieces) == 0) {
    stop("No pieces generated")
  }

  for (i in seq_along(result$pieces)) {
    piece <- result$pieces[[i]]

    if (!validate_piece_path(piece)) {
      stop(sprintf("Piece %d has invalid path (must start with M and end with Z)", i))
    }

    if (is.null(piece$center) || length(piece$center) != 2) {
      stop(sprintf("Piece %d has invalid center coordinates", i))
    }
  }

  # Verify piece count
  expected_count <- get_piece_count(result$type,
    if (result$type == "hexagonal") result$parameters$rings else result$parameters$grid)

  if (length(result$pieces) != expected_count) {
    stop(sprintf("Expected %d pieces, got %d", expected_count, length(result$pieces)))
  }

  return(TRUE)
}
