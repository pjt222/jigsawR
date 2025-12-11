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
#' @param fusion_groups List of piece ID vectors to fuse together (optional)
#' @param fusion_style Style for fused internal edges: "none" (invisible), "dashed", "solid"
#' @param fusion_opacity Opacity for fused edges when style != "none" (0.0 to 1.0)
#' @return List with:
#'   - pieces: List of piece objects with id, path, center, grid_pos/ring_pos
#'   - canvas_size: c(width, height) for compact (offset=0) layout
#'   - type: "rectangular", "hexagonal", or "concentric"
#'   - parameters: Generation parameters used
#'   - fusion_data: Fusion edge data (if fusion_groups provided)
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
                                     boundary_facing = "outward",
                                     fusion_groups = NULL,
                                     fusion_style = "none",
                                     fusion_opacity = 0.3) {

  # Generate seed if not provided
  if (is.null(seed)) {
    seed <- as.integer(runif(1) * 10000)
  }

  # Dispatch to type-specific implementation
  if (type == "concentric") {
    result <- generate_concentric_pieces_internal(
      seed = seed,
      rings = if (length(grid) == 1) grid else grid[1],
      diameter = if (length(size) == 1) size else size[1],
      tabsize = tabsize,
      jitter = jitter,
      center_shape = center_shape,
      do_circular_border = do_circular_border,
      boundary_facing = boundary_facing,
      fusion_groups = fusion_groups,
      fusion_style = fusion_style,
      fusion_opacity = fusion_opacity
    )
  } else if (type == "hexagonal") {
    result <- generate_hex_pieces_internal(
      seed = seed,
      rings = if (length(grid) == 1) grid else grid[1],
      diameter = if (length(size) == 1) size else size[1],
      tabsize = tabsize,
      jitter = jitter,
      do_warp = do_warp,
      do_trunc = do_trunc,
      do_circular_border = do_circular_border,
      fusion_groups = fusion_groups,
      fusion_style = fusion_style,
      fusion_opacity = fusion_opacity
    )
  } else {
    result <- generate_rect_pieces_internal(
      seed = seed,
      grid = grid,
      size = size,
      tabsize = tabsize,
      jitter = jitter,
      fusion_groups = fusion_groups,
      fusion_style = fusion_style,
      fusion_opacity = fusion_opacity
    )
  }

  return(result)
}


#' Generate rectangular puzzle pieces internally
#'
#' @param seed Random seed
#' @param grid c(rows, cols)
#' @param size c(width, height) in mm
#' @param tabsize Tab size percentage
#' @param jitter Jitter percentage
#' @param fusion_groups List of piece ID vectors to fuse (optional)
#' @param fusion_style Style for fused edges: "none", "dashed", "solid"
#' @param fusion_opacity Opacity for fused edges (0.0 to 1.0)
#' @return Piece generation result
#' @keywords internal
generate_rect_pieces_internal <- function(seed, grid, size, tabsize, jitter,
                                          fusion_groups = NULL,
                                          fusion_style = "none",
                                          fusion_opacity = 0.3) {

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

  # Compute fused edges if fusion groups provided
  # Note: We need a minimal puzzle_result structure for compute_fused_edges
  fused_edge_data <- NULL
  if (!is.null(fusion_groups) && length(fusion_groups) > 0) {
    # Create minimal structure for adjacency API
    temp_result <- list(
      type = "rectangular",
      parameters = list(grid = grid)
    )
    fused_edge_data <- compute_fused_edges(fusion_groups, temp_result)
  }

  # Generate all pieces
  pieces <- list()
  piece_idx <- 1

  for (yi in 0:(yn - 1)) {
    for (xi in 0:(xn - 1)) {
      # Always generate full bezier path (no fusion modification at path level)
      piece_path <- generate_single_piece(xi, yi, puzzle_structure)

      # Determine which edges are fused (for render-time styling)
      fused_edges <- list(N = FALSE, E = FALSE, S = FALSE, W = FALSE)
      # Store neighbor IDs for fused edges (for deduplication in renderer)
      fused_neighbor_ids <- list()

      if (!is.null(fused_edge_data)) {
        # Check each direction and store neighbor ID if fused
        for (dir in c("N", "E", "S", "W")) {
          is_fused <- is_edge_fused(piece_idx, dir, fused_edge_data)
          fused_edges[[dir]] <- is_fused

          if (is_fused) {
            # Calculate neighbor index based on direction
            neighbor_idx <- switch(dir,
              "N" = if (yi > 0) piece_idx - xn else NA,
              "E" = if (xi < xn - 1) piece_idx + 1 else NA,
              "S" = if (yi < yn - 1) piece_idx + xn else NA,
              "W" = if (xi > 0) piece_idx - 1 else NA
            )
            if (!is.na(neighbor_idx)) {
              fused_neighbor_ids[[dir]] <- neighbor_idx
            }
          }
        }
      }

      # Calculate piece center
      center_x <- (xi + 0.5) * piece_width
      center_y <- (yi + 0.5) * piece_height

      # Determine fusion group for this piece
      fusion_group <- NA
      if (!is.null(fused_edge_data)) {
        fusion_group <- get_piece_fusion_group(piece_idx, fused_edge_data)
      }

      # Create standardized piece object
      pieces[[piece_idx]] <- list(
        id = piece_idx,  # Store as integer for deduplication
        path = piece_path,
        center = c(center_x, center_y),
        grid_pos = c(xi = xi, yi = yi),
        type = "rectangular",
        fusion_group = fusion_group,  # NA if not in any group
        fused_edges = fused_edges,    # list(N, E, S, W) - TRUE if edge is fused
        fused_neighbor_ids = fused_neighbor_ids  # Neighbor IDs for deduplication
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
      piece_height = piece_height,
      fusion_groups = fusion_groups,
      fusion_style = fusion_style,
      fusion_opacity = fusion_opacity
    ),
    fusion_data = fused_edge_data,
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
#' @param fusion_groups List of piece ID vectors to fuse (optional)
#' @param fusion_style Style for fused edges: "none", "dashed", "solid"
#' @param fusion_opacity Opacity for fused edges (0.0 to 1.0)
#' @return Piece generation result
#' @keywords internal
generate_hex_pieces_internal <- function(seed, rings, diameter, tabsize, jitter,
                                         do_warp = FALSE, do_trunc = FALSE,
                                         do_circular_border = FALSE,
                                         fusion_groups = NULL,
                                         fusion_style = "none",
                                         fusion_opacity = 0.3) {
  # Calculate total pieces for progress reporting
  num_pieces <- 3 * rings * (rings - 1) + 1

  # Use existing edge map generation
  # This generates all pieces with proper complementary edges
  cli::cli_progress_step("Generating {num_pieces} hexagonal pieces...")
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

  # Compute fused edges if fusion groups provided
  fused_edge_data <- NULL
  if (!is.null(fusion_groups) && length(fusion_groups) > 0) {
    n_groups <- length(fusion_groups)
    cli::cli_progress_step("Computing fused edges for {n_groups} fusion group{?s}...")
    # Use optimized fast version with hash sets and cached adjacency matrix
    fused_edge_data <- compute_hex_fused_edges_fast(fusion_groups, rings)
  }

  # Convert to standardized piece format
  cli::cli_progress_step("Processing piece geometry ({num_pieces} pieces)...")
  pieces <- lapply(seq_along(hex_pieces), function(i) {
    hp <- hex_pieces[[i]]
    piece_id <- hp$id

    # Determine which edges are fused (for render-time styling)
    # Hexagonal pieces have 6 sides numbered 0-5
    # IMPORTANT: fused_edges keys must be GEOMETRIC side numbers (0-5)
    # which correspond to edge path segments in order.
    # The topology side (from get_hex_neighbor) may differ from geometric side!
    fused_edges <- list(`0` = FALSE, `1` = FALSE, `2` = FALSE,
                        `3` = FALSE, `4` = FALSE, `5` = FALSE)
    # Also store neighbor IDs for fused edges (for deduplication in renderer)
    fused_neighbor_ids <- list()

    if (!is.null(fused_edge_data)) {
      # Get this piece's center coordinates for direction calculations
      piece_cx <- hp$center_x
      piece_cy <- hp$center_y

      # Get neighbors using the SAME function that compute_hex_fused_edges_fast uses
      # This ensures topo_side numbers match edge keys in fused_edge_data
      neighbors <- get_hex_neighbors_for_fusion(piece_id, rings)

      # First, compute which geometric side each topology side maps to
      # by checking the actual direction to each neighbor
      topo_to_geo_map <- list()
      topo_to_neighbor_map <- list()

      for (i in seq_len(nrow(neighbors))) {
        topo_side <- as.integer(neighbors$direction[i])
        neighbor_id <- neighbors$neighbor_id[i]
        if (is.na(neighbor_id)) next

        topo_to_neighbor_map[[as.character(topo_side)]] <- neighbor_id

        # Get neighbor center
        neighbor_hp <- hex_pieces[[neighbor_id]]
        neighbor_cx <- neighbor_hp$center_x
        neighbor_cy <- neighbor_hp$center_y

        # Direction from this piece to neighbor
        dir_to_neighbor <- atan2(neighbor_cy - piece_cy, neighbor_cx - piece_cx) * 180 / pi

        # Find which geometric side (0-5) faces this direction
        # For pointed-top hexagons (SVG): Side 0 → 30°, Side 1 → -30°, etc.
        # Formula: geo_side = round((30 - dir) / 60) %% 6
        # This is derived from: expected_dir = 30 - side * 60
        geo_side <- round((30 - dir_to_neighbor) / 60) %% 6
        topo_to_geo_map[[as.character(topo_side)]] <- geo_side
      }

      # Now populate fused_edges using geometric side keys
      for (topo_side in as.character(0:5)) {
        is_fused <- is_edge_fused(piece_id, topo_side, fused_edge_data)

        if (is_fused) {
          # Get geometric side for this topology side
          geo_side <- topo_to_geo_map[[topo_side]]
          if (!is.null(geo_side)) {
            geo_key <- as.character(geo_side)
            fused_edges[[geo_key]] <- TRUE

            # Get neighbor ID for deduplication
            neighbor_id <- topo_to_neighbor_map[[topo_side]]
            if (!is.null(neighbor_id) && !is.na(neighbor_id)) {
              fused_neighbor_ids[[geo_key]] <- neighbor_id
            }
          }
        }
      }
    }

    # Determine fusion group for this piece
    fusion_group <- NA
    if (!is.null(fused_edge_data)) {
      fusion_group <- get_piece_fusion_group(piece_id, fused_edge_data)
    }

    list(
      id = piece_id,  # Store as integer for deduplication
      path = hp$path,
      center = c(hp$center_x, hp$center_y),
      ring_pos = list(ring = hp$ring, position = hp$position_in_ring),
      type = "hexagonal",
      fusion_group = fusion_group,
      fused_edges = fused_edges,
      fused_neighbor_ids = fused_neighbor_ids
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
      num_pieces = num_pieces,
      fusion_groups = fusion_groups,
      fusion_style = fusion_style,
      fusion_opacity = fusion_opacity
    ),
    fusion_data = fused_edge_data
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
#' @param fusion_groups List of piece ID vectors to fuse (optional)
#' @param fusion_style Style for fused edges: "none", "dashed", "solid"
#' @param fusion_opacity Opacity for fused edges (0.0 to 1.0)
#' @return Piece generation result
#' @keywords internal
generate_concentric_pieces_internal <- function(seed, rings, diameter, tabsize, jitter,
                                                 center_shape = "hexagon",
                                                 do_circular_border = FALSE,
                                                 boundary_facing = "outward",
                                                 fusion_groups = NULL,
                                                 fusion_style = "none",
                                                 fusion_opacity = 0.3) {
  # Calculate total pieces for progress reporting
  num_pieces <- 3 * rings * (rings - 1) + 1

  # Source concentric modules if needed
  if (!exists("generate_concentric_pieces")) {
    source("R/concentric_geometry.R")
    source("R/concentric_edge_generation.R")
  }

  # Generate pieces using concentric edge generation
  cli::cli_progress_step("Generating {num_pieces} concentric pieces...")
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

  # Compute fused edges if fusion groups provided
  fused_edge_data <- NULL
  if (!is.null(fusion_groups) && length(fusion_groups) > 0) {
    n_groups <- length(fusion_groups)
    cli::cli_progress_step("Computing fused edges for {n_groups} fusion group{?s}...")
    # Create minimal structure for adjacency API
    temp_result <- list(
      type = "concentric",
      parameters = list(rings = rings)
    )
    fused_edge_data <- compute_concentric_fused_edges(fusion_groups, temp_result)
  }

  # Convert to standardized piece format
  cli::cli_progress_step("Processing piece geometry ({num_pieces} pieces)...")
  pieces <- lapply(seq_along(concentric_pieces), function(i) {
    cp <- concentric_pieces[[i]]
    piece_id <- cp$id

    # Determine which edges are fused (for render-time styling)
    # Center piece (ring 0) has 6 edges; trapezoid pieces have 4 edges
    # Also store neighbor IDs for fused edges (for deduplication in renderer)
    fused_neighbor_ids <- list()

    if (cp$ring == 0) {
      # Center piece: edges 1-6
      fused_edges <- list(`1` = FALSE, `2` = FALSE, `3` = FALSE,
                          `4` = FALSE, `5` = FALSE, `6` = FALSE)
      if (!is.null(fused_edge_data)) {
        for (edge in as.character(1:6)) {
          is_fused <- is_edge_fused(piece_id, edge, fused_edge_data)
          fused_edges[[edge]] <- is_fused

          if (is_fused) {
            # Get neighbor (ring 1 piece connected to this edge)
            neighbor_info <- get_concentric_neighbor(piece_id, as.integer(edge), rings)
            if (!neighbor_info$is_boundary && !is.na(neighbor_info$neighbor_id)) {
              fused_neighbor_ids[[edge]] <- neighbor_info$neighbor_id
            }
          }
        }
      }
    } else {
      # Trapezoid piece: INNER, RIGHT, OUTER, LEFT
      fused_edges <- list(INNER = FALSE, RIGHT = FALSE, OUTER = FALSE, LEFT = FALSE)
      if (!is.null(fused_edge_data)) {
        for (edge_name in c("INNER", "RIGHT", "OUTER", "LEFT")) {
          edge_idx <- switch(edge_name,
            "INNER" = 1, "RIGHT" = 2, "OUTER" = 3, "LEFT" = 4
          )
          is_fused <- is_edge_fused(piece_id, edge_name, fused_edge_data)
          fused_edges[[edge_name]] <- is_fused

          if (is_fused) {
            neighbor_info <- get_concentric_neighbor(piece_id, edge_idx, rings)
            if (!neighbor_info$is_boundary && !is.na(neighbor_info$neighbor_id)) {
              fused_neighbor_ids[[edge_name]] <- neighbor_info$neighbor_id
            }
          }
        }
      }
    }

    # Determine fusion group for this piece
    fusion_group <- NA
    if (!is.null(fused_edge_data)) {
      fusion_group <- get_piece_fusion_group(piece_id, fused_edge_data)
    }

    list(
      id = piece_id,  # Store as integer for deduplication
      path = cp$path,
      center = c(cp$center_x, cp$center_y),
      ring_pos = list(ring = cp$ring, position = cp$position),
      type = "concentric",
      fusion_group = fusion_group,
      fused_edges = fused_edges,
      fused_neighbor_ids = fused_neighbor_ids
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
      num_pieces = num_pieces,
      fusion_groups = fusion_groups,
      fusion_style = fusion_style,
      fusion_opacity = fusion_opacity
    ),
    fusion_data = fused_edge_data
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

# =============================================================================
# FUSION APPLICATION (Post-generation)
# =============================================================================

#' Apply fusion groups to generated pieces
#'
#' Applies fusion styling to pieces after they have been generated.
#' This is called after keyword resolution when we have full puzzle context.
#'
#' @param pieces_result Result from generate_pieces_internal()
#' @param fusion_groups List of integer vectors (resolved piece IDs)
#' @param puzzle_result Puzzle result structure for adjacency lookup
#' @return Updated pieces_result with fusion data applied
#' @export
apply_fusion_to_pieces <- function(pieces_result, fusion_groups, puzzle_result) {
  if (is.null(fusion_groups) || length(fusion_groups) == 0) {
    return(pieces_result)
  }

  type <- pieces_result$type
  pieces <- pieces_result$pieces

  # Compute fused edges using the adjacency API
  fused_edge_data <- compute_fused_edges(fusion_groups, puzzle_result)

  if (is.null(fused_edge_data) || length(fused_edge_data$fused_edges) == 0) {
    return(pieces_result)
  }

  # Get edge names based on puzzle type
  edge_names <- switch(type,
    "rectangular" = c("N", "E", "S", "W"),
    "hexagonal" = as.character(0:5),
    "concentric" = c("INNER", "RIGHT", "OUTER", "LEFT"),
    c("N", "E", "S", "W")
  )

  # Apply fused edge markers to each piece
  for (i in seq_along(pieces)) {
    piece <- pieces[[i]]
    piece_id <- piece$id %||% i

    # Initialize fused_edges if not present
    if (is.null(piece$fused_edges)) {
      piece$fused_edges <- setNames(as.list(rep(FALSE, length(edge_names))), edge_names)
    }
    if (is.null(piece$fused_neighbor_ids)) {
      piece$fused_neighbor_ids <- list()
    }

    # Check each edge for fusion
    neighbors <- get_piece_neighbors(piece_id, puzzle_result, include_boundary = FALSE)

    # For hexagonal puzzles, we need to map topology side to geometric side
    # because the path segments are ordered by geometric side (0-5 starting from East)
    # but neighbor detection returns topology sides
    if (type == "hexagonal") {
      # Get piece center for direction calculations
      piece_cx <- piece$center[1]
      piece_cy <- piece$center[2]

      # Build topology-to-geometry mapping
      topo_to_geo_map <- list()
      for (j in seq_len(nrow(neighbors))) {
        topo_side <- neighbors$direction[j]
        neighbor_id <- neighbors$neighbor_id[j]
        if (is.na(neighbor_id)) next

        # Get neighbor center
        neighbor_piece <- pieces[[neighbor_id]]
        if (is.null(neighbor_piece)) next
        neighbor_cx <- neighbor_piece$center[1]
        neighbor_cy <- neighbor_piece$center[2]

        # Direction from this piece to neighbor
        dir_to_neighbor <- atan2(neighbor_cy - piece_cy, neighbor_cx - piece_cx) * 180 / pi

        # Find which geometric side (0-5) faces this direction
        # For pointed-top hexagons (SVG): Side 0 → 30°, Side 1 → -30°, etc.
        # Formula: geo_side = round((30 - dir) / 60) %% 6
        # This is derived from: expected_dir = 30 - side * 60
        geo_side <- as.character(round((30 - dir_to_neighbor) / 60) %% 6)
        topo_to_geo_map[[topo_side]] <- geo_side
      }

      # Apply fusion using geometric side keys
      for (j in seq_len(nrow(neighbors))) {
        topo_dir <- neighbors$direction[j]
        neighbor_id <- neighbors$neighbor_id[j]

        if (is_edge_fused(piece_id, topo_dir, fused_edge_data)) {
          geo_dir <- topo_to_geo_map[[topo_dir]]
          if (!is.null(geo_dir)) {
            piece$fused_edges[[geo_dir]] <- TRUE
            if (!is.na(neighbor_id)) {
              piece$fused_neighbor_ids[[geo_dir]] <- neighbor_id
            }
          }
        }
      }
    } else if (type == "concentric") {
      # For concentric puzzles, handle many-to-one OUTER edge relationships
      rings <- puzzle_result$parameters$grid[1] %||% puzzle_result$parameters$rings
      diameter <- puzzle_result$parameters$size[1] %||% puzzle_result$parameters$diameter

      # Calculate and store radius data for this piece (needed for segment rendering)
      if (!is.null(rings) && !is.null(diameter) && !is.null(piece$ring_pos)) {
        piece_height <- get_concentric_piece_height(diameter, rings)
        ring <- piece$ring_pos$ring
        piece$inner_radius <- ring * piece_height
        piece$outer_radius <- (ring + 1) * piece_height
      }

      for (j in seq_len(nrow(neighbors))) {
        dir <- neighbors$direction[j]
        neighbor_id <- neighbors$neighbor_id[j]

        if (is_edge_fused(piece_id, dir, fused_edge_data)) {
          piece$fused_edges[[dir]] <- TRUE
          if (!is.na(neighbor_id)) {
            piece$fused_neighbor_ids[[dir]] <- neighbor_id
          }
        }
      }

      # Compute segment-level fusion for OUTER edge (many-to-one relationships)
      # This allows rendering different styles for each segment based on neighbor fusion
      if (!is.null(rings) && !is.null(piece$ring_pos) && piece$ring_pos$ring > 0) {
        outer_segments <- get_outer_edge_segments(piece_id, rings)

        if (length(outer_segments) > 1) {
          # Many-to-one relationship exists - compute per-segment fusion status
          piece_group <- fused_edge_data$piece_to_group[[as.character(piece_id)]]
          any_fused <- FALSE
          all_fused <- TRUE

          for (seg_idx in seq_along(outer_segments)) {
            neighbor_id <- outer_segments[[seg_idx]]$neighbor_id
            neighbor_group <- fused_edge_data$piece_to_group[[as.character(neighbor_id)]]

            is_fused <- !is.null(piece_group) &&
                       !is.null(neighbor_group) &&
                       piece_group == neighbor_group

            outer_segments[[seg_idx]]$fused <- is_fused
            if (is_fused) any_fused <- TRUE
            if (!is_fused) all_fused <- FALSE
          }

          # Store segment-level fusion data
          piece$fused_edge_segments <- list(OUTER = outer_segments)

          # Update overall OUTER fusion flag (TRUE if ANY segment is fused)
          piece$fused_edges[["OUTER"]] <- any_fused

          # Store whether segments have mixed fusion status
          piece$outer_segments_mixed <- any_fused && !all_fused
        } else if (length(outer_segments) == 1) {
          # Single outer neighbor - use existing logic
          outer_neighbor_id <- outer_segments[[1]]$neighbor_id
          piece_group <- fused_edge_data$piece_to_group[[as.character(piece_id)]]
          outer_group <- fused_edge_data$piece_to_group[[as.character(outer_neighbor_id)]]
          if (!is.null(piece_group) && !is.null(outer_group) && piece_group == outer_group) {
            piece$fused_edges[["OUTER"]] <- TRUE
            piece$fused_neighbor_ids[["OUTER"]] <- outer_neighbor_id
          }
        }
      }
    } else {
      # For rectangular puzzles, topology = geometry
      for (j in seq_len(nrow(neighbors))) {
        dir <- neighbors$direction[j]
        neighbor_id <- neighbors$neighbor_id[j]

        if (is_edge_fused(piece_id, dir, fused_edge_data)) {
          piece$fused_edges[[dir]] <- TRUE
          if (!is.na(neighbor_id)) {
            piece$fused_neighbor_ids[[dir]] <- neighbor_id
          }
        }
      }
    }

    pieces[[i]] <- piece
  }

  # Update pieces_result
  pieces_result$pieces <- pieces
  pieces_result$fusion_data <- fused_edge_data
  pieces_result$parameters$fusion_groups <- fusion_groups

  return(pieces_result)
}
