# Stat layer for puzzle piece geometry computation
# Part of the jigsawR ggpuzzle extension

#' @include bezier_utils.R
NULL

#' Compute puzzle piece geometry from data
#'
#' This stat generates puzzle piece polygon coordinates from input data.
#' Each row in the input data is mapped to a puzzle piece, with recycling
#' if there are fewer data rows than puzzle pieces.
#'
#' @param puzzle_type Type of puzzle ("rectangular", "hexagonal", "concentric", "voronoi", "random")
#' @param rows Number of rows (rectangular only)
#' @param cols Number of columns (rectangular only)
#' @param rings Number of rings (hexagonal/concentric only)
#' @param n_cells Number of cells (voronoi only)
#' @param n_pieces Number of interior points (random only)
#' @param tabsize Tab size parameter
#' @param jitter Tab jitter parameter
#' @param seed Random seed for reproducibility
#' @param bezier_resolution Points per Bezier curve approximation
#' @param do_warp Apply circular warping (hexagonal only)
#' @param do_trunc Truncate edge pieces (hexagonal only)
#' @param do_circular_border Use perfect circular arc borders (hexagonal/concentric)
#' @param center_shape Center piece shape for concentric: "hexagon" or "circle"
#' @param point_distribution Point distribution for voronoi: "fermat", "uniform", "jittered"
#' @param n_corner Number of corners for base polygon (random only)
#' @export
StatPuzzle <- ggplot2::ggproto("StatPuzzle", ggplot2::Stat,

  # No required aesthetics - we generate x/y from puzzle geometry
  required_aes = character(),

  # Computed variables that will be available to the geom
  computed_vars = c("x", "y", "piece_id", "center_x", "center_y"),

  # Extra parameters that get passed to compute_panel
 default_aes = ggplot2::aes(x = ggplot2::after_stat(x), y = ggplot2::after_stat(y)),

  setup_params = function(data, params) {
    # Ensure we have defaults
    params$puzzle_type <- params$puzzle_type %||% "rectangular"
    params$rows <- params$rows %||% 3
    params$cols <- params$cols %||% 3
    params$rings <- params$rings %||% 3
    params$n_cells <- params$n_cells %||% 12
    params$n_pieces <- params$n_pieces %||% 12
    params$tabsize <- params$tabsize %||% 20
    params$jitter <- params$jitter %||% 4
    params$bezier_resolution <- params$bezier_resolution %||% 20
    # Hexagonal parameters
    params$do_warp <- params$do_warp %||% TRUE
    params$do_trunc <- params$do_trunc %||% TRUE
    params$do_circular_border <- params$do_circular_border %||% FALSE
    # Concentric parameters
    params$center_shape <- params$center_shape %||% "hexagon"
    params$boundary_facing <- params$boundary_facing %||% "outward"
    # Voronoi parameters
    params$point_distribution <- params$point_distribution %||% "fermat"
    # Random parameters
    params$n_corner <- params$n_corner %||% 4
    # Fusion parameters
    params$fusion_groups <- params$fusion_groups %||% NULL
    params$fusion_style <- params$fusion_style %||% "none"
    params$fusion_opacity <- params$fusion_opacity %||% 0.3
    params
  },

  compute_panel = function(data, scales, puzzle_type = "rectangular",
                           rows = 3, cols = 3, rings = 3,
                           n_cells = 12, n_pieces = 12,
                           tabsize = 20, jitter = 4, seed = NULL,
                           offset = 0,
                           bezier_resolution = 20,
                           do_warp = TRUE, do_trunc = TRUE, do_circular_border = FALSE,
                           center_shape = "hexagon",
                           boundary_facing = "outward",
                           point_distribution = "fermat",
                           n_corner = 4,
                           fusion_groups = NULL,
                           fusion_style = "none",
                           fusion_opacity = 0.3,
                           # Size parameters
                           width = 100, height = 100, diameter = 100,
                           # Tab constraints
                           min_tab_size = NULL, max_tab_size = NULL) {

    # Determine grid configuration and piece count
    # Note: size = c(height, width) to match grid = c(rows, cols)
    if (puzzle_type == "rectangular") {
      grid <- c(rows, cols)
      expected_pieces <- rows * cols
      size <- c(height, width)
    } else if (puzzle_type == "hexagonal") {
      grid <- c(rings)
      expected_pieces <- 3 * rings * (rings - 1) + 1
      size <- c(diameter)
    } else if (puzzle_type == "concentric") {
      grid <- c(rings)
      expected_pieces <- rings * 6 + 1  # Center + 6 per ring
      size <- c(diameter)
    } else if (puzzle_type == "voronoi") {
      grid <- c(n_cells)
      expected_pieces <- n_cells
      size <- c(height, width)
    } else if (puzzle_type == "random") {
      grid <- c(n_pieces)
      # Random puzzle piece count is approximate (depends on triangulation)
      expected_pieces <- n_pieces * 2  # Upper bound estimate
      size <- c(height, width)
    } else {
      stop("Unknown puzzle type: ", puzzle_type, call. = FALSE)
    }

    # Generate the puzzle with type-specific parameters
    result <- generate_puzzle(
      type = puzzle_type,
      grid = grid,
      size = size,
      seed = seed,
      tabsize = tabsize,
      jitter = jitter,
      offset = offset,
      save_files = FALSE,
      # Tab constraints
      min_tab_size = min_tab_size,
      max_tab_size = max_tab_size,
      # Hexagonal parameters
      do_warp = do_warp,
      do_trunc = do_trunc,
      do_circular_border = do_circular_border,
      # Concentric parameters
      center_shape = center_shape,
      boundary_facing = boundary_facing,
      # Voronoi parameters
      point_distribution = point_distribution,
      # Random parameters
      n_corner = n_corner,
      # Fusion parameters
      fusion_groups = fusion_groups,
      fusion_style = fusion_style,
      fusion_opacity = fusion_opacity
    )

    # Get actual piece count from result
    actual_pieces <- length(result$pieces)

    # Convert each piece path to polygon coordinates
    piece_data_list <- lapply(seq_along(result$pieces), function(i) {
      piece <- result$pieces[[i]]
      poly <- svg_path_to_polygon(piece$path, bezier_resolution)

      if (nrow(poly) == 0) {
        return(NULL)
      }

      poly$piece_id <- i
      poly$center_x <- piece$center[1]
      poly$center_y <- piece$center[2]

      # Add ring information for hex/concentric puzzles
      if (!is.null(piece$ring_pos)) {
        poly$ring <- piece$ring_pos$ring
        poly$is_center <- piece$ring_pos$ring == 0
      }

      # Add row/col for rectangular puzzles
      if (!is.null(piece$row_col)) {
        poly$row <- piece$row_col$row
        poly$col <- piece$row_col$col
      }

      # Add boundary flag if available
      if (!is.null(piece$is_boundary)) {
        poly$is_boundary <- piece$is_boundary
      }

      # Add group_id for fusion groups
      if (!is.null(piece$fusion_group) && !is.na(piece$fusion_group)) {
        poly$group_id <- piece$fusion_group
      } else {
        poly$group_id <- i
      }

      # Store edge data for fusion-aware rendering
      # Edge paths and fusion status are stored as list-columns
      # (replicated across all vertices of the piece for ggplot2 compatibility)
      edge_paths <- get_piece_edge_paths(piece)
      edge_names <- get_piece_edge_names(piece)
      fused_edges <- piece$fused_edges %||% list()

      # Store as list-column (same value for all rows of this piece)
      n_rows <- nrow(poly)
      poly$edge_paths <- rep(list(edge_paths), n_rows)
      poly$edge_names <- rep(list(edge_names), n_rows)
      poly$fused_edges <- rep(list(fused_edges), n_rows)

      poly
    })

    # Remove any NULL entries and combine
    piece_data_list <- piece_data_list[!sapply(piece_data_list, is.null)]
    pieces_df <- do.call(rbind, piece_data_list)

    if (is.null(pieces_df) || nrow(pieces_df) == 0) {
      return(data.frame())
    }

    # Map input data rows to pieces (1:1 with recycling)
    if (nrow(data) > 0) {
      # Create mapping from piece_id to data row index (with recycling)
      data_idx <- rep_len(seq_len(nrow(data)), actual_pieces)

      # Get unique piece IDs from geometry
      unique_pieces <- unique(pieces_df$piece_id)

      # For each unique piece, add the data attributes
      result_list <- lapply(unique_pieces, function(pid) {
        piece_geom <- pieces_df[pieces_df$piece_id == pid, , drop = FALSE]

        # Which data row does this piece use?
        data_row_idx <- data_idx[pid]

        if (!is.na(data_row_idx) && data_row_idx <= nrow(data)) {
          # Copy all columns from data except PANEL and group
          data_cols <- data[data_row_idx, , drop = FALSE]

          # Add data columns to geometry rows (replicate for all points in piece)
          for (col in names(data_cols)) {
            if (!(col %in% names(piece_geom))) {
              piece_geom[[col]] <- rep(data_cols[[col]], nrow(piece_geom))
            }
          }
        }

        piece_geom
      })

      pieces_df <- do.call(rbind, result_list)
    }

    # Ensure PANEL column exists
    if (!"PANEL" %in% names(pieces_df)) {
      pieces_df$PANEL <- 1L
    }

    # Ensure group column exists for proper polygon grouping
    if (!"group" %in% names(pieces_df)) {
      pieces_df$group <- pieces_df$piece_id
    }

    pieces_df
  }
)

# Null-coalescing operator if not already defined
`%||%` <- function(a, b) if (is.null(a)) b else a
