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
#' @param puzzle_type Type of puzzle ("rectangular", "hexagonal", "concentric")
#' @param rows Number of rows (rectangular only)
#' @param cols Number of columns (rectangular only)
#' @param rings Number of rings (hexagonal/concentric only)
#' @param tabsize Tab size parameter
#' @param jitter Tab jitter parameter
#' @param seed Random seed for reproducibility
#' @param bezier_resolution Points per Bezier curve approximation
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
    params$tabsize <- params$tabsize %||% 20
    params$jitter <- params$jitter %||% 4
    params$bezier_resolution <- params$bezier_resolution %||% 20
    params
  },

  compute_panel = function(data, scales, puzzle_type = "rectangular",
                           rows = 3, cols = 3, rings = 3,
                           tabsize = 20, jitter = 4, seed = NULL,
                           bezier_resolution = 20) {

    # Determine grid configuration and piece count
    if (puzzle_type == "rectangular") {
      grid <- c(rows, cols)
      n_pieces <- rows * cols
      size <- c(100, 100)  # Normalized coordinates
    } else if (puzzle_type == "hexagonal") {
      grid <- c(rings)
      n_pieces <- 3 * rings * (rings - 1) + 1
      size <- c(100)  # Diameter
    } else if (puzzle_type == "concentric") {
      grid <- c(rings)
      n_pieces <- rings * 6 + 1  # Center + 6 per ring
      size <- c(100)
    } else {
      stop("Unknown puzzle type: ", puzzle_type, call. = FALSE)
    }

    # Generate the puzzle
    result <- generate_puzzle(
      type = puzzle_type,
      grid = grid,
      size = size,
      seed = seed,
      tabsize = tabsize,
      jitter = jitter,
      offset = 0,
      save_files = FALSE
    )

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
      data_idx <- rep_len(seq_len(nrow(data)), n_pieces)

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
