# Geom layer for puzzle piece rendering
# Part of the jigsawR ggpuzzle extension

#' @include stat_puzzle.R
#' @import ggplot2
#' @import grid
NULL

# Helper function to ensure data has at least one row
# This is needed because our stat generates its own geometry
# and ggplot2 skips compute_panel when data has 0 rows
ensure_puzzle_data <- function(data) {
  # If user provides data explicitly, use it
  if (!is.null(data) && !inherits(data, "waiver")) {
    return(data)
  }


  # Return a function that ensures we have data
  # This function receives the plot's data and returns layer data
  function(plot_data) {
    if (is.data.frame(plot_data) && nrow(plot_data) > 0) {
      # Inherit from plot data
      plot_data
    } else {
      # No data available - create minimal dummy row
      # This ensures compute_panel is called
      data.frame(.dummy = 1L)
    }
  }
}

#' Puzzle piece geom
#'
#' Renders puzzle pieces as filled polygons. Works with StatPuzzle to
#' generate piece geometry from data.
#'
#' @export
GeomPuzzle <- ggplot2::ggproto("GeomPuzzle", ggplot2::Geom,

  required_aes = c("x", "y"),

  default_aes = ggplot2::aes(
    fill = "grey80",
    colour = "black",
    linewidth = 0.5,
    linetype = 1,
    alpha = NA
  ),

  # Extra params passed to draw_panel (beyond na.rm)
  extra_params = c("na.rm", "show_labels", "label_color", "label_size",
                   "fusion_style", "fusion_opacity", "bezier_resolution"),

  # Use polygon key for legend
  draw_key = ggplot2::draw_key_polygon,

  draw_panel = function(data, panel_params, coord,
                        show_labels = FALSE, label_color = "black", label_size = NULL,
                        fusion_style = "none", fusion_opacity = 0.3,
                        bezier_resolution = 20) {
    # Handle empty data
    if (nrow(data) == 0) {
      return(grid::nullGrob())
    }

    # Transform coordinates to panel coordinates
    coords <- coord$transform(data, panel_params)

    # Ensure we have piece_id for grouping
    if (!"piece_id" %in% names(coords)) {
      if ("group" %in% names(coords)) {
        coords$piece_id <- coords$group
      } else {
        coords$piece_id <- 1L
      }
    }

    # Split by piece_id and create grobs
    pieces <- split(coords, coords$piece_id)

    # Calculate auto label size if needed (based on average piece size)
    if (show_labels && is.null(label_size)) {
      # Estimate piece size from bounding box of first piece
      first_piece <- pieces[[1]]
      piece_width <- diff(range(first_piece$x, na.rm = TRUE))
      piece_height <- diff(range(first_piece$y, na.rm = TRUE))
      # Use 20% of smaller dimension, scaled to points
      label_size <- min(piece_width, piece_height) * 0.2 * 72  # Convert to points
      label_size <- max(6, min(label_size, 14))  # Clamp to reasonable range
    }

    # Check if we have edge data for fusion-aware rendering
    has_edge_data <- "edge_paths" %in% names(coords) &&
                     !is.null(coords$edge_paths[[1]]) &&
                     length(coords$edge_paths[[1]]) > 0

    # Determine if we need fusion-aware rendering
    # Only use complex rendering if we have fusion groups with non-"none" style
    use_fusion_rendering <- has_edge_data && fusion_style != "none"

    grobs <- lapply(names(pieces), function(pid) {
      piece <- pieces[[pid]]
      # Get first row for aesthetics (all rows in piece should have same aes)
      first <- piece[1, , drop = FALSE]

      # Handle alpha - NA means fully opaque
      fill_color <- first$fill
      if (!is.na(first$alpha) && first$alpha < 1) {
        fill_color <- scales::alpha(first$fill, first$alpha)
      }

      piece_grobs <- list()

      if (use_fusion_rendering) {
        # Fusion-aware rendering: fill polygon + separate edge strokes

        # Pass 1: Fill polygon (no stroke)
        fill_grob <- grid::polygonGrob(
          x = piece$x,
          y = piece$y,
          gp = grid::gpar(
            col = NA,  # No stroke on fill
            fill = fill_color
          ),
          default.units = "native"
        )
        piece_grobs <- c(piece_grobs, list(fill_grob))

        # Get edge data from first row (same for all rows in piece)
        edge_paths <- first$edge_paths[[1]]
        edge_names <- first$edge_names[[1]]
        fused_edges <- first$fused_edges[[1]]

        if (length(edge_paths) > 0 && length(edge_names) > 0) {
          # Get coordinate transformation info for edge paths
          # We need to transform SVG coordinates to panel coordinates
          # Use the bounding box of original vs transformed to compute scale/offset
          orig_x_range <- range(data$x[data$piece_id == as.integer(pid)], na.rm = TRUE)
          orig_y_range <- range(data$y[data$piece_id == as.integer(pid)], na.rm = TRUE)
          trans_x_range <- range(piece$x, na.rm = TRUE)
          trans_y_range <- range(piece$y, na.rm = TRUE)

          # Scale factors
          x_scale <- diff(trans_x_range) / max(diff(orig_x_range), 1e-10)
          y_scale <- diff(trans_y_range) / max(diff(orig_y_range), 1e-10)
          x_offset <- trans_x_range[1] - orig_x_range[1] * x_scale
          y_offset <- trans_y_range[1] - orig_y_range[1] * y_scale

          # Pass 2 & 3: Render edges with appropriate styling
          for (edge_name in edge_names) {
            edge_path <- edge_paths[[edge_name]]
            if (is.null(edge_path) || !nzchar(edge_path)) next

            is_fused <- isTRUE(fused_edges[[edge_name]])

            # Convert edge SVG path to polygon coordinates
            edge_coords <- svg_path_to_polygon(edge_path, bezier_resolution)
            if (nrow(edge_coords) == 0) next

            # Transform coordinates
            edge_x <- edge_coords$x * x_scale + x_offset
            edge_y <- edge_coords$y * y_scale + y_offset

            # Determine edge styling
            if (is_fused) {
              # Fused edge: use fusion_style and fusion_opacity
              edge_col <- scales::alpha(first$colour, fusion_opacity)
              edge_lty <- if (fusion_style == "dashed") 2 else 1  # 2 = dashed
            } else {
              # Normal edge: use regular stroke
              edge_col <- first$colour
              edge_lty <- first$linetype
            }

            # Create polyline grob for this edge
            edge_grob <- grid::polylineGrob(
              x = edge_x,
              y = edge_y,
              gp = grid::gpar(
                col = edge_col,
                lwd = first$linewidth * ggplot2::.pt,
                lty = edge_lty,
                lineend = "round",
                linejoin = "round"
              ),
              default.units = "native"
            )
            piece_grobs <- c(piece_grobs, list(edge_grob))
          }
        }
      } else {
        # Standard rendering: single polygon with stroke
        poly_grob <- grid::polygonGrob(
          x = piece$x,
          y = piece$y,
          gp = grid::gpar(
            col = first$colour,
            fill = fill_color,
            lwd = first$linewidth * ggplot2::.pt,
            lty = first$linetype
          ),
          default.units = "native"
        )
        piece_grobs <- c(piece_grobs, list(poly_grob))
      }

      # Add label if requested
      if (show_labels) {
        # Calculate centroid (mean of transformed coordinates)
        label_x <- mean(piece$x, na.rm = TRUE)
        label_y <- mean(piece$y, na.rm = TRUE)

        text_grob <- grid::textGrob(
          label = pid,
          x = label_x,
          y = label_y,
          gp = grid::gpar(
            col = label_color,
            fontsize = label_size,
            fontface = "bold"
          ),
          default.units = "native"
        )
        piece_grobs <- c(piece_grobs, list(text_grob))
      }

      # Combine all grobs for this piece
      do.call(grid::grobTree, piece_grobs)
    })

    # Combine all piece grobs
    do.call(grid::grobTree, grobs)
  }
)


#' Add rectangular puzzle layer to a ggplot
#'
#' Creates a puzzle visualization where each puzzle piece represents
#' a data point. The fill aesthetic is typically mapped to a variable
#' to color-code the pieces.
#'
#' @param mapping Set of aesthetic mappings. Usually includes `fill`.
#' @param data The data to visualize. Each row maps to a puzzle piece.
#' @param stat The statistical transformation to use (default: "puzzle").
#' @param position Position adjustment (default: "identity").
#' @param rows Number of rows in the puzzle grid.
#' @param cols Number of columns in the puzzle grid.
#' @param tabsize Size of the puzzle tabs (default: 20).
#' @param jitter Random variation in tab positions (default: 4).
#' @param seed Random seed for reproducible puzzle shapes.
#' @param bezier_resolution Points per Bezier curve (default: 20).
#' @param fusion_groups Piece fusion specification: PILES notation string (e.g., "1-2-3,4-5"),
#'   list of integer vectors, or NULL for no fusion.
#' @param fusion_style Style for fused internal edges: "none" (invisible), "dashed", "solid".
#' @param fusion_opacity Opacity for fused edges when style != "none" (0.0 to 1.0).
#' @param show_labels Logical; if TRUE, display piece ID labels at piece centers (default: FALSE).
#' @param label_color Color for piece labels (default: "black").
#' @param label_size Font size for labels in points. NULL for auto-sizing based on piece dimensions.
#' @param na.rm Remove NA values? (default: FALSE).
#' @param show.legend Include this layer in the legend? (default: NA).
#' @param inherit.aes Inherit aesthetics from the plot? (default: TRUE).
#' @param ... Other arguments passed to layer().
#'
#' @return A ggplot2 layer.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # Basic rectangular puzzle
#' df <- data.frame(value = 1:9)
#' ggplot(df, aes(fill = value)) +
#'   geom_puzzle_rect(rows = 3, cols = 3, seed = 42) +
#'   scale_fill_viridis_c() +
#'   theme_void()
#'
#' # With piece labels
#' ggplot(df, aes(fill = value)) +
#'   geom_puzzle_rect(rows = 3, cols = 3, seed = 42, show_labels = TRUE) +
#'   scale_fill_viridis_c() +
#'   theme_void()
#'
#' # With fusion groups
#' df <- data.frame(value = 1:9)
#' ggplot(df, aes(fill = value)) +
#'   geom_puzzle_rect(rows = 3, cols = 3, seed = 42,
#'                    fusion_groups = "1-2-3,7-8-9") +
#'   scale_fill_viridis_c() +
#'   theme_void()
#' }
#'
#' @export
geom_puzzle_rect <- function(mapping = NULL,
                              data = NULL,
                              stat = "puzzle",
                              position = "identity",
                              rows = 3,
                              cols = 3,
                              tabsize = 20,
                              jitter = 4,
                              seed = NULL,
                              offset = 0,
                              bezier_resolution = 20,
                              fusion_groups = NULL,
                              fusion_style = "none",
                              fusion_opacity = 0.3,
                              show_labels = FALSE,
                              label_color = "black",
                              label_size = NULL,
                              ...,
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {

  ggplot2::layer(
    data = ensure_puzzle_data(data),
    mapping = mapping,
    stat = StatPuzzle,
    geom = GeomPuzzle,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      puzzle_type = "rectangular",
      rows = rows,
      cols = cols,
      tabsize = tabsize,
      jitter = jitter,
      seed = seed,
      offset = offset,
      bezier_resolution = bezier_resolution,
      fusion_groups = fusion_groups,
      fusion_style = fusion_style,
      fusion_opacity = fusion_opacity,
      show_labels = show_labels,
      label_color = label_color,
      label_size = label_size,
      na.rm = na.rm,
      ...
    )
  )
}


#' Add hexagonal puzzle layer to a ggplot
#'
#' Creates a hexagonal puzzle visualization where each puzzle piece represents
#' a data point. The puzzle has a honeycomb-like structure with configurable
#' number of rings.
#'
#' @param mapping Set of aesthetic mappings. Usually includes `fill`.
#' @param data The data to visualize. Each row maps to a puzzle piece.
#' @param stat The statistical transformation to use (default: "puzzle").
#' @param position Position adjustment (default: "identity").
#' @param rings Number of rings in the hexagonal puzzle (minimum 2).
#'   Piece count formula: 3 * rings * (rings - 1) + 1.
#'   Example: 3 rings = 19 pieces.
#' @param tabsize Size of the puzzle tabs (default: 20).
#' @param jitter Random variation in tab positions (default: 4).
#' @param seed Random seed for reproducible puzzle shapes.
#' @param bezier_resolution Points per Bezier curve (default: 20).
#' @param do_warp Apply circular warping transformation (default: TRUE).
#' @param do_trunc Truncate edge pieces at boundary (default: TRUE).
#' @param do_circular_border Use perfect circular arc borders (default: FALSE).
#' @param fusion_groups Piece fusion specification: PILES notation string (e.g., "1-2-3,4-5"),
#'   list of integer vectors, or NULL for no fusion.
#' @param fusion_style Style for fused internal edges: "none" (invisible), "dashed", "solid".
#' @param fusion_opacity Opacity for fused edges when style != "none" (0.0 to 1.0).
#' @param show_labels Logical; if TRUE, display piece ID labels at piece centers (default: FALSE).
#' @param label_color Color for piece labels (default: "black").
#' @param label_size Font size for labels in points. NULL for auto-sizing based on piece dimensions.
#' @param na.rm Remove NA values? (default: FALSE).
#' @param show.legend Include this layer in the legend? (default: NA).
#' @param inherit.aes Inherit aesthetics from the plot? (default: TRUE).
#' @param ... Other arguments passed to layer().
#'
#' @return A ggplot2 layer.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # Hexagonal puzzle with 3 rings (19 pieces)
#' df <- data.frame(value = 1:19)
#' ggplot(df, aes(fill = value)) +
#'   geom_puzzle_hex(rings = 3, seed = 42) +
#'   scale_fill_viridis_c() +
#'   theme_void() +
#'   coord_fixed()
#'
#' # With piece labels
#' ggplot(df, aes(fill = value)) +
#'   geom_puzzle_hex(rings = 3, seed = 42, show_labels = TRUE) +
#'   scale_fill_viridis_c() +
#'   theme_void() +
#'   coord_fixed()
#' }
#'
#' @export
geom_puzzle_hex <- function(mapping = NULL,
                            data = NULL,
                            stat = "puzzle",
                            position = "identity",
                            rings = 3,
                            tabsize = 20,
                            jitter = 4,
                            seed = NULL,
                            offset = 0,
                            bezier_resolution = 20,
                            do_warp = TRUE,
                            do_trunc = TRUE,
                            do_circular_border = FALSE,
                            fusion_groups = NULL,
                            fusion_style = "none",
                            fusion_opacity = 0.3,
                            show_labels = FALSE,
                            label_color = "black",
                            label_size = NULL,
                            ...,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {

  ggplot2::layer(
    data = ensure_puzzle_data(data),
    mapping = mapping,
    stat = StatPuzzle,
    geom = GeomPuzzle,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      puzzle_type = "hexagonal",
      rings = rings,
      tabsize = tabsize,
      jitter = jitter,
      seed = seed,
      offset = offset,
      bezier_resolution = bezier_resolution,
      do_warp = do_warp,
      do_trunc = do_trunc,
      do_circular_border = do_circular_border,
      fusion_groups = fusion_groups,
      fusion_style = fusion_style,
      fusion_opacity = fusion_opacity,
      show_labels = show_labels,
      label_color = label_color,
      label_size = label_size,
      na.rm = na.rm,
      ...
    )
  )
}


#' Add concentric puzzle layer to a ggplot
#'
#' Creates a concentric ring puzzle visualization where each puzzle piece
#' represents a data point. The puzzle has a central piece surrounded by
#' concentric rings of pieces.
#'
#' @param mapping Set of aesthetic mappings. Usually includes `fill`.
#' @param data The data to visualize. Each row maps to a puzzle piece.
#' @param stat The statistical transformation to use (default: "puzzle").
#' @param position Position adjustment (default: "identity").
#' @param rings Number of rings in the puzzle (minimum 1).
#'   Piece count formula: rings * 6 + 1.
#'   Example: 3 rings = 19 pieces (1 center + 6 + 6 + 6).
#' @param tabsize Size of the puzzle tabs (default: 20).
#' @param jitter Random variation in tab positions (default: 4).
#' @param seed Random seed for reproducible puzzle shapes.
#' @param bezier_resolution Points per Bezier curve (default: 20).
#' @param center_shape Shape of the center piece: "hexagon" or "circle".
#' @param do_circular_border Use perfect circular arc borders (default: FALSE).
#' @param fusion_groups Piece fusion specification: PILES notation string (e.g., "1-2-3,4-5"),
#'   list of integer vectors, or NULL for no fusion.
#' @param fusion_style Style for fused internal edges: "none" (invisible), "dashed", "solid".
#' @param fusion_opacity Opacity for fused edges when style != "none" (0.0 to 1.0).
#' @param show_labels Logical; if TRUE, display piece ID labels at piece centers (default: FALSE).
#' @param label_color Color for piece labels (default: "black").
#' @param label_size Font size for labels in points. NULL for auto-sizing based on piece dimensions.
#' @param na.rm Remove NA values? (default: FALSE).
#' @param show.legend Include this layer in the legend? (default: NA).
#' @param inherit.aes Inherit aesthetics from the plot? (default: TRUE).
#' @param ... Other arguments passed to layer().
#'
#' @return A ggplot2 layer.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # Concentric puzzle with 3 rings (19 pieces)
#' df <- data.frame(value = 1:19)
#' ggplot(df, aes(fill = value)) +
#'   geom_puzzle_conc(rings = 3, seed = 42) +
#'   scale_fill_viridis_c() +
#'   theme_void() +
#'   coord_fixed()
#' }
#'
#' @export
geom_puzzle_conc <- function(mapping = NULL,
                             data = NULL,
                             stat = "puzzle",
                             position = "identity",
                             rings = 3,
                             tabsize = 20,
                             jitter = 4,
                             seed = NULL,
                             offset = 0,
                             bezier_resolution = 20,
                             center_shape = "hexagon",
                             do_circular_border = FALSE,
                             fusion_groups = NULL,
                             fusion_style = "none",
                             fusion_opacity = 0.3,
                             show_labels = FALSE,
                             label_color = "black",
                             label_size = NULL,
                             ...,
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE) {

  ggplot2::layer(
    data = ensure_puzzle_data(data),
    mapping = mapping,
    stat = StatPuzzle,
    geom = GeomPuzzle,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      puzzle_type = "concentric",
      rings = rings,
      tabsize = tabsize,
      jitter = jitter,
      seed = seed,
      offset = offset,
      bezier_resolution = bezier_resolution,
      center_shape = center_shape,
      do_circular_border = do_circular_border,
      fusion_groups = fusion_groups,
      fusion_style = fusion_style,
      fusion_opacity = fusion_opacity,
      show_labels = show_labels,
      label_color = label_color,
      label_size = label_size,
      na.rm = na.rm,
      ...
    )
  )
}


#' Add Voronoi puzzle layer to a ggplot
#'
#' Creates a Voronoi tessellation puzzle visualization where each puzzle
#' piece represents a data point. The puzzle pieces have organic, irregular
#' shapes based on Voronoi cells.
#'
#' @param mapping Set of aesthetic mappings. Usually includes `fill`.
#' @param data The data to visualize. Each row maps to a puzzle piece.
#' @param stat The statistical transformation to use (default: "puzzle").
#' @param position Position adjustment (default: "identity").
#' @param n_cells Number of Voronoi cells (pieces) in the puzzle.
#' @param tabsize Size of the puzzle tabs (default: 20).
#' @param jitter Random variation in tab positions (default: 4).
#' @param seed Random seed for reproducible puzzle shapes.
#' @param bezier_resolution Points per Bezier curve (default: 20).
#' @param point_distribution How to distribute seed points:
#'   "fermat" (default, golden angle spiral), "uniform" (random), or "jittered" (grid with noise).
#' @param fusion_groups Piece fusion specification: PILES notation string (e.g., "1-2-3,4-5"),
#'   list of integer vectors, or NULL for no fusion.
#' @param fusion_style Style for fused internal edges: "none" (invisible), "dashed", "solid".
#' @param fusion_opacity Opacity for fused edges when style != "none" (0.0 to 1.0).
#' @param show_labels Logical; if TRUE, display piece ID labels at piece centers (default: FALSE).
#' @param label_color Color for piece labels (default: "black").
#' @param label_size Font size for labels in points. NULL for auto-sizing based on piece dimensions.
#' @param na.rm Remove NA values? (default: FALSE).
#' @param show.legend Include this layer in the legend? (default: NA).
#' @param inherit.aes Inherit aesthetics from the plot? (default: TRUE).
#' @param ... Other arguments passed to layer().
#'
#' @return A ggplot2 layer.
#'
#' @note Requires the 'deldir' package to be installed.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # Voronoi puzzle with 12 cells
#' df <- data.frame(value = 1:12)
#' ggplot(df, aes(fill = value)) +
#'   geom_puzzle_voronoi(n_cells = 12, seed = 42) +
#'   scale_fill_viridis_c() +
#'   theme_void()
#' }
#'
#' @export
geom_puzzle_voronoi <- function(mapping = NULL,
                                data = NULL,
                                stat = "puzzle",
                                position = "identity",
                                n_cells = 12,
                                tabsize = 20,
                                jitter = 4,
                                seed = NULL,
                                offset = 0,
                                bezier_resolution = 20,
                                point_distribution = "fermat",
                                fusion_groups = NULL,
                                fusion_style = "none",
                                fusion_opacity = 0.3,
                                show_labels = FALSE,
                                label_color = "black",
                                label_size = NULL,
                                ...,
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE) {

  ggplot2::layer(
    data = ensure_puzzle_data(data),
    mapping = mapping,
    stat = StatPuzzle,
    geom = GeomPuzzle,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      puzzle_type = "voronoi",
      n_cells = n_cells,
      tabsize = tabsize,
      jitter = jitter,
      seed = seed,
      offset = offset,
      bezier_resolution = bezier_resolution,
      point_distribution = point_distribution,
      fusion_groups = fusion_groups,
      fusion_style = fusion_style,
      fusion_opacity = fusion_opacity,
      show_labels = show_labels,
      label_color = label_color,
      label_size = label_size,
      na.rm = na.rm,
      ...
    )
  )
}


#' Add random shape puzzle layer to a ggplot
#'
#' Creates a puzzle visualization with randomly-shaped pieces based on
#' Delaunay triangulation. Each puzzle piece represents a data point.
#' The pieces have irregular, organic shapes.
#'
#' @param mapping Set of aesthetic mappings. Usually includes `fill`.
#' @param data The data to visualize. Each row maps to a puzzle piece.
#' @param stat The statistical transformation to use (default: "puzzle").
#' @param position Position adjustment (default: "identity").
#' @param n_pieces Number of interior points that influence piece count.
#'   Actual piece count depends on triangulation.
#' @param tabsize Size of the puzzle tabs (default: 20).
#' @param jitter Random variation in tab positions (default: 4).
#' @param seed Random seed for reproducible puzzle shapes.
#' @param bezier_resolution Points per Bezier curve (default: 20).
#' @param n_corner Number of corners for the base polygon (default: 4 for rectangle).
#' @param fusion_groups Piece fusion specification: PILES notation string (e.g., "1-2-3,4-5"),
#'   list of integer vectors, or NULL for no fusion.
#' @param fusion_style Style for fused internal edges: "none" (invisible), "dashed", "solid".
#' @param fusion_opacity Opacity for fused edges when style != "none" (0.0 to 1.0).
#' @param show_labels Logical; if TRUE, display piece ID labels at piece centers (default: FALSE).
#' @param label_color Color for piece labels (default: "black").
#' @param label_size Font size for labels in points. NULL for auto-sizing based on piece dimensions.
#' @param na.rm Remove NA values? (default: FALSE).
#' @param show.legend Include this layer in the legend? (default: NA).
#' @param inherit.aes Inherit aesthetics from the plot? (default: TRUE).
#' @param ... Other arguments passed to layer().
#'
#' @return A ggplot2 layer.
#'
#' @note Requires the 'RCDT' package to be installed.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # Random shape puzzle with ~12 pieces
#' df <- data.frame(value = 1:12)
#' ggplot(df, aes(fill = value)) +
#'   geom_puzzle_random(n_pieces = 12, seed = 42) +
#'   scale_fill_viridis_c() +
#'   theme_void()
#' }
#'
#' @export
geom_puzzle_random <- function(mapping = NULL,
                               data = NULL,
                               stat = "puzzle",
                               position = "identity",
                               n_pieces = 12,
                               tabsize = 20,
                               jitter = 4,
                               seed = NULL,
                               offset = 0,
                               bezier_resolution = 20,
                               n_corner = 4,
                               fusion_groups = NULL,
                               fusion_style = "none",
                               fusion_opacity = 0.3,
                               show_labels = FALSE,
                               label_color = "black",
                               label_size = NULL,
                               ...,
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE) {

  ggplot2::layer(
    data = ensure_puzzle_data(data),
    mapping = mapping,
    stat = StatPuzzle,
    geom = GeomPuzzle,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      puzzle_type = "random",
      n_pieces = n_pieces,
      tabsize = tabsize,
      jitter = jitter,
      seed = seed,
      offset = offset,
      bezier_resolution = bezier_resolution,
      n_corner = n_corner,
      fusion_groups = fusion_groups,
      fusion_style = fusion_style,
      fusion_opacity = fusion_opacity,
      show_labels = show_labels,
      label_color = label_color,
      label_size = label_size,
      na.rm = na.rm,
      ...
    )
  )
}
