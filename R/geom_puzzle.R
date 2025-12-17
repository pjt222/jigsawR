# Geom layer for puzzle piece rendering
# Part of the jigsawR ggpuzzle extension

#' @include stat_puzzle.R
#' @import ggplot2
#' @import grid
NULL

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

  # Use polygon key for legend
 draw_key = ggplot2::draw_key_polygon,

  draw_panel = function(data, panel_params, coord) {
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

    grobs <- lapply(pieces, function(piece) {
      # Get first row for aesthetics (all rows in piece should have same aes)
      first <- piece[1, , drop = FALSE]

      # Handle alpha - NA means fully opaque
      fill_color <- first$fill
      if (!is.na(first$alpha) && first$alpha < 1) {
        fill_color <- scales::alpha(first$fill, first$alpha)
      }

      grid::polygonGrob(
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
#' # With categorical data
#' sales <- data.frame(
#'   region = c("North", "South", "East", "West"),
#'   value = c(100, 150, 80, 120)
#' )
#' ggplot(sales, aes(fill = value)) +
#'   geom_puzzle_rect(rows = 2, cols = 2, seed = 42) +
#'   scale_fill_gradient(low = "white", high = "steelblue") +
#'   theme_void() +
#'   labs(title = "Sales by Region")
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
                              bezier_resolution = 20,
                              ...,
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {

  ggplot2::layer(
    data = data,
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
      bezier_resolution = bezier_resolution,
      na.rm = na.rm,
      ...
    )
  )
}


# Future: geom_puzzle_hex() and geom_puzzle_concentric()
# Will be implemented in follow-up PRs after rectangular is validated
