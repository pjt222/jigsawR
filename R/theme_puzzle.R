#' Minimal Theme Optimized for Puzzle Visualization
#'
#' A clean, minimal theme based on \code{theme_void()} that is optimized for
#' displaying puzzle pieces. Unlike \code{theme_void()}, this theme ensures
#' proper rendering in all contexts including Quarto/RMarkdown documents.
#'
#' @details
#' The issue with \code{theme_void()} is that it sets \code{plot.background}
#' to an empty fill and \code{plot.margin} to zero. Combined with
#' \code{guide = "none"} (hiding the legend), this can cause knitr/Quarto
#' to fail when capturing figure output, resulting in blank images.
#'
#' \code{theme_puzzle()} fixes this by:
#' \itemize{
#'   \item Setting an explicit background color (default: white)
#'   \item Adding a small plot margin for proper dimension calculation
#' }
#'
#' @param base_size Base font size (default 11)
#' @param base_family Base font family (default "")
#' @param background Background color. Use \code{"white"} for a white background
#'   (default), \code{"transparent"} or \code{NA} for a transparent background.
#'   Note that transparent backgrounds may cause rendering issues in some
#'   contexts when combined with \code{guide = "none"}.
#' @param margin Plot margin. Default is \code{margin(2, 2, 2, 2, "pt")} which
#'   provides minimal spacing while ensuring proper rendering.
#'
#' @return A ggplot2 theme object
#'
#' @examples
#' library(ggplot2)
#'
#' # Basic usage - clean puzzle visualization
#' ggplot() +
#'   geom_puzzle_rect(
#'     aes(fill = after_stat(piece_id)),
#'     cols = 3, rows = 2, seed = 42
#'   ) +
#'   scale_fill_viridis_c(guide = "none") +
#'   coord_fixed() +
#'   theme_puzzle()
#'
#' # With custom background color
#' ggplot() +
#'   geom_puzzle_hex(
#'     aes(fill = after_stat(piece_id)),
#'     rings = 2, seed = 42
#'   ) +
#'   scale_fill_viridis_c(guide = "none") +
#'   coord_fixed() +
#'   theme_puzzle(background = "grey95")
#'
#' # Transparent background (use with caution in documents)
#' ggplot() +
#'   geom_puzzle_rect(
#'     aes(fill = after_stat(piece_id)),
#'     cols = 2, rows = 2, seed = 42
#'   ) +
#'   scale_fill_viridis_c(guide = "none") +
#'   coord_fixed() +
#'   theme_puzzle(background = NA)
#'
#' @seealso \code{\link[ggplot2]{theme_void}}, \code{\link[ggplot2]{theme}}
#'
#' @export
theme_puzzle <- function(base_size = 11,
                         base_family = "",
                         background = "white",
                         margin = ggplot2::margin(2, 2, 2, 2, "pt")) {


  # Handle "transparent" as an alias for NA

if (identical(background, "transparent")) {
    background <- NA
  }

  # Start with theme_void for the clean, minimal appearance
  ggplot2::theme_void(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(
      # Explicit background color fixes the rendering issue
      # When background is NA, we still set it explicitly (as transparent)
      # rather than leaving it as element_blank
      plot.background = ggplot2::element_rect(
        fill = if (is.na(background)) "transparent" else background,
        colour = NA
      ),

      # Small margin ensures proper dimension calculation by knitr
      plot.margin = margin,

      # Mark as complete theme
      complete = TRUE
    )
}
