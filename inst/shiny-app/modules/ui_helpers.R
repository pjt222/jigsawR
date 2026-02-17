# =============================================================================
# UI Helper Functions for jigsawR Shiny App
# Extracted from app.R (#108)
# =============================================================================

#' Create Slider with Numeric Input
#'
#' @param id Base input ID (numeric input will be {id}_num)
#' @param label Label text (NULL for no label)
#' @param min Minimum value
#' @param max Maximum value
#' @param value Initial value
#' @param step Step increment (default 1)
#' @param post Suffix to display on slider value (e.g., "%" or " mm")
#' @param ticks Whether to show tick marks on slider
#' @param width Width for numeric input (default "80px")
#' @return Shiny tagList with slider and numeric input side by side
slider_with_numeric <- function(id, label, min, max, value, step = 1,
                                 post = NULL, ticks = TRUE, width = "80px") {
  tagList(
    # Label above the inputs (if provided)
    if (!is.null(label)) {
      tags$label(label, class = "form-label mb-1", `for` = id)
    },
    # Flex container: slider (grows) + numeric input (fixed width)
    div(
      class = "d-flex align-items-center gap-2",
      # Slider takes remaining space
      div(
        class = "flex-grow-1",
        sliderInput(
          inputId = id,
          label = NULL,
          min = min,
          max = max,
          value = value,
          step = step,
          ticks = ticks,
          post = post,
          sep = "",
          width = "100%"
        )
      ),
      # Compact numeric input with optional suffix
      if (!is.null(post) && nchar(trimws(post)) > 0) {
        div(
          class = "d-flex align-items-center",
          numericInput(
            inputId = paste0(id, "_num"),
            label = NULL,
            value = value,
            min = min,
            max = max,
            step = step,
            width = width
          ),
          tags$span(class = "ms-1 text-muted small", trimws(post))
        )
      } else {
        numericInput(
          inputId = paste0(id, "_num"),
          label = NULL,
          value = value,
          min = min,
          max = max,
          step = step,
          width = width
        )
      }
    )
  )
}

# Helper function to map boundary shape selection to internal parameters
get_hex_boundary_params <- function(boundary_choice) {
  switch(boundary_choice,
    "zigzag"     = list(do_warp = FALSE, do_trunc = FALSE, do_circular_border = FALSE),
    "hexagon"    = list(do_warp = FALSE, do_trunc = TRUE,  do_circular_border = FALSE),
    "warped"     = list(do_warp = TRUE,  do_trunc = FALSE, do_circular_border = FALSE),
    "warped_hex" = list(do_warp = TRUE,  do_trunc = TRUE,  do_circular_border = FALSE),
    "circle"     = list(do_warp = TRUE,  do_trunc = TRUE,  do_circular_border = TRUE),
    # Default fallback
    list(do_warp = FALSE, do_trunc = FALSE, do_circular_border = FALSE)
  )
}

# Helper function to map concentric boundary choice to parameters
get_conc_boundary_params <- function(boundary_choice, boundary_facing = "outward") {
  switch(boundary_choice,
    "circle" = list(do_circular_border = TRUE, boundary_facing = boundary_facing),
    # Default: straight (boundary_facing ignored for straight boundary)
    list(do_circular_border = FALSE, boundary_facing = "outward")
  )
}
