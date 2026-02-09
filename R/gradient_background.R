# Circular Gradient Background Generation Functions
# Part of the jigsawR package

#' Create circular gradient background
#' @importFrom ggplot2 ggplot aes geom_polygon annotation_raster coord_fixed
#'   theme_void theme element_rect margin ggsave
#' @importFrom ggfx as_reference with_mask ch_alpha
#' @param size_px Size in pixels for output
#' @param diameter Puzzle diameter in mm (optional, for size matching)
#' @param palette Viridis palette name (NULL = use config default)
#' @return ggplot2 object with gradient background
create_gradient_circle_png <- function(size_px = NULL, diameter = NULL, palette = NULL) {

  # Load config defaults
  cfg <- get_puzzle_config()

  if (is.null(size_px)) {
    size_px <- cfg$gradient$default_size_px
  }

  resolution <- cfg$gradient$dpi

  # Calculate coordinate limits to match puzzle dimensions if provided
  if (!is.null(diameter)) {
    # Direct approach: match the exact SVG circle size
    # SVG: diameter=200 in canvas=240, so circle uses 200/240 = 83.33% of canvas
    # But we need to account for ggplot coordinate system differences

    puzzle_radius <- diameter / 2.0
    puzzle_offset <- puzzle_radius * 0.2
    canvas_size <- 2.0 * (puzzle_radius + puzzle_offset)

    # SVG circle diameter in canvas units: diameter
    # SVG canvas size: canvas_size
    # We want ggplot circle (radius=1.0) to have same relative size

    # The key insight: ggplot circle with radius=1.0 in coord_fixed should match
    # SVG circle with radius=puzzle_radius in viewBox of canvas_size
    # So: coord_limit = (canvas_size/2) / puzzle_radius = canvas_size / diameter
    coord_limit <- canvas_size / diameter
  } else {
    # Default coordinates from config
    coord_limit <- cfg$gradient$coord_limit
  }

  # Generate coordinate grid
  # Make background slightly larger to ensure complete coverage
  coord_range <- coord_limit * cfg$gradient$range_expansion
  coord_limit <- coord_limit * cfg$gradient$limit_contraction
  x_coords <- seq(-coord_range, coord_range, length.out = resolution)
  y_coords <- seq(-coord_range, coord_range, length.out = resolution)

  # Create gradient matrix (diagonal from bottom-left to top-right)
  gradient_matrix <- outer(x_coords, y_coords, function(x, y) {
    # Normalize to 0-1 range based on actual coordinate range
    (x + y + 2*coord_range) / (4*coord_range)
  })

  # Convert to viridis colors using selected palette
  palette_size <- cfg$gradient$palette_size
  viridis_colors <- get_puzzle_colors(palette_size, palette)
  color_indices <- pmax(1, pmin(palette_size, round(gradient_matrix * (palette_size - 1)) + 1))
  color_matrix <- array(viridis_colors[color_indices], dim = dim(gradient_matrix))
  gradient_raster <- as.raster(color_matrix)

  # Create circle mask for gradient - ensure exact radius=1.0
  circle_resolution <- cfg$gradient$circle_resolution
  angles <- seq(0, 2*pi, length.out = circle_resolution)[-circle_resolution]
  circle_mask <- data.frame(
    x = cos(angles),
    y = sin(angles)
  )

  # Create the gradient plot
  p <- ggplot() +
    # Create circle reference for masking
    as_reference(
      geom_polygon(aes(x = x, y = y), data = circle_mask,
                   fill = "white", color = NA),
      id = "circle_mask"
    ) +
    # Apply gradient with circular mask
    with_mask(
      annotation_raster(gradient_raster,
                        xmin = -coord_range, xmax = coord_range,
                        ymin = -coord_range, ymax = coord_range),
      mask = ch_alpha("circle_mask")
    ) +
    # Configure coordinate system and theme
    coord_fixed(xlim = c(-coord_limit, coord_limit), ylim = c(-coord_limit, coord_limit)) +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(0, 0, 0, 0, "pt")
    )

  return(p)
}

#' Save gradient background to file
#' @param gradient_plot ggplot2 object from create_gradient_circle_png()
#' @param background_file Output filename for background
#' @param size_px Size in pixels (NULL = use config default)
save_gradient_background <- function(gradient_plot, background_file, size_px = NULL) {

  # Load config defaults
  cfg <- get_puzzle_config()

  if (is.null(size_px)) {
    size_px <- cfg$gradient$default_size_px
  }

  # Calculate dimensions in inches for high DPI
  size_inches <- size_px / cfg$gradient$dpi

  # Ensure parent directory exists
  output_parent <- dirname(background_file)
  if (nzchar(output_parent) && output_parent != "." && !dir.exists(output_parent)) {
    dir.create(output_parent, recursive = TRUE)
  }

  ggsave(
    filename = background_file,
    plot = gradient_plot,
    width = size_inches, height = size_inches, units = "in", dpi = 300,
    bg = "white"
  )

  log_success("Background PNG saved: {.file {background_file}}")
}
