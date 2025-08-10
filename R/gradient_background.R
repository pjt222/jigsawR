# Circular Gradient Background Generation Functions
# Part of the jigsawR package

#' @import ggplot2
#' @import ggforce
#' @import ggfx
#' @import viridis

# Temporary library calls for development
library(ggplot2)
library(ggforce)
library(ggfx)
library(viridis)

#' Create circular gradient background
#' @param size_px Size in pixels for output
#' @param diameter Puzzle diameter in mm (optional, for size matching)
#' @return ggplot2 object with gradient background
#' @export
create_gradient_circle_png <- function(size_px = 2000, diameter = NULL) {

  resolution <- 300 # Fixed resolution for consistent quality

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
    # Default coordinates for backward compatibility
    coord_limit <- 1.3
  }

  # Generate coordinate grid
  # Make background slightly larger to ensure complete coverage
  coord_range <- coord_limit * 1.20  # Larger range for gradient coverage
  coord_limit <- coord_limit * 0.90   # Slightly expand plot limits for perfect alignment
  x_coords <- seq(-coord_range, coord_range, length.out = resolution)
  y_coords <- seq(-coord_range, coord_range, length.out = resolution)

  # Create gradient matrix (diagonal from bottom-left to top-right)
  gradient_matrix <- outer(x_coords, y_coords, function(x, y) {
    # Normalize to 0-1 range based on actual coordinate range
    (x + y + 2*coord_range) / (4*coord_range)
  })

  # Convert to viridis colors
  viridis_colors <- viridis::viridis(256)
  color_indices <- pmax(1, pmin(256, round(gradient_matrix * 255) + 1))
  color_matrix <- array(viridis_colors[color_indices], dim = dim(gradient_matrix))
  gradient_raster <- as.raster(color_matrix)

  # Create circle mask for gradient - ensure exact radius=1.0
  angles <- seq(0, 2*pi, length.out = 361)[-361]  # Remove duplicate point
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
#' @param size_px Size in pixels
#' @export
save_gradient_background <- function(gradient_plot, background_file, size_px = 2000) {

  # Calculate dimensions in inches for high DPI
  size_inches <- size_px / 300  # 300 DPI

  # Ensure output directory exists
  if (!dir.exists("output")) {
    dir.create("output", recursive = TRUE)
  }

  # Add output/ prefix if not already present
  if (!grepl("^output/", background_file)) {
    background_file <- file.path("output", background_file)
  }

  ggsave(
    filename = background_file,
    plot = gradient_plot,
    width = size_inches, height = size_inches, units = "in", dpi = 300,
    bg = "white"
  )

  cat("  Background PNG saved:", background_file, "\n")
}
