library(ggplot2)
library(ggforce)
library(ggfx)
library(viridis)

# Source the hexagonal jigsaw functions
source("jigsaw-hex.R")

#' Create circular gradient background
#' @param size_px Size in pixels for output
#' @return ggplot2 object with gradient background
create_gradient_circle_png <- function(size_px = 2000) {

  resolution <- 300 #min(400, size_px / 5)  # Adjust resolution based on output size

  # Generate coordinate grid
  x_coords <- seq(-1.5, 1.5, length.out = resolution)
  y_coords <- seq(-1.5, 1.5, length.out = resolution)

  # Create gradient matrix (diagonal from bottom-left to top-right)
  gradient_matrix <- outer(x_coords, y_coords, function(x, y) {
    (x + y + 3) / 6  # Normalize to 0-1 range
  })

  # Convert to viridis colors
  viridis_colors <- viridis::viridis(256)
  color_indices <- pmax(1, pmin(256, round(gradient_matrix * 255) + 1))
  color_matrix <- array(viridis_colors[color_indices], dim = dim(gradient_matrix))
  gradient_raster <- as.raster(color_matrix)

  # Create circle mask for gradient
  circle_mask <- data.frame(
    x = cos(seq(0, 2*pi, length.out = 360)),
    y = sin(seq(0, 2*pi, length.out = 360))
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
                        xmin = -1.5, xmax = 1.5,
                        ymin = -1.5, ymax = 1.5),
      mask = ch_alpha("circle_mask")
    ) +
    # Configure coordinate system and theme
    coord_fixed(xlim = c(-1.3, 1.3), ylim = c(-1.3, 1.3)) +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(0, 0, 0, 0, "pt")
    )

  return(p)
}

#' Create enhanced SVG with better styling for conversion
#' @param seed Random seed for puzzle generation
#' @param diameter Puzzle diameter in mm
#' @param rings Number of rings in the puzzle
#' @param tabsize Tab size percentage
#' @param jitter Jitter percentage
#' @param line_color Color for puzzle lines
#' @param line_width Width for puzzle lines
#' @return Enhanced SVG content
create_enhanced_puzzle_svg <- function(seed = 1234, diameter = 240, rings = 4,
                                      tabsize = 27, jitter = 5,
                                      line_color = "white", line_width = 2.0) {

  # Generate circular jigsaw puzzle
  puzzle <- generate_hex_jigsaw_svg(
    seed = seed,
    tabsize = tabsize,
    jitter = jitter,
    diameter = diameter,
    rings = rings,
    do_warp = TRUE,    # Enable circular warp
    do_trunc = TRUE    # Truncate for clean circle
  )

  # Get puzzle dimensions
  radius <- puzzle$parameters$diameter / 2.0
  offset <- radius * 0.2
  width <- 2.0 * (radius + offset)
  height <- 2.0 * (radius + offset)

  # Create enhanced SVG with better styling
  enhanced_svg <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>\n',
    '<svg xmlns="http://www.w3.org/2000/svg" version="1.1" ',
    'width="', width, '" height="', height, '" ',
    'viewBox="0 0 ', width, ' ', height, '">\n',

    # Add transparent background
    '<rect width="100%" height="100%" fill="transparent"/>\n',

    # Add puzzle paths with enhanced styling
    '<path fill="none" stroke="', line_color, '" stroke-width="', line_width, '" ',
    'stroke-linecap="round" stroke-linejoin="round" ',
    'stroke-opacity="1.0" d="', puzzle$horizontal, '"/>\n',

    '<path fill="none" stroke="', line_color, '" stroke-width="', line_width, '" ',
    'stroke-linecap="round" stroke-linejoin="round" ',
    'stroke-opacity="1.0" d="', puzzle$vertical, '"/>\n',

    '<path fill="none" stroke="black" stroke-width="', line_width * 1.5, '" ',
    'stroke-linecap="round" stroke-linejoin="round" ',
    'stroke-opacity="1.0" d="', puzzle$border, '"/>\n',

    '</svg>'
  )

  return(list(
    svg_content = enhanced_svg,
    parameters = puzzle$parameters,
    width = width,
    height = height
  ))
}

#' Convert SVG to PNG using available tools
#' @param svg_content SVG content as string
#' @param output_file Output PNG filename
#' @param width_px Width in pixels
#' @param height_px Height in pixels
#' @return TRUE if successful, FALSE otherwise
convert_svg_to_png <- function(svg_content, output_file, width_px = 2000, height_px = 2000) {

  # Save SVG to temporary file
  temp_svg <- tempfile(fileext = ".svg")
  writeLines(svg_content, temp_svg)

  cat("Converting SVG to PNG:", output_file, "\n")

  # Try different conversion methods in order of preference

  # Method 1: Try rsvg package (R-based, most reliable if available)
  if (requireNamespace("rsvg", quietly = TRUE)) {
    cat("  Using rsvg package...\n")
    tryCatch({
      png_data <- rsvg::rsvg_png(temp_svg, width = width_px, height = height_px)
      writeBin(png_data, output_file)
      unlink(temp_svg)
      return(TRUE)
    }, error = function(e) {
      cat("  rsvg failed:", e$message, "\n")
    })
  }

  # Method 2: Try magick package
  if (requireNamespace("magick", quietly = TRUE)) {
    cat("  Using magick package...\n")
    tryCatch({
      # Read SVG content directly instead of file path
      img <- magick::image_read(svg_content, density = 300)
      img <- magick::image_resize(img, paste0(width_px, "x", height_px))
      magick::image_write(img, output_file, format = "PNG")
      unlink(temp_svg)
      return(TRUE)
    }, error = function(e) {
      cat("  magick failed:", e$message, "\n")
    })
  }

  # Method 3: Try Inkscape command line (if available)
  inkscape_cmd <- Sys.which("inkscape")
  if (nzchar(inkscape_cmd)) {
    cat("  Using Inkscape command line...\n")
    cmd <- sprintf('"%s" --export-type=png --export-filename="%s" --export-width=%d --export-height=%d "%s"',
                   inkscape_cmd, output_file, width_px, height_px, temp_svg)

    result <- tryCatch({
      system(cmd, intern = TRUE)
      file.exists(output_file)
    }, error = function(e) FALSE)

    unlink(temp_svg)
    if (result && file.exists(output_file)) {
      return(TRUE)
    }
  }

  # Method 4: Try ImageMagick convert command line
  convert_cmd <- Sys.which("convert")
  if (nzchar(convert_cmd)) {
    cat("  Using ImageMagick convert...\n")
    cmd <- sprintf('"%s" -density 300 -resize %dx%d "%s" "%s"',
                   convert_cmd, width_px, height_px, temp_svg, output_file)

    result <- tryCatch({
      system(cmd, intern = TRUE)
      file.exists(output_file)
    }, error = function(e) FALSE)

    unlink(temp_svg)
    if (result && file.exists(output_file)) {
      return(TRUE)
    }
  }

  # All methods failed
  unlink(temp_svg)
  cat("  ERROR: No suitable SVG conversion tool found!\n")
  cat("  Install one of: rsvg, magick packages, or Inkscape/ImageMagick CLI tools\n")
  return(FALSE)
}

#' Generate puzzle layers using SVG to PNG conversion
#' @param seed Random seed for puzzle
#' @param diameter Puzzle diameter
#' @param rings Number of rings
#' @param tabsize Tab size percentage
#' @param jitter Jitter percentage
#' @param base_filename Base name for output files
#' @param size_px Output size in pixels
generate_svg_puzzle_layers <- function(seed = 1234, diameter = 240, rings = 4,
                                      tabsize = 27, jitter = 5,
                                      base_filename = "svg_puzzle",
                                      size_px = 2000) {

  cat("Generating SVG puzzle (seed:", seed, ", rings:", rings, ")...\n")

  # Create enhanced SVG
  puzzle_svg <- create_enhanced_puzzle_svg(
    seed = seed, diameter = diameter, rings = rings,
    tabsize = tabsize, jitter = jitter,
    line_color = "black", line_width = 2.0
  )

  # Save the SVG file
  svg_file <- paste0(base_filename, ".svg")
  writeLines(puzzle_svg$svg_content, svg_file)
  cat("  SVG saved:", svg_file, "\n")

  # Convert SVG to PNG overlay
  overlay_file <- paste0(base_filename, "_overlay.png")
  success <- convert_svg_to_png(puzzle_svg$svg_content, overlay_file, size_px, size_px)

  if (success) {
    cat("  Overlay PNG saved:", overlay_file, "\n")
  } else {
    cat("  Failed to create PNG overlay\n")
    return(NULL)
  }

  # Create gradient background
  cat("  Creating gradient background...\n")
  gradient_plot <- create_gradient_circle_png(size_px)

  background_file <- paste0(base_filename, "_background.png")

  # Calculate dimensions in inches for high DPI
  size_inches <- size_px / 300  # 300 DPI

  ggsave(
    filename = background_file,
    plot = gradient_plot,
    width = size_inches, height = size_inches, units = "in", dpi = 300,
    bg = "white"
  )

  cat("  Background PNG saved:", background_file, "\n")

  # Combine if magick is available
  if (requireNamespace("magick", quietly = TRUE)) {
    combined_file <- paste0(base_filename, "_combined.png")
    cat("  Combining layers...\n")

    tryCatch({
      background <- magick::image_read(background_file)
      overlay <- magick::image_read(overlay_file)
      combined <- magick::image_composite(background, overlay, operator = "over")
      magick::image_write(combined, combined_file, quality = 95)
      cat("  Combined PNG saved:", combined_file, "\n")
    }, error = function(e) {
      cat("  Failed to combine:", e$message, "\n")
    })
  }

  return(list(
    svg_file = svg_file,
    background_file = background_file,
    overlay_file = overlay_file,
    parameters = puzzle_svg$parameters
  ))
}

# Generate puzzle variations using SVG conversion
cat("=== SVG to PNG Conversion Approach ===\n")

# Check available tools
cat("Checking available conversion tools...\n")
has_rsvg <- requireNamespace("rsvg", quietly = TRUE)
has_magick <- requireNamespace("magick", quietly = TRUE)
has_inkscape <- nzchar(Sys.which("inkscape"))
has_convert <- nzchar(Sys.which("convert"))

cat("  rsvg package:", if(has_rsvg) "✓ Available" else "✗ Not available", "\n")
cat("  magick package:", if(has_magick) "✓ Available" else "✗ Not available", "\n")
cat("  Inkscape CLI:", if(has_inkscape) "✓ Available" else "✗ Not available", "\n")
cat("  ImageMagick CLI:", if(has_convert) "✓ Available" else "✗ Not available", "\n")

if (!has_rsvg && !has_magick && !has_inkscape && !has_convert) {
  cat("\nERROR: No SVG conversion tools available!\n")
  cat("Please install one of:\n")
  cat("  - R packages: install.packages(c('rsvg', 'magick'))\n")
  cat("  - Inkscape: https://inkscape.org/\n")
  cat("  - ImageMagick: https://imagemagick.org/\n")
} else {
  cat("\nGenerating puzzle variations...\n\n")

  # Generate different puzzles
  # result1 <- generate_svg_puzzle_layers(
  #   seed = 2024, rings = 4, diameter = 200,
  #   tabsize = 25, jitter = 4,
  #   base_filename = "svg_puzzle_4rings",
  #   size_px = 2000
  # )
  #
  # result2 <- generate_svg_puzzle_layers(
  #   seed = 5555, rings = 6, diameter = 220,
  #   tabsize = 30, jitter = 6,
  #   base_filename = "svg_puzzle_6rings",
  #   size_px = 2400
  # )

  result3 <- generate_svg_puzzle_layers(
    seed = 9999, rings = 3, diameter = 200,
    tabsize = 20, jitter = 3,
    base_filename = "svg_puzzle_3rings",
    size_px = 2000
  )

  cat("\nAll SVG-based puzzles generated successfully!\n")
}
