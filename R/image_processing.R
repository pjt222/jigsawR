# Image Processing and Layer Combination Functions
# Part of the jigsawR package

#' Convert SVG to PNG using available tools
#' @param svg_content SVG content as string
#' @param output_file Output PNG filename
#' @param width_px Width in pixels
#' @param height_px Height in pixels
#' @return TRUE if successful, FALSE otherwise
#' @export
convert_svg_to_png <- function(svg_content, output_file, width_px = 2000, height_px = 2000) {
  
  # Ensure parent directory exists
  output_parent <- dirname(output_file)
  if (nzchar(output_parent) && output_parent != "." && !dir.exists(output_parent)) {
    dir.create(output_parent, recursive = TRUE)
  }

  # Save SVG to temporary file
  temp_svg <- tempfile(fileext = ".svg")
  writeLines(svg_content, temp_svg)

  log_info("Converting SVG to PNG: {.file {output_file}}")

  # Try different conversion methods in order of preference

  # Method 1: Try rsvg package (R-based, most reliable if available)
  if (requireNamespace("rsvg", quietly = TRUE)) {
    log_info("Using rsvg package...")
    tryCatch({
      png_data <- rsvg::rsvg_png(temp_svg, width = width_px, height = height_px)
      writeBin(png_data, output_file)
      unlink(temp_svg)
      return(TRUE)
    }, error = function(e) {
      log_warn("rsvg failed: {e$message}")
    })
  }

  # Method 2: Try magick package
  if (requireNamespace("magick", quietly = TRUE)) {
    log_info("Using magick package...")
    tryCatch({
      # Read SVG content directly instead of file path
      img <- magick::image_read(svg_content, density = 300)
      img <- magick::image_resize(img, paste0(width_px, "x", height_px))
      magick::image_write(img, output_file, format = "PNG")
      unlink(temp_svg)
      return(TRUE)
    }, error = function(e) {
      log_warn("magick failed: {e$message}")
    })
  }

  # Method 3: Try Inkscape command line (if available)
  inkscape_cmd <- Sys.which("inkscape")
  if (nzchar(inkscape_cmd)) {
    log_info("Using Inkscape command line...")
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
    log_info("Using ImageMagick convert...")
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
  log_error("No suitable SVG conversion tool found!")
  log_info("Install one of: rsvg, magick packages, or Inkscape/ImageMagick CLI tools")
  return(FALSE)
}

#' Combine background and overlay images
#' @param background_file Path to background PNG file
#' @param overlay_file Path to overlay PNG file
#' @param combined_file Output path for combined image
#' @param transparent_background Logical, whether to make final background transparent
#' @return TRUE if successful, FALSE otherwise
#' @export
combine_image_layers <- function(background_file, overlay_file, combined_file, transparent_background = FALSE) {
  
  # Ensure parent directory exists
  output_parent <- dirname(combined_file)
  if (nzchar(output_parent) && output_parent != "." && !dir.exists(output_parent)) {
    dir.create(output_parent, recursive = TRUE)
  }

  if (requireNamespace("magick", quietly = TRUE)) {
    log_info("Combining layers...")

    tryCatch({
      background <- magick::image_read(background_file)
      overlay <- magick::image_read(overlay_file)

      # Combine the layers first
      combined <- magick::image_composite(background, overlay, operator = "over")

      if (transparent_background) {
        # Create a circular mask to make areas outside the circle transparent
        img_info <- magick::image_info(combined)
        width <- img_info$width
        height <- img_info$height

        # Create circular mask - everything outside circle becomes transparent
        # Calculate center and radius based on image dimensions
        center_x <- width / 2
        center_y <- height / 2
        # Puzzle circle diameter / canvas_size ratio ~= 0.833 (180/216)
        # So radius = (diameter/canvas_size) * (image_size/2) = 0.833 * (width/2)
        radius <- min(width, height) * 0.833 / 2  # More precise puzzle circle size

        # Create SVG circle mask - white circle on black background
        # This makes the circle area opaque and everything outside transparent
        mask_svg <- sprintf(
          '<svg width="%d" height="%d"><rect width="100%%" height="100%%" fill="black"/><circle cx="%g" cy="%g" r="%g" fill="white"/></svg>',
          width, height, center_x, center_y, radius
        )

        # Apply circular mask to make background transparent outside circle
        mask <- magick::image_read_svg(mask_svg, width = width, height = height)
        combined <- magick::image_composite(combined, mask, operator = "copyopacity")

        log_success("Applied circular transparency mask")
      }

      magick::image_write(combined, combined_file, quality = 95)
      log_success("Combined PNG saved: {.file {combined_file}}")
      return(TRUE)
    }, error = function(e) {
      log_error("Failed to combine: {e$message}")
      return(FALSE)
    })
  } else {
    log_warn("magick package not available for layer combination")
    return(FALSE)
  }
}

#' Check available conversion tools and report status
#' @return List of available tools
#' @export
check_conversion_tools <- function() {

  log_subheader("Checking available conversion tools")

  has_rsvg <- requireNamespace("rsvg", quietly = TRUE)
  has_magick <- requireNamespace("magick", quietly = TRUE)
  has_inkscape <- nzchar(Sys.which("inkscape"))
  has_convert <- nzchar(Sys.which("convert"))

  if (has_rsvg) {
    log_success("rsvg package: Available")
  } else {
    log_warn("rsvg package: Not available")
  }

  if (has_magick) {
    log_success("magick package: Available")
  } else {
    log_warn("magick package: Not available")
  }

  if (has_inkscape) {
    log_success("Inkscape CLI: Available")
  } else {
    log_warn("Inkscape CLI: Not available")
  }

  if (has_convert) {
    log_success("ImageMagick CLI: Available")
  } else {
    log_warn("ImageMagick CLI: Not available")
  }

  tools_available <- has_rsvg || has_magick || has_inkscape || has_convert

  if (!tools_available) {
    log_error("No SVG conversion tools available!")
    log_info("Please install one of:")
    log_info("  - R packages: install.packages(c('rsvg', 'magick'))")
    log_info("  - Inkscape: {.url https://inkscape.org/}")
    log_info("  - ImageMagick: {.url https://imagemagick.org/}")
  }

  return(list(
    rsvg = has_rsvg,
    magick = has_magick,
    inkscape = has_inkscape,
    imagemagick = has_convert,
    any_available = tools_available
  ))
}