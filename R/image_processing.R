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
  
  # Ensure output directory exists
  if (!dir.exists("output")) {
    dir.create("output", recursive = TRUE)
  }
  
  # Add output/ prefix if not already present
  if (!grepl("^output/", output_file)) {
    output_file <- file.path("output", output_file)
  }
  
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

#' Combine background and overlay images
#' @param background_file Path to background PNG file
#' @param overlay_file Path to overlay PNG file
#' @param combined_file Output path for combined image
#' @param transparent_background Logical, whether to make final background transparent
#' @return TRUE if successful, FALSE otherwise
#' @export
combine_image_layers <- function(background_file, overlay_file, combined_file, transparent_background = FALSE) {
  
  # Ensure output directory exists and add prefix if needed
  if (!dir.exists("output")) {
    dir.create("output", recursive = TRUE)
  }
  
  if (!grepl("^output/", combined_file)) {
    combined_file <- file.path("output", combined_file)
  }
  
  if (requireNamespace("magick", quietly = TRUE)) {
    cat("  Combining layers...\n")
    
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
        
        cat("  Applied circular transparency mask\n")
      }
      
      magick::image_write(combined, combined_file, quality = 95)
      cat("  Combined PNG saved:", combined_file, "\n")
      return(TRUE)
    }, error = function(e) {
      cat("  Failed to combine:", e$message, "\n")
      return(FALSE)
    })
  } else {
    cat("  magick package not available for layer combination\n")
    return(FALSE)
  }
}

#' Check available conversion tools and report status
#' @return List of available tools
#' @export
check_conversion_tools <- function() {
  
  cat("Checking available conversion tools...\n")
  
  has_rsvg <- requireNamespace("rsvg", quietly = TRUE)
  has_magick <- requireNamespace("magick", quietly = TRUE)
  has_inkscape <- nzchar(Sys.which("inkscape"))
  has_convert <- nzchar(Sys.which("convert"))
  
  cat("  rsvg package:", if(has_rsvg) "[OK] Available" else "[X] Not available", "\n")
  cat("  magick package:", if(has_magick) "[OK] Available" else "[X] Not available", "\n")
  cat("  Inkscape CLI:", if(has_inkscape) "[OK] Available" else "[X] Not available", "\n")
  cat("  ImageMagick CLI:", if(has_convert) "[OK] Available" else "[X] Not available", "\n")
  
  tools_available <- has_rsvg || has_magick || has_inkscape || has_convert
  
  if (!tools_available) {
    cat("\nERROR: No SVG conversion tools available!\n")
    cat("Please install one of:\n")
    cat("  - R packages: install.packages(c('rsvg', 'magick'))\n")
    cat("  - Inkscape: https://inkscape.org/\n")
    cat("  - ImageMagick: https://imagemagick.org/\n")
  }
  
  return(list(
    rsvg = has_rsvg,
    magick = has_magick,
    inkscape = has_inkscape,
    imagemagick = has_convert,
    any_available = tools_available
  ))
}