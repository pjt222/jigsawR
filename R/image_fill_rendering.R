# Image Fill Rendering for SVG
#
# Provides SVG rendering support for image-filled puzzle pieces.
# Used by SNIC puzzles where each piece shows its region of the source image.
#
# Implements Issue #80: Image fill support for SNIC puzzle type

# ============================================================================
# Image Encoding
# ============================================================================

#' Encode image as base64 data URI
#'
#' Loads an image and encodes it as a base64 data URI string suitable
#' for embedding in SVG.
#'
#' @param image_path Path to the image file
#' @return Base64 data URI string (e.g., "data:image/jpeg;base64,...")
#'
#' @keywords internal
encode_image_base64 <- function(image_path) {
  if (!has_magick()) {
    cli::cli_abort(c(
      "Package {.pkg magick} is required for image fill rendering.",
      "i" = "Install with: {.code install.packages('magick')}"
    ))
  }

  if (!file.exists(image_path)) {
    cli::cli_abort("Image file not found: {.file {image_path}}")
  }

  img <- magick::image_read(image_path)

  # Write to raw JPEG bytes
  raw_bytes <- magick::image_write(img, format = "jpeg", quality = 90)
  encoded <- base64enc::base64encode(raw_bytes)

  paste0("data:image/jpeg;base64,", encoded)
}

# ============================================================================
# SVG Clip Path Generation
# ============================================================================

#' Render SVG clip path definitions for pieces
#'
#' Generates SVG <defs> with <clipPath> elements for each piece,
#' allowing image regions to be clipped to piece shapes.
#'
#' @param pieces List of piece objects with path data
#' @return SVG string containing <defs> with clip paths
#'
#' @keywords internal
render_image_clip_defs <- function(pieces) {
  clip_paths <- sapply(seq_along(pieces), function(i) {
    piece <- pieces[[i]]
    piece_id <- piece$id %||% i
    sprintf('  <clipPath id="piece-%d-clip"><path d="%s" /></clipPath>',
            piece_id, piece$path)
  })

  paste0("<defs>\n", paste(clip_paths, collapse = "\n"), "\n</defs>")
}

# ============================================================================
# Image-Filled Piece Rendering
# ============================================================================

#' Render image-filled puzzle pieces
#'
#' Generates SVG elements where each piece shows its region of the source image,
#' with optional stroke outlines.
#'
#' @param pieces List of piece objects
#' @param image_data_uri Base64 data URI of the source image
#' @param canvas_size Canvas dimensions c(width, height)
#' @param canvas_offset ViewBox offset c(x, y)
#' @param stroke_width Stroke width for piece outlines
#' @param colors Vector of stroke colors per piece
#' @param opacity Piece opacity (0.0 to 1.0)
#' @param image_width Original image width in pixels
#' @param image_height Original image height in pixels
#' @return Character vector of SVG group elements
#'
#' @keywords internal
render_image_filled_pieces <- function(pieces, image_data_uri, canvas_size,
                                        canvas_offset = c(0, 0),
                                        stroke_width = 1.5, colors = "black",
                                        opacity = 1.0,
                                        image_width = NULL, image_height = NULL) {
  if (length(colors) == 1) {
    colors <- rep(colors, length(pieces))
  }

  # Image dimensions in canvas mm units
  img_w <- canvas_size[1]  # width
  img_h <- canvas_size[2]  # height
  # If canvas_size is c(height, width), swap
  if (!is.null(image_width) && !is.null(image_height)) {
    # Use actual canvas_size values (already in mm from generate_puzzle)
    img_w <- canvas_size[2]  # width is second element
    img_h <- canvas_size[1]  # height is first element
  }

  sapply(seq_along(pieces), function(i) {
    piece <- pieces[[i]]
    piece_id <- piece$id %||% i
    color <- colors[min(i, length(colors))]

    opacity_attr <- if (opacity < 1.0) sprintf(' opacity="%.2f"', opacity) else ""

    # Clipped image group + stroke outline
    sprintf(paste0(
      '<g%s>\n',
      '  <clipPath id="piece-%d-clip"><path d="%s" /></clipPath>\n',
      '  <g clip-path="url(#piece-%d-clip)">\n',
      '    <image href="%s" x="0" y="0" width="%.1f" height="%.1f" preserveAspectRatio="none" />\n',
      '  </g>\n',
      '  <path d="%s" fill="none" stroke="%s" stroke-width="%.1f" />\n',
      '</g>'),
      opacity_attr,
      piece_id, piece$path,
      piece_id,
      image_data_uri, img_w, img_h,
      piece$path, color, stroke_width
    )
  })
}
