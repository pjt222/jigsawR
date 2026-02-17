# Noise-based fills using ambient package
# Issue #57: Procedural noise textures for puzzle backgrounds and pieces

#' Generate noise texture as base64-encoded PNG
#'
#' Creates a procedural noise texture using the ambient package and
#' encodes it as a base64 PNG string for SVG embedding.
#'
#' @param width Width of noise texture in pixels (default: 256)
#' @param height Height of noise texture in pixels (default: 256)
#' @param noise_type Type of noise: "perlin", "simplex", "worley", "cubic",
#'   "value", "white" (default: "perlin")
#' @param frequency Base frequency of noise (default: 0.02, higher = more detail)
#' @param octaves Number of octaves for fractal noise (default: 4)
#' @param lacunarity Frequency multiplier per octave (default: 2)
#' @param gain Amplitude multiplier per octave (default: 0.5)
#' @param seed Random seed for reproducibility (default: NULL = random)
#' @param color_low Color for lowest noise values (default: "#1a1a2e")
#' @param color_high Color for highest noise values (default: "#eaeaea")
#' @param color_mid Optional middle color for 3-stop gradient (default: NULL)
#' @return Base64-encoded PNG string (data URI format)
#'
#' @examples
#' \dontrun{
#' # Basic Perlin noise
#' noise_png <- generate_noise_texture(seed = 42)
#'
#' # High-frequency Worley noise
#' noise_png <- generate_noise_texture(
#'   noise_type = "worley",
#'   frequency = 0.05,
#'   color_low = "#000033",
#'   color_high = "#66ccff"
#' )
#' }
generate_noise_texture <- function(width = 256,
                                   height = 256,
                                   noise_type = "perlin",
                                   frequency = 0.02,
                                   octaves = 4,
                                   lacunarity = 2,
                                   gain = 0.5,
                                   seed = NULL,
                                   color_low = "#1a1a2e",
                                   color_high = "#eaeaea",
                                   color_mid = NULL) {

  # Check for required packages
  check_ambient_available()

  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Generate noise using ambient's simple interface
  # noise_* functions take dimension vector c(width, height)
  # Note: noise_white only takes 'dim' parameter, no octaves/frequency
  if (noise_type == "white") {
    # White noise is simple random values, no fractal parameters
    noise_matrix <- ambient::noise_white(dim = c(height, width))
  } else {
    noise_fn <- switch(noise_type,
      "perlin" = ambient::noise_perlin,
      "simplex" = ambient::noise_simplex,
      "worley" = ambient::noise_worley,
      "cubic" = ambient::noise_cubic,
      "value" = ambient::noise_value,
      ambient::noise_perlin  # Default
    )

    # Generate noise matrix
    # The frequency parameter scales the coordinate space
    noise_matrix <- noise_fn(
      dim = c(height, width),
      frequency = frequency,
      octaves = octaves,
      lacunarity = lacunarity,
      gain = gain
    )
  }

  # Normalize noise values to [0, 1]
  noise_values <- ambient::normalise(noise_matrix)

  # Convert to color matrix
  color_matrix <- noise_to_colors(noise_values, width, height,
                                  color_low, color_high, color_mid)

  # Encode as PNG
  encode_noise_png(color_matrix, width, height)
}


#' Convert noise values to color matrix
#'
#' Maps normalized noise values [0,1] to colors using a gradient.
#'
#' @param noise_matrix Matrix of normalized noise values (height x width)
#' @param width Image width
#' @param height Image height
#' @param color_low Color for value 0
#' @param color_high Color for value 1
#' @param color_mid Optional color for value 0.5
#' @return 3D array (height x width x 3) of RGB values [0,1]
#' @keywords internal
noise_to_colors <- function(noise_matrix, width, height,
                            color_low, color_high, color_mid = NULL) {

  # Parse hex colors to RGB
  rgb_low <- grDevices::col2rgb(color_low) / 255
  rgb_high <- grDevices::col2rgb(color_high) / 255

  # Ensure noise_matrix is a matrix with correct dimensions
  if (!is.matrix(noise_matrix)) {
    noise_matrix <- matrix(noise_matrix, nrow = height, ncol = width, byrow = TRUE)
  }

  # Create RGB arrays
  r_channel <- matrix(0, nrow = height, ncol = width)
  g_channel <- matrix(0, nrow = height, ncol = width)
  b_channel <- matrix(0, nrow = height, ncol = width)

  if (!is.null(color_mid)) {
    # 3-stop gradient: low -> mid -> high
    rgb_mid <- grDevices::col2rgb(color_mid) / 255

    # Values 0-0.5 interpolate low->mid, 0.5-1 interpolate mid->high
    below_mid <- noise_matrix <= 0.5
    t_low <- noise_matrix * 2  # Scale [0, 0.5] to [0, 1]
    t_high <- (noise_matrix - 0.5) * 2  # Scale [0.5, 1] to [0, 1]

    r_channel[below_mid] <- rgb_low[1] + t_low[below_mid] * (rgb_mid[1] - rgb_low[1])
    g_channel[below_mid] <- rgb_low[2] + t_low[below_mid] * (rgb_mid[2] - rgb_low[2])
    b_channel[below_mid] <- rgb_low[3] + t_low[below_mid] * (rgb_mid[3] - rgb_low[3])

    r_channel[!below_mid] <- rgb_mid[1] + t_high[!below_mid] * (rgb_high[1] - rgb_mid[1])
    g_channel[!below_mid] <- rgb_mid[2] + t_high[!below_mid] * (rgb_high[2] - rgb_mid[2])
    b_channel[!below_mid] <- rgb_mid[3] + t_high[!below_mid] * (rgb_high[3] - rgb_mid[3])
  } else {
    # 2-stop gradient: linear interpolation
    r_channel <- rgb_low[1] + noise_matrix * (rgb_high[1] - rgb_low[1])
    g_channel <- rgb_low[2] + noise_matrix * (rgb_high[2] - rgb_low[2])
    b_channel <- rgb_low[3] + noise_matrix * (rgb_high[3] - rgb_low[3])
  }

  # Combine into 3D array (height x width x 3)
  array(c(r_channel, g_channel, b_channel), dim = c(height, width, 3))
}


#' Encode color matrix as base64 PNG
#'
#' Uses the png package to write the color matrix to a PNG file
#' and encodes it as a base64 data URI.
#'
#' @param color_matrix 3D array (height x width x 3) of RGB values [0,1]
#' @param width Image width
#' @param height Image height
#' @return Base64-encoded PNG data URI string
#' @keywords internal
encode_noise_png <- function(color_matrix, width, height) {

  # Check for png package
  if (!requireNamespace("png", quietly = TRUE)) {
    stop("Package 'png' is required for noise textures. Install with: install.packages('png')")
  }

  # Write to temporary file
  tmp_file <- tempfile(fileext = ".png")
  on.exit(unlink(tmp_file), add = TRUE)

  png::writePNG(color_matrix, tmp_file)

  # Read and encode as base64
  raw_bytes <- readBin(tmp_file, "raw", file.info(tmp_file)$size)
  base64_str <- base64enc::base64encode(raw_bytes)

  # Return as data URI
  paste0("data:image/png;base64,", base64_str)
}


#' Check if ambient package is available
#'
#' @return TRUE if all required packages are available
#' @keywords internal
check_ambient_available <- function() {
  if (!requireNamespace("ambient", quietly = TRUE)) {
    stop(
      "Package 'ambient' is required for noise textures.\n",
      "Install with: install.packages('ambient')",
      call. = FALSE
    )
  }
  if (!requireNamespace("base64enc", quietly = TRUE)) {
    stop(
      "Package 'base64enc' is required for noise textures.\n",
      "Install with: install.packages('base64enc')",
      call. = FALSE
    )
  }
  invisible(TRUE)
}


#' Check if object is a noise fill specification
#'
#' @param x Object to check
#' @return TRUE if x is a noise fill spec, FALSE otherwise
#'
#' @examples
#' is_noise_fill_spec(list(type = "noise", noise_type = "perlin"))
#' is_noise_fill_spec("#FF0000")
#' is_noise_fill_spec(list(type = "gradient"))
#'
#' @export
is_noise_fill_spec <- function(x) {
  is.list(x) && !is.null(x$type) && x$type == "noise"
}


#' Create SVG pattern definition for noise texture
#'
#' Generates an SVG `<defs>` section containing a pattern element
#' with the noise texture embedded as a base64 PNG image.
#'
#' @param noise_data_uri Base64-encoded PNG data URI from generate_noise_texture()
#' @param pattern_id Unique ID for the pattern (default: "noisePattern")
#' @param width Pattern width (default: 256)
#' @param height Pattern height (default: 256)
#' @param pattern_units SVG patternUnits attribute: "userSpaceOnUse" or
#'   "objectBoundingBox" (default: "userSpaceOnUse")
#' @return SVG defs string containing the pattern definition
create_noise_pattern_defs <- function(noise_data_uri,
                                       pattern_id = "noisePattern",
                                       width = 256,
                                       height = 256,
                                       pattern_units = "userSpaceOnUse") {

  if (pattern_units == "objectBoundingBox") {
    # For objectBoundingBox, use relative coordinates for pattern bounds
    # IMPORTANT: patternContentUnits defaults to userSpaceOnUse, so image dimensions
    # would be interpreted as pixels (width="1" = 1 pixel, not 100%)
    # Solution: Use patternContentUnits="objectBoundingBox" so image width/height
    # are also relative (0-1 range = 0-100% of bounding box)
    # Use xlink:href for browser compatibility (especially in HTML contexts)
    pattern_def <- sprintf(
      '<defs>\n  <pattern id="%s" patternUnits="objectBoundingBox" patternContentUnits="objectBoundingBox" width="1" height="1">\n    <image xlink:href="%s" x="0" y="0" width="1" height="1" preserveAspectRatio="none"/>\n  </pattern>\n</defs>',
      pattern_id, noise_data_uri
    )
  } else {
    # For userSpaceOnUse, use absolute pixel coordinates
    # Use xlink:href for browser compatibility (especially in HTML contexts)
    pattern_def <- sprintf(
      '<defs>\n  <pattern id="%s" patternUnits="userSpaceOnUse" x="0" y="0" width="%d" height="%d">\n    <image xlink:href="%s" x="0" y="0" width="%d" height="%d"/>\n  </pattern>\n</defs>',
      pattern_id, width, height, noise_data_uri, width, height
    )
  }

  pattern_def
}


#' Create noise fill specification
#'
#' Creates a list specification for noise-based fills that can be passed
#' to render_puzzle_svg() or generate_puzzle().
#'
#' @param noise_type Type of noise: "perlin", "simplex", "worley", "cubic",
#'   "value", "white" (default: "perlin")
#' @param frequency Base frequency (default: 0.02)
#' @param octaves Number of octaves (default: 4)
#' @param lacunarity Frequency multiplier per octave (default: 2)
#' @param gain Amplitude multiplier per octave (default: 0.5)
#' @param seed Random seed (default: NULL)
#' @param color_low Color for lowest values (default: "#1a1a2e")
#' @param color_high Color for highest values (default: "#eaeaea")
#' @param color_mid Optional middle color (default: NULL)
#' @param resolution Texture resolution in pixels (default: 256)
#' @param target Where to apply: "background", "all_pieces", or "individual" (default: "background")
#' @return List with type="noise" and all parameters
#' @export
#'
#' @examples
#' \dontrun{
#' # Create noise fill for background
#' bg_noise <- noise_fill_spec(
#'   noise_type = "perlin",
#'   frequency = 0.03,
#'   color_low = "#1a1a2e",
#'   color_high = "#4a4a6e",
#'   target = "background"
#' )
#'
#' # Use in puzzle generation
#' result <- generate_puzzle(
#'   type = "rectangular",
#'   grid = c(3, 4),
#'   size = c(200, 150),
#'   background = bg_noise
#' )
#' }
noise_fill_spec <- function(noise_type = "perlin",
                            frequency = 0.02,
                            octaves = 4,
                            lacunarity = 2,
                            gain = 0.5,
                            seed = NULL,
                            color_low = "#1a1a2e",
                            color_high = "#eaeaea",
                            color_mid = NULL,
                            resolution = 256,
                            target = "background") {

  list(
    type = "noise",
    noise_type = noise_type,
    frequency = frequency,
    octaves = octaves,
    lacunarity = lacunarity,
    gain = gain,
    seed = seed,
    color_low = color_low,
    color_high = color_high,
    color_mid = color_mid,
    resolution = resolution,
    target = target
  )
}


#' Render noise-based background
#'
#' Creates SVG elements for a noise-textured background.
#' Uses objectBoundingBox so noise scales smoothly with canvas size.
#'
#' @param noise_spec Noise specification from noise_fill_spec()
#' @param canvas_size c(width, height) of the canvas
#' @param canvas_offset c(x, y) offset for viewBox
#' @return SVG defs + rect elements string
#' @keywords internal
render_noise_background <- function(noise_spec, canvas_size, canvas_offset = NULL) {

  if (is.null(canvas_offset)) {
    canvas_offset <- c(0, 0)
  }

  # Generate noise texture at higher resolution for quality
  # Use resolution from spec, defaulting to 512 for backgrounds
  resolution <- noise_spec$resolution %||% 512
  noise_png <- generate_noise_texture(
    width = resolution,
    height = resolution,
    noise_type = noise_spec$noise_type %||% "perlin",
    frequency = noise_spec$frequency %||% 0.02,
    octaves = noise_spec$octaves %||% 4,
    lacunarity = noise_spec$lacunarity %||% 2,
    gain = noise_spec$gain %||% 0.5,
    seed = noise_spec$seed,
    color_low = noise_spec$color_low %||% "#1a1a2e",
    color_high = noise_spec$color_high %||% "#eaeaea",
    color_mid = noise_spec$color_mid
  )

  # Create pattern definition with objectBoundingBox
  # This scales the noise to fill the rect, avoiding tile edges
  pattern_defs <- create_noise_pattern_defs(
    noise_png,
    pattern_id = "bgNoisePattern",
    width = resolution,
    height = resolution,
    pattern_units = "objectBoundingBox"
  )

  # Create background rect
  rect_element <- sprintf(
    '<rect x="%.2f" y="%.2f" width="%.2f" height="%.2f" fill="url(#bgNoisePattern)"/>',
    canvas_offset[1], canvas_offset[2],
    canvas_size[1], canvas_size[2]
  )

  paste(pattern_defs, rect_element, sep = "\n")
}


#' Render noise-based piece fill pattern definition
#'
#' Creates SVG defs section with noise pattern for piece fills.
#' Uses objectBoundingBox units so each piece gets its own scaled texture.
#'
#' @param noise_spec Noise specification from noise_fill_spec()
#' @return SVG defs element string
#' @keywords internal
render_noise_piece_fill_defs <- function(noise_spec) {

  # Generate noise texture
  noise_png <- generate_noise_texture(
    width = noise_spec$resolution %||% 256,
    height = noise_spec$resolution %||% 256,
    noise_type = noise_spec$noise_type %||% "perlin",
    frequency = noise_spec$frequency %||% 0.02,
    octaves = noise_spec$octaves %||% 4,
    lacunarity = noise_spec$lacunarity %||% 2,
    gain = noise_spec$gain %||% 0.5,
    seed = noise_spec$seed,
    color_low = noise_spec$color_low %||% "#1a1a2e",
    color_high = noise_spec$color_high %||% "#eaeaea",
    color_mid = noise_spec$color_mid
  )

  # Create pattern with objectBoundingBox so it scales to each piece
  create_noise_pattern_defs(
    noise_png,
    pattern_id = "pieceFillNoisePattern",
    pattern_units = "objectBoundingBox"
  )
}


#' Generate per-piece noise fills with unique seeds
#'
#' Creates a vector of noise data URIs, one per piece, each with a
#' unique seed derived from the base seed and piece index.
#'
#' @param n_pieces Number of pieces
#' @param noise_spec Base noise specification
#' @return Vector of base64-encoded PNG data URIs
#' @keywords internal
generate_per_piece_noise_fills <- function(n_pieces, noise_spec) {

  base_seed <- noise_spec$seed %||% sample.int(10000, 1)

  sapply(seq_len(n_pieces), function(i) {
    generate_noise_texture(
      width = noise_spec$resolution %||% 128,  # Smaller for per-piece
      height = noise_spec$resolution %||% 128,
      noise_type = noise_spec$noise_type %||% "perlin",
      frequency = noise_spec$frequency %||% 0.02,
      octaves = noise_spec$octaves %||% 4,
      lacunarity = noise_spec$lacunarity %||% 2,
      gain = noise_spec$gain %||% 0.5,
      seed = base_seed + i,  # Unique seed per piece
      color_low = noise_spec$color_low %||% "#1a1a2e",
      color_high = noise_spec$color_high %||% "#eaeaea",
      color_mid = noise_spec$color_mid
    )
  })
}


#' Get available noise types
#'
#' Returns a named vector of available noise types with descriptions.
#'
#' @return Named character vector of noise types
get_noise_types <- function() {
  c(
    "perlin" = "Perlin noise - smooth, organic patterns",
    "simplex" = "Simplex noise - similar to Perlin, less artifacts",
    "worley" = "Worley/cellular noise - bubble-like patterns",
    "cubic" = "Cubic noise - smoother than Perlin",
    "value" = "Value noise - blocky, grid-aligned patterns",
    "white" = "White noise - random static"
  )
}
