# Unified SVG Renderer
# Part of Epic #32 - Unified Puzzle Generation Pipeline
# Renders positioned pieces to SVG with consistent styling

#' Render positioned pieces to SVG
#'
#' Takes output from apply_piece_positioning() and renders to a complete SVG string.
#' Supports solid backgrounds, gradient backgrounds, or no background.
#'
#' @param positioned Output from apply_piece_positioning()
#' @param fill Fill color for pieces ("none" or a color value)
#' @param stroke_width Line width for piece strokes
#' @param colors Color palette for piece strokes (NULL = use default)
#' @param palette Viridis palette name (NULL = use config default)
#' @param background Background specification:
#'   - "none": No background
#'   - "white", "#FFFFFF", etc.: Solid color background
#'   - list(type="gradient", ...): Gradient background
#' @param opacity Piece opacity (0.0 to 1.0)
#' @param show_labels Logical; if TRUE, display piece ID labels at piece centers
#' @param label_color Color for piece labels (default: "black")
#' @param label_size Font size for labels in mm (default: auto-calculated based on piece size)
#' @return Complete SVG string
#' @export
render_puzzle_svg <- function(positioned, fill = "none", stroke_width = 1.5,
                               colors = NULL, palette = NULL,
                               background = "white", opacity = 1.0,
                               show_labels = FALSE, label_color = "black",
                               label_size = NULL) {

  # Get number of pieces for color generation
  n_pieces <- length(positioned$pieces)

  # Generate colors if not provided
  if (is.null(colors)) {
    colors <- get_puzzle_colors(n_pieces, palette)
  }

  # Ensure we have enough colors (cycle if needed)
  if (length(colors) < n_pieces) {
    colors <- rep_len(colors, n_pieces)
  }

  # Build SVG components
  svg_header <- build_svg_header(
    positioned$canvas_size,
    positioned$canvas_offset
  )

  # Render background (may include its own defs for background gradient)
  bg_element <- render_background(background, positioned$canvas_size, positioned$canvas_offset)

  # Handle piece fill gradient - create defs section if needed
  piece_fill_defs <- ""
  fill_value <- fill
  if (is.list(fill) && !is.null(fill$type) && fill$type == "gradient") {
    # Create piece gradient definition
    piece_fill_defs <- render_piece_fill_gradient_defs(fill)
    fill_value <- "url(#pieceFillGradient)"
  }

  # Render each piece
  piece_elements <- sapply(seq_along(positioned$pieces), function(i) {
    piece <- positioned$pieces[[i]]
    color <- colors[i]
    render_piece(piece, fill_value, color, stroke_width, opacity)
  })

  # Render labels if requested
  label_elements <- character(0)
  if (show_labels) {
    # Calculate auto label size if not provided
    if (is.null(label_size)) {
      # Estimate piece size from canvas and piece count
      # For rectangular: avg piece dimension
      # For hexagonal: estimate from diameter
      if (!is.null(positioned$parameters$piece_width)) {
        piece_dim <- min(positioned$parameters$piece_width, positioned$parameters$piece_height)
      } else {
        # Fallback: estimate from canvas size and piece count
        canvas_area <- positioned$canvas_size[1] * positioned$canvas_size[2]
        piece_area <- canvas_area / n_pieces
        piece_dim <- sqrt(piece_area)
      }
      # Label size is ~20% of piece dimension, with min/max bounds
      label_size <- max(4, min(20, piece_dim * 0.2))
    }

    label_elements <- sapply(seq_along(positioned$pieces), function(i) {
      piece <- positioned$pieces[[i]]
      render_piece_label(piece, i, label_color, label_size)
    })
  }

  # Combine and close SVG
  # piece_fill_defs goes after header but before bg_element (defs should be early)
  svg_parts <- c(svg_header, piece_fill_defs, bg_element, piece_elements, label_elements, "</svg>")
  svg_parts <- svg_parts[svg_parts != ""]  # Remove empty strings

  paste(svg_parts, collapse = "\n")
}


#' Build SVG header with proper viewBox
#'
#' @param canvas_size c(width, height) of the canvas
#' @param canvas_offset c(x, y) offset for viewBox (default: c(0, 0))
#' @return SVG header string
#' @keywords internal
build_svg_header <- function(canvas_size, canvas_offset = NULL) {

  width <- canvas_size[1]
  height <- canvas_size[2]

  # Handle viewBox offset (for hexagonal puzzles centered at origin)
  if (is.null(canvas_offset)) {
    canvas_offset <- c(0, 0)
  }
  vb_x <- canvas_offset[1]
  vb_y <- canvas_offset[2]

  paste0(
    '<?xml version="1.0" encoding="UTF-8"?>\n',
    '<svg xmlns="http://www.w3.org/2000/svg" version="1.1" ',
    'width="', sprintf("%.2f", width), '" ',
    'height="', sprintf("%.2f", height), '" ',
    'viewBox="', sprintf("%.2f %.2f %.2f %.2f", vb_x, vb_y, width, height), '">'
  )
}


#' Render background element
#'
#' Handles solid color backgrounds, gradients, or no background.
#'
#' @param background Background specification
#' @param canvas_size c(width, height)
#' @param canvas_offset c(x, y) offset
#' @return SVG element string (empty string for "none")
#' @keywords internal
render_background <- function(background, canvas_size, canvas_offset = NULL) {

  if (is.null(canvas_offset)) {
    canvas_offset <- c(0, 0)
  }

  # No background

  if (is.character(background) && background == "none") {
    return("")
  }

  # Solid color background
  if (is.character(background)) {
    return(sprintf(
      '<rect x="%.2f" y="%.2f" width="%.2f" height="%.2f" fill="%s"/>',
      canvas_offset[1], canvas_offset[2],
      canvas_size[1], canvas_size[2],
      background
    ))
  }

  # Gradient background (specified as list)
  if (is.list(background) && !is.null(background$type)) {
    if (background$type == "gradient" || background$type == "radial") {
      return(render_gradient_background(background, canvas_size, canvas_offset))
    }
  }

  # Default: transparent
  ""
}


#' Render gradient background
#'
#' Creates an SVG radial gradient definition and rect element.
#' Supports 2-stop (center, edge) or 3-stop (center, middle, edge) gradients.
#'
#' @param gradient_spec List with gradient specification:
#'   - center (or center_color): Color at center (default: from palette)
#'   - middle: Color at 50% (optional, for 3-stop gradient)
#'   - edge (or edge_color): Color at edge (default: from palette)
#'   - palette: Viridis palette to use for auto colors
#' @param canvas_size c(width, height)
#' @param canvas_offset c(x, y) offset
#' @return SVG defs + rect elements
#' @keywords internal
render_gradient_background <- function(gradient_spec, canvas_size, canvas_offset) {

  # Get colors from spec - support both field name formats
  # Shiny app sends: center, middle, edge
  # Legacy code may send: center_color, edge_color
  center_color <- gradient_spec$center %||% gradient_spec$center_color
  middle_color <- gradient_spec$middle  # Optional, may be NULL
  edge_color <- gradient_spec$edge %||% gradient_spec$edge_color

  # If colors not provided, generate from palette
  if (is.null(center_color) || is.null(edge_color)) {
    palette <- gradient_spec$palette
    colors <- get_puzzle_colors(10, palette)
    if (is.null(center_color)) center_color <- colors[8]  # Lighter at center
    if (is.null(middle_color)) middle_color <- colors[5]  # Mid tone
    if (is.null(edge_color)) edge_color <- colors[2]      # Darker at edge
  }

  # Calculate center point (relative to canvas)
  cx <- 0.5  # Center at 50%
  cy <- 0.5

  gradient_id <- "puzzleGradient"

  # Build gradient definition - 2-stop or 3-stop based on middle_color
  if (!is.null(middle_color) && nzchar(middle_color)) {
    # 3-stop gradient: center (0%), middle (50%), edge (100%)
    gradient_def <- paste0(
      '<defs>\n',
      '  <radialGradient id="', gradient_id, '" cx="', cx, '" cy="', cy, '" r="0.7">\n',
      '    <stop offset="0%" stop-color="', center_color, '"/>\n',
      '    <stop offset="50%" stop-color="', middle_color, '"/>\n',
      '    <stop offset="100%" stop-color="', edge_color, '"/>\n',
      '  </radialGradient>\n',
      '</defs>'
    )
  } else {
    # 2-stop gradient: center (0%), edge (100%)
    gradient_def <- paste0(
      '<defs>\n',
      '  <radialGradient id="', gradient_id, '" cx="', cx, '" cy="', cy, '" r="0.7">\n',
      '    <stop offset="0%" stop-color="', center_color, '"/>\n',
      '    <stop offset="100%" stop-color="', edge_color, '"/>\n',
      '  </radialGradient>\n',
      '</defs>'
    )
  }

  # Build rect with gradient fill
  rect_element <- sprintf(
    '<rect x="%.2f" y="%.2f" width="%.2f" height="%.2f" fill="url(#%s)"/>',
    canvas_offset[1], canvas_offset[2],
    canvas_size[1], canvas_size[2],
    gradient_id
  )

  paste(gradient_def, rect_element, sep = "\n")
}


#' Render piece fill gradient definition
#'
#' Creates SVG defs section with radial gradient for piece fills.
#' Uses objectBoundingBox units so each piece gets a centered gradient.
#'
#' @param gradient_spec List with gradient specification:
#'   - center: Color at center (required)
#'   - middle: Color at 50% (optional)
#'   - edge: Color at edge (required)
#' @return SVG defs element string
#' @keywords internal
render_piece_fill_gradient_defs <- function(gradient_spec) {
  center_color <- gradient_spec$center %||% "#ffffff"
  middle_color <- gradient_spec$middle  # May be NULL
  edge_color <- gradient_spec$edge %||% "#808080"

  gradient_id <- "pieceFillGradient"

  # Build gradient definition - 2-stop or 3-stop based on middle_color
  # Use objectBoundingBox (default) so gradient is relative to each piece
  if (!is.null(middle_color) && nzchar(middle_color)) {
    # 3-stop gradient: center (0%), middle (50%), edge (100%)
    gradient_def <- paste0(
      '<defs>\n',
      '  <radialGradient id="', gradient_id, '" cx="50%" cy="50%" r="70%">\n',
      '    <stop offset="0%" stop-color="', center_color, '"/>\n',
      '    <stop offset="50%" stop-color="', middle_color, '"/>\n',
      '    <stop offset="100%" stop-color="', edge_color, '"/>\n',
      '  </radialGradient>\n',
      '</defs>'
    )
  } else {
    # 2-stop gradient: center (0%), edge (100%)
    gradient_def <- paste0(
      '<defs>\n',
      '  <radialGradient id="', gradient_id, '" cx="50%" cy="50%" r="70%">\n',
      '    <stop offset="0%" stop-color="', center_color, '"/>\n',
      '    <stop offset="100%" stop-color="', edge_color, '"/>\n',
      '  </radialGradient>\n',
      '</defs>'
    )
  }

  return(gradient_def)
}


#' Render a single piece element
#'
#' @param piece Piece object with path, center, etc.
#' @param fill Fill color for the piece
#' @param stroke_color Stroke color
#' @param stroke_width Stroke width
#' @param opacity Opacity (0.0 to 1.0)
#' @return SVG path element string
#' @keywords internal
render_piece <- function(piece, fill, stroke_color, stroke_width, opacity) {

  # Build opacity attribute if not fully opaque
  opacity_attr <- ""
  if (opacity < 1.0) {
    opacity_attr <- sprintf(' opacity="%.2f"', opacity)
  }

  # Build the path element
  sprintf(
    '<path d="%s" fill="%s" stroke="%s" stroke-width="%.2f" stroke-linecap="round" stroke-linejoin="round"%s/>',
    piece$path,
    fill,
    stroke_color,
    stroke_width,
    opacity_attr
  )
}


#' Calculate bounding box center from SVG path
#'
#' Parses an SVG path and calculates the center of its bounding box.
#' This matches how SVG objectBoundingBox gradients center themselves.
#'
#' @param path SVG path d attribute string
#' @return Named vector with x and y center coordinates
#' @keywords internal
calculate_path_bounding_box_center <- function(path) {
  # Extract all coordinate points from the path
  # Pattern matches: M x y, L x y, C x1 y1 x2 y2 x y, A rx ry rot large sweep x y

  # Extract all numbers from the path (handles both comma and space separators)
  # First, remove command letters and split on separators
  coords_str <- gsub("[MLCAZ]", " ", path)
  coords <- as.numeric(unlist(strsplit(trimws(coords_str), "[, ]+")))
  coords <- coords[!is.na(coords)]

  if (length(coords) < 2) {
    return(c(x = 0, y = 0))
  }

  # Extract x,y pairs - we need to parse more carefully for different commands
  # Use parse_svg_path for accurate parsing
  segments <- parse_svg_path(path)

  xs <- c()
  ys <- c()

  for (seg in segments) {
    if (seg$type %in% c("M", "L")) {
      xs <- c(xs, seg$x)
      ys <- c(ys, seg$y)
    } else if (seg$type == "C") {
      # For cubic bezier, include control points for accurate bounds
      xs <- c(xs, seg$cp1x, seg$cp2x, seg$x)
      ys <- c(ys, seg$cp1y, seg$cp2y, seg$y)
    } else if (seg$type == "A") {
      xs <- c(xs, seg$x)
      ys <- c(ys, seg$y)
    }
  }

  if (length(xs) == 0 || length(ys) == 0) {
    return(c(x = 0, y = 0))
  }

  # Calculate bounding box center
  cx <- (min(xs) + max(xs)) / 2
  cy <- (min(ys) + max(ys)) / 2

  return(c(x = cx, y = cy))
}


#' Render a piece label
#'
#' Creates an SVG text element positioned at the piece's geometric center.
#' Uses the bounding box center calculated from the actual path geometry
#' to match where gradient fills appear centered.
#'
#' @param piece Piece object with path and center coordinates
#' @param index Piece index (1-based) to display as label
#' @param color Label text color
#' @param font_size Font size in mm
#' @return SVG text element string
#' @keywords internal
render_piece_label <- function(piece, index, color, font_size) {
  # Calculate geometric center from actual path
  # This matches SVG objectBoundingBox gradient centering
  bbox_center <- calculate_path_bounding_box_center(piece$path)
  cx <- bbox_center["x"]
  cy <- bbox_center["y"]

  # Build SVG text element centered on piece
  # Use dominant-baseline and text-anchor for proper centering
  sprintf(
    '<text x="%.2f" y="%.2f" font-family="sans-serif" font-size="%.1f" font-weight="bold" fill="%s" text-anchor="middle" dominant-baseline="central">%d</text>',
    cx, cy, font_size, color, index
  )
}


#' Save rendered SVG to file
#'
#' @param svg_content SVG string from render_puzzle_svg()
#' @param filename Output filename
#' @param output_dir Output directory (default: "output")
#' @return Invisible path to saved file
#' @export
save_puzzle_svg <- function(svg_content, filename, output_dir = "output") {

  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Build full path
  if (!grepl(paste0("^", output_dir), filename)) {
    filepath <- file.path(output_dir, filename)
  } else {
    filepath <- filename
  }

  # Write file
  writeLines(svg_content, filepath)
  log_success("SVG saved: {.file {filepath}}")

  invisible(filepath)
}


#' Convenience function to generate and render puzzle
#'
#' Combines piece generation, positioning, and rendering into one call.
#'
#' @param type "rectangular" or "hexagonal"
#' @param seed Random seed
#' @param grid Grid specification
#' @param size Size specification
#' @param offset Separation offset (0 = compact)
#' @param fill Fill color for pieces
#' @param stroke_width Stroke width
#' @param palette Viridis palette name
#' @param background Background specification
#' @param opacity Piece opacity
#' @param filename Output filename (NULL = don't save)
#' @param ... Additional arguments passed to generate_pieces_internal()
#' @return List with svg, pieces, and parameters
#' @export
generate_and_render_puzzle <- function(type = "rectangular",
                                        seed = NULL,
                                        grid = c(3, 4),
                                        size = c(200, 150),
                                        offset = 0,
                                        fill = "none",
                                        stroke_width = 1.5,
                                        palette = NULL,
                                        background = "white",
                                        opacity = 1.0,
                                        filename = NULL,
                                        ...) {

  # Generate pieces
  pieces <- generate_pieces_internal(
    type = type,
    seed = seed,
    grid = grid,
    size = size,
    ...
  )

  # Apply positioning
  positioned <- apply_piece_positioning(pieces, offset = offset)

  # Render to SVG
  svg_content <- render_puzzle_svg(
    positioned,
    fill = fill,
    stroke_width = stroke_width,
    palette = palette,
    background = background,
    opacity = opacity
  )

  # Save if filename provided
  if (!is.null(filename)) {
    save_puzzle_svg(svg_content, filename)
  }

  # Return result
  list(
    svg = svg_content,
    pieces = positioned,
    parameters = list(
      type = type,
      seed = pieces$parameters$seed,
      grid = grid,
      size = size,
      offset = offset,
      fill = fill,
      stroke_width = stroke_width,
      palette = palette,
      background = background,
      opacity = opacity
    )
  )
}
