# Main SVG Renderer
# Split from unified_renderer.R for maintainability
# Contains primary rendering entry points and SVG utilities

#' Sanitize a string for safe use in SVG attributes
#'
#' Strips characters that could enable SVG injection attacks.
#' Allows alphanumeric, hex color codes, named colors, CSS functions,
#' and common SVG attribute characters.
#'
#' @param value Character string to sanitize
#' @return Sanitized character string safe for SVG attribute interpolation
#' @keywords internal
sanitize_svg_attr <- function(value) {
  if (is.null(value) || is.na(value)) return("none")
  value <- as.character(value)
  # Allow only safe characters for SVG attributes
  gsub("[^a-zA-Z0-9#(),. %_-]", "", value)
}


# put id:"svg_render", label:"SVG Rendering", node_type:"output", input:"positioned", output:"svg_content"
#' Render positioned pieces to SVG
#'
#' Takes output from apply_piece_positioning() and renders to a complete SVG string.
#' Supports solid backgrounds, gradient backgrounds, or no background.
#'
#' @param positioned Output from apply_piece_positioning()
#' @param fill Fill color for pieces ("none", a color value, or gradient list)
#' @param fills Vector of per-piece fill colors (optional, overrides fill parameter).
#'   When provided, each piece gets its own fill color from this vector.
#' @param stroke_width Line width for piece strokes
#' @param colors Color palette for piece strokes (NULL = use default)
#' @param palette Viridis palette name (NULL = use config default)
#' @param palette_invert Logical, if TRUE reverses the palette direction (default: FALSE)
#' @param fill_direction Character, either "forward" (default) or "reverse".
#'   Controls spatial color assignment order. For ring-based puzzles, reverses
#'   within each ring. For rectangular, reverses entire sequence.
#' @param background Background specification:
#'   - "none": No background
#'   - "white", "#FFFFFF", etc.: Solid color background
#'   - list(type="gradient", ...): Gradient background
#' @param opacity Piece opacity (0.0 to 1.0)
#' @param show_labels Logical; if TRUE, display piece ID labels at piece centers
#' @param label_color Color for piece labels (default: "black")
#' @param label_size Font size for labels in mm (default: auto-calculated based on piece size)
#' @param inline Logical; if TRUE, omit XML declaration for inline HTML embedding.
#'   Use TRUE when embedding SVG in HTML (e.g., Shiny apps). Default: FALSE.
#' @param image_path Path to an image file for SNIC image-fill rendering (NULL for no image)
#' @return Complete SVG string
#'
#' @examples
#' \donttest{
#' result <- generate_puzzle(type = "rectangular", grid = c(2, 2),
#'                           size = c(200, 200), seed = 42, save_files = FALSE)
#' svg <- render_puzzle_svg(result, fill = "white", stroke_width = 2)
#' nchar(svg) > 0
#' }
#'
#' @export
render_puzzle_svg <- function(positioned, fill = "none", fills = NULL,
                               stroke_width = 1.5,
                               colors = NULL, palette = NULL, palette_invert = FALSE,
                               fill_direction = "forward",
                               background = "white", opacity = 1.0,
                               show_labels = FALSE, label_color = "black",
                               label_size = NULL, inline = FALSE,
                               image_path = NULL) {

  # Get number of pieces for color generation
  n_pieces <- length(positioned$pieces)

  # Generate colors if not provided (used for strokes)
  if (is.null(colors)) {
    colors <- get_puzzle_colors(n_pieces, palette, invert = palette_invert)
  }

  # Ensure we have enough colors (cycle if needed)
  if (length(colors) < n_pieces) {
    colors <- rep_len(colors, n_pieces)
  }

  # Handle per-piece fills if provided
  # fills parameter overrides fill for per-piece coloring (e.g., palette fills)
  use_per_piece_fills <- !is.null(fills) && length(fills) > 0
  if (use_per_piece_fills && length(fills) < n_pieces) {
    fills <- rep_len(fills, n_pieces)
  }

  # Apply fill direction reordering if requested
  if (!is.null(fill_direction) && fill_direction == "reverse") {
    colors <- reorder_colors_for_direction(colors, positioned$pieces, fill_direction)
    if (use_per_piece_fills) {
      fills <- reorder_colors_for_direction(fills, positioned$pieces, fill_direction)
    }
  }

  # Build SVG components
  svg_header <- build_svg_header(
    positioned$canvas_size,
    positioned$canvas_offset,
    inline = inline
  )

  # Render background (may include its own defs for background gradient)
  bg_element <- render_background(background, positioned$canvas_size, positioned$canvas_offset)

  # Handle piece fill gradient/noise - create defs section if needed
  # Skip gradient handling if using per-piece fills
  piece_fill_defs <- ""
  fill_value <- fill
  if (!use_per_piece_fills && is.list(fill) && !is.null(fill$type)) {
    if (fill$type == "gradient") {
      # Create piece gradient definition
      piece_fill_defs <- render_piece_fill_gradient_defs(fill)
      fill_value <- "url(#pieceFillGradient)"
    } else if (fill$type == "noise") {
      # Create piece noise pattern definition
      piece_fill_defs <- render_noise_piece_fill_defs(fill)
      fill_value <- "url(#pieceFillNoisePattern)"
    }
  }

  # Check if fusion styling is needed
  fusion_style <- positioned$parameters$fusion_style
  fusion_opacity <- positioned$parameters$fusion_opacity
  has_fusion <- any(sapply(positioned$pieces, function(p) {
    !is.null(p$fused_edges) && any(unlist(p$fused_edges))
  }))

  # Check if we have voronoi/random pieces that need edge-by-edge rendering
  puzzle_type <- positioned$type %||% "rectangular"
  needs_edge_rendering <- puzzle_type %in% c("voronoi", "random", "snic")

  # Render pieces (with or without fusion styling)
  # For voronoi/random, always use edge-by-edge rendering for consistent strokes
  if (has_fusion || needs_edge_rendering) {
    # For "none" mode: use opacity=0 to hide fused edges
    # For other modes: use user-specified opacity
    effective_fusion_opacity <- if (!is.null(fusion_style) && fusion_style == "none") {
      0  # Invisible
    } else {
      fusion_opacity %||% 0.3
    }

    # Use styled rendering for all fusion cases
    # Note: Per-piece fills not currently supported with fusion styling
    # (would require significant changes to render_pieces_with_fusion_styled)
    piece_elements <- render_pieces_with_fusion_styled(
      positioned$pieces, colors, if (use_per_piece_fills) fills else fill_value,
      stroke_width, opacity, fusion_style %||% "none", effective_fusion_opacity
    )
  } else {
    # Standard rendering: each piece as single path with fill and stroke
    piece_elements <- sapply(seq_along(positioned$pieces), function(i) {
      piece <- positioned$pieces[[i]]
      color <- colors[i]
      piece_fill <- if (use_per_piece_fills) fills[i] else fill_value
      render_piece(piece, piece_fill, color, stroke_width, opacity)
    })
  }

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

  # Image fill rendering for SNIC puzzles
  if (!is.null(image_path) && puzzle_type == "snic" && file.exists(image_path)) {
    image_data_uri <- encode_image_base64(image_path)
    # Get image dimensions from parameters
    img_w <- positioned$parameters$image_width
    img_h <- positioned$parameters$image_height
    piece_elements <- render_image_filled_pieces(
      positioned$pieces, image_data_uri, positioned$canvas_size,
      canvas_offset = if (!is.null(positioned$canvas_offset)) positioned$canvas_offset else c(0, 0),
      stroke_width = stroke_width, colors = colors,
      opacity = opacity,
      image_width = img_w, image_height = img_h
    )
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
#' @param inline Logical; if TRUE, omit XML declaration for inline HTML embedding (default: FALSE)
#' @return SVG header string
#' @keywords internal
build_svg_header <- function(canvas_size, canvas_offset = NULL, inline = FALSE) {

  width <- canvas_size[1]
  height <- canvas_size[2]

  # Handle viewBox offset (for hexagonal puzzles centered at origin)
  if (is.null(canvas_offset)) {
    canvas_offset <- c(0, 0)
  }
  vb_x <- canvas_offset[1]
  vb_y <- canvas_offset[2]

  # XML declaration is only valid for standalone SVG files, not inline HTML
  # See: https://www.w3.org/Graphics/SVG/WG/wiki/SVG_in_HTML
  xml_declaration <- if (inline) "" else '<?xml version="1.0" encoding="UTF-8"?>\n'

  paste0(
    xml_declaration,
    '<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" ',
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
    if (background$type == "noise") {
      return(render_noise_background(background, canvas_size, canvas_offset))
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
    sanitize_svg_attr(fill),
    sanitize_svg_attr(stroke_color),
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
#' @param piece Optional piece object with cached parsed_segments (Phase 2 optimization)
#' @return Named vector with x and y center coordinates
#' @keywords internal
calculate_path_bounding_box_center <- function(path, piece = NULL) {
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
  # Use cached parsed_segments if available (Phase 2 optimization)
  segments <- if (!is.null(piece) && !is.null(piece$parsed_segments)) {
    piece$parsed_segments
  } else {
    parse_svg_path(path)
  }

  coord_lists <- lapply(segments, function(seg) {
    if (seg$type %in% c("M", "L")) {
      list(x = seg$x, y = seg$y)
    } else if (seg$type == "C") {
      list(x = c(seg$cp1x, seg$cp2x, seg$x), y = c(seg$cp1y, seg$cp2y, seg$y))
    } else if (seg$type == "A") {
      list(x = seg$x, y = seg$y)
    } else {
      list(x = numeric(0), y = numeric(0))
    }
  })
  xs <- unlist(lapply(coord_lists, `[[`, "x"), use.names = FALSE)
  ys <- unlist(lapply(coord_lists, `[[`, "y"), use.names = FALSE)

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
  bbox_center <- calculate_path_bounding_box_center(piece$path, piece)
  cx <- bbox_center["x"]
  cy <- bbox_center["y"]

  # Build SVG text element centered on piece
  # Use dominant-baseline and text-anchor for proper centering
  sprintf(
    '<text x="%.2f" y="%.2f" font-family="sans-serif" font-size="%.1f" font-weight="bold" fill="%s" text-anchor="middle" dominant-baseline="central">%d</text>',
    cx, cy, font_size, sanitize_svg_attr(color), index
  )
}


#' Save rendered SVG to file
#'
#' @param svg_content SVG string from render_puzzle_svg()
#' @param filename Output filename
#' @param output_dir Output directory (default: "output")
#' @return Invisible path to saved file
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


#' Build SVG path string from segments efficiently
#'
#' Converts a list of path segments to an SVG path string using vectorized
#' operations instead of repeated string concatenation. This is significantly
#' faster for paths with many segments.
#'
#' @param segs List of segment objects with type, x, y, and other properties
#' @param start_point Starting c(x, y) coordinates for M command
#' @return SVG path string
#' @keywords internal
build_path_string_fast <- function(segs, start_point) {
  if (length(segs) == 0) {
    return(sprintf("M %.2f %.2f", start_point[1], start_point[2]))
  }

  # Pre-allocate character vector for all segments
  n_segs <- length(segs)
  seg_strings <- character(n_segs)

  for (i in seq_len(n_segs)) {
    seg <- segs[[i]]
    seg_strings[i] <- switch(
      seg$type,
      "L" = sprintf(" L %.2f %.2f", seg$x, seg$y),
      "C" = sprintf(" C %.2f %.2f %.2f %.2f %.2f %.2f",
                    seg$cp1x, seg$cp1y, seg$cp2x, seg$cp2y, seg$x, seg$y),
      "A" = sprintf(" A %.2f %.2f %d %d %d %.2f %.2f",
                    seg$rx, seg$ry, seg$rotation, seg$large_arc, seg$sweep,
                    seg$x, seg$y),
      ""
    )
  }

  # Single paste operation at the end

  paste0(sprintf("M %.2f %.2f", start_point[1], start_point[2]),
         paste(seg_strings, collapse = ""))
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


#' Render a single puzzle piece as a standalone SVG
#'
#' Takes a piece object from generate_puzzle()$pieces and renders it as
#' a complete SVG with appropriate viewBox calculated from the piece's bounds.
#'
#' @param piece A piece object containing path, center, id, and type fields
#' @param fill Fill specification: "none", color string, or gradient list
#' @param stroke_color Stroke color (default: "black")
#' @param stroke_width Stroke line width in mm (default: 1.5)
#' @param opacity Piece opacity 0.0-1.0 (default: 1.0)
#' @param background Background: "none", color string, or gradient list (default: "none")
#' @param padding Padding around piece as fraction of size (default: 0.15)
#' @param show_label Show piece ID label (default: FALSE)
#' @param label_color Label text color (default: "black")
#' @param label_size Label font size in mm (default: auto-calculated)
#' @return Complete SVG string for the single piece
#'
#' @examples
#' \donttest{
#' result <- generate_puzzle(type = "rectangular", grid = c(2, 2),
#'                           size = c(200, 200), seed = 42, save_files = FALSE)
#' piece_svg <- render_single_piece_svg(result$pieces[[1]], fill = "#cccccc")
#' nchar(piece_svg) > 0
#' }
#'
#' @export
render_single_piece_svg <- function(piece,
                                     fill = "none",
                                     stroke_color = "black",
                                     stroke_width = 1.5,
                                     opacity = 1.0,
                                     background = "none",
                                     padding = 0.15,
                                     show_label = FALSE,
                                     label_color = "black",
                                     label_size = NULL) {

  # Validate piece object

  if (is.null(piece$path) || !nzchar(piece$path)) {
    stop("Piece object must have a non-empty 'path' field")
  }

  # Calculate bounding box from path (uses cached parsed_segments if available)
  bounds <- calculate_piece_bounds(piece$path, piece)

  # Add padding
  pad_x <- bounds$width * padding
  pad_y <- bounds$height * padding

  vb_x <- bounds$min_x - pad_x
  vb_y <- bounds$min_y - pad_y
  vb_width <- bounds$width + 2 * pad_x
  vb_height <- bounds$height + 2 * pad_y

  # Build SVG header with viewBox
  svg_header <- sprintf(
    '<?xml version="1.0" encoding="UTF-8"?>\n<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="%.2fmm" height="%.2fmm" viewBox="%.2f %.2f %.2f %.2f">',
    vb_width, vb_height, vb_x, vb_y, vb_width, vb_height
  )

  # Render background
  bg_element <- ""
  if (is.character(background) && background != "none") {
    bg_element <- sprintf(
      '<rect x="%.2f" y="%.2f" width="%.2f" height="%.2f" fill="%s"/>',
      vb_x, vb_y, vb_width, vb_height, background
    )
  } else if (is.list(background) && !is.null(background$type)) {
    if (background$type == "gradient") {
      # Simple radial gradient for background
      bg_element <- sprintf(
        '<defs><radialGradient id="bgGrad"><stop offset="0%%" stop-color="%s"/><stop offset="50%%" stop-color="%s"/><stop offset="100%%" stop-color="%s"/></radialGradient></defs><rect x="%.2f" y="%.2f" width="%.2f" height="%.2f" fill="url(#bgGrad)"/>',
        background$center %||% "#ffffff",
        background$middle %||% "#e0e0e0",
        background$edge %||% "#808080",
        vb_x, vb_y, vb_width, vb_height
      )
    } else if (background$type == "noise") {
      # Noise background for single piece
      bg_element <- render_noise_background(
        background,
        canvas_size = c(vb_width, vb_height),
        canvas_offset = c(vb_x, vb_y)
      )
    }
  }

  # Handle fill (including gradient and noise)
  defs_section <- ""
  fill_value <- fill

  if (is.list(fill) && !is.null(fill$type)) {
    if (fill$type == "gradient") {
      defs_section <- render_piece_fill_gradient_defs(fill)
      fill_value <- "url(#pieceFillGradient)"
    } else if (fill$type == "noise") {
      defs_section <- render_noise_piece_fill_defs(fill)
      fill_value <- "url(#pieceFillNoisePattern)"
    }
  }

  # Render piece path
  opacity_attr <- if (opacity < 1.0) sprintf(' opacity="%.2f"', opacity) else ""
  piece_element <- sprintf(
    '<path d="%s" fill="%s" stroke="%s" stroke-width="%.2f" stroke-linecap="round" stroke-linejoin="round"%s/>',
    piece$path, fill_value, stroke_color, stroke_width, opacity_attr
  )

  # Render label if requested
  label_element <- ""
  if (show_label) {
    bbox_center <- calculate_path_bounding_box_center(piece$path, piece)

    # Auto-calculate label size
    if (is.null(label_size)) {
      piece_dim <- min(bounds$width, bounds$height)
      label_size <- max(4, min(20, piece_dim * 0.25))
    }

    piece_id <- piece$id %||% 1
    label_element <- sprintf(
      '<text x="%.2f" y="%.2f" font-family="sans-serif" font-size="%.1f" font-weight="bold" fill="%s" text-anchor="middle" dominant-baseline="central">%d</text>',
      bbox_center["x"], bbox_center["y"], label_size, label_color, piece_id
    )
  }

  # Assemble SVG
  svg_parts <- c(
    svg_header,
    if (nzchar(defs_section)) defs_section else NULL,
    if (nzchar(bg_element)) bg_element else NULL,
    piece_element,
    if (nzchar(label_element)) label_element else NULL,
    "</svg>"
  )

  paste(svg_parts[!sapply(svg_parts, is.null)], collapse = "\n")
}


#' Calculate bounding box from SVG path
#'
#' Parses an SVG path and extracts all coordinates to compute bounds.
#'
#' @param path SVG path d attribute string
#' @param piece Optional piece object with cached parsed_segments (Phase 2 optimization)
#' @return List with min_x, max_x, min_y, max_y, width, height
#' @keywords internal
calculate_piece_bounds <- function(path, piece = NULL) {
  # Use cached parsed_segments if available (Phase 2 optimization)
  segments <- if (!is.null(piece) && !is.null(piece$parsed_segments)) {
    piece$parsed_segments
  } else {
    parse_svg_path(path)
  }

  # Extract coordinates using O(n) list+unlist pattern instead of O(n²) grow-on-append
  coord_lists <- lapply(segments, function(seg) {
    if (seg$type %in% c("M", "L")) {
      list(x = seg$x, y = seg$y)
    } else if (seg$type == "C") {
      # Include control points for accurate bezier bounds
      list(x = c(seg$cp1x, seg$cp2x, seg$x), y = c(seg$cp1y, seg$cp2y, seg$y))
    } else if (seg$type == "A") {
      list(x = seg$x, y = seg$y)
    } else {
      list(x = NULL, y = NULL)
    }
  })

  xs <- unlist(lapply(coord_lists, `[[`, "x"), use.names = FALSE)
  ys <- unlist(lapply(coord_lists, `[[`, "y"), use.names = FALSE)

  if (length(xs) == 0 || length(ys) == 0) {
    return(list(min_x = 0, max_x = 100, min_y = 0, max_y = 100,
                width = 100, height = 100))
  }

  list(
    min_x = min(xs),
    max_x = max(xs),
    min_y = min(ys),
    max_y = max(ys),
    width = max(xs) - min(xs),
    height = max(ys) - min(ys)
  )
}
