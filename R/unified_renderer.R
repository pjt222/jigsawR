# Unified SVG Renderer
# Part of Epic #32 - Unified Puzzle Generation Pipeline
# Renders positioned pieces to SVG with consistent styling

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


#' Split rectangular piece path into individual edge paths
#'
#' Parses a closed rectangular piece path and extracts the 4 edge segments.
#' Uses corner detection to identify edge boundaries.
#'
#' @param path SVG path string for a rectangular piece
#' @param piece Piece object with path and grid position
#' @return List with N, E, S, W edge path strings (each starting with M)
#' @keywords internal
split_rect_path_into_edges <- function(path, piece = NULL) {
  # Use cached parsed_segments if available (Phase 2 optimization)
  segments <- if (!is.null(piece) && !is.null(piece$parsed_segments)) {
    piece$parsed_segments
  } else {
    parse_svg_path(path)
  }

  # Find the corners by tracking endpoints
  # First segment should be M (move to top-left)
  if (length(segments) == 0 || segments[[1]]$type != "M") {
    return(list(N = "", E = "", S = "", W = ""))
  }

  # Track current position and collect segments for each edge
  current_x <- segments[[1]]$x
  current_y <- segments[[1]]$y
  start_x <- current_x
  start_y <- current_y

  # For rectangular pieces, corners are when x or y changes direction
  # N: left to right (x increasing, y constant or slight variation)
  # E: top to bottom (y increasing, x constant or slight variation)
  # S: right to left (x decreasing, y constant or slight variation)
  # W: bottom to top (y decreasing, x constant or slight variation)

  edges <- list(N = list(), E = list(), S = list(), W = list())
  current_edge <- "N"  # Start with north edge

  for (i in 2:length(segments)) {
    seg <- segments[[i]]

    if (seg$type == "Z") {
      next
    }

    # Determine endpoint of this segment
    end_x <- seg$x
    end_y <- seg$y

    # Calculate direction from current position to segment endpoint
    dx <- end_x - current_x
    dy <- end_y - current_y

    # Detect edge transitions BEFORE adding segment
    # Check if this segment starts a new direction
    if (current_edge == "N" && abs(dy) > abs(dx) && dy > 0.1) {
      current_edge <- "E"
    } else if (current_edge == "E" && abs(dx) > abs(dy) && dx < -0.1) {
      current_edge <- "S"
    } else if (current_edge == "S" && abs(dy) > abs(dx) && dy < -0.1) {
      current_edge <- "W"
    }

    # Add segment to current edge (after transition check)
    edges[[current_edge]][[length(edges[[current_edge]]) + 1]] <- seg

    current_x <- end_x
    current_y <- end_y
  }

  # Convert segment lists to path strings using fast builder
  edge_paths <- list()
  prev_end <- c(start_x, start_y)

  for (edge_name in c("N", "E", "S", "W")) {
    segs <- edges[[edge_name]]
    if (length(segs) == 0) {
      edge_paths[[edge_name]] <- ""
      next
    }

    # Use optimized path string builder (avoids O(n²) string concatenation)
    edge_paths[[edge_name]] <- build_path_string_fast(segs, prev_end)

    # Update prev_end to last segment's endpoint
    if (length(segs) > 0) {
      last_seg <- segs[[length(segs)]]
      prev_end <- c(last_seg$x, last_seg$y)
    }
  }

  return(edge_paths)
}


#' Split concentric piece path into individual edge paths
#'
#' Parses a concentric piece path and extracts the edge segments.
#' Trapezoid pieces have 4 edges: INNER, RIGHT, OUTER, LEFT
#' Center piece (hexagon) has 6 edges: 1-6
#'
#' Uses vertex detection based on angle changes to correctly identify edge boundaries.
#'
#' @param path SVG path string for a concentric piece
#' @param piece Piece object with path and ring_pos
#' @return List with edge path strings keyed by edge name
#' @keywords internal
split_concentric_path_into_edges <- function(path, piece = NULL) {
  # Use cached parsed_segments if available (Phase 2 optimization)
  segments <- if (!is.null(piece) && !is.null(piece$parsed_segments)) {
    piece$parsed_segments
  } else {
    parse_svg_path(path)
  }

  if (length(segments) == 0 || segments[[1]]$type != "M") {
    if (!is.null(piece) && !is.null(piece$ring_pos) && piece$ring_pos$ring == 0) {
      return(list(`1` = "", `2` = "", `3` = "", `4` = "", `5` = "", `6` = ""))
    }
    return(list(INNER = "", RIGHT = "", OUTER = "", LEFT = ""))
  }

  # Determine if this is center piece (6 edges) or trapezoid (4 edges)
  is_center <- FALSE
  if (!is.null(piece) && !is.null(piece$ring_pos)) {
    is_center <- piece$ring_pos$ring == 0
  }

  start_x <- segments[[1]]$x
  start_y <- segments[[1]]$y

  # Extract content segments (excluding M and Z)
  content_segments <- list()
  for (i in 2:length(segments)) {
    if (segments[[i]]$type != "Z") {
      content_segments[[length(content_segments) + 1]] <- segments[[i]]
    }
  }

  if (is_center) {
    # Center piece: 6 edges with equal segments (typically 3 beziers each)
    edge_names <- as.character(1:6)
    n_edges <- 6
    segs_per_edge <- max(1, ceiling(length(content_segments) / n_edges))
    edges <- setNames(vector("list", n_edges), edge_names)
    for (e in edge_names) edges[[e]] <- list()

    for (i in seq_along(content_segments)) {
      edge_idx <- min(n_edges, ceiling(i / segs_per_edge))
      edge_name <- edge_names[edge_idx]
      edges[[edge_name]][[length(edges[[edge_name]]) + 1]] <- content_segments[[i]]
    }
  } else {
    # Trapezoid piece: 4 edges (INNER, RIGHT, OUTER, LEFT)
    # Use vertex coordinates to correctly identify edge boundaries
    # Path order: V1 -> INNER -> V2 -> RIGHT -> V3 -> OUTER -> V4 -> LEFT -> V1
    edge_names <- c("INNER", "RIGHT", "OUTER", "LEFT")
    edges <- setNames(vector("list", 4), edge_names)
    for (e in edge_names) edges[[e]] <- list()

    # Identify vertices by scanning path endpoint coordinates
    # We find vertices by looking for segment endpoints that mark "corners"
    # (significant direction changes in the path)
    #
    # For concentric trapezoids with bezier edges:
    # - Each edge has 3 bezier segments
    # - A vertex is where one edge ends and another begins
    # - The 4 vertices are visited in order: V1 -> INNER -> V2 -> RIGHT -> V3 -> OUTER -> V4 -> LEFT -> V1

    # Collect all segment endpoints
    all_endpoints <- list()
    for (seg in content_segments) {
      all_endpoints[[length(all_endpoints) + 1]] <- c(seg$x, seg$y)
    }

    # Find vertices by detecting "corners" where direction changes significantly
    # Or use the fact that each edge has a consistent segment count (3 beziers per internal edge)
    # For internal pieces: typically 3+3+3+3 = 12 bezier segments
    # For boundary pieces: 3+3+1(L)+3 = 10 segments

    # Strategy: The path visits V1 -> V2 -> V3 -> V4 -> V1
    # We find these by looking at the endpoints that repeat as both end-of-edge and start-of-edge
    # Simpler approach: count segments and divide into 4 groups

    # Check if there's an L segment - it marks the OUTER boundary
    l_indices <- which(sapply(content_segments, function(s) s$type == "L"))

    if (length(l_indices) > 0) {
      # Has boundary L segment - use it to identify edge splits
      l_idx <- l_indices[1]

      # OUTER is at l_idx, so:
      # Segments 1 to (l_idx-1) are INNER + RIGHT
      # Segment l_idx is OUTER
      # Segments (l_idx+1) to end are LEFT

      before_l <- seq_len(l_idx - 1)
      n_before <- length(before_l)

      # Split INNER and RIGHT evenly
      n_inner <- ceiling(n_before / 2)
      inner_indices <- before_l[seq_len(n_inner)]
      right_indices <- before_l[(n_inner + 1):n_before]

      outer_indices <- l_idx
      left_indices <- (l_idx + 1):length(content_segments)

      for (i in inner_indices) edges$INNER[[length(edges$INNER) + 1]] <- content_segments[[i]]
      for (i in right_indices) edges$RIGHT[[length(edges$RIGHT) + 1]] <- content_segments[[i]]
      for (i in outer_indices) edges$OUTER[[length(edges$OUTER) + 1]] <- content_segments[[i]]
      if (length(left_indices) > 0 && left_indices[1] <= length(content_segments)) {
        for (i in left_indices) edges$LEFT[[length(edges$LEFT) + 1]] <- content_segments[[i]]
      }

    } else {
      # No L segment - internal piece with all bezier edges
      # Total segments should be ~12-16 (3 beziers per edge, OUTER may have more)
      # Use V1 position to detect where we return to start
      n_segs <- length(content_segments)

      # For internal pieces, try to find V1 return point to identify LEFT edge boundary
      # V1 is at (start_x, start_y), path should return close to V1 at end of LEFT
      tolerance <- 1.0

      # Scan for where path endpoints match V1 (return point)
      # This marks the end of LEFT edge (should be near last segment)
      v1_return_idx <- NULL
      for (i in seq_along(all_endpoints)) {
        dist <- sqrt(sum((all_endpoints[[i]] - c(start_x, start_y))^2))
        if (dist < tolerance) {
          v1_return_idx <- i
          # Don't break - we want the LAST match (in case of multiple near-hits)
        }
      }

      if (!is.null(v1_return_idx) && v1_return_idx >= 10) {
        # Found V1 return point - use it to split
        # LEFT ends at v1_return_idx
        # Everything before that is INNER + RIGHT + OUTER
        left_start <- v1_return_idx - 2  # LEFT is typically 3 segments, starts 3 before return

        if (left_start > 6) {
          # Reasonable split: INNER (3) + RIGHT (3) + OUTER (variable) + LEFT (3)
          for (i in 1:3) edges$INNER[[length(edges$INNER) + 1]] <- content_segments[[i]]
          for (i in 4:6) edges$RIGHT[[length(edges$RIGHT) + 1]] <- content_segments[[i]]
          for (i in 7:(left_start - 1)) edges$OUTER[[length(edges$OUTER) + 1]] <- content_segments[[i]]
          for (i in left_start:n_segs) edges$LEFT[[length(edges$LEFT) + 1]] <- content_segments[[i]]
        } else {
          # Fallback to even distribution
          segs_per_edge <- max(1, ceiling(n_segs / 4))
          for (i in seq_along(content_segments)) {
            edge_idx <- min(4, ceiling(i / segs_per_edge))
            edge_name <- edge_names[edge_idx]
            edges[[edge_name]][[length(edges[[edge_name]]) + 1]] <- content_segments[[i]]
          }
        }
      } else {
        # Can't find V1 return - fall back to fixed split assuming 3-3-X-3 pattern
        if (n_segs >= 12) {
          # Standard internal piece: INNER=3, RIGHT=3, OUTER=variable, LEFT=3
          for (i in 1:3) edges$INNER[[length(edges$INNER) + 1]] <- content_segments[[i]]
          for (i in 4:6) edges$RIGHT[[length(edges$RIGHT) + 1]] <- content_segments[[i]]
          outer_end <- n_segs - 3
          for (i in 7:outer_end) edges$OUTER[[length(edges$OUTER) + 1]] <- content_segments[[i]]
          for (i in (outer_end + 1):n_segs) edges$LEFT[[length(edges$LEFT) + 1]] <- content_segments[[i]]
        } else {
          # Small segment count - distribute evenly
          segs_per_edge <- max(1, ceiling(n_segs / 4))
          for (i in seq_along(content_segments)) {
            edge_idx <- min(4, ceiling(i / segs_per_edge))
            edge_name <- edge_names[edge_idx]
            edges[[edge_name]][[length(edges[[edge_name]]) + 1]] <- content_segments[[i]]
          }
        }
      }
    }

  }

  # Convert segment lists to path strings using fast builder
  edge_paths <- list()
  prev_end <- c(start_x, start_y)

  for (edge_name in edge_names) {
    segs <- edges[[edge_name]]
    if (length(segs) == 0) {
      edge_paths[[edge_name]] <- ""
      next
    }

    # Use optimized path string builder (avoids O(n²) string concatenation)
    edge_paths[[edge_name]] <- build_path_string_fast(segs, prev_end)

    # Update prev_end to last segment's endpoint
    if (length(segs) > 0) {
      last_seg <- segs[[length(segs)]]
      prev_end <- c(last_seg$x, last_seg$y)
    }
  }

  return(edge_paths)
}


#' Find vertices (corners) in a path by detecting angle changes
#'
#' @param endpoints List of c(x, y) endpoint coordinates
#' @param n_edges Expected number of edges (4 for trapezoid, 6 for hexagon)
#' @return List of vertex indices in the endpoints list
#' @keywords internal
find_path_vertices <- function(endpoints, n_edges) {
  if (length(endpoints) < 3) {
    # Not enough points to detect vertices
    return(list())
  }

  # Calculate direction angles between consecutive points
  angles <- numeric(length(endpoints) - 1)
  for (i in 1:(length(endpoints) - 1)) {
    dx <- endpoints[[i + 1]][1] - endpoints[[i]][1]
    dy <- endpoints[[i + 1]][2] - endpoints[[i]][2]
    angles[i] <- atan2(dy, dx)
  }

  # Calculate angle changes
  angle_changes <- numeric(length(angles) - 1)
  for (i in 1:(length(angles) - 1)) {
    diff <- angles[i + 1] - angles[i]
    # Normalize to [-pi, pi]
    while (diff > pi) diff <- diff - 2 * pi
    while (diff < -pi) diff <- diff + 2 * pi
    angle_changes[i] <- abs(diff)
  }

  # Find the n_edges-1 largest angle changes (vertices between edges)
  # Add 1 to indices because angle_changes[i] corresponds to endpoint[i+1]
  if (length(angle_changes) == 0) {
    return(list())
  }

  # Get indices sorted by angle change magnitude (descending)
  sorted_indices <- order(angle_changes, decreasing = TRUE)

  # Take top n_edges - 1 vertices (we need n_edges-1 boundaries for n_edges edges)
  n_vertices <- min(n_edges - 1, length(sorted_indices))
  vertex_indices <- sort(sorted_indices[1:n_vertices] + 1)  # +1 to get endpoint index

  return(vertex_indices)
}


#' Assign path segments to edges based on vertex boundaries
#'
#' @param segments Parsed path segments
#' @param vertex_indices Indices of vertices in the path
#' @param edge_names Names of edges
#' @return Named list of segment lists per edge
#' @keywords internal
assign_segments_to_edges <- function(segments, vertex_indices, edge_names) {
  n_edges <- length(edge_names)
  edges <- setNames(vector("list", n_edges), edge_names)
  for (e in edge_names) edges[[e]] <- list()

  # Content segments (excluding M and Z)
  content_segments <- list()
  for (i in 2:length(segments)) {
    if (segments[[i]]$type != "Z") {
      content_segments[[length(content_segments) + 1]] <- segments[[i]]
    }
  }

  if (length(content_segments) == 0) {
    return(edges)
  }

  # If no vertices detected, distribute evenly as fallback
  if (length(vertex_indices) == 0) {
    segs_per_edge <- ceiling(length(content_segments) / n_edges)
    for (i in seq_along(content_segments)) {
      edge_idx <- min(n_edges, ceiling(i / segs_per_edge))
      edge_name <- edge_names[edge_idx]
      edges[[edge_name]][[length(edges[[edge_name]]) + 1]] <- content_segments[[i]]
    }
    return(edges)
  }

  # Assign segments based on vertex boundaries
  # vertex_indices are 1-based into the endpoint list
  # Segment i ends at endpoint i+1 (since endpoint 1 is the M start)
  current_edge <- 1
  vertex_ptr <- 1

  for (i in seq_along(content_segments)) {
    seg <- content_segments[[i]]
    edge_name <- edge_names[current_edge]
    edges[[edge_name]][[length(edges[[edge_name]]) + 1]] <- seg

    # Check if this segment ends at a vertex (transition to next edge)
    # Segment i ends at endpoint i+1
    seg_endpoint_idx <- i + 1
    if (vertex_ptr <= length(vertex_indices) && seg_endpoint_idx == vertex_indices[vertex_ptr]) {
      current_edge <- min(current_edge + 1, n_edges)
      vertex_ptr <- vertex_ptr + 1
    }
  }

  return(edges)
}


#' Split hexagonal piece path into individual edge paths
#'
#' Parses a hexagonal piece path and extracts the 6 edge segments (sides 0-5).
#' Uses vertex detection based on angle changes to correctly identify edge boundaries.
#'
#' @param path SVG path string for a hexagonal piece
#' @param piece Piece object with path and ring_pos
#' @return List with edge path strings keyed by side number ("0" through "5")
#' @keywords internal
split_hex_path_into_edges <- function(path, piece = NULL) {
  # Use cached parsed_segments if available (Phase 2 optimization)
  segments <- if (!is.null(piece) && !is.null(piece$parsed_segments)) {
    piece$parsed_segments
  } else {
    parse_svg_path(path)
  }

  if (length(segments) == 0 || segments[[1]]$type != "M") {
    return(list(`0` = "", `1` = "", `2` = "", `3` = "", `4` = "", `5` = ""))
  }

  # Hexagonal pieces have 6 sides numbered 0-5
  edge_names <- as.character(0:5)
  n_edges <- 6

  start_x <- segments[[1]]$x
  start_y <- segments[[1]]$y

  # Extract content segments (excluding M and Z)
  content_segments <- list()
  for (i in 2:length(segments)) {
    if (segments[[i]]$type != "Z") {
      content_segments[[length(content_segments) + 1]] <- segments[[i]]
    }
  }

  # For hexagonal pieces:
  # - Each edge typically has 3 bezier segments (one complete bezier tab)
  # - Total: 18 segments for internal pieces (6 edges x 3 beziers)
  # - Boundary edges may have L or A segments
  edges <- setNames(vector("list", n_edges), edge_names)
  for (e in edge_names) edges[[e]] <- list()

  n_segs <- length(content_segments)

  if (n_segs == 0) {
    # Empty path
    edge_paths <- list()
    for (edge_name in edge_names) edge_paths[[edge_name]] <- ""
    return(edge_paths)
  }

  # Find L and A segments - they often mark boundary edges
  special_indices <- which(sapply(content_segments, function(s) s$type %in% c("L", "A")))

  if (length(special_indices) > 0 && length(special_indices) <= 3) {
    # Boundary piece: L/A segments mark specific edges
    # Distribute beziers evenly, assigning special segments to their own edges
    bezier_count <- n_segs - length(special_indices)
    beziers_per_edge <- max(1, round(bezier_count / (n_edges - length(special_indices))))

    current_edge <- 1
    bezier_in_edge <- 0

    for (i in seq_along(content_segments)) {
      seg <- content_segments[[i]]
      edge_name <- edge_names[current_edge]

      if (seg$type %in% c("L", "A")) {
        # Special segment gets its own edge if it's at a boundary
        if (bezier_in_edge > 0) {
          # Move to next edge for this special segment
          current_edge <- min(n_edges, current_edge + 1)
          edge_name <- edge_names[current_edge]
        }
        edges[[edge_name]][[length(edges[[edge_name]]) + 1]] <- seg
        # Move to next edge after special segment
        current_edge <- min(n_edges, current_edge + 1)
        bezier_in_edge <- 0
      } else {
        # Bezier segment
        edges[[edge_name]][[length(edges[[edge_name]]) + 1]] <- seg
        bezier_in_edge <- bezier_in_edge + 1
        if (bezier_in_edge >= beziers_per_edge && current_edge < n_edges) {
          current_edge <- current_edge + 1
          bezier_in_edge <- 0
        }
      }
    }
  } else {
    # Internal piece or standard distribution: 3 segments per edge
    segs_per_edge <- max(1, ceiling(n_segs / n_edges))

    for (i in seq_along(content_segments)) {
      edge_idx <- min(n_edges, ceiling(i / segs_per_edge))
      edge_name <- edge_names[edge_idx]
      edges[[edge_name]][[length(edges[[edge_name]]) + 1]] <- content_segments[[i]]
    }
  }

  # Convert segment lists to path strings using fast builder
  edge_paths <- list()
  prev_end <- c(start_x, start_y)

  for (edge_name in edge_names) {
    segs <- edges[[edge_name]]
    if (length(segs) == 0) {
      edge_paths[[edge_name]] <- ""
      next
    }

    # Use optimized path string builder (avoids O(n²) string concatenation)
    edge_paths[[edge_name]] <- build_path_string_fast(segs, prev_end)

    # Update prev_end to last segment's endpoint
    if (length(segs) > 0) {
      last_seg <- segs[[length(segs)]]
      prev_end <- c(last_seg$x, last_seg$y)
    }
  }

  return(edge_paths)
}


#' Get edge paths for a piece based on its type
#'
#' Dispatches to the appropriate path splitting function based on piece type.
#'
#' @param piece Piece object with path, type, and position info
#' @return List with edge path strings keyed by edge name/number
#' @keywords internal
get_piece_edge_paths <- function(piece) {
  piece_type <- piece$type %||% "rectangular"

  if (piece_type == "concentric") {
    return(split_concentric_path_into_edges(piece$path, piece))
  } else if (piece_type == "hexagonal") {
    return(split_hex_path_into_edges(piece$path, piece))
  } else if (piece_type %in% c("voronoi", "random", "snic")) {
    # For voronoi/random/snic, use edge_segments keyed by neighbor_id
    # Returns list with neighbor_id keys -> path values
    if (is.null(piece$edge_segments) || length(piece$edge_segments) == 0) {
      return(list())
    }
    # Extract just the path from each segment
    edge_paths <- lapply(piece$edge_segments, function(seg) seg$path)
    return(edge_paths)
  } else {
    return(split_rect_path_into_edges(piece$path, piece))
  }
}


#' Get edge names for a piece based on its type
#'
#' Returns the edge names/numbers used for a piece type.
#'
#' @param piece Piece object with type and position info
#' @return Character vector of edge names
#' @keywords internal
get_piece_edge_names <- function(piece) {
  piece_type <- piece$type %||% "rectangular"

  if (piece_type == "concentric") {
    # Check if center piece (6 edges) or trapezoid (4 edges)
    if (!is.null(piece$ring_pos) && piece$ring_pos$ring == 0) {
      return(as.character(1:6))
    }
    return(c("INNER", "RIGHT", "OUTER", "LEFT"))
  } else if (piece_type == "hexagonal") {
    return(as.character(0:5))
  } else if (piece_type %in% c("voronoi", "random", "snic")) {
    # For voronoi/random/snic, edge names are neighbor IDs (as strings)
    if (is.null(piece$edge_segments) || length(piece$edge_segments) == 0) {
      return(character())
    }
    return(names(piece$edge_segments))
  } else {
    return(c("N", "E", "S", "W"))
  }
}


#' Generate SVG arc path for an angular segment
#'
#' Creates an SVG path string for an arc segment at a given radius between
#' two angles. Used for rendering segment-level fusion in concentric puzzles.
#'
#' @param radius Arc radius
#' @param start_angle Start angle in radians
#' @param end_angle End angle in radians
#' @param center Optional center point (default: c(0, 0))
#' @return SVG path string
#' @keywords internal
generate_arc_segment_path <- function(radius, start_angle, end_angle, center = c(0, 0)) {
  # Calculate start and end points
  x1 <- center[1] + radius * cos(start_angle)
  y1 <- center[2] + radius * sin(start_angle)
  x2 <- center[1] + radius * cos(end_angle)
  y2 <- center[2] + radius * sin(end_angle)

  # Determine arc flags
  # large-arc-flag: 1 if arc > 180 degrees, 0 otherwise
  arc_span <- end_angle - start_angle
  large_arc <- if (arc_span > pi) 1 else 0
  # sweep-flag: 1 for clockwise (which matches increasing angle in SVG coords)
  sweep <- 1

  sprintf("M%.2f,%.2f A%.2f,%.2f 0 %d,%d %.2f,%.2f",
          x1, y1, radius, radius, large_arc, sweep, x2, y2)
}


#' Render pieces with styled fusion edges
#'
#' Three-pass rendering: fills, non-fused edges, fused edges with styling.
#' For "none" mode, fusion_opacity should be 0 to make fused edges invisible.
#' Supports rectangular, hexagonal, and concentric puzzle types.
#'
#' @param pieces List of positioned pieces
#' @param colors Vector of stroke colors
#' @param fill Fill value for pieces (single value or vector of per-piece fills)
#' @param stroke_width Stroke width
#' @param opacity Piece opacity
#' @param fusion_style "dashed" or "solid"
#' @param fusion_opacity Opacity for fused edges (0.0 to 1.0)
#' @return Vector of SVG elements
#' @keywords internal
render_pieces_with_fusion_styled <- function(pieces, colors, fill, stroke_width, opacity,
                                              fusion_style, fusion_opacity) {
  n_pieces <- length(pieces)
  element_list <- vector("list", n_pieces * 8L)  # pre-allocate generously
  element_idx <- 0L
  add_element <- function(el) {
    element_idx <<- element_idx + 1L
    if (element_idx > length(element_list)) {
      element_list[[element_idx * 2L]] <<- NULL  # grow if needed
    }
    element_list[[element_idx]] <<- el
  }

  # Build piece_id lookup hash map for O(1) neighbor lookup (instead of O(n) loops)
  piece_lookup <- new.env(hash = TRUE, parent = emptyenv())
  for (i in seq_along(pieces)) {
    p_id <- pieces[[i]]$id %||% i
    piece_lookup[[as.character(p_id)]] <- i
  }

  # Helper function for O(1) piece lookup by ID
  get_piece_by_id <- function(target_id) {
    idx <- piece_lookup[[as.character(target_id)]]
    if (!is.null(idx)) pieces[[idx]] else NULL
  }

  # Check if fill is per-piece (vector) or single value
  use_per_piece_fills <- length(fill) == n_pieces && n_pieces > 1

  # Pass 1: Draw fills only (no stroke)
  opacity_attr <- if (opacity < 1.0) sprintf(' opacity="%.2f"', opacity) else ""

  for (i in seq_along(pieces)) {
    piece <- pieces[[i]]
    piece_fill <- if (use_per_piece_fills) fill[i] else fill[1]
    fill_element <- sprintf(
      '<path d="%s" fill="%s" stroke="none"%s/>',
      piece$path, piece_fill, opacity_attr
    )
    add_element(fill_element)
  }

  # Pass 2: Draw non-fused edges with normal stroke
  for (i in seq_along(pieces)) {
    piece <- pieces[[i]]
    color <- colors[i]
    piece_type <- piece$type %||% "rectangular"

    # Use type-aware path splitting (for all types including voronoi/random)
    # For voronoi/random, edge_segments are keyed by neighbor_id
    edge_paths <- get_piece_edge_paths(piece)
    edge_names <- get_piece_edge_names(piece)

    # Fallback: if no edge paths available, draw full outline
    if (length(edge_paths) == 0 || length(edge_names) == 0) {
      edge_element <- sprintf(
        '<path d="%s" fill="none" stroke="%s" stroke-width="%.2f" stroke-linecap="round" stroke-linejoin="round"/>',
        piece$path, color, stroke_width
      )
      add_element(edge_element)
      next
    }

    for (edge_name in edge_names) {
      # Check for segment-level fusion (many-to-one OUTER edges)
      if (edge_name == "OUTER" && isTRUE(piece$outer_segments_mixed)) {
        # Mixed segments - handle in pass 3.5 (segment-level rendering)
        next
      }

      if (isTRUE(piece$fused_edges[[edge_name]])) {
        next  # Will handle in pass 3
      }

      edge_path <- edge_paths[[edge_name]]
      if (!is.null(edge_path) && nzchar(edge_path)) {
        edge_element <- sprintf(
          '<path d="%s" fill="none" stroke="%s" stroke-width="%.2f" stroke-linecap="round" stroke-linejoin="round"/>',
          edge_path, color, stroke_width
        )
        add_element(edge_element)
      }
    }
  }

  # Pass 3: Draw fused edges with special styling
  # Each fused edge is shared by two pieces - draw it only ONCE
  # Use piece ID + edge name for deduplication (works with offset > 0)
  # Key format: "min_id-edge|max_id-edge" where ids are sorted
  drawn_fused_edges <- new.env(hash = TRUE, parent = emptyenv())

  # Build style attributes for fused edges
  dash_attr <- ""
  if (fusion_style == "dashed") {
    dash_attr <- sprintf(' stroke-dasharray="%.1f %.1f"', stroke_width * 3, stroke_width * 2)
  }
  fusion_opacity_attr <- sprintf(' opacity="%.2f"', fusion_opacity)

  for (i in seq_along(pieces)) {
    piece <- pieces[[i]]
    color <- colors[i]
    piece_id <- piece$id %||% i
    piece_type <- piece$type %||% "rectangular"

    if (is.null(piece$fused_edges) || length(piece$fused_edges) == 0) {
      next
    }

    # Use type-aware path splitting (for all types including voronoi/random)
    # For voronoi/random, edge_segments are keyed by neighbor_id
    edge_paths <- get_piece_edge_paths(piece)
    edge_names <- get_piece_edge_names(piece)

    for (edge_name in edge_names) {
      # Skip mixed segment edges - handled in Pass 3.5
      if (edge_name == "OUTER" && isTRUE(piece$outer_segments_mixed)) {
        next
      }

      # Skip INNER edges if the neighbor has outer_segments_mixed = TRUE
      # Those edges are handled in Pass 3.5 from the neighbor's perspective
      if (edge_name == "INNER") {
        neighbor_id <- piece$fused_neighbor_ids[[edge_name]]
        if (!is.null(neighbor_id)) {
          # O(1) lookup via hash map (instead of O(n) loop)
          np <- get_piece_by_id(neighbor_id)
          if (!is.null(np) && isTRUE(np$outer_segments_mixed)) {
            next  # Skip - will be handled in Pass 3.5
          }
        }
      }

      if (!isTRUE(piece$fused_edges[[edge_name]])) {
        next  # Only draw fused edges here
      }

      edge_path <- edge_paths[[edge_name]]
      if (!is.null(edge_path) && nzchar(edge_path)) {
        # Create canonical edge key using piece ID and edge name
        # This works regardless of coordinate translation (offset > 0)
        # Format: "piece_id-edge_name" with canonical ordering
        this_edge_key <- sprintf("%d-%s", piece_id, edge_name)

        # For deduplication, we need a canonical key that's the same
        # whether we're processing piece A or piece B of a shared edge.
        # Use sorted piece IDs to create consistent keys.
        # We'll use the piece's own key and check if we've drawn the complementary edge.
        #
        # Since fused_edges marks BOTH pieces' edges as fused, we can simply
        # use the smaller piece ID's edge key as the canonical form.
        # If this piece has a smaller ID than any neighbor that shares this edge,
        # we draw it. Otherwise, we skip (the neighbor with smaller ID will draw it).

        # Get neighbor info from the fused_edges structure
        # For now, use a simple approach: track drawn edges by this_edge_key
        # and its complement (which will have format "neighbor_id-opposite_edge")
        if (exists(this_edge_key, envir = drawn_fused_edges, inherits = FALSE)) {
          next
        }

        # Mark this edge as drawn
        drawn_fused_edges[[this_edge_key]] <- TRUE

        # Also mark the complementary edge as drawn to prevent double-drawing
        # We find the neighbor piece and mark ALL of their fused edges that point to us
        # This handles asymmetric adjacency (like in hexagonal puzzles)
        #
        # For OUTER edges with many-to-one relationships (segment data), we need to
        # mark ALL neighbors' INNER edges as drawn, not just the one in fused_neighbor_ids
        neighbor_ids <- NULL

        # Check for segment data first (many-to-one relationships)
        if (edge_name == "OUTER" && !is.null(piece$fused_edge_segments) &&
            !is.null(piece$fused_edge_segments$OUTER)) {
          # Get all neighbor IDs from segment data
          neighbor_ids <- sapply(piece$fused_edge_segments$OUTER, function(seg) seg$neighbor_id)
          neighbor_ids <- unique(neighbor_ids[!is.na(neighbor_ids)])
        }

        # Fall back to single neighbor from fused_neighbor_ids
        if (is.null(neighbor_ids) || length(neighbor_ids) == 0) {
          neighbor_ids <- piece$fused_neighbor_ids[[edge_name]]
        }

        if (!is.null(neighbor_ids) && length(neighbor_ids) > 0) {
          for (neighbor_id in neighbor_ids) {
            # O(1) lookup via hash map (instead of O(n) loop)
            neighbor_piece <- get_piece_by_id(neighbor_id)

            if (!is.null(neighbor_piece) && !is.null(neighbor_piece$fused_neighbor_ids)) {
              # Mark all neighbor edges that point back to this piece as drawn
              for (n_edge in names(neighbor_piece$fused_neighbor_ids)) {
                if (neighbor_piece$fused_neighbor_ids[[n_edge]] == piece_id) {
                  comp_edge_key <- sprintf("%d-%s", neighbor_id, n_edge)
                  drawn_fused_edges[[comp_edge_key]] <- TRUE
                }
              }
            }
          }
        }

        edge_element <- sprintf(
          '<path d="%s" fill="none" stroke="%s" stroke-width="%.2f" stroke-linecap="round" stroke-linejoin="round"%s%s/>',
          edge_path, color, stroke_width, dash_attr, fusion_opacity_attr
        )
        add_element(edge_element)
      }
    }
  }

  # Pass 3.5: Draw segment-level fused OUTER edges for concentric puzzles
  # This handles many-to-one relationships where different segments of an
  # OUTER edge have different fusion status (some fused, some not)
  drawn_segment_keys <- new.env(hash = TRUE, parent = emptyenv())

  for (i in seq_along(pieces)) {
    piece <- pieces[[i]]
    color <- colors[i]
    piece_id <- piece$id %||% i

    # Only process pieces with mixed segment fusion
    if (!isTRUE(piece$outer_segments_mixed)) {
      next
    }

    # Get outer radius for arc generation
    outer_radius <- piece$outer_radius
    if (is.null(outer_radius)) {
      next  # Can't render without radius
    }

    # Get segment fusion data
    segments <- piece$fused_edge_segments[["OUTER"]]
    if (is.null(segments) || length(segments) == 0) {
      next
    }

    # Render each segment with appropriate styling
    for (seg in segments) {
      # Create segment key for deduplication: "innerPiece-outerPiece"
      # Use sorted IDs for canonical form
      neighbor_id <- seg$neighbor_id
      seg_key <- sprintf("%d-%d", min(piece_id, neighbor_id), max(piece_id, neighbor_id))

      if (exists(seg_key, envir = drawn_segment_keys, inherits = FALSE)) {
        next
      }
      drawn_segment_keys[[seg_key]] <- TRUE

      # IMPORTANT: Also mark the neighbor's complementary edge as drawn in
      # drawn_fused_edges so Pass 3 doesn't draw it again.
      # The neighbor's INNER edge points to this piece.
      neighbor_inner_key <- sprintf("%d-INNER", neighbor_id)
      drawn_fused_edges[[neighbor_inner_key]] <- TRUE

      # Use the actual edge path with bezier tabs if available,
      # otherwise fall back to arc generation
      if (!is.null(seg$path) && !is.null(seg$start_point)) {
        # Use the pre-computed bezier path
        # Path needs M (moveto) + the bezier curves
        edge_path <- sprintf("M %.2f %.2f %s",
                            seg$start_point[1], seg$start_point[2], seg$path)
      } else {
        # Fallback to arc generation
        edge_path <- generate_arc_segment_path(
          radius = outer_radius,
          start_angle = seg$start_angle,
          end_angle = seg$end_angle
        )
      }

      # Apply styling based on segment fusion status
      if (isTRUE(seg$fused)) {
        # Fused segment - dashed with fusion opacity
        edge_element <- sprintf(
          '<path d="%s" fill="none" stroke="%s" stroke-width="%.2f" stroke-linecap="round" stroke-linejoin="round"%s%s/>',
          edge_path, color, stroke_width, dash_attr, fusion_opacity_attr
        )
      } else {
        # Non-fused segment - solid with normal opacity
        edge_element <- sprintf(
          '<path d="%s" fill="none" stroke="%s" stroke-width="%.2f" stroke-linecap="round" stroke-linejoin="round"/>',
          edge_path, color, stroke_width
        )
      }
      add_element(edge_element)
    }
  }

  return(unlist(element_list[seq_len(element_idx)], use.names = FALSE))
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
