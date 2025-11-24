# Puzzle Piece Separation Functions
# Implements offset/separation between individual pieces while maintaining grid positions

#' Calculate offset position for a puzzle piece
#' 
#' Determines how much to translate a piece based on its grid position
#' and the desired separation between pieces.
#' 
#' @param xi Column index (0-based)
#' @param yi Row index (0-based)
#' @param offset Separation distance in mm
#' @param piece_width Original piece width
#' @param piece_height Original piece height
#' @return Vector c(x_offset, y_offset) for translation
calculate_piece_offset <- function(xi, yi, offset, piece_width, piece_height) {
  # Calculate offset based on grid position
  # Each piece moves away from center by offset amount per gap
  x_offset <- xi * offset
  y_offset <- yi * offset
  
  return(c(x_offset, y_offset))
}

#' Apply offset to SVG path coordinates
#' 
#' Translates all coordinates in an SVG path by the specified offset.
#' 
#' @param path_string SVG path d attribute string
#' @param x_offset X translation amount
#' @param y_offset Y translation amount
#' @return Translated SVG path string
translate_svg_path <- function(path_string, x_offset, y_offset) {
  if (x_offset == 0 && y_offset == 0) {
    return(path_string)
  }
  
  # Parse the path string to extract and modify coordinates
  # This handles M (move), L (line), C (cubic bezier), and Z (close) commands
  
  # Split path into tokens
  tokens <- strsplit(path_string, "\\s+")[[1]]
  result <- character()
  i <- 1
  
  while (i <= length(tokens)) {
    token <- tokens[i]
    
    if (token %in% c("M", "L")) {
      # Move or Line: has x,y coordinates
      result <- c(result, token)
      if (i + 2 <= length(tokens)) {
        x <- as.numeric(tokens[i + 1]) + x_offset
        y <- as.numeric(tokens[i + 2]) + y_offset
        result <- c(result, sprintf("%.2f", x), sprintf("%.2f", y))
        i <- i + 3
      } else {
        i <- i + 1
      }
      
    } else if (token == "C") {
      # Cubic Bezier: has 6 coordinates (3 points)
      result <- c(result, token)
      if (i + 6 <= length(tokens)) {
        for (j in 1:3) {
          x <- as.numeric(tokens[i + j*2 - 1]) + x_offset
          y <- as.numeric(tokens[i + j*2]) + y_offset
          result <- c(result, sprintf("%.2f", x), sprintf("%.2f", y))
        }
        i <- i + 7
      } else {
        i <- i + 1
      }
      
    } else if (token == "Z") {
      # Close path: no coordinates
      result <- c(result, token)
      i <- i + 1
      
    } else {
      # Try to parse as number (continuation of previous command)
      num_val <- suppressWarnings(as.numeric(token))
      if (!is.na(num_val)) {
        # It's a number, probably a coordinate
        result <- c(result, token)
      } else {
        result <- c(result, token)
      }
      i <- i + 1
    }
  }
  
  return(paste(result, collapse = " "))
}

#' Generate SVG with separated puzzle pieces
#' 
#' Creates an SVG with individual pieces separated by the specified offset,
#' maintaining their original grid positions.
#' 
#' @param puzzle_structure Output from generate_puzzle_core()
#' @param offset Separation distance between pieces in mm
#' @param colors Optional vector of colors for pieces
#' @param stroke_width Line width for piece outlines
#' @return SVG string with separated pieces
#' @export
generate_separated_puzzle_svg <- function(puzzle_structure, 
                                         offset = 10, 
                                         colors = NULL,
                                         stroke_width = 1.5,
                                         background = "white") {
  
  xn <- puzzle_structure$grid[2]
  yn <- puzzle_structure$grid[1]
  piece_width <- puzzle_structure$piece_width
  piece_height <- puzzle_structure$piece_height
  
  # Calculate new canvas size with offsets
  total_width <- puzzle_structure$size[1] + (xn - 1) * offset
  total_height <- puzzle_structure$size[2] + (yn - 1) * offset
  
  # Add padding for visual clarity
  padding <- offset
  canvas_width <- total_width + 2 * padding
  canvas_height <- total_height + 2 * padding
  
  # Default colors
  if (is.null(colors)) {
    colors <- "black"
  }
  
  # Start SVG with expanded viewBox
  svg <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" 
     width="%.0f" height="%.0f" viewBox="%.0f %.0f %.0f %.0f">\n', 
    canvas_width, canvas_height, 
    -padding, -padding, canvas_width, canvas_height)
  
  # Add background based on type
  if (background == "gradient") {
    svg <- paste0(svg, '  <defs>
    <radialGradient id="bg-gradient" cx="50%" cy="50%" r="50%">
      <stop offset="0%" style="stop-color:#e3f2fd;stop-opacity:1" />
      <stop offset="50%" style="stop-color:#bbdefb;stop-opacity:1" />
      <stop offset="100%" style="stop-color:#90caf9;stop-opacity:1" />
    </radialGradient>
  </defs>
  <rect width="100%" height="100%" fill="url(#bg-gradient)"/>\n')
  } else if (background == "none" || background == "") {
    # No background rect
  } else {
    # Solid color background
    svg <- paste0(svg, sprintf('  <rect width="100%%" height="100%%" fill="%s"/>\n', background))
  }
  
  svg <- paste0(svg, '<g id="separated-puzzle">\n')
  
  # Generate each piece with offset
  piece_num <- 0
  for (yi in 0:(yn - 1)) {
    for (xi in 0:(xn - 1)) {
      # Generate original piece path
      piece_path <- generate_single_piece(xi, yi, puzzle_structure)
      
      # Calculate offset for this piece
      offsets <- calculate_piece_offset(xi, yi, offset, piece_width, piece_height)
      
      # Apply offset to path
      translated_path <- translate_svg_path(piece_path, offsets[1], offsets[2])
      
      # Determine color
      color <- colors[(piece_num %% length(colors)) + 1]
      
      # Add to SVG
      svg <- paste0(svg, sprintf('  <path id="piece-%d-%d" d="%s" fill="none" stroke="%s" stroke-width="%.1f"/>\n',
                                xi, yi, translated_path, color, stroke_width))
      
      piece_num <- piece_num + 1
    }
  }
  
  # Add grid guides (optional - for visualization)
  if (offset > 0) {
    svg <- paste0(svg, '  <!-- Grid guides -->\n')
    svg <- paste0(svg, '  <g id="guides" stroke="lightgray" stroke-width="0.5" stroke-dasharray="5,5" opacity="0.3">\n')
    
    # Horizontal guides
    for (yi in 0:yn) {
      y_pos <- yi * (piece_height + offset) - offset/2
      if (yi > 0 && yi < yn) {
        svg <- paste0(svg, sprintf('    <line x1="%.0f" y1="%.2f" x2="%.0f" y2="%.2f"/>\n',
                                  -padding/2, y_pos, total_width + padding/2, y_pos))
      }
    }
    
    # Vertical guides  
    for (xi in 0:xn) {
      x_pos <- xi * (piece_width + offset) - offset/2
      if (xi > 0 && xi < xn) {
        svg <- paste0(svg, sprintf('    <line x1="%.2f" y1="%.0f" x2="%.2f" y2="%.0f"/>\n',
                                  x_pos, -padding/2, x_pos, total_height + padding/2))
      }
    }
    
    svg <- paste0(svg, '  </g>\n')
  }
  
  # Close SVG
  svg <- paste0(svg, '</g>\n</svg>')
  
  return(svg)
}

#' Enhanced puzzle generation with separation option
#' 
#' Extends generate_puzzle_svg to support piece separation for both
#' rectangular and hexagonal puzzles.
#' 
#' @param puzzle_structure Output from generate_puzzle_core() or extract_hexagonal_puzzle_structure()
#' @param mode "complete", "individual", or "separated"
#' @param colors Optional vector of colors for pieces
#' @param offset Separation distance for "separated" mode (in mm)
#' @param show_guides Show alignment guides in separated mode
#' @param arrangement For hexagonal: "hexagonal" or "rectangular" packing
#' @param stroke_width Line width for piece outlines (default 1.5)
#' @param background Background color for SVG (default "white")
#' @return SVG string
#' @export
generate_puzzle_svg_enhanced <- function(puzzle_structure, 
                                        mode = "complete", 
                                        colors = NULL,
                                        offset = 0,
                                        show_guides = TRUE,
                                        arrangement = "hexagonal",
                                        stroke_width = 1.5,
                                        background = "white") {
  
  # Check puzzle type
  puzzle_type <- puzzle_structure$type
  if (is.null(puzzle_type)) {
    # Assume rectangular for backward compatibility
    puzzle_type <- "rectangular"
  }
  
  if (puzzle_type == "hexagonal") {
    # Source hexagonal functions if not already loaded
    if (!exists("generate_separated_hexagonal_svg")) {
      source(system.file("R", "hexagonal_separation.R", package = "jigsawR"))
    }
    
    if (mode == "separated" || (mode == "individual" && offset > 0)) {
      # Use hexagonal separation function
      return(generate_separated_hexagonal_svg(
        rings = puzzle_structure$rings,
        seed = puzzle_structure$seed,
        diameter = puzzle_structure$diameter,
        offset = offset,
        arrangement = arrangement,
        tabsize = puzzle_structure$parameters$tabsize,
        jitter = puzzle_structure$parameters$jitter,
        do_warp = puzzle_structure$parameters$do_warp,
        do_trunc = puzzle_structure$parameters$do_trunc,
        colors = colors,
        stroke_width = stroke_width,
        background = background
      ))
    } else {
      # Use standard hexagonal generation
      return(generate_hex_jigsaw_svg(
        rings = puzzle_structure$rings,
        diameter = puzzle_structure$diameter,
        seed = puzzle_structure$seed,
        tabsize = puzzle_structure$parameters$tabsize,
        jitter = puzzle_structure$parameters$jitter,
        do_warp = puzzle_structure$parameters$do_warp,
        do_trunc = puzzle_structure$parameters$do_trunc,
        stroke_width = stroke_width,
        background = background
      ))
    }
  } else {
    # Rectangular puzzle handling (existing code)
    if (mode == "separated" || (mode == "individual" && offset > 0)) {
      # Use separation function
      return(generate_separated_puzzle_svg(
        puzzle_structure = puzzle_structure,
        offset = offset,
        colors = colors,
        stroke_width = stroke_width,
        background = background
      ))
    } else {
      # Use original function
      return(generate_puzzle_svg(
        puzzle_structure = puzzle_structure,
        mode = mode,
        colors = colors,
        stroke_width = stroke_width,
        background = background
      ))
    }
  }
}

#' Calculate optimal offset for laser cutting
#' 
#' Determines appropriate separation based on piece size and kerf.
#' 
#' @param piece_width Average width of pieces
#' @param piece_height Average height of pieces
#' @param kerf Laser kerf width (material removed by cutting)
#' @param min_separation Minimum separation between pieces
#' @return Recommended offset value
#' @export
calculate_optimal_offset <- function(piece_width, piece_height, 
                                    kerf = 0.2, min_separation = 2) {
  # Minimum offset should account for:
  # 1. Kerf (material removed)
  # 2. Minimum safe separation
  # 3. Percentage of piece size for visual clarity
  
  min_offset <- kerf + min_separation
  proportional_offset <- min(piece_width, piece_height) * 0.05  # 5% of piece size
  
  return(max(min_offset, proportional_offset))
}