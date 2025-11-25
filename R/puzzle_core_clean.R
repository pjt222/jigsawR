# Clean Puzzle Core Implementation
# Pure algorithmic puzzle generation with shared edges between pieces
# No hard-coded adjustments or manual tinkering

#' Generate puzzle with reproducible seed-based randomization
#' 
#' This is the clean core implementation that ensures:
#' - Deterministic output for same seed
#' - Adjacent pieces share exact same edge path
#' - No hard-coded special cases
#' 
#' @param seed Random seed for reproducibility
#' @param grid Vector c(rows, columns) for puzzle dimensions
#' @param size Vector c(width, height) in specified units
#' @param unit Unit specification: "mm" (millimeters) or "px" (pixels)
#' @param dpi DPI for pixel/mm conversion (96=screen, 300=print quality)
#' @param tabsize Tab size as percentage (10-30)
#' @param jitter Jitter as percentage (0-10)
#' @return List with puzzle data and parameters
#' @export
generate_puzzle_core <- function(seed = 1234, 
                                grid = c(2, 2),
                                size = c(200, 200),
                                unit = "mm",
                                dpi = 96,
                                tabsize = 20,
                                jitter = 4) {
  
  # Conversion utilities
  px_to_mm <- function(px_value, dpi) px_value * 25.4 / dpi
  mm_to_px <- function(mm_value, dpi) mm_value * dpi / 25.4
  
  # Extract dimensions
  yn <- grid[1]  # rows
  xn <- grid[2]  # columns
  width <- size[1]
  height <- size[2]
  
  # Convert to mm for internal calculations (algorithms expect mm)
  if (unit == "px") {
    width_mm <- px_to_mm(width, dpi)
    height_mm <- px_to_mm(height, dpi)
  } else {
    width_mm <- width
    height_mm <- height
  }
  
  # Initialize the jigsaw environment from rectangular_puzzle.R
  init_jigsaw(seed = seed, 
             tabsize = tabsize, 
             jitter = jitter,
             width = width_mm, 
             height = height_mm,
             xn = xn, 
             yn = yn)
  
  # Generate shared edges for the entire puzzle
  edges <- generate_all_edges(xn, yn)
  
  # Store complete puzzle structure
  puzzle_structure <- list(
    seed = seed,
    grid = grid,
    size = size,              # Original size in user units
    unit = unit,              # Unit specification
    dpi = dpi,                # DPI setting
    size_mm = c(width_mm, height_mm),  # Size in mm (internal calculations)
    tabsize = tabsize,
    jitter = jitter,
    edges = edges,
    piece_width = width_mm / xn,      # Piece dimensions in mm
    piece_height = height_mm / yn
  )
  
  return(puzzle_structure)
}

#' Generate all shared edges for the puzzle
#' 
#' Creates the complete set of edges that pieces will share.
#' Each edge is defined once and used by both adjacent pieces.
#' 
#' @param xn Number of columns
#' @param yn Number of rows
#' @return List with horizontal and vertical edge paths
generate_all_edges <- function(xn, yn) {
  
  edges <- list(
    horizontal = list(),  # Horizontal edges (between rows)
    vertical = list()     # Vertical edges (between columns)
  )
  
  # Generate horizontal edges (dividing rows)
  # These run from left to right
  .jigsaw_env$vertical <- 0
  for (yi in 1:(yn - 1)) {
    .jigsaw_env$yi <- yi
    .jigsaw_env$xi <- 0
    
    row_edges <- list()
    first()  # Initialize tab parameters
    
    for (xi in 0:(xn - 1)) {
      .jigsaw_env$xi <- xi
      
      # Calculate edge endpoints
      x1 <- .jigsaw_env$offset + xi * (.jigsaw_env$width / xn)
      x2 <- .jigsaw_env$offset + (xi + 1) * (.jigsaw_env$width / xn)
      y <- .jigsaw_env$offset + yi * (.jigsaw_env$height / yn)
      
      # Generate the bezier path for this edge segment
      edge_path <- generate_edge_segment(c(x1, y), c(x2, y), vertical = FALSE)
      
      row_edges[[xi + 1]] <- edge_path
      
      if (xi < xn - 1) {
        next_tab()
      }
    }
    
    edges$horizontal[[yi]] <- row_edges
  }
  
  # Generate vertical edges (dividing columns)
  # These run from top to bottom
  .jigsaw_env$vertical <- 1
  for (xi in 1:(xn - 1)) {
    .jigsaw_env$xi <- xi
    .jigsaw_env$yi <- 0
    
    col_edges <- list()
    first()  # Initialize tab parameters
    
    for (yi in 0:(yn - 1)) {
      .jigsaw_env$yi <- yi
      
      # Calculate edge endpoints
      x <- .jigsaw_env$offset + xi * (.jigsaw_env$width / xn)
      y1 <- .jigsaw_env$offset + yi * (.jigsaw_env$height / yn)
      y2 <- .jigsaw_env$offset + (yi + 1) * (.jigsaw_env$height / yn)
      
      # Generate the bezier path for this edge segment
      edge_path <- generate_edge_segment(c(x, y1), c(x, y2), vertical = TRUE)
      
      col_edges[[yi + 1]] <- edge_path
      
      if (yi < yn - 1) {
        next_tab()
      }
    }
    
    edges$vertical[[xi]] <- col_edges
  }
  
  return(edges)
}

#' Generate a single edge segment with bezier curves
#' 
#' Creates the bezier curve path for one edge segment.
#' This path will be shared by two adjacent pieces.
#' 
#' @param start_point c(x, y) starting coordinates
#' @param end_point c(x, y) ending coordinates
#' @param vertical TRUE for vertical edge, FALSE for horizontal
#' @return List with forward and reverse path strings
generate_edge_segment <- function(start_point, end_point, vertical = FALSE) {
  
  # Use the existing coordinate functions from rectangular_puzzle.R
  # These already handle the tab generation correctly
  
  if (!vertical) {
    # Horizontal edge - use existing l() and w() functions
    # The coordinates are already calculated in the environment
    
    # Get the bezier control points
    p1_l <- l(0.2); p1_w <- w(.jigsaw_env$a)
    p2_l <- l(0.5 + .jigsaw_env$b + .jigsaw_env$d); p2_w <- w(-.jigsaw_env$t + .jigsaw_env$c)
    p3_l <- l(0.5 - .jigsaw_env$t + .jigsaw_env$b); p3_w <- w(.jigsaw_env$t + .jigsaw_env$c)
    p4_l <- l(0.5 - 2.0 * .jigsaw_env$t + .jigsaw_env$b - .jigsaw_env$d); p4_w <- w(3.0 * .jigsaw_env$t + .jigsaw_env$c)
    p5_l <- l(0.5 + 2.0 * .jigsaw_env$t + .jigsaw_env$b - .jigsaw_env$d); p5_w <- w(3.0 * .jigsaw_env$t + .jigsaw_env$c)
    p6_l <- l(0.5 + .jigsaw_env$t + .jigsaw_env$b); p6_w <- w(.jigsaw_env$t + .jigsaw_env$c)
    p7_l <- l(0.5 + .jigsaw_env$b + .jigsaw_env$d); p7_w <- w(-.jigsaw_env$t + .jigsaw_env$c)
    p8_l <- l(0.8); p8_w <- w(.jigsaw_env$e)
    p9_l <- l(1.0); p9_w <- w(0.0)
    
    # Forward path (left to right)
    forward <- sprintf("C %.2f %.2f %.2f %.2f %.2f %.2f ", p1_l, p1_w, p2_l, p2_w, p3_l, p3_w)
    forward <- paste0(forward, sprintf("C %.2f %.2f %.2f %.2f %.2f %.2f ", p4_l, p4_w, p5_l, p5_w, p6_l, p6_w))
    forward <- paste0(forward, sprintf("C %.2f %.2f %.2f %.2f %.2f %.2f ", p7_l, p7_w, p8_l, p8_w, p9_l, p9_w))
    
    # Reverse path (right to left) - same points, reversed order with swapped control points
    # For a cubic bezier from P0 to P3 with control points P1, P2:
    # The reverse goes from P3 to P0 with control points P2, P1
    reverse <- sprintf("C %.2f %.2f %.2f %.2f %.2f %.2f ", p8_l, p8_w, p7_l, p7_w, p6_l, p6_w)
    reverse <- paste0(reverse, sprintf("C %.2f %.2f %.2f %.2f %.2f %.2f ", p5_l, p5_w, p4_l, p4_w, p3_l, p3_w))
    reverse <- paste0(reverse, sprintf("C %.2f %.2f %.2f %.2f %.2f %.2f ", p2_l, p2_w, p1_l, p1_w, start_point[1], start_point[2]))
    
  } else {
    # Vertical edge - swap l and w for vertical orientation
    p1_w <- w(.jigsaw_env$a); p1_l <- l(0.2)
    p2_w <- w(-.jigsaw_env$t + .jigsaw_env$c); p2_l <- l(0.5 + .jigsaw_env$b + .jigsaw_env$d)
    p3_w <- w(.jigsaw_env$t + .jigsaw_env$c); p3_l <- l(0.5 - .jigsaw_env$t + .jigsaw_env$b)
    p4_w <- w(3.0 * .jigsaw_env$t + .jigsaw_env$c); p4_l <- l(0.5 - 2.0 * .jigsaw_env$t + .jigsaw_env$b - .jigsaw_env$d)
    p5_w <- w(3.0 * .jigsaw_env$t + .jigsaw_env$c); p5_l <- l(0.5 + 2.0 * .jigsaw_env$t + .jigsaw_env$b - .jigsaw_env$d)
    p6_w <- w(.jigsaw_env$t + .jigsaw_env$c); p6_l <- l(0.5 + .jigsaw_env$t + .jigsaw_env$b)
    p7_w <- w(-.jigsaw_env$t + .jigsaw_env$c); p7_l <- l(0.5 + .jigsaw_env$b + .jigsaw_env$d)
    p8_w <- w(.jigsaw_env$e); p8_l <- l(0.8)
    p9_w <- w(0.0); p9_l <- l(1.0)
    
    # Forward path (top to bottom)
    forward <- sprintf("C %.2f %.2f %.2f %.2f %.2f %.2f ", p1_w, p1_l, p2_w, p2_l, p3_w, p3_l)
    forward <- paste0(forward, sprintf("C %.2f %.2f %.2f %.2f %.2f %.2f ", p4_w, p4_l, p5_w, p5_l, p6_w, p6_l))
    forward <- paste0(forward, sprintf("C %.2f %.2f %.2f %.2f %.2f %.2f ", p7_w, p7_l, p8_w, p8_l, p9_w, p9_l))
    
    # Reverse path (bottom to top)
    reverse <- sprintf("C %.2f %.2f %.2f %.2f %.2f %.2f ", p8_w, p8_l, p7_w, p7_l, p6_w, p6_l)
    reverse <- paste0(reverse, sprintf("C %.2f %.2f %.2f %.2f %.2f %.2f ", p5_w, p5_l, p4_w, p4_l, p3_w, p3_l))
    reverse <- paste0(reverse, sprintf("C %.2f %.2f %.2f %.2f %.2f %.2f ", p2_w, p2_l, p1_w, p1_l, start_point[1], start_point[2]))
  }
  
  return(list(
    forward = forward,
    reverse = reverse,
    start = start_point,
    end = end_point
  ))
}

#' Generate a single puzzle piece path
#' 
#' Creates the complete boundary path for one puzzle piece,
#' using the shared edges with adjacent pieces.
#' 
#' @param xi Column index (0-based)
#' @param yi Row index (0-based)
#' @param puzzle_structure Output from generate_puzzle_core()
#' @return SVG path string for the piece
#' @export
generate_single_piece <- function(xi, yi, puzzle_structure) {
  
  xn <- puzzle_structure$grid[2]
  yn <- puzzle_structure$grid[1]
  piece_width <- puzzle_structure$piece_width
  piece_height <- puzzle_structure$piece_height
  edges <- puzzle_structure$edges
  
  # Calculate corners
  x1 <- xi * piece_width
  y1 <- yi * piece_height
  x2 <- x1 + piece_width
  y2 <- y1 + piece_height
  
  # Start from top-left, go clockwise
  path <- sprintf("M %.2f %.2f ", x1, y1)
  
  # Top edge (left to right)
  if (yi == 0) {
    # Border - straight line
    path <- paste0(path, sprintf("L %.2f %.2f ", x2, y1))
  } else {
    # Shared edge with piece above - use forward direction
    edge <- edges$horizontal[[yi]][[xi + 1]]
    path <- paste0(path, edge$forward)
  }
  
  # Right edge (top to bottom)
  if (xi == xn - 1) {
    # Border - straight line
    path <- paste0(path, sprintf("L %.2f %.2f ", x2, y2))
  } else {
    # Shared edge with piece to the right - use forward direction
    edge <- edges$vertical[[xi + 1]][[yi + 1]]
    path <- paste0(path, edge$forward)
  }
  
  # Bottom edge (right to left)
  if (yi == yn - 1) {
    # Border - straight line
    path <- paste0(path, sprintf("L %.2f %.2f ", x1, y2))
  } else {
    # Shared edge with piece below - use REVERSE direction
    edge <- edges$horizontal[[yi + 1]][[xi + 1]]
    path <- paste0(path, edge$reverse)
  }
  
  # Left edge (bottom to top)
  if (xi == 0) {
    # Border - straight line
    path <- paste0(path, sprintf("L %.2f %.2f ", x1, y1))
  } else {
    # Shared edge with piece to the left - use REVERSE direction
    edge <- edges$vertical[[xi]][[yi + 1]]
    path <- paste0(path, edge$reverse)
  }
  
  # Close path
  path <- paste0(path, "Z")
  
  return(path)
}

#' Generate complete SVG with all pieces
#'
#' @param puzzle_structure Output from generate_puzzle_core()
#' @param mode "complete" for full puzzle, "individual" for separate pieces
#' @param colors Optional vector of colors for pieces (NULL = use viridis palette)
#' @param background Background color for the SVG (default: "white")
#' @param stroke_width Stroke width for puzzle lines (default: 1.5)
#' @param palette Viridis palette name (if colors is NULL)
#' @return SVG string
#' @export
generate_puzzle_svg <- function(puzzle_structure, mode = "complete", colors = NULL, background = "white", stroke_width = 1.5, palette = NULL) {

  xn <- puzzle_structure$grid[2]
  yn <- puzzle_structure$grid[1]
  width <- puzzle_structure$size[1]
  height <- puzzle_structure$size[2]

  # Add padding to prevent stroke clipping at viewBox boundary
  # SVG strokes are centered on the path, so half the stroke extends outside
  padding <- stroke_width / 2
  canvas_width <- width + stroke_width
  canvas_height <- height + stroke_width

  # Start SVG with expanded viewBox to accommodate border strokes
  svg <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1"
     width="%.0f" height="%.0f" viewBox="%.2f %.2f %.2f %.2f">\n',
     canvas_width, canvas_height, -padding, -padding, canvas_width, canvas_height)

  # Add background based on type
  # Background can be: "none", a color string, or a list with gradient colors
  # Important: Position background at viewBox origin to cover expanded area
  if (is.list(background) && !is.null(background$type) && background$type == "gradient") {
    # Custom gradient with user-specified colors
    center_color <- background$center
    middle_color <- background$middle
    edge_color <- background$edge
    svg <- paste0(svg, sprintf('  <defs>
    <radialGradient id="bg-gradient" cx="50%%" cy="50%%" r="50%%">
      <stop offset="0%%" style="stop-color:%s;stop-opacity:1" />
      <stop offset="50%%" style="stop-color:%s;stop-opacity:1" />
      <stop offset="100%%" style="stop-color:%s;stop-opacity:1" />
    </radialGradient>
  </defs>
  <rect x="%.2f" y="%.2f" width="%.0f" height="%.0f" fill="url(#bg-gradient)"/>\n',
                               center_color, middle_color, edge_color,
                               -padding, -padding, canvas_width, canvas_height))
  } else if (is.character(background) && background == "gradient") {
    # Legacy: default gradient colors for backward compatibility
    svg <- paste0(svg, sprintf('  <defs>
    <radialGradient id="bg-gradient" cx="50%%" cy="50%%" r="50%%">
      <stop offset="0%%" style="stop-color:#e3f2fd;stop-opacity:1" />
      <stop offset="50%%" style="stop-color:#bbdefb;stop-opacity:1" />
      <stop offset="100%%" style="stop-color:#90caf9;stop-opacity:1" />
    </radialGradient>
  </defs>
  <rect x="%.2f" y="%.2f" width="%.0f" height="%.0f" fill="url(#bg-gradient)"/>\n',
                              -padding, -padding, canvas_width, canvas_height))
  } else if (is.character(background) && (background == "none" || background == "")) {
    # No background rect
  } else if (is.character(background)) {
    # Solid color background
    svg <- paste0(svg, sprintf('  <rect x="%.2f" y="%.2f" width="%.0f" height="%.0f" fill="%s"/>\n',
                              -padding, -padding, canvas_width, canvas_height, background))
  }

  svg <- paste0(svg, '<g id="puzzle">\n')
  
  # Generate pieces
  if (mode == "complete") {
    # For complete puzzle, use full palette for edges
    edges <- puzzle_structure$edges

    # Count total edges (horizontal + vertical + 4 border sides)
    n_h_edges <- sum(sapply(edges$horizontal, length))
    n_v_edges <- sum(sapply(edges$vertical, length))
    total_edges <- n_h_edges + n_v_edges + 4  # +4 for border sides

    # Generate colors for all edges
    if (is.null(colors)) {
      edge_colors <- get_puzzle_colors(total_edges, palette)
    } else if (length(colors) == 1 && colors[1] == "black") {
      # Special case: solid black uses single color for all edges
      edge_colors <- rep(colors[1], total_edges)
    } else {
      # Cycle through provided colors
      edge_colors <- rep_len(colors, total_edges)
    }

    edge_idx <- 1

    # Draw all horizontal edges
    for (yi in seq_along(edges$horizontal)) {
      for (xi in seq_along(edges$horizontal[[yi]])) {
        edge <- edges$horizontal[[yi]][[xi]]
        svg <- paste0(svg, sprintf('  <path d="M %.2f %.2f %s" fill="none" stroke="%s" stroke-width="%.1f"/>\n',
                                  edge$start[1], edge$start[2], edge$forward, edge_colors[edge_idx], stroke_width))
        edge_idx <- edge_idx + 1
      }
    }

    # Draw all vertical edges
    for (xi in seq_along(edges$vertical)) {
      for (yi in seq_along(edges$vertical[[xi]])) {
        edge <- edges$vertical[[xi]][[yi]]
        svg <- paste0(svg, sprintf('  <path d="M %.2f %.2f %s" fill="none" stroke="%s" stroke-width="%.1f"/>\n',
                                  edge$start[1], edge$start[2], edge$forward, edge_colors[edge_idx], stroke_width))
        edge_idx <- edge_idx + 1
      }
    }

    # Draw border with remaining colors (4 sides)
    border_color <- edge_colors[edge_idx]  # Use next color for border
    svg <- paste0(svg, sprintf('  <rect x="0" y="0" width="%.0f" height="%.0f" fill="none" stroke="%s" stroke-width="%.1f"/>\n',
                              width, height, border_color, stroke_width))
    
  } else if (mode == "individual") {
    # Separate path for each piece
    if (is.null(colors)) {
      # Use viridis palette if no colors specified
      total_pieces <- xn * yn
      colors <- get_puzzle_colors(total_pieces, palette)
    }

    piece_num <- 0
    for (yi in 0:(yn - 1)) {
      for (xi in 0:(xn - 1)) {
        piece_path <- generate_single_piece(xi, yi, puzzle_structure)
        color <- colors[(piece_num %% length(colors)) + 1]

        svg <- paste0(svg, sprintf('  <path id="piece-%d-%d" d="%s" fill="none" stroke="%s" stroke-width="%.1f"/>\n',
                                  xi, yi, piece_path, color, stroke_width))
        piece_num <- piece_num + 1
      }
    }
  }
  
  # Close SVG
  svg <- paste0(svg, '</g>\n</svg>')
  
  return(svg)
}