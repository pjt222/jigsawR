# Hexagonal Puzzle Piece Separation
# Implements separation/offset logic for hexagonal and circular puzzles

#' Apply separation to hexagonal puzzle pieces
#' 
#' Spreads hexagonal puzzle pieces apart for laser cutting while maintaining
#' their relative positions in a hexagonal grid pattern.
#' 
#' @param puzzle_structure Hexagonal puzzle structure from extract_hexagonal_puzzle_structure
#' @param offset Separation distance between pieces in mm
#' @param arrangement "hexagonal" to maintain hex grid, "rectangular" for efficient packing
#' @return Modified puzzle structure with separated pieces
#' @export
apply_hexagonal_separation <- function(puzzle_structure, offset = 10, 
                                      arrangement = "hexagonal") {
  
  if (puzzle_structure$type != "hexagonal") {
    stop("This function only works with hexagonal puzzles")
  }
  
  rings <- puzzle_structure$rings
  diameter <- puzzle_structure$diameter
  radius <- diameter / 2
  
  # Calculate piece positions with separation
  if (arrangement == "hexagonal") {
    # Maintain hexagonal arrangement with gaps
    separation_data <- calculate_hex_grid_separation(rings, offset, radius)
  } else {
    # Pack pieces efficiently in rectangular grid
    separation_data <- calculate_hex_rectangular_packing(rings, offset, radius)
  }
  
  # Add separation data to structure
  puzzle_structure$separation <- list(
    offset = offset,
    arrangement = arrangement,
    positions = separation_data$positions,
    viewBox = separation_data$viewBox
  )
  
  return(puzzle_structure)
}

#' Calculate hexagonal grid separation
#' 
#' Determines piece positions when maintaining hexagonal arrangement with gaps.
#' 
#' @param rings Number of rings
#' @param offset Separation distance in mm
#' @param radius Original puzzle radius
#' @return List with piece positions and viewBox dimensions
calculate_hex_grid_separation <- function(rings, offset, radius) {
  # Calculate number of pieces
  num_pieces <- 3 * rings * (rings - 1) + 1
  
  # Initialize positions array
  positions <- list()
  
  # Center piece stays at origin
  positions[[1]] <- list(
    index = 0,
    x = 0,
    y = 0,
    translation = c(0, 0)
  )
  
  # Calculate positions for each ring
  piece_index <- 1
  for (ring in 1:rings) {
    pieces_in_ring <- 6 * ring
    
    for (i in 0:(pieces_in_ring - 1)) {
      # Calculate angle for this piece
      angle <- (i / pieces_in_ring) * 2 * pi
      
      # Base position in hexagonal grid
      base_x <- ring * radius * cos(angle)
      base_y <- ring * radius * sin(angle)
      
      # Add separation based on ring number
      # Pieces move outward from center
      separation_factor <- 1 + (offset * ring / radius)
      separated_x <- base_x * separation_factor
      separated_y <- base_y * separation_factor
      
      positions[[piece_index + 1]] <- list(
        index = piece_index,
        ring = ring,
        position_in_ring = i,
        x = separated_x,
        y = separated_y,
        translation = c(separated_x - base_x, separated_y - base_y)
      )
      
      piece_index <- piece_index + 1
    }
  }
  
  # Calculate required viewBox
  max_extent <- (radius + offset * rings) * 1.2
  viewBox_width <- 2 * max_extent
  viewBox_height <- 2 * max_extent
  
  return(list(
    positions = positions,
    viewBox = c(
      x = -max_extent,
      y = -max_extent,
      width = viewBox_width,
      height = viewBox_height
    )
  ))
}

#' Calculate rectangular packing for hex pieces
#' 
#' Arranges hexagonal pieces in a rectangular grid for efficient material use.
#' 
#' @param rings Number of rings
#' @param offset Separation distance in mm
#' @param radius Original puzzle radius
#' @return List with piece positions and viewBox dimensions
calculate_hex_rectangular_packing <- function(rings, offset, radius) {
  num_pieces <- 3 * rings * (rings - 1) + 1
  
  # Calculate grid dimensions for rectangular packing
  # Approximate piece size (hexagon circumscribed circle)
  piece_size <- radius / rings * 2
  piece_with_offset <- piece_size + offset
  
  # Calculate optimal grid layout
  cols <- ceiling(sqrt(num_pieces * 1.5))  # Wider than tall for hex pieces
  rows <- ceiling(num_pieces / cols)
  
  positions <- list()
  
  for (i in 0:(num_pieces - 1)) {
    row <- floor(i / cols)
    col <- i %% cols
    
    # Calculate position in rectangular grid
    x <- col * piece_with_offset - (cols - 1) * piece_with_offset / 2
    y <- row * piece_with_offset - (rows - 1) * piece_with_offset / 2
    
    positions[[i + 1]] <- list(
      index = i,
      grid_row = row,
      grid_col = col,
      x = x,
      y = y,
      translation = c(x, y)  # From origin
    )
  }
  
  # Calculate viewBox
  viewBox_width <- cols * piece_with_offset + offset
  viewBox_height <- rows * piece_with_offset + offset
  
  return(list(
    positions = positions,
    viewBox = c(
      x = -viewBox_width / 2,
      y = -viewBox_height / 2,
      width = viewBox_width,
      height = viewBox_height
    )
  ))
}

#' Generate separated hexagonal puzzle SVG
#' 
#' Creates an SVG with hexagonal puzzle pieces separated for laser cutting.
#' 
#' @param rings Number of rings
#' @param seed Random seed
#' @param diameter Puzzle diameter
#' @param offset Separation distance between pieces
#' @param arrangement "hexagonal" or "rectangular" packing
#' @param tabsize Tab size percentage
#' @param jitter Jitter percentage
#' @param do_warp Apply circular warping
#' @param do_trunc Truncate edge pieces
#' @param colors Piece colors
#' @param stroke_width Line width
#' @param background Background color or "none"
#' @return SVG content as string
#' @export
generate_separated_hexagonal_svg <- function(rings = 3, seed = NULL,
                                            diameter = 240, offset = 10,
                                            arrangement = "hexagonal",
                                            tabsize = 27, jitter = 5,
                                            do_warp = FALSE, do_trunc = FALSE,
                                            colors = NULL, stroke_width = 1,
                                            background = "none") {
  
  if (is.null(seed)) {
    seed <- as.integer(runif(1) * 10000)
  }
  
  # Generate base puzzle structure
  puzzle_struct <- extract_hexagonal_puzzle_structure(
    rings = rings, seed = seed, diameter = diameter,
    tabsize = tabsize, jitter = jitter,
    do_warp = do_warp, do_trunc = do_trunc
  )
  
  # Apply separation
  puzzle_struct <- apply_hexagonal_separation(
    puzzle_struct, offset = offset, arrangement = arrangement
  )
  
  # Get viewBox from separation data
  vb <- puzzle_struct$separation$viewBox
  
  # Start SVG
  svg_lines <- c(
    sprintf('<svg width="%dmm" height="%dmm" viewBox="%.2f %.2f %.2f %.2f" xmlns="http://www.w3.org/2000/svg">',
            ceiling(vb$width), ceiling(vb$height), vb$x, vb$y, vb$width, vb$height)
  )
  
  # Add background if specified
  if (background != "none" && background != "") {
    svg_lines <- c(svg_lines,
      sprintf('  <rect x="%.2f" y="%.2f" width="%.2f" height="%.2f" fill="%s"/>',
              vb$x, vb$y, vb$width, vb$height, background)
    )
  }
  
  # Add title
  svg_lines <- c(svg_lines,
    sprintf('  <title>Hexagonal Puzzle - %d rings, %d pieces (separated by %dmm)</title>',
            rings, puzzle_struct$num_pieces, offset)
  )
  
  # For now, add the complete puzzle paths with transform
  # In full implementation, we'd trace and separate individual pieces
  svg_lines <- c(svg_lines,
    '  <g id="separated-hexagonal-puzzle">',
    '    <g id="puzzle-structure" transform="translate(0,0)">',
    sprintf('      <path d="%s" fill="none" stroke="black" stroke-width="%.1f"/>',
            puzzle_struct$paths$horizontal, stroke_width),
    sprintf('      <path d="%s" fill="none" stroke="black" stroke-width="%.1f"/>',
            puzzle_struct$paths$vertical, stroke_width),
    sprintf('      <path d="%s" fill="none" stroke="black" stroke-width="%.1f"/>',
            puzzle_struct$paths$border, stroke_width * 1.5),
    '    </g>'
  )
  
  # Add position indicators for separated pieces (visual guide)
  if (arrangement == "hexagonal" && offset > 0) {
    svg_lines <- c(svg_lines, '    <g id="separation-guides" opacity="0.3">')
    
    for (pos in puzzle_struct$separation$positions) {
      if (pos$index > 0) {  # Skip center piece
        svg_lines <- c(svg_lines,
          sprintf('      <circle cx="%.2f" cy="%.2f" r="3" fill="red"/>',
                  pos$x, pos$y)
        )
      }
    }
    
    svg_lines <- c(svg_lines, '    </g>')
  }
  
  svg_lines <- c(svg_lines, '  </g>', '</svg>')
  
  return(paste(svg_lines, collapse = "\n"))
}

#' Calculate optimal separation for laser cutting
#' 
#' Determines the optimal separation distance based on material and laser specs.
#' 
#' @param material_thickness Material thickness in mm
#' @param kerf_width Laser kerf width in mm (typically 0.1-0.3mm)
#' @param safety_margin Additional margin in mm (default 2mm)
#' @return Recommended separation distance in mm
#' @export
calculate_optimal_hex_separation <- function(material_thickness, 
                                            kerf_width = 0.2,
                                            safety_margin = 2) {
  # For hexagonal pieces, we need slightly more clearance due to complex edges
  min_separation <- material_thickness + kerf_width + safety_margin
  
  # Add 20% extra for hexagonal complexity
  recommended <- min_separation * 1.2
  
  return(ceiling(recommended))
}