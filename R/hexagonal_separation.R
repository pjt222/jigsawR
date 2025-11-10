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
  
  # Calculate positions for each ring (excluding the outer border ring)
  piece_index <- 1
  for (ring in 1:(rings - 1)) {
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
    viewBox = list(
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
    viewBox = list(
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
  
  # Generate individual pieces at separated positions
  svg_lines <- c(svg_lines,
    '  <g id="separated-hexagonal-puzzle">'
  )
  
  # First generate individual pieces using standard function  
  individual_result <- generate_hexagonal_individual_pieces(
    rings = rings, seed = seed, diameter = diameter,
    tabsize = tabsize, jitter = jitter,
    do_warp = do_warp, do_trunc = do_trunc,
    colors = colors, stroke_width = stroke_width,
    save_files = FALSE
  )
  
  # Show complete puzzle at original position (faded) for reference
  svg_lines <- c(svg_lines,
    '    <g id="original-puzzle-reference" opacity="0.2">',
    sprintf('      <path d="%s" fill="none" stroke="gray" stroke-width="%.1f"/>',
            puzzle_struct$paths$horizontal, stroke_width * 0.5),
    sprintf('      <path d="%s" fill="none" stroke="gray" stroke-width="%.1f"/>',
            puzzle_struct$paths$vertical, stroke_width * 0.5),
    sprintf('      <path d="%s" fill="none" stroke="gray" stroke-width="%.1f"/>',
            puzzle_struct$paths$border, stroke_width * 0.8),
    '    </g>'
  )
  
  # Add piece representations using cumulative offset grid arrangement
  svg_lines <- c(svg_lines, '    <g id="separated-pieces">')
  
  # Calculate grid dimensions for optimal arrangement
  num_pieces <- puzzle_struct$num_pieces
  pieces_per_row <- ceiling(sqrt(num_pieces))
  
  piece_num <- 1
  piece_size <- diameter / (rings * 3)  # Base piece size
  
  # Use cumulative offset approach as requested
  for (piece_index in 1:num_pieces) {
    # Calculate grid position (0-based)
    row <- floor((piece_index - 1) / pieces_per_row)
    col <- (piece_index - 1) %% pieces_per_row
    
    # Apply cumulative offsets as requested:
    # Piece 1: (0, 0), Piece 2: (0 + offset, 0), Piece 3: (0 + offset + offset, 0), etc.
    cumulative_x <- col * (piece_size + offset)
    cumulative_y <- row * (piece_size + offset)
    
    # Center the grid
    total_grid_width <- pieces_per_row * (piece_size + offset) - offset
    total_grid_height <- ceiling(num_pieces / pieces_per_row) * (piece_size + offset) - offset
    centered_x <- cumulative_x - total_grid_width / 2
    centered_y <- cumulative_y - total_grid_height / 2
    
    # Get color for this piece
    piece_color <- "black"
    if (!is.null(colors) && length(colors) > 0) {
      if (length(colors) == 1 && colors[1] != "black") {
        piece_color <- colors[1]
      } else if (length(colors) > 1) {
        piece_color <- colors[((piece_num - 1) %% length(colors)) + 1]
      }
    }
    
    # Use actual piece shape instead of placeholder hexagon
    # Generate individual piece to get its actual path
    if (!exists("generate_hexagonal_individual_pieces")) {
      # Load the function if not available
      if (file.exists("R/hexagonal_individual_pieces.R")) {
        source("R/hexagonal_individual_pieces.R")
      }
    }
    
    # Try to get the actual piece shape
    piece_path <- get_individual_hexagonal_piece_path(piece_index, rings, seed, diameter, 
                                                     tabsize, jitter, do_warp, do_trunc)
    
    # Transform the piece path to the separated position
    if (!is.null(piece_path) && nchar(piece_path) > 10) {
      transformed_path <- transform_hexagonal_piece_to_position(
        piece_path, centered_x, centered_y, piece_index
      )
      
      svg_lines <- c(svg_lines,
        sprintf('      <path d="%s" fill="none" stroke="%s" stroke-width="%.1f" opacity="0.9"/>',
                transformed_path, piece_color, stroke_width)
      )
    } else {
      # Fallback to hexagon if actual piece not available
      hex_radius <- piece_size * 0.4
      svg_lines <- c(svg_lines,
        sprintf('      <polygon points="%.2f,%.2f %.2f,%.2f %.2f,%.2f %.2f,%.2f %.2f,%.2f %.2f,%.2f" fill="none" stroke="%s" stroke-width="%.1f" opacity="0.8"/>',
                centered_x + hex_radius, centered_y,
                centered_x + hex_radius/2, centered_y - hex_radius*0.866,
                centered_x - hex_radius/2, centered_y - hex_radius*0.866,
                centered_x - hex_radius, centered_y,
                centered_x - hex_radius/2, centered_y + hex_radius*0.866,
                centered_x + hex_radius/2, centered_y + hex_radius*0.866,
                piece_color, stroke_width)
      )
    }
    
    # Piece number label
    svg_lines <- c(svg_lines,
      sprintf('      <text x="%.2f" y="%.2f" text-anchor="middle" dominant-baseline="central" font-family="Arial, sans-serif" font-size="8" font-weight="bold" fill="%s">%d</text>',
              centered_x, centered_y + 1, piece_color, piece_num)
    )
    
    piece_num <- piece_num + 1
  }
  
  svg_lines <- c(svg_lines, '    </g>')
  
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

#' Get individual hexagonal piece path
#' 
#' Retrieves the actual path for a specific piece in a hexagonal puzzle
#' 
#' @param piece_index Index of the piece (1-based)
#' @param rings Number of rings
#' @param seed Random seed
#' @param diameter Puzzle diameter
#' @param tabsize Tab size percentage
#' @param jitter Jitter percentage
#' @param do_warp Apply circular warping
#' @param do_trunc Truncate edge pieces
#' @return SVG path string for the piece, or NULL if not available
get_individual_hexagonal_piece_path <- function(piece_index, rings, seed, diameter,
                                               tabsize, jitter, do_warp, do_trunc) {
  
  # Try to generate individual pieces efficiently
  tryCatch({
    # Generate just the pieces we need
    individual_result <- generate_hexagonal_individual_pieces(
      rings = rings, seed = seed, diameter = diameter,
      tabsize = tabsize, jitter = jitter,
      do_warp = do_warp, do_trunc = do_trunc,
      save_files = FALSE
    )
    
    if (piece_index <= length(individual_result$pieces)) {
      return(individual_result$pieces[[piece_index]]$path)
    }
  }, error = function(e) {
    cat(sprintf("Could not generate individual piece %d: %s\n", piece_index, e$message))
  })
  
  return(NULL)
}

#' Transform hexagonal piece to new position
#' 
#' Applies translation to move a piece from its original position to a new location
#' 
#' @param piece_path Original SVG path
#' @param new_x New X position
#' @param new_y New Y position  
#' @param piece_index Piece index for unique transformations
#' @return Transformed SVG path
transform_hexagonal_piece_to_position <- function(piece_path, new_x, new_y, piece_index) {
  
  # Simple translation approach
  # Extract coordinates and apply offset
  coords <- regmatches(piece_path, gregexpr("-?\\d+\\.?\\d*", piece_path))[[1]]
  coords_num <- as.numeric(coords)
  coords_num <- coords_num[!is.na(coords_num)]
  
  if (length(coords_num) >= 2) {
    # Calculate offset from first coordinate to new position
    offset_x <- new_x - coords_num[1]
    offset_y <- new_y - coords_num[2]
    
    # Apply offset to all coordinates
    for (i in seq(1, length(coords_num), by = 2)) {
      if (i + 1 <= length(coords_num)) {
        coords_num[i] <- coords_num[i] + offset_x
        coords_num[i + 1] <- coords_num[i + 1] + offset_y
      }
    }
    
    # Replace coordinates in the original path
    coord_strings <- sprintf("%.2f", coords_num)
    transformed_path <- piece_path
    for (i in 1:length(coords)) {
      if (i <= length(coord_strings)) {
        transformed_path <- sub("-?\\d+\\.?\\d*", coord_strings[i], transformed_path)
      }
    }
    
    return(transformed_path)
  }
  
  return(piece_path)
}

#' Calculate bounds for hexagonal pieces
#'
#' @param pieces List of hexagonal pieces
#' @return List of bounding box data for each piece
calculate_hexagonal_piece_bounds <- function(pieces) {
  
  bounds_list <- list()
  
  for (i in seq_along(pieces)) {
    piece <- pieces[[i]]
    
    if (!is.null(piece$path)) {
      coords <- extract_coordinates_from_path(piece$path)
      
      if (length(coords) >= 4) {
        x_coords <- coords[seq(1, length(coords), by = 2)]
        y_coords <- coords[seq(2, length(coords), by = 2)]
        
        bounds_list[[i]] <- list(
          x_min = min(x_coords, na.rm = TRUE),
          x_max = max(x_coords, na.rm = TRUE),
          y_min = min(y_coords, na.rm = TRUE),
          y_max = max(y_coords, na.rm = TRUE),
          width = max(x_coords, na.rm = TRUE) - min(x_coords, na.rm = TRUE),
          height = max(y_coords, na.rm = TRUE) - min(y_coords, na.rm = TRUE)
        )
      } else {
        # Default bounds if coordinates can't be extracted
        bounds_list[[i]] <- list(
          x_min = -10, x_max = 10, y_min = -10, y_max = 10,
          width = 20, height = 20
        )
      }
    } else {
      # Default bounds for pieces without paths
      bounds_list[[i]] <- list(
        x_min = -10, x_max = 10, y_min = -10, y_max = 10,
        width = 20, height = 20
      )
    }
  }
  
  return(bounds_list)
}