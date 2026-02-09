# SVG Enhancement and Conversion Utilities
# Part of the jigsawR package

#' Create enhanced SVG with better styling for conversion
#' @param seed Random seed for puzzle generation
#' @param diameter Puzzle diameter in mm
#' @param rings Number of rings in the puzzle
#' @param tabsize Tab size percentage
#' @param jitter Jitter percentage
#' @param line_color Color for puzzle lines
#' @param line_width Width for puzzle lines
#' @return Enhanced SVG content
create_enhanced_puzzle_svg <- function(seed = 1234, diameter = 240, rings = 4,
                                      tabsize = 27, jitter = 5, 
                                      line_color = "black", line_width = 2.0) {
  
  # Generate circular jigsaw puzzle
  puzzle <- generate_hex_jigsaw_svg(
    seed = seed,
    tabsize = tabsize,
    jitter = jitter,
    diameter = diameter,
    rings = rings,
    do_warp = TRUE,    # Enable circular warp
    do_trunc = TRUE    # Truncate for clean circle
  )
  
  # Get puzzle dimensions
  radius <- puzzle$parameters$diameter / 2.0
  offset <- radius * 0.2
  width <- 2.0 * (radius + offset)
  height <- 2.0 * (radius + offset)
  
  # Create enhanced SVG with better styling
  enhanced_svg <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>\n',
    '<svg xmlns="http://www.w3.org/2000/svg" version="1.1" ',
    'width="', width, '" height="', height, '" ',
    'viewBox="0 0 ', width, ' ', height, '">\n',
    
    # Add transparent background
    '<rect width="100%" height="100%" fill="transparent"/>\n',
    
    # Add puzzle paths with enhanced styling
    '<path fill="none" stroke="', line_color, '" stroke-width="', line_width, '" ',
    'stroke-linecap="round" stroke-linejoin="round" ',
    'stroke-opacity="1.0" d="', puzzle$horizontal, '"/>\n',
    
    '<path fill="none" stroke="', line_color, '" stroke-width="', line_width, '" ',
    'stroke-linecap="round" stroke-linejoin="round" ',
    'stroke-opacity="1.0" d="', puzzle$vertical, '"/>\n',
    
    '<path fill="none" stroke="black" stroke-width="', line_width * 1.5, '" ',
    'stroke-linecap="round" stroke-linejoin="round" ',
    'stroke-opacity="1.0" d="', puzzle$border, '"/>\n',
    
    '</svg>'
  )
  
  return(list(
    svg_content = enhanced_svg,
    parameters = puzzle$parameters,
    width = width,
    height = height
  ))
}

#' Save SVG content to file
#' @param svg_content SVG content string
#' @param svg_file Output filename for SVG
save_enhanced_svg <- function(svg_content, svg_file) {
  # Ensure parent directory exists
  output_parent <- dirname(svg_file)
  if (nzchar(output_parent) && output_parent != "." && !dir.exists(output_parent)) {
    dir.create(output_parent, recursive = TRUE)
  }

  writeLines(svg_content, svg_file)
  log_success("SVG saved: {.file {svg_file}}")
}