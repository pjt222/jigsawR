# Main Puzzle Generation Orchestrator
# Part of the jigsawR package

#' Generate puzzle layers using SVG to PNG conversion (main orchestration function)
#' @param seed Random seed for puzzle
#' @param diameter Puzzle diameter
#' @param rings Number of rings
#' @param tabsize Tab size percentage
#' @param jitter Jitter percentage
#' @param base_filename Base name for output files
#' @param size_px Output size in pixels
#' @param line_color Color for puzzle lines
#' @param line_width Width for puzzle lines
#' @param transparent_background Logical, whether to make combined image background transparent
#' @return List with file paths and parameters, or NULL if failed
#' @export
generate_svg_puzzle_layers <- function(seed = 1234, diameter = 240, rings = 4,
                                      tabsize = 27, jitter = 5, 
                                      base_filename = "svg_puzzle", 
                                      size_px = 2000,
                                      line_color = "black", line_width = 2.0,
                                      transparent_background = FALSE) {
  
  cat("Generating SVG puzzle (seed:", seed, ", rings:", rings, ")...\n")
  
  # Check conversion tools before starting
  tools_status <- check_conversion_tools()
  if (!tools_status$any_available) {
    return(NULL)
  }
  
  # Create enhanced SVG
  puzzle_svg <- create_enhanced_puzzle_svg(
    seed = seed, diameter = diameter, rings = rings,
    tabsize = tabsize, jitter = jitter,
    line_color = line_color, line_width = line_width
  )
  
  # Save the SVG file
  svg_file <- paste0(base_filename, ".svg")
  save_enhanced_svg(puzzle_svg$svg_content, svg_file)
  
  # Convert SVG to PNG overlay
  overlay_file <- paste0(base_filename, "_overlay.png")
  conversion_success <- convert_svg_to_png(puzzle_svg$svg_content, overlay_file, size_px, size_px)
  
  if (!conversion_success) {
    cat("  Failed to create PNG overlay\n")
    return(NULL)
  } else {
    cat("  Overlay PNG saved:", overlay_file, "\n")
  }
  
  # Create gradient background
  cat("  Creating gradient background...\n")
  gradient_plot <- create_gradient_circle_png(size_px, diameter)
  
  background_file <- paste0(base_filename, "_background.png")
  save_gradient_background(gradient_plot, background_file, size_px)
  
  # Combine layers
  combined_file <- paste0(base_filename, "_combined.png")
  # Ensure we use the full paths with output/ prefix
  background_file_full <- if (!grepl("^output/", background_file)) file.path("output", background_file) else background_file
  overlay_file_full <- if (!grepl("^output/", overlay_file)) file.path("output", overlay_file) else overlay_file
  combination_success <- combine_image_layers(background_file_full, overlay_file_full, combined_file, transparent_background)
  
  return(list(
    svg_file = svg_file,
    background_file = background_file,
    overlay_file = overlay_file,
    combined_file = if(combination_success) combined_file else NULL,
    parameters = puzzle_svg$parameters,
    success = TRUE
  ))
}

#' Generate multiple puzzle variations
#' @param variations List of variation parameters, each containing seed, rings, etc.
#' @return List of results for each variation
#' @export
generate_puzzle_variations <- function(variations) {
  
  cat("=== SVG to PNG Conversion Approach ===\n")
  
  # Check tools once at the beginning
  tools_status <- check_conversion_tools()
  if (!tools_status$any_available) {
    return(NULL)
  }
  
  cat("\nGenerating puzzle variations...\n\n")
  
  results <- list()
  
  for (i in seq_along(variations)) {
    variation <- variations[[i]]
    
    # Set defaults for missing parameters
    params <- list(
      seed = variation$seed %||% 1234,
      rings = variation$rings %||% 4,
      diameter = variation$diameter %||% 200,
      tabsize = variation$tabsize %||% 27,
      jitter = variation$jitter %||% 5,
      base_filename = variation$base_filename %||% paste0("svg_puzzle_", i),
      size_px = variation$size_px %||% 2000,
      line_color = variation$line_color %||% "black",
      line_width = variation$line_width %||% 2.0,
      transparent_background = variation$transparent_background %||% FALSE
    )
    
    # Generate puzzle using main function
    result <- do.call(generate_svg_puzzle_layers, params)
    results[[i]] <- result
    
    if (is.null(result)) {
      cat("  Failed to generate variation", i, "\n")
    }
    
    cat("\n")
  }
  
  cat("All SVG-based puzzles generated successfully!\n")
  
  return(results)
}

# Helper function for default values (R equivalent of JavaScript's || operator)
`%||%` <- function(a, b) if (is.null(a)) b else a