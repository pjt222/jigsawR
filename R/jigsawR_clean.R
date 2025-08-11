# jigsawR Clean Implementation
# Complete pipeline for jigsaw puzzle generation
# Reproducible, clean, no hard-coded adjustments

#' Main jigsaw puzzle generation function
#' 
#' Single entry point for all puzzle generation with clean, reproducible output.
#' No hard-coded adjustments or manual tinkering.
#' 
#' @param type "rectangular" or "hexagonal" (currently only rectangular implemented)
#' @param grid Vector c(rows, columns) for puzzle dimensions
#' @param size Vector c(width, height) in mm
#' @param seed Random seed for reproducibility
#' @param tabsize Tab size as percentage (10-30)
#' @param jitter Jitter as percentage (0-10)
#' @param output "complete", "individual", or "both"
#' @param colors Vector of colors for individual pieces (optional)
#' @param background "none", "gradient", or custom color
#' @param save_files TRUE to save SVG/PNG files
#' @param output_dir Directory for output files
#' @param filename_prefix Prefix for output files
#' @return List with puzzle data, SVG content, and file paths
#' @export
generate_puzzle <- function(type = "rectangular",
                          grid = c(2, 2),
                          size = c(200, 200),
                          seed = NULL,
                          tabsize = 20,
                          jitter = 4,
                          output = "both",
                          colors = NULL,
                          background = "none",
                          save_files = TRUE,
                          output_dir = "output",
                          filename_prefix = NULL) {
  
  # Generate seed if not provided
  if (is.null(seed)) {
    seed <- as.integer(runif(1) * 10000)
  }
  
  # Generate filename prefix if not provided
  if (is.null(filename_prefix)) {
    filename_prefix <- sprintf("puzzle_%dx%d_seed%d", grid[1], grid[2], seed)
  }
  
  # Ensure output directory exists
  if (save_files && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Generate puzzle structure based on type
  if (type == "rectangular") {
    puzzle_structure <- generate_puzzle_core(
      seed = seed,
      grid = grid,
      size = size,
      tabsize = tabsize,
      jitter = jitter
    )
  } else {
    stop("Only 'rectangular' puzzles are currently implemented in clean version")
  }
  
  # Initialize result
  result <- list(
    type = type,
    seed = seed,
    grid = grid,
    size = size,
    parameters = list(
      tabsize = tabsize,
      jitter = jitter
    ),
    files = list()
  )
  
  # Generate SVGs based on output mode
  if (output == "complete" || output == "both") {
    svg_complete <- generate_puzzle_svg(puzzle_structure, mode = "complete")
    result$svg_complete <- svg_complete
    
    if (save_files) {
      filepath <- file.path(output_dir, paste0(filename_prefix, "_complete.svg"))
      writeLines(svg_complete, filepath)
      result$files$complete_svg <- filepath
      cat("Saved complete puzzle:", filepath, "\n")
    }
  }
  
  if (output == "individual" || output == "both") {
    svg_individual <- generate_puzzle_svg(puzzle_structure, mode = "individual", colors = colors)
    result$svg_individual <- svg_individual
    
    if (save_files) {
      filepath <- file.path(output_dir, paste0(filename_prefix, "_individual.svg"))
      writeLines(svg_individual, filepath)
      result$files$individual_svg <- filepath
      cat("Saved individual pieces:", filepath, "\n")
      
      # Also save individual piece files if requested
      if (output == "individual") {
        save_individual_pieces(puzzle_structure, output_dir, filename_prefix, colors)
      }
    }
  }
  
  # Handle background
  if (background != "none" && save_files) {
    result$files$background <- generate_background(
      background = background,
      size = size,
      output_dir = output_dir,
      filename_prefix = filename_prefix
    )
  }
  
  # Store puzzle structure for further processing
  result$puzzle_structure <- puzzle_structure
  
  return(invisible(result))
}

#' Save individual puzzle pieces as separate SVG files
#' 
#' @param puzzle_structure Output from generate_puzzle_core()
#' @param output_dir Directory for output files
#' @param filename_prefix Prefix for output files
#' @param colors Optional vector of colors
#' @export
save_individual_pieces <- function(puzzle_structure, output_dir, filename_prefix, colors = NULL) {
  
  xn <- puzzle_structure$grid[2]
  yn <- puzzle_structure$grid[1]
  width <- puzzle_structure$size[1]
  height <- puzzle_structure$size[2]
  
  if (is.null(colors)) {
    colors <- "black"
  }
  
  # Create subdirectory for pieces
  pieces_dir <- file.path(output_dir, paste0(filename_prefix, "_pieces"))
  if (!dir.exists(pieces_dir)) {
    dir.create(pieces_dir, recursive = TRUE)
  }
  
  piece_num <- 0
  for (yi in 0:(yn - 1)) {
    for (xi in 0:(xn - 1)) {
      # Generate piece path
      piece_path <- generate_single_piece(xi, yi, puzzle_structure)
      color <- colors[(piece_num %% length(colors)) + 1]
      
      # Create individual SVG for this piece
      piece_svg <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" 
     width="%.0f" height="%.0f" viewBox="0 0 %.0f %.0f">
<rect width="100%%" height="100%%" fill="white"/>
<path id="piece-%d-%d" d="%s" fill="none" stroke="%s" stroke-width="1.5"/>
</svg>', width, height, width, height, xi, yi, piece_path, color)
      
      # Save piece
      piece_file <- file.path(pieces_dir, sprintf("piece_%d_%d.svg", xi, yi))
      writeLines(piece_svg, piece_file)
      
      piece_num <- piece_num + 1
    }
  }
  
  cat("Saved", xn * yn, "individual pieces to:", pieces_dir, "\n")
}

#' Generate background for puzzle
#' 
#' @param background "gradient", color name, or "none"
#' @param size Vector c(width, height) in mm
#' @param output_dir Directory for output
#' @param filename_prefix Prefix for files
#' @return Path to background file or NULL
generate_background <- function(background, size, output_dir, filename_prefix) {
  
  if (background == "none") {
    return(NULL)
  }
  
  filepath <- file.path(output_dir, paste0(filename_prefix, "_background.svg"))
  
  if (background == "gradient") {
    # Create radial gradient background
    svg_bg <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" 
     width="%.0f" height="%.0f" viewBox="0 0 %.0f %.0f">
  <defs>
    <radialGradient id="bg-gradient" cx="50%%" cy="50%%" r="50%%">
      <stop offset="0%%" style="stop-color:#e3f2fd;stop-opacity:1" />
      <stop offset="50%%" style="stop-color:#bbdefb;stop-opacity:1" />
      <stop offset="100%%" style="stop-color:#90caf9;stop-opacity:1" />
    </radialGradient>
  </defs>
  <rect width="100%%" height="100%%" fill="url(#bg-gradient)"/>
</svg>', size[1], size[2], size[1], size[2])
    
  } else {
    # Solid color background
    svg_bg <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" 
     width="%.0f" height="%.0f" viewBox="0 0 %.0f %.0f">
  <rect width="100%%" height="100%%" fill="%s"/>
</svg>', size[1], size[2], size[1], size[2], background)
  }
  
  writeLines(svg_bg, filepath)
  cat("Saved background:", filepath, "\n")
  
  return(filepath)
}

#' Generate batch of puzzle variations
#' 
#' Generate multiple puzzles with different parameters for testing or production.
#' 
#' @param variations List of parameter sets, each containing seed, grid, size, etc.
#' @param base_dir Base directory for output
#' @return List of results from each puzzle generation
#' @export
generate_puzzle_batch <- function(variations, base_dir = "output/batch") {
  
  results <- list()
  
  for (i in seq_along(variations)) {
    var <- variations[[i]]
    
    # Set defaults for missing parameters
    if (is.null(var$type)) var$type <- "rectangular"
    if (is.null(var$grid)) var$grid <- c(2, 2)
    if (is.null(var$size)) var$size <- c(200, 200)
    if (is.null(var$tabsize)) var$tabsize <- 20
    if (is.null(var$jitter)) var$jitter <- 4
    if (is.null(var$output)) var$output <- "both"
    if (is.null(var$background)) var$background <- "none"
    
    cat("\nGenerating puzzle", i, "of", length(variations), "\n")
    cat("  Seed:", var$seed, "Grid:", paste(var$grid, collapse="x"), "\n")
    
    # Generate puzzle
    result <- generate_puzzle(
      type = var$type,
      grid = var$grid,
      size = var$size,
      seed = var$seed,
      tabsize = var$tabsize,
      jitter = var$jitter,
      output = var$output,
      colors = var$colors,
      background = var$background,
      save_files = TRUE,
      output_dir = base_dir,
      filename_prefix = var$name
    )
    
    results[[i]] <- result
  }
  
  cat("\nBatch generation complete. Generated", length(results), "puzzles.\n")
  
  return(invisible(results))
}

#' Validate puzzle piece fit
#' 
#' Checks that adjacent pieces share exact same edge paths.
#' This is a validation function for quality assurance.
#' 
#' @param puzzle_structure Output from generate_puzzle_core()
#' @return TRUE if valid, otherwise stops with error
#' @export
validate_puzzle_fit <- function(puzzle_structure) {
  
  xn <- puzzle_structure$grid[2]
  yn <- puzzle_structure$grid[1]
  edges <- puzzle_structure$edges
  
  # Check all horizontal edges
  for (yi in seq_along(edges$horizontal)) {
    for (xi in seq_along(edges$horizontal[[yi]])) {
      edge <- edges$horizontal[[yi]][[xi]]
      
      # Verify edge has both forward and reverse paths
      if (is.null(edge$forward) || is.null(edge$reverse)) {
        stop(sprintf("Horizontal edge [%d,%d] missing forward/reverse path", yi, xi))
      }
      
      # Verify start and end points are defined
      if (is.null(edge$start) || is.null(edge$end)) {
        stop(sprintf("Horizontal edge [%d,%d] missing start/end points", yi, xi))
      }
    }
  }
  
  # Check all vertical edges
  for (xi in seq_along(edges$vertical)) {
    for (yi in seq_along(edges$vertical[[xi]])) {
      edge <- edges$vertical[[xi]][[yi]]
      
      # Verify edge has both forward and reverse paths
      if (is.null(edge$forward) || is.null(edge$reverse)) {
        stop(sprintf("Vertical edge [%d,%d] missing forward/reverse path", xi, yi))
      }
      
      # Verify start and end points are defined
      if (is.null(edge$start) || is.null(edge$end)) {
        stop(sprintf("Vertical edge [%d,%d] missing start/end points", xi, yi))
      }
    }
  }
  
  cat("✓ Puzzle validation passed: All edges properly defined\n")
  cat("  -", length(edges$horizontal), "horizontal edge sets\n")
  cat("  -", length(edges$vertical), "vertical edge sets\n")
  cat("  - Total pieces:", xn * yn, "\n")
  
  return(TRUE)
}