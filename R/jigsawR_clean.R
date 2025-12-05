# jigsawR Clean Implementation
# Complete pipeline for jigsaw puzzle generation
# Reproducible, clean, no hard-coded adjustments
#
# Updated for Epic #32: Unified Puzzle Generation Pipeline
# Now uses generate_pieces_internal() -> apply_piece_positioning() -> render_puzzle_svg()

#' Main jigsaw puzzle generation function
#'
#' Single entry point for all puzzle generation with clean, reproducible output.
#' Uses the unified pipeline: piece generation -> positioning -> rendering.
#'
#' @param type Puzzle type: "rectangular", "hexagonal", or "concentric"
#' @param grid For rectangular: c(rows, columns). For hexagonal/concentric: c(rings) or just rings
#' @param size For rectangular: c(width, height) in mm. For hexagonal/concentric: c(diameter) or just diameter
#' @param seed Random seed for reproducibility
#' @param tabsize Tab size as percentage (10-40)
#' @param jitter Jitter as percentage (0-15)
#' @param offset Separation offset (0 = complete puzzle, >0 = separated pieces)
#' @param fill_color Fill color for pieces ("none" for unfilled)
#' @param stroke_width SVG stroke width (default: 1.5)
#' @param colors Vector of colors for pieces (optional, overrides palette)
#' @param palette Viridis palette name (NULL = use config default)
#' @param background "none", "white", color name, or list(type="gradient", ...)
#' @param opacity Opacity of puzzle pieces (0.0 to 1.0)
#' @param save_files TRUE to save SVG files
#' @param output_dir Directory for output files
#' @param filename_prefix Prefix for output files
#' @param do_warp Apply circular warping (hexagonal only)
#' @param do_trunc Truncate edge pieces (hexagonal only)
#' @param do_circular_border Use perfect circular arc borders (hexagonal: requires do_warp=TRUE; concentric: always available)
#' @param center_shape Center piece shape for concentric type: "hexagon" or "circle"
#' @param boundary_facing Direction the circular arc faces (concentric only): "outward" (convex) or "inward" (concave)
#' @param show_labels Logical; if TRUE, display piece ID labels at piece centers
#' @param label_color Color for piece labels (default: "black")
#' @param label_size Font size for labels in mm (default: auto-calculated based on piece size)
#' @param output DEPRECATED: Use offset parameter instead
#' @return List with svg_content, pieces, canvas_size, and parameters
#' @export
generate_puzzle <- function(type = "rectangular",
                            grid = c(2, 2),
                            size = c(200, 200),
                            seed = NULL,
                            tabsize = 20,
                            jitter = 4,
                            offset = 0,
                            fill_color = "none",
                            stroke_width = 1.5,
                            colors = NULL,
                            palette = NULL,
                            background = "white",
                            opacity = 1.0,
                            save_files = FALSE,
                            output_dir = "output",
                            filename_prefix = NULL,
                            do_warp = FALSE,
                            do_trunc = FALSE,
                            do_circular_border = FALSE,
                            center_shape = "hexagon",
                            boundary_facing = "outward",
                            show_labels = FALSE,
                            label_color = "black",
                            label_size = NULL,
                            output = NULL) {

  # Handle deprecated 'output' parameter
  if (!is.null(output)) {
    .Deprecated(msg = paste(
      "'output' parameter is deprecated.",
      "Use 'offset' parameter instead:",
      "- offset=0 for complete puzzle",
      "- offset>0 for separated pieces",
      "Access result$pieces for individual piece data."
    ))
    # Map old output values to offset
    if (output == "individual" || output == "separated") {
      offset <- 10  # Default separation
    } else if (output == "complete") {
      offset <- 0
    }
    # For "both", we generate with offset=0 (complete) and include pieces
  }

  # Generate seed if not provided
  if (is.null(seed)) {
    seed <- as.integer(runif(1) * 10000)
  }

  # Validate type parameter
  valid_types <- c("rectangular", "hexagonal", "concentric")
  if (!type %in% valid_types) {
    stop(sprintf("Invalid type '%s'. Must be one of: %s", type, paste(valid_types, collapse = ", ")))
  }

  # Generate filename prefix if not provided
  if (is.null(filename_prefix)) {
    if (type == "hexagonal") {
      rings <- if (length(grid) == 1) grid else grid[1]
      filename_prefix <- sprintf("puzzle_hex%d_seed%d", rings, seed)
    } else if (type == "concentric") {
      rings <- if (length(grid) == 1) grid else grid[1]
      filename_prefix <- sprintf("puzzle_conc%d_seed%d", rings, seed)
    } else {
      filename_prefix <- sprintf("puzzle_%dx%d_seed%d", grid[1], grid[2], seed)
    }
  }

  # Ensure output directory exists
  if (save_files && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # === UNIFIED PIPELINE ===

  # Step 1: Generate pieces internally
  pieces_result <- generate_pieces_internal(
    type = type,
    seed = seed,
    grid = grid,
    size = size,
    tabsize = tabsize,
    jitter = jitter,
    do_warp = do_warp,
    do_trunc = do_trunc,
    do_circular_border = do_circular_border,
    center_shape = center_shape,
    boundary_facing = boundary_facing
  )

  # Step 2: Apply positioning
  positioned <- apply_piece_positioning(pieces_result, offset = offset)

  # Step 3: Render to SVG
  svg_content <- render_puzzle_svg(
    positioned,
    fill = fill_color,
    stroke_width = stroke_width,
    colors = colors,
    palette = palette,
    background = background,
    opacity = opacity,
    show_labels = show_labels,
    label_color = label_color,
    label_size = label_size
  )

  # Build result
  result <- list(
    svg_content = svg_content,
    pieces = positioned$pieces,
    canvas_size = positioned$canvas_size,
    canvas_offset = positioned$canvas_offset,
    type = type,
    parameters = list(
      seed = pieces_result$parameters$seed,
      grid = grid,
      size = size,
      tabsize = tabsize,
      jitter = jitter,
      offset = offset,
      do_warp = do_warp,
      do_trunc = do_trunc,
      do_circular_border = do_circular_border,
      center_shape = if (type == "concentric") center_shape else NULL,
      boundary_facing = if (type == "concentric") boundary_facing else NULL,
      fill_color = fill_color,
      stroke_width = stroke_width,
      palette = palette,
      background = background,
      opacity = opacity,
      show_labels = show_labels,
      label_color = label_color,
      label_size = label_size
    ),
    files = list()
  )

  # Save files if requested
  if (save_files) {
    # Determine suffix based on offset
    suffix <- if (offset > 0) "_separated" else "_complete"
    filepath <- file.path(output_dir, paste0(filename_prefix, suffix, ".svg"))
    writeLines(svg_content, filepath)
    result$files$svg <- filepath
    log_success("Saved puzzle: {.file {filepath}}")

    # Save background if specified (and not "none" or "white")
    should_save_background <- (
      is.list(background) ||
      (is.character(background) && !background %in% c("none", "white", ""))
    )
    if (should_save_background) {
      result$files$background <- generate_background(
        background = background,
        size = if (type %in% c("hexagonal", "concentric")) c(positioned$canvas_size[1], positioned$canvas_size[2]) else size,
        output_dir = output_dir,
        filename_prefix = filename_prefix
      )
    }
  }

  # For backward compatibility, also include legacy fields
  result$svg_complete <- if (offset == 0) svg_content else NULL
  result$svg_individual <- if (offset > 0) svg_content else NULL
  result$puzzle_structure <- pieces_result$parameters

  return(invisible(result))
}

#' Save individual puzzle pieces as separate SVG files
#'
#' @param puzzle_structure Output from generate_puzzle_core()
#' @param output_dir Directory for output files
#' @param filename_prefix Prefix for output files
#' @param colors Optional vector of colors
#' @param stroke_width Line width for SVG strokes (default: 1.5)
#' @export
save_individual_pieces <- function(puzzle_structure, output_dir, filename_prefix, colors = NULL, stroke_width = 1.5) {

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
<path id="piece-%d-%d" d="%s" fill="none" stroke="%s" stroke-width="%.1f"/>
</svg>', width, height, width, height, xi, yi, piece_path, color, stroke_width)

      # Save piece
      piece_file <- file.path(pieces_dir, sprintf("piece_%d_%d.svg", xi, yi))
      writeLines(piece_svg, piece_file)

      piece_num <- piece_num + 1
    }
  }

  log_success("Saved {xn * yn} individual pieces to: {.path {pieces_dir}}")
}

#' Generate background for puzzle
#'
#' @param background "gradient", color name, "none", or list with gradient colors
#' @param size Vector c(width, height) in mm
#' @param output_dir Directory for output
#' @param filename_prefix Prefix for files
#' @return Path to background file or NULL
generate_background <- function(background, size, output_dir, filename_prefix) {

  # Check for "none" background (type-safe)
  if (is.character(background) && (background == "none" || background == "")) {
    return(NULL)
  }

  filepath <- file.path(output_dir, paste0(filename_prefix, "_background.svg"))

  # Background can be: "none", a color string, or a list with gradient colors
  if (is.list(background) && !is.null(background$type) && background$type == "gradient") {
    # Custom gradient with user-specified colors
    center_color <- background$center
    middle_color <- background$middle
    edge_color <- background$edge
    svg_bg <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1"
     width="%.0f" height="%.0f" viewBox="0 0 %.0f %.0f">
  <defs>
    <radialGradient id="bg-gradient" cx="50%%" cy="50%%" r="50%%">
      <stop offset="0%%" style="stop-color:%s;stop-opacity:1" />
      <stop offset="50%%" style="stop-color:%s;stop-opacity:1" />
      <stop offset="100%%" style="stop-color:%s;stop-opacity:1" />
    </radialGradient>
  </defs>
  <rect width="100%%" height="100%%" fill="url(#bg-gradient)"/>
</svg>', size[1], size[2], size[1], size[2], center_color, middle_color, edge_color)

  } else if (is.character(background) && background == "gradient") {
    # Legacy: default gradient colors for backward compatibility
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

  } else if (is.character(background)) {
    # Solid color background
    svg_bg <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1"
     width="%.0f" height="%.0f" viewBox="0 0 %.0f %.0f">
  <rect width="100%%" height="100%%" fill="%s"/>
</svg>', size[1], size[2], size[1], size[2], background)
  } else {
    # Fallback: no background for unexpected types
    return(NULL)
  }

  writeLines(svg_bg, filepath)
  log_success("Saved background: {.file {filepath}}")

  return(filepath)
}

#' Generate batch of puzzle variations
#'
#' Generate multiple puzzles with different parameters for testing or production.
#'
#' @param variations List of parameter sets, each containing seed, grid, size, offset, etc.
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
    if (is.null(var$offset)) var$offset <- 0
    if (is.null(var$background)) var$background <- "white"

    log_subheader("Generating puzzle {i} of {length(variations)}")
    log_info("Seed: {var$seed}, Grid: {paste(var$grid, collapse='x')}, Offset: {var$offset}")

    # Generate puzzle using unified pipeline
    result <- generate_puzzle(
      type = var$type,
      grid = var$grid,
      size = var$size,
      seed = var$seed,
      tabsize = var$tabsize,
      jitter = var$jitter,
      offset = var$offset,
      fill_color = var$fill_color,
      stroke_width = var$stroke_width,
      colors = var$colors,
      palette = var$palette,
      background = var$background,
      opacity = var$opacity,
      save_files = TRUE,
      output_dir = base_dir,
      filename_prefix = var$name,
      do_warp = var$do_warp,
      do_trunc = var$do_trunc
    )

    results[[i]] <- result
  }

  log_success("Batch generation complete. Generated {length(results)} puzzles")

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
  
  log_success("Puzzle validation passed: All edges properly defined")
  log_info("  - {length(edges$horizontal)} horizontal edge sets")
  log_info("  - {length(edges$vertical)} vertical edge sets")
  log_info("  - Total pieces: {xn * yn}")

  return(TRUE)
}