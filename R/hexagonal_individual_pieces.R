# Hexagonal Individual Puzzle Piece Generation
# Save individual pieces and combined SVG visualizations

#' Generate individual hexagonal puzzle pieces with file saving
#'
#' This function generates individual SVG files for each hexagonal puzzle piece
#' using the edge mapping system. Adjacent pieces have complementary edges
#' ensuring perfect fit.
#'
#' @param rings Number of rings (2-6)
#' @param seed Random seed for reproducibility
#' @param diameter Puzzle diameter in mm
#' @param tabsize Tab size percentage (10-40, default: 27)
#' @param jitter Jitter percentage (0-15, default: 5)
#' @param output_dir Directory to save individual piece SVGs
#' @param save_combined Whether to save a combined view (default: TRUE)
#' @param save_individual Whether to save individual piece files (default: TRUE)
#' @param colors Color palette for pieces (default: rainbow palette)
#' @param stroke_width Line width for SVG strokes (default: 1.5)
#' @param stroke_color Color for piece outlines (default: "black")
#' @param background Background color for pieces (default: "none")
#' @param opacity Opacity of puzzle pieces (0.0 to 1.0, default 1.0 = fully opaque)
#' @return List containing piece paths and metadata
generate_hexagonal_individual_pieces <- function(rings = 3, seed = NULL,
                                                 diameter = 240,
                                                 tabsize = 6, jitter = 5,
                                                 output_dir = "output",
                                                 save_combined = TRUE,
                                                 save_individual = TRUE,
                                                 colors = NULL,
                                                 stroke_width = 1.5,
                                                 stroke_color = "black",
                                                 background = "none",
                                                 opacity = 1.0) {

  # Generate seed if not provided
  if (is.null(seed)) {
    seed <- as.integer(runif(1) * 10000)
  }

  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Calculate number of pieces
  num_pieces <- 3 * rings * (rings - 1) + 1

  cat(sprintf("Generating %d hexagonal pieces (rings: %d, seed: %d)...\n",
              num_pieces, rings, seed))

  # Generate pieces with edge mapping (compact positions for individual files)
  pieces <- generate_hex_pieces_with_edge_map(
    rings = rings,
    seed = seed,
    diameter = diameter,
    tabsize = tabsize,
    jitter = jitter,
    separated = FALSE,  # Use compact positions
    separation_factor = 1.0
  )

  # Default colors (rainbow palette)
  if (is.null(colors)) {
    colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#FFA07A", "#98D8C8",
                "#DDA0DD", "#F0E68C", "#87CEEB", "#FFB6C1", "#90EE90")
  }

  # Calculate viewBox from all piece paths (not just centers)
  # This is critical when warp/trunc are enabled, as pieces extend beyond centers
  # Correct formula: diameter / (4 * rings - 2)
  piece_radius <- diameter / (4 * rings - 2)
  view_bounds <- calculate_pieces_viewbox(pieces, piece_radius)
  min_x <- view_bounds$min_x
  max_x <- view_bounds$max_x
  min_y <- view_bounds$min_y
  max_y <- view_bounds$max_y
  view_width <- view_bounds$width
  view_height <- view_bounds$height

  # Store file paths
  individual_files <- c()

  # Save individual pieces
  if (save_individual) {
    cat("Saving individual piece files...\n")
    for (i in seq_along(pieces)) {
      piece <- pieces[[i]]
      filename <- save_hexagonal_piece_svg(
        piece = piece,
        piece_index = i,
        seed = seed,
        output_dir = output_dir,
        stroke_width = stroke_width,
        stroke_color = stroke_color,
        background = background,
        piece_radius = piece_radius,
        opacity = opacity
      )
      individual_files <- c(individual_files, filename)
    }
    cat(sprintf("Saved %d individual piece files.\n", length(individual_files)))
  }

  # Save combined view
  combined_file <- NULL
  if (save_combined) {
    cat("Saving combined view...\n")
    combined_file <- save_hexagonal_combined_svg(
      pieces = pieces,
      seed = seed,
      output_dir = output_dir,
      colors = colors,
      stroke_width = stroke_width,
      view_bounds = list(min_x = min_x, min_y = min_y,
                         width = view_width, height = view_height),
      opacity = opacity
    )
    cat(sprintf("Combined view saved: %s\n", combined_file))
  }

  cat(sprintf("Successfully generated %d hexagonal pieces!\n", num_pieces))

  return(list(
    pieces = pieces,
    parameters = list(
      rings = rings,
      seed = seed,
      diameter = diameter,
      tabsize = tabsize,
      jitter = jitter,
      num_pieces = num_pieces
    ),
    files = list(
      output_dir = output_dir,
      individual = individual_files,
      combined = combined_file
    )
  ))
}


#' Save individual hexagonal piece as SVG file
#'
#' @param piece Piece object with path and center coordinates
#' @param piece_index Piece index (1-based)
#' @param seed Random seed for filename
#' @param output_dir Output directory
#' @param stroke_width Line width for SVG strokes
#' @param stroke_color Color for the stroke
#' @param background Background color or "none"
#' @param piece_radius Radius of hexagonal piece
#' @param opacity Opacity of puzzle pieces (0.0 to 1.0, default 1.0 = fully opaque)
#' @return Filename of saved file
#' @keywords internal
save_hexagonal_piece_svg <- function(piece, piece_index, seed, output_dir,
                                     stroke_width = 1.5, stroke_color = "black",
                                     background = "none", piece_radius = 30,
                                     opacity = 1.0) {

  # Calculate bounding box from piece path
  bounds <- calculate_path_bounds(piece$path)

  # Add padding
  padding <- piece_radius * 0.3
  min_x <- bounds$min_x - padding
  min_y <- bounds$min_y - padding
  view_width <- bounds$width + 2 * padding
  view_height <- bounds$height + 2 * padding

  # Handle background value
  bg_fill <- if (is.null(background) || background == "none" || background == "transparent") {
    "none"
  } else {
    background
  }

  svg_content <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1"
     width="%.1fmm" height="%.1fmm" viewBox="%.2f %.2f %.2f %.2f">
  <title>Hexagonal Puzzle Piece %d (seed %d)</title>
  <rect x="%.2f" y="%.2f" width="%.2f" height="%.2f" fill="%s"/>
  <path id="piece-%d" d="%s"
        fill="none" stroke="%s" stroke-width="%.1f" opacity="%.2f"
        stroke-linecap="round" stroke-linejoin="round"/>
</svg>',
    view_width, view_height, min_x, min_y, view_width, view_height,
    piece_index, seed,
    min_x, min_y, view_width, view_height, bg_fill,
    piece_index, piece$path, stroke_color, stroke_width, opacity)

  filename <- file.path(output_dir, sprintf("hexagonal_piece_%02d_seed%d.svg",
                                            piece_index, seed))
  writeLines(svg_content, filename)
  return(filename)
}


#' Save combined view of all hexagonal pieces
#'
#' @param pieces List of piece objects
#' @param seed Random seed for filename
#' @param output_dir Output directory
#' @param colors Color palette for pieces
#' @param stroke_width Line width for SVG strokes
#' @param view_bounds ViewBox bounds (min_x, min_y, width, height)
#' @param opacity Opacity of puzzle pieces (0.0 to 1.0, default 1.0 = fully opaque)
#' @return Filename of saved file
#' @keywords internal
save_hexagonal_combined_svg <- function(pieces, seed, output_dir, colors,
                                        stroke_width = 1.5, view_bounds,
                                        opacity = 1.0) {

  num_pieces <- length(pieces)

  # Create semi-transparent fill colors from stroke colors
  fill_colors <- sapply(colors, function(color) {
    rgb_vals <- col2rgb(color)
    sprintf("rgba(%d,%d,%d,0.3)", rgb_vals[1], rgb_vals[2], rgb_vals[3])
  })

  svg_parts <- c(
    sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1"
     width="%.1fmm" height="%.1fmm" viewBox="%.2f %.2f %.2f %.2f">
  <title>Hexagonal Puzzle - %d pieces (seed %d)</title>
  <rect x="%.2f" y="%.2f" width="%.2f" height="%.2f" fill="white"/>',
      view_bounds$width, view_bounds$height,
      view_bounds$min_x, view_bounds$min_y, view_bounds$width, view_bounds$height,
      num_pieces, seed,
      view_bounds$min_x, view_bounds$min_y, view_bounds$width, view_bounds$height),
    '  <g id="hexagonal-puzzle-pieces">'
  )

  # Add each piece with color
  for (i in seq_along(pieces)) {
    piece <- pieces[[i]]
    color_idx <- ((i - 1) %% length(colors)) + 1
    fill_idx <- ((i - 1) %% length(fill_colors)) + 1

    svg_parts <- c(svg_parts, sprintf(
      '    <path id="piece-%d" d="%s" fill="%s" stroke="%s" stroke-width="%.1f" opacity="%.2f"/>',
      i, piece$path, fill_colors[fill_idx], colors[color_idx], stroke_width, opacity
    ))

    # Add piece label
    svg_parts <- c(svg_parts, sprintf(
      '    <text x="%.2f" y="%.2f" text-anchor="middle" dominant-baseline="central" font-size="8" fill="%s">%d</text>',
      piece$center_x, piece$center_y, colors[color_idx], i
    ))
  }

  svg_parts <- c(svg_parts, '  </g>', '</svg>')

  svg_content <- paste(svg_parts, collapse = "\n")
  filename <- file.path(output_dir, sprintf("hexagonal_combined_seed%d.svg", seed))
  writeLines(svg_content, filename)

  return(filename)
}


#' Calculate bounding box from SVG path
#'
#' @param path SVG path string
#' @return List with min_x, min_y, max_x, max_y, width, height
#' @keywords internal
calculate_path_bounds <- function(path) {
  # Extract all numbers from path (coordinates)
  numbers <- as.numeric(unlist(regmatches(path, gregexpr("-?[0-9]+\\.?[0-9]*", path))))
  numbers <- numbers[!is.na(numbers)]

  if (length(numbers) < 2) {
    return(list(min_x = 0, min_y = 0, max_x = 100, max_y = 100,
                width = 100, height = 100))
  }

  # Separate x and y coordinates (alternating)
  x_coords <- numbers[seq(1, length(numbers), by = 2)]
  y_coords <- numbers[seq(2, length(numbers), by = 2)]

  min_x <- min(x_coords)
  max_x <- max(x_coords)
  min_y <- min(y_coords)
  max_y <- max(y_coords)

  list(
    min_x = min_x,
    min_y = min_y,
    max_x = max_x,
    max_y = max_y,
    width = max_x - min_x,
    height = max_y - min_y
  )
}


#' Calculate viewBox bounds from all piece paths
#'
#' Extracts coordinates from all piece paths to find the actual extent
#' of the puzzle. This is critical for warp/trunc modes where pieces
#' extend beyond their center positions.
#'
#' @param pieces List of piece objects with $path containing SVG path data
#' @param piece_radius Piece radius for margin calculation
#' @return List with min_x, min_y, max_x, max_y, width, height
#' @keywords internal
calculate_pieces_viewbox <- function(pieces, piece_radius) {
  all_path_x <- c()
  all_path_y <- c()

  for (piece in pieces) {
    # Extract coordinates from path
    path <- piece$path
    numbers <- as.numeric(unlist(regmatches(path, gregexpr("-?[0-9]+\\.?[0-9]*", path))))
    numbers <- numbers[!is.na(numbers)]

    if (length(numbers) >= 2) {
      # Coordinates alternate: x, y, x, y, ...
      x_coords <- numbers[seq(1, length(numbers), by = 2)]
      y_coords <- numbers[seq(2, length(numbers), by = 2)]
      all_path_x <- c(all_path_x, x_coords)
      all_path_y <- c(all_path_y, y_coords)
    }
  }

  if (length(all_path_x) > 0 && length(all_path_y) > 0) {
    path_min_x <- min(all_path_x)
    path_max_x <- max(all_path_x)
    path_min_y <- min(all_path_y)
    path_max_y <- max(all_path_y)
  } else {
    # Fallback to center-based calculation
    all_x <- sapply(pieces, function(p) {
      if (!is.null(p$center_x)) p$center_x else p$center[1]
    })
    all_y <- sapply(pieces, function(p) {
      if (!is.null(p$center_y)) p$center_y else p$center[2]
    })
    path_min_x <- min(all_x) - piece_radius
    path_max_x <- max(all_x) + piece_radius
    path_min_y <- min(all_y) - piece_radius
    path_max_y <- max(all_y) + piece_radius
  }

  # Add small margin for stroke width
  stroke_margin <- piece_radius * 0.15
  min_x <- path_min_x - stroke_margin
  max_x <- path_max_x + stroke_margin
  min_y <- path_min_y - stroke_margin
  max_y <- path_max_y + stroke_margin

  list(
    min_x = min_x,
    min_y = min_y,
    max_x = max_x,
    max_y = max_y,
    width = max_x - min_x,
    height = max_y - min_y
  )
}


#' Create hexagonal individual pieces SVG (combined view only)
#'
#' Generates a combined SVG showing all pieces with colors, without saving
#' individual files. Useful for preview or when only combined view is needed.
#'
#' @param rings Number of rings
#' @param seed Random seed
#' @param diameter Puzzle diameter
#' @param tabsize Tab size percentage
#' @param jitter Jitter percentage
#' @param colors Color palette
#' @param stroke_width Line width
#' @param opacity Opacity of puzzle pieces (0.0 to 1.0, default 1.0 = fully opaque)
#' @return SVG content as string
create_hexagonal_individual_pieces_svg <- function(rings = 3, seed = NULL,
                                                   diameter = 240,
                                                   tabsize = 6, jitter = 5,
                                                   colors = NULL,
                                                   stroke_width = 1.5,
                                                   opacity = 1.0) {

  if (is.null(seed)) {
    seed <- as.integer(runif(1) * 10000)
  }

  # Generate pieces
  pieces <- generate_hex_pieces_with_edge_map(
    rings = rings,
    seed = seed,
    diameter = diameter,
    tabsize = tabsize,
    jitter = jitter,
    separated = FALSE,
    separation_factor = 1.0
  )

  num_pieces <- length(pieces)

  # Default colors
  if (is.null(colors)) {
    colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#FFA07A", "#98D8C8",
                "#DDA0DD", "#F0E68C", "#87CEEB", "#FFB6C1", "#90EE90")
  }

  # Calculate viewBox from all piece paths (not just centers)
  # Correct formula: diameter / (4 * rings - 2)
  piece_radius <- diameter / (4 * rings - 2)
  view_bounds <- calculate_pieces_viewbox(pieces, piece_radius)
  min_x <- view_bounds$min_x
  min_y <- view_bounds$min_y
  view_width <- view_bounds$width
  view_height <- view_bounds$height

  # Create fill colors
  fill_colors <- sapply(colors, function(color) {
    rgb_vals <- col2rgb(color)
    sprintf("rgba(%d,%d,%d,0.3)", rgb_vals[1], rgb_vals[2], rgb_vals[3])
  })

  # Build SVG
  svg_parts <- c(
    sprintf('<svg width="%.1fmm" height="%.1fmm" viewBox="%.2f %.2f %.2f %.2f" xmlns="http://www.w3.org/2000/svg">',
            view_width, view_height, min_x, min_y, view_width, view_height),
    sprintf('  <title>Hexagonal Puzzle - %d pieces (seed %d)</title>', num_pieces, seed),
    sprintf('  <rect x="%.2f" y="%.2f" width="%.2f" height="%.2f" fill="white"/>',
            min_x, min_y, view_width, view_height),
    '  <g id="hexagonal-individual-pieces">'
  )

  for (i in seq_along(pieces)) {
    piece <- pieces[[i]]
    color_idx <- ((i - 1) %% length(colors)) + 1
    fill_idx <- ((i - 1) %% length(fill_colors)) + 1

    svg_parts <- c(svg_parts, sprintf(
      '    <path id="piece-%d" d="%s" fill="%s" stroke="%s" stroke-width="%.1f" opacity="%.2f"/>',
      i, piece$path, fill_colors[fill_idx], colors[color_idx], stroke_width, opacity
    ))

    svg_parts <- c(svg_parts, sprintf(
      '    <text x="%.2f" y="%.2f" text-anchor="middle" dominant-baseline="central" font-size="8" fill="%s">%d</text>',
      piece$center_x, piece$center_y, colors[color_idx], i
    ))
  }

  svg_parts <- c(svg_parts, '  </g>', '</svg>')

  paste(svg_parts, collapse = "\n")
}
