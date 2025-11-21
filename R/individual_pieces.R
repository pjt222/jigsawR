# Individual Puzzle Piece Generation
# Clean implementation using core generation functions (no SVG manipulation)

#' Generate individual puzzle pieces with proper complementary edges
#'
#' This function generates separate SVG files for each puzzle piece using the
#' core generation functions directly. Adjacent pieces share exact same edge
#' paths ensuring perfect fit.
#'
#' Key principles:
#' - Uses generation functions from rectangular_puzzle.R directly
#' - No hardcoded paths or SVG manipulation
#' - Works for any puzzle size (not just 2x2)
#' - Deterministic output based on seed
#'
#' @param seed Random seed for reproducibility
#' @param xn Number of columns
#' @param yn Number of rows
#' @param width Puzzle width in mm
#' @param height Puzzle height in mm
#' @param tabsize Tab size percentage (10-30, default: 20)
#' @param jitter Jitter percentage (0-10, default: 4)
#' @param output_dir Directory to save individual piece SVGs
#' @param corner_radius Corner radius for border pieces (default: 2)
#' @param save_combined Whether to save a combined view (default: TRUE)
#' @param palette Viridis palette name for colors (default: from config)
#' @return List containing piece paths and metadata
#' @export
generate_individual_pieces <- function(seed = 42, xn = 2, yn = 2,
                                      width = 200, height = 200,
                                      tabsize = 20, jitter = 4,
                                      output_dir = "output",
                                      corner_radius = 2,
                                      save_combined = TRUE,
                                      palette = NULL) {

  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Source required functions if not in package context
  if (!exists("init_jigsaw")) {
    if (file.exists("R/rectangular_puzzle.R")) {
      source("R/rectangular_puzzle.R")
    }
  }
  if (!exists("generate_all_edges")) {
    if (file.exists("R/puzzle_core_clean.R")) {
      source("R/puzzle_core_clean.R")
    }
  }

  log_info("Generating {xn}x{yn} puzzle (seed: {seed})...")

  # Initialize puzzle environment
  init_jigsaw(seed = seed, tabsize = tabsize, jitter = jitter,
              width = width, height = height, xn = xn, yn = yn)

  # Generate all shared edges
  edges <- generate_all_edges(xn, yn)

  # Calculate piece dimensions
  piece_width <- width / xn
  piece_height <- height / yn

  # Storage for piece data
  pieces <- list()

  # Generate each piece
  for (yi in 0:(yn - 1)) {
    for (xi in 0:(xn - 1)) {

      # Generate piece path
      piece_path <- build_piece_path(xi, yi, edges, xn, yn,
                                     piece_width, piece_height,
                                     corner_radius)

      # Store piece data
      piece_id <- sprintf("%d_%d", xi, yi)
      pieces[[piece_id]] <- list(
        xi = xi,
        yi = yi,
        path = piece_path
      )

      # Save individual piece SVG
      save_individual_piece_svg(xi, yi, piece_path, width, height, output_dir)

      log_info("Generated piece [{xi},{yi}]")
    }
  }

  # Save combined view if requested
  if (save_combined) {
    save_combined_pieces_svg(pieces, width, height, output_dir, palette)
  }

  log_success("Successfully generated {xn * yn} pieces!")

  return(list(
    pieces = pieces,
    parameters = list(
      seed = seed,
      dimensions = c(xn, yn),
      size = c(width, height),
      tabsize = tabsize,
      jitter = jitter,
      total_pieces = xn * yn
    ),
    files = list(
      output_dir = output_dir,
      individual_pattern = sprintf("piece_%%d_%%d_seed%d.svg", seed),
      combined = if(save_combined) sprintf("combined_seed%d.svg", seed) else NULL
    )
  ))
}

#' Build complete path for a single puzzle piece
#'
#' Assembles the 4 edges (top, right, bottom, left) for a piece,
#' using shared edges with adjacent pieces or straight border lines.
#'
#' @param xi Column index (0-based)
#' @param yi Row index (0-based)
#' @param edges Edge data from generate_all_edges()
#' @param xn Total number of columns
#' @param yn Total number of rows
#' @param piece_width Width of each piece
#' @param piece_height Height of each piece
#' @param corner_radius Radius for rounded corners on border pieces
#' @return SVG path string
#' @keywords internal
build_piece_path <- function(xi, yi, edges, xn, yn,
                            piece_width, piece_height,
                            corner_radius = 2) {

  # Calculate corner coordinates
  x1 <- xi * piece_width
  y1 <- yi * piece_height
  x2 <- x1 + piece_width
  y2 <- y1 + piece_height

  # Calculate total puzzle dimensions
  width <- xn * piece_width
  height <- yn * piece_height

  # Determine if this is a corner piece
  is_top_left <- (xi == 0 && yi == 0)
  is_top_right <- (xi == xn - 1 && yi == 0)
  is_bottom_right <- (xi == xn - 1 && yi == yn - 1)
  is_bottom_left <- (xi == 0 && yi == yn - 1)

  # Start path from top-left corner
  if (is_top_left) {
    path <- sprintf("M %.2f 0 ", corner_radius)
  } else {
    path <- sprintf("M %.2f %.2f ", x1, y1)
  }

  # TOP EDGE (left to right)
  if (yi == 0) {
    # Border edge - straight or with corner
    if (is_top_right) {
      path <- paste0(path, sprintf("L %.2f 0 ", width - corner_radius))
      path <- paste0(path, sprintf("A %.2f %.2f 0 0 1 %.2f %.2f ",
                                  corner_radius, corner_radius, width, corner_radius))
    } else {
      path <- paste0(path, sprintf("L %.2f %.2f ", x2, y1))
    }
  } else {
    # Internal edge - use forward direction of horizontal edge
    edge <- edges$horizontal[[yi]][[xi + 1]]
    path <- paste0(path, edge$forward)
  }

  # RIGHT EDGE (top to bottom)
  if (xi == xn - 1) {
    # Border edge - straight or with corner
    if (is_bottom_right) {
      path <- paste0(path, sprintf("L %.2f %.2f ", width, height - corner_radius))
      path <- paste0(path, sprintf("A %.2f %.2f 0 0 1 %.2f %.2f ",
                                  corner_radius, corner_radius, width - corner_radius, height))
    } else {
      path <- paste0(path, sprintf("L %.2f %.2f ", x2, y2))
    }
  } else {
    # Internal edge - use forward direction of vertical edge
    edge <- edges$vertical[[xi + 1]][[yi + 1]]
    path <- paste0(path, edge$forward)
  }

  # BOTTOM EDGE (right to left) - REVERSED
  if (yi == yn - 1) {
    # Border edge - straight or with corner
    if (is_bottom_left) {
      path <- paste0(path, sprintf("L %.2f %.2f ", corner_radius, height))
      path <- paste0(path, sprintf("A %.2f %.2f 0 0 1 0 %.2f ",
                                  corner_radius, corner_radius, height - corner_radius))
    } else {
      path <- paste0(path, sprintf("L %.2f %.2f ", x1, y2))
    }
  } else {
    # Internal edge - use REVERSE direction of horizontal edge below
    edge <- edges$horizontal[[yi + 1]][[xi + 1]]
    path <- paste0(path, edge$reverse)
  }

  # LEFT EDGE (bottom to top) - REVERSED
  if (xi == 0) {
    # Border edge - straight or with corner
    if (is_top_left) {
      path <- paste0(path, sprintf("L 0 %.2f ", corner_radius))
      path <- paste0(path, sprintf("A %.2f %.2f 0 0 1 %.2f 0 ",
                                  corner_radius, corner_radius, corner_radius))
    } else {
      path <- paste0(path, sprintf("L %.2f %.2f ", x1, y1))
    }
  } else {
    # Internal edge - use REVERSE direction of vertical edge to the left
    edge <- edges$vertical[[xi]][[yi + 1]]
    path <- paste0(path, edge$reverse)
  }

  # Close path
  path <- paste0(path, "Z")

  return(path)
}

#' Save individual piece as SVG file
#' @keywords internal
save_individual_piece_svg <- function(xi, yi, piece_path, width, height, output_dir) {

  svg_content <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1"
     width="%.0f" height="%.0f" viewBox="0 0 %.0f %.0f">
  <rect width="100%%" height="100%%" fill="transparent"/>
  <path id="piece-%d-%d" d="%s"
        fill="none" stroke="black" stroke-width="1.5"
        stroke-linecap="round" stroke-linejoin="round"/>
</svg>', width, height, width, height, xi, yi, piece_path)

  filename <- file.path(output_dir, sprintf("piece_%d_%d.svg", xi, yi))
  writeLines(svg_content, filename)
}

#' Save combined view of all pieces
#' @keywords internal
save_combined_pieces_svg <- function(pieces, width, height, output_dir, palette = NULL) {

  # Use viridis palette for colors
  n_colors <- length(pieces)
  stroke_colors <- get_puzzle_colors(n_colors, palette)

  # Create semi-transparent fill colors from stroke colors
  colors <- sapply(stroke_colors, function(color) {
    # Convert hex to rgba with 30% opacity
    rgb_vals <- col2rgb(color)
    sprintf("rgba(%d,%d,%d,0.3)", rgb_vals[1], rgb_vals[2], rgb_vals[3])
  })

  svg_parts <- c(
    sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1"
     width="%.0f" height="%.0f" viewBox="0 0 %.0f %.0f">
  <rect width="100%%" height="100%%" fill="white"/>', width, height, width, height),
    '  <g id="puzzle-pieces">'
  )

  # Add each piece with color
  piece_num <- 0
  for (key in names(pieces)) {
    piece <- pieces[[key]]
    color_idx <- (piece_num %% length(colors)) + 1

    svg_parts <- c(svg_parts, sprintf(
      '    <path id="piece-%d-%d" d="%s" fill="%s" stroke="%s" stroke-width="1.5"/>',
      piece$xi, piece$yi, piece$path, colors[color_idx], stroke_colors[color_idx]
    ))

    piece_num <- piece_num + 1
  }

  svg_parts <- c(svg_parts, '  </g>', '</svg>')

  svg_content <- paste(svg_parts, collapse = "\n")
  filename <- file.path(output_dir, "combined_pieces.svg")
  writeLines(svg_content, filename)

  log_success("Combined view saved: {.file {filename}}")
}

#' Extract tab/blank data from puzzle generation
#'
#' This function pre-generates all tab/blank information needed to create
#' individual piece boundaries consistently. Useful for advanced manipulation.
#'
#' @param seed Random seed for puzzle generation
#' @param xn Number of columns
#' @param yn Number of rows
#' @param tabsize Tab size percentage
#' @param jitter Jitter percentage
#' @param width Puzzle width in mm (default: 300)
#' @param height Puzzle height in mm (default: 200)
#' @return List containing tab/blank data for all dividers
#' @export
extract_tab_data <- function(seed, xn, yn, tabsize = 20, jitter = 4,
                            width = 300, height = 200) {

  # Source required functions if not in package context
  if (!exists("init_jigsaw")) {
    if (file.exists("R/rectangular_puzzle.R")) {
      source("R/rectangular_puzzle.R")
    }
  }

  # Initialize environment
  init_jigsaw(seed = seed, tabsize = tabsize, jitter = jitter,
              width = width, height = height, xn = xn, yn = yn)

  # Storage for tab data
  tab_data <- list(
    horizontal = list(),  # For horizontal dividers (between rows)
    vertical = list()     # For vertical dividers (between columns)
  )

  # Extract horizontal divider tab data
  .jigsaw_env$vertical <- 0
  for (yi in 1:(yn - 1)) {
    .jigsaw_env$yi <- yi
    .jigsaw_env$xi <- 0

    line_tabs <- list()
    first()  # Initialize first tab

    for (xi in 0:(xn - 1)) {
      .jigsaw_env$xi <- xi

      # Store current tab parameters
      line_tabs[[xi + 1]] <- list(
        flip = .jigsaw_env$flip,
        a = .jigsaw_env$a,
        b = .jigsaw_env$b,
        c = .jigsaw_env$c,
        d = .jigsaw_env$d,
        e = .jigsaw_env$e
      )

      if (xi < xn - 1) {
        next_tab()  # Prepare for next segment
      }
    }

    tab_data$horizontal[[yi]] <- line_tabs
  }

  # Extract vertical divider tab data
  .jigsaw_env$vertical <- 1
  for (xi in 1:(xn - 1)) {
    .jigsaw_env$xi <- xi
    .jigsaw_env$yi <- 0

    line_tabs <- list()
    first()  # Initialize first tab

    for (yi in 0:(yn - 1)) {
      .jigsaw_env$yi <- yi

      # Store current tab parameters
      line_tabs[[yi + 1]] <- list(
        flip = .jigsaw_env$flip,
        a = .jigsaw_env$a,
        b = .jigsaw_env$b,
        c = .jigsaw_env$c,
        d = .jigsaw_env$d,
        e = .jigsaw_env$e
      )

      if (yi < yn - 1) {
        next_tab()  # Prepare for next segment
      }
    }

    tab_data$vertical[[xi]] <- line_tabs
  }

  return(tab_data)
}
