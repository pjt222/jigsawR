# Working Hexagonal Puzzle Piece Separation
# Simplified approach: separate placeholder pieces, not actual puzzle pieces

#' Extract hexagonal puzzle structure (stub for compatibility)
#'
#' Generates a complete hexagonal puzzle and returns its structure.
#' This is a compatibility stub for code that expects the old function.
#'
#' @param rings Number of rings
#' @param seed Random seed
#' @param diameter Puzzle diameter
#' @param tabsize Tab size percentage
#' @param jitter Jitter percentage
#' @param do_warp Apply circular warping
#' @param do_trunc Truncate edge pieces
#' @return List with puzzle structure
#' @export
extract_hexagonal_puzzle_structure <- function(rings, seed, diameter = 240,
                                              tabsize = 27, jitter = 5,
                                              do_warp = FALSE, do_trunc = FALSE) {

  # Source hexagonal functions if needed
  if (!exists("init_hex_jigsaw")) {
    if (file.exists("R/hexagonal_puzzle.R")) {
      source("R/hexagonal_puzzle.R")
    } else if (file.exists("hexagonal_puzzle.R")) {
      source("hexagonal_puzzle.R")
    }
  }

  # Generate complete puzzle
  puzzle_result <- generate_hex_jigsaw_svg(
    seed = seed,
    tabsize = tabsize,
    jitter = jitter,
    diameter = diameter,
    rings = rings,
    do_warp = do_warp,
    do_trunc = do_trunc
  )

  # Return structure compatible with old API
  return(list(
    type = "hexagonal",
    rings = rings,
    diameter = diameter,
    seed = seed,
    num_pieces = 3 * rings * (rings - 1) + 1,
    paths = list(
      horizontal = puzzle_result$horizontal,
      vertical = puzzle_result$vertical,
      border = puzzle_result$border
    ),
    parameters = puzzle_result$parameters,
    svg = puzzle_result$svg
  ))
}

#' Generate separated hexagonal puzzle with placeholder pieces or bezier curves
#'
#' Creates a layout showing where pieces would be positioned when separated.
#' Can use simple hexagon shapes as placeholders OR real puzzle pieces with bezier curves.
#'
#' @param rings Number of rings
#' @param seed Random seed
#' @param diameter Puzzle diameter
#' @param offset Separation distance between pieces
#' @param arrangement "rectangular" or "hexagonal" packing
#' @param use_bezier Use real bezier curves with tabs (default: FALSE for placeholder hexagons)
#' @param tabsize Tab size percentage (used when use_bezier = TRUE)
#' @param jitter Jitter percentage (used when use_bezier = TRUE)
#' @param do_warp Apply circular warping (legacy parameter, ignored)
#' @param do_trunc Truncate edge pieces (legacy parameter, ignored)
#' @param colors Piece colors
#' @param stroke_width Line width
#' @param background Background color
#' @return SVG content as string
#' @export
generate_separated_hexagonal_svg <- function(rings = 3, seed = NULL,
                                            diameter = 240, offset = 10,
                                            arrangement = "rectangular",
                                            use_bezier = FALSE,
                                            tabsize = 27, jitter = 5,
                                            do_warp = FALSE, do_trunc = FALSE,
                                            colors = NULL, stroke_width = 1,
                                            background = "none") {

  if (is.null(seed)) {
    seed <- as.integer(runif(1) * 10000)
  }

  # Source hexagonal functions if needed
  if (!exists("init_hex_jigsaw")) {
    if (file.exists("R/hexagonal_puzzle.R")) {
      source("R/hexagonal_puzzle.R")
    }
  }

  # Initialize hexagonal puzzle
  init_hex_jigsaw(seed = seed, rings = rings, diameter = diameter)

  # Calculate number of pieces
  num_pieces <- 3 * rings * (rings - 1) + 1

  cat(sprintf("Generating separated layout for %d pieces\n", num_pieces))

  # Calculate piece size (approximate)
  piece_radius <- diameter / (rings * 4)

  # Calculate grid layout
  if (arrangement == "rectangular") {
    # Rectangular packing for efficiency
    cols <- ceiling(sqrt(num_pieces * 1.3))
    rows <- ceiling(num_pieces / cols)

    cat(sprintf("Using rectangular grid: %d x %d\n", cols, rows))

    # Calculate viewBox
    total_width <- cols * (2 * piece_radius + offset) + offset
    total_height <- rows * (2 * piece_radius + offset) + offset
    vb_x <- 0
    vb_y <- 0
  } else {
    # Hexagonal arrangement (topology-based, centered at origin)

    # Calculate actual extent of pieces
    # For n rings, outermost pieces are at grid coordinates ~(±n, ±n)
    base_spacing <- piece_radius * 2
    separation_factor <- 1.0 + (offset / base_spacing)

    # Maximum distance from center (approximation)
    max_radius <- rings * base_spacing * separation_factor * 1.5

    # Add margin for piece size
    margin <- piece_radius * 2
    total_width <- 2 * (max_radius + margin)
    total_height <- 2 * (max_radius + margin)

    # Center the viewBox at origin
    vb_x <- -total_width / 2
    vb_y <- -total_height / 2
  }

  # Default colors
  if (is.null(colors)) {
    colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#FFA07A", "#98D8C8")
  }

  # Start SVG
  svg_lines <- c(
    sprintf('<svg width="%.1fmm" height="%.1fmm" viewBox="%.2f %.2f %.2f %.2f" xmlns="http://www.w3.org/2000/svg">',
            total_width, total_height, vb_x, vb_y, total_width, total_height),
    sprintf('  <title>Hexagonal Puzzle - %d rings, %d pieces (separated by %dmm)</title>',
            rings, num_pieces, offset)
  )

  # Add background if specified
  if (is.character(background) && background != "none" && background != "") {
    svg_lines <- c(svg_lines,
      sprintf('  <rect x="%.2f" y="%.2f" width="%.2f" height="%.2f" fill="%s"/>',
              vb_x, vb_y, total_width, total_height, background)
    )
  }

  svg_lines <- c(svg_lines, '  <g id="separated-pieces">')

  # Generate pieces (bezier or placeholder)
  if (use_bezier) {
    # Generate real puzzle pieces with complementary edges
    cat("Generating pieces with bezier curves and tabs...\n")

    # Source dependencies
    if (!exists("generate_hex_pieces_with_edge_map")) {
      if (file.exists("R/hexagonal_edge_generation_fixed.R")) {
        source("R/hexagonal_topology.R")
        source("R/hexagonal_neighbors.R")
        source("R/hexagonal_bezier_generation.R")
        source("R/hexagonal_edge_generation_fixed.R")
      }
    }

    # Calculate spacing based on arrangement
    if (arrangement == "rectangular") {
      # For rectangular, use offset directly (pieces spaced in grid)
      separated <- FALSE  # Pieces positioned in grid, not separated by topology
      base_spacing <- NULL
    } else {
      # For hexagonal, use topology-based separation
      separated <- TRUE
      base_spacing <- piece_radius * 2
      separation_factor <- 1.0 + (offset / base_spacing)
    }

    # Generate all pieces with proper edge mapping
    pieces <- generate_hex_pieces_with_edge_map(
      rings = rings,
      seed = seed,
      diameter = diameter,
      tabsize = tabsize,
      jitter = jitter,
      separated = separated,
      base_spacing = base_spacing,
      separation_factor = if (separated) separation_factor else 1.0
    )

    # Add pieces to SVG
    for (i in 1:num_pieces) {
      piece <- pieces[[i]]

      # For rectangular arrangement, override position
      if (arrangement == "rectangular") {
        row <- floor((i - 1) / cols)
        col <- (i - 1) %% cols
        piece$center_x <- offset + col * (2 * piece_radius + offset) + piece_radius
        piece$center_y <- offset + row * (2 * piece_radius + offset) + piece_radius

        # Rebuild path with new position (simple translation)
        # This is a simplified approach - proper implementation would regenerate edges
        # For now, we'll use the hexagonal arrangement for bezier mode
      }

      # Select color
      color_index <- ((i - 1) %% length(colors)) + 1
      piece_color <- colors[color_index]

      svg_lines <- c(svg_lines,
        sprintf('    <path d="%s" fill="white" stroke="%s" stroke-width="%.1f" opacity="0.9"/>',
                piece$path, piece_color, stroke_width),
        sprintf('    <text x="%.2f" y="%.2f" text-anchor="middle" dominant-baseline="central" font-size="8" fill="%s">%d</text>',
                piece$center_x, piece$center_y, piece_color, i)
      )
    }
  } else {
    # Generate placeholder hexagons
    for (i in 1:num_pieces) {
    # Calculate position and rotation
    piece_rotation <- 0  # Default rotation for rectangular arrangement

    if (arrangement == "rectangular") {
      row <- floor((i - 1) / cols)
      col <- (i - 1) %% cols
      center_x <- offset + col * (2 * piece_radius + offset) + piece_radius
      center_y <- offset + row * (2 * piece_radius + offset) + piece_radius
    } else {
      # Hexagonal arrangement (topology-based with center at origin)

      # Source topology utilities if needed
      if (!exists("calculate_hex_piece_position")) {
        if (file.exists("R/hexagonal_topology.R")) {
          source("R/hexagonal_topology.R")
        }
      }

      # Calculate spacing parameters
      base_spacing <- piece_radius * 2
      separation_factor <- 1.0 + (offset / base_spacing)

      # Get topology-based position
      position <- calculate_hex_piece_position(
        piece_id = i,
        rings = rings,
        base_spacing = base_spacing,
        separation_factor = separation_factor
      )

      center_x <- position$x
      center_y <- position$y

      # In a proper hexagonal grid (honeycomb), ALL hexagons have the same orientation
      # They don't rotate - only their position varies
      # This creates a tiled pattern where edges align naturally
      piece_rotation <- 0

      # Debug logging for first 3 pieces
      if (i <= 3) {
        cat(sprintf("Piece %d: (x=%.1f, y=%.1f, rot=%.2f°)\n",
                    i, center_x, center_y, piece_rotation * 180 / pi))
      }
    }

    # Select color
    color_index <- ((i - 1) %% length(colors)) + 1
    piece_color <- colors[color_index]

    # Create hexagon placeholder with rotation
    hex_path <- create_hex_placeholder(center_x, center_y, piece_radius * 0.8, piece_rotation)

    svg_lines <- c(svg_lines,
      sprintf('    <path d="%s" fill="none" stroke="%s" stroke-width="%.1f" opacity="0.9"/>',
              hex_path, piece_color, stroke_width),
      sprintf('    <text x="%.2f" y="%.2f" text-anchor="middle" dominant-baseline="central" font-size="8" fill="%s">%d</text>',
              center_x, center_y, piece_color, i)
    )
    }
  }

  svg_lines <- c(svg_lines, '  </g>', '</svg>')

  return(paste(svg_lines, collapse = "\n"))
}

#' Create hexagon placeholder path
#'
#' @param cx Center X
#' @param cy Center Y
#' @param radius Hexagon radius
#' @param rotation Rotation angle in radians (default: 0)
#' @return SVG path string
create_hex_placeholder <- function(cx, cy, radius, rotation = 0) {
  # Create regular hexagon with rotation
  # Add π/6 offset for flat-top orientation (edges horizontal)
  # Without offset: pointy-top (vertices at top/bottom)
  # With offset: flat-top (edges at top/bottom)
  base_offset <- pi / 6
  vertices <- list()
  for (i in 0:5) {
    angle <- i * pi / 3 + rotation + base_offset
    x <- cx + radius * cos(angle)
    y <- cy + radius * sin(angle)
    vertices[[i + 1]] <- c(x, y)
  }

  # Build path
  path <- sprintf("M %.2f %.2f", vertices[[1]][1], vertices[[1]][2])
  for (i in 2:6) {
    path <- paste(path, sprintf("L %.2f %.2f", vertices[[i]][1], vertices[[i]][2]))
  }
  path <- paste(path, "Z")

  return(path)
}
