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

#' Generate separated hexagonal puzzle with placeholder pieces
#'
#' Creates a layout showing where pieces would be positioned when separated.
#' Uses simple hexagon shapes as placeholders until individual piece extraction works.
#'
#' @param rings Number of rings
#' @param seed Random seed
#' @param diameter Puzzle diameter
#' @param offset Separation distance between pieces
#' @param arrangement "rectangular" or "hexagonal" packing
#' @param colors Piece colors
#' @param stroke_width Line width
#' @param background Background color
#' @return SVG content as string
#' @export
generate_separated_hexagonal_svg <- function(rings = 3, seed = NULL,
                                            diameter = 240, offset = 10,
                                            arrangement = "rectangular",
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

  # Generate placeholder pieces
  for (i in 1:num_pieces) {
    # Calculate position
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

      # Debug logging for first 3 pieces
      if (i <= 3) {
        cat(sprintf("Piece %d: (x=%.1f, y=%.1f)\n", i, center_x, center_y))
      }
    }

    # Select color
    color_index <- ((i - 1) %% length(colors)) + 1
    piece_color <- colors[color_index]

    # Create hexagon placeholder
    hex_path <- create_hex_placeholder(center_x, center_y, piece_radius * 0.8)

    svg_lines <- c(svg_lines,
      sprintf('    <path d="%s" fill="none" stroke="%s" stroke-width="%.1f" opacity="0.9"/>',
              hex_path, piece_color, stroke_width),
      sprintf('    <text x="%.2f" y="%.2f" text-anchor="middle" dominant-baseline="central" font-size="8" fill="%s">%d</text>',
              center_x, center_y, piece_color, i)
    )
  }

  svg_lines <- c(svg_lines, '  </g>', '</svg>')

  return(paste(svg_lines, collapse = "\n"))
}

#' Create hexagon placeholder path
#'
#' @param cx Center X
#' @param cy Center Y
#' @param radius Hexagon radius
#' @return SVG path string
create_hex_placeholder <- function(cx, cy, radius) {
  # Create regular hexagon
  vertices <- list()
  for (i in 0:5) {
    angle <- i * pi / 3
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
