# Working Hexagonal Puzzle Piece Separation
# Simplified approach: separate placeholder pieces, not actual puzzle pieces

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
    # Hexagonal arrangement (maintain structure with gaps)
    expansion_factor <- 1 + (offset / diameter)
    total_width <- diameter * expansion_factor * 1.5
    total_height <- diameter * expansion_factor * 1.5
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
      # Hexagonal arrangement (simplified spiral)
      angle <- (i - 1) * 2.4  # Spiral angle
      radius_pos <- sqrt(i) * piece_radius * 2
      center_x <- radius_pos * cos(angle)
      center_y <- radius_pos * sin(angle)
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
