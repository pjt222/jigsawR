# Direct R Translation of Draradech's Hexagonal JavaScript Jigsaw Puzzle Generator
# Original source: https://draradech.github.io/jigsaw/jigsaw-hex.html
# License: CC0 (Public Domain)
#
# Optimized with batch RNG generation using C++ uniform_batch() when available.
# See R/rng_iterator.R for the batch optimization implementation.

# Global variables for hexagonal puzzle (matching JS implementation exactly)
.hex_jigsaw_env <- new.env()

#' Initialize hexagonal jigsaw environment
#' @param seed Random seed (default: random)
#' @param tabsize Tab size percentage (default: 27)
#' @param jitter Jitter percentage (default: 5)
#' @param diameter Puzzle diameter in mm (default: 240)
#' @param rings Number of rings (default: 6)
#' @param do_warp Circle warp (default: FALSE)
#' @param do_trunc Truncate edge pieces (default: FALSE)
init_hex_jigsaw <- function(seed = NULL, tabsize = 27, jitter = 5,
                            diameter = 240, rings = 6, do_warp = FALSE, do_trunc = FALSE) {

  if (is.null(seed)) {
    seed <- as.integer(runif(1) * 10000)
  }

  # Store all parameters in environment (matching JS exactly)
  .hex_jigsaw_env$seed_initial <- seed
  .hex_jigsaw_env$seed <- seed
  .hex_jigsaw_env$tabsize <- tabsize
  .hex_jigsaw_env$jitter <- jitter
  .hex_jigsaw_env$diameter <- diameter
  .hex_jigsaw_env$rings <- rings
  .hex_jigsaw_env$do_warp <- do_warp
  .hex_jigsaw_env$do_trunc <- do_trunc

  # Parse input (equivalent to parse_input() in JS)
  .hex_jigsaw_env$t <- tabsize / 200.0
  .hex_jigsaw_env$j <- jitter / 100.0
  .hex_jigsaw_env$n <- rings

  # Set radius and offset for display/generation
  .hex_jigsaw_env$radius <- diameter / 2.0
  .hex_jigsaw_env$offset <- .hex_jigsaw_env$radius * 0.2

  # Create RNG iterator with pre-generated batch values for performance
  # This uses C++ uniform_batch() when available (~27x speedup)
  rng_count <- calc_hex_rng_count(rings)
  .hex_jigsaw_env$rng <- create_rng_iterator(seed, rng_count)
}

# Random number generator - uses pre-generated batch values
# (Original JS translation used per-call sine-based RNG)
hex_random <- function() {
  .hex_jigsaw_env$rng$next_val()
}

hex_uniform <- function(min_val, max_val) {
  .hex_jigsaw_env$rng$uniform(min_val, max_val)
}

hex_rbool <- function() {
  .hex_jigsaw_env$rng$rbool()
}

# Tab generation functions (exact JS translation)
hex_next <- function() {
  .hex_jigsaw_env$flip <- hex_rbool()
  .hex_jigsaw_env$a <- hex_uniform(-.hex_jigsaw_env$j, .hex_jigsaw_env$j)
  .hex_jigsaw_env$b <- hex_uniform(-.hex_jigsaw_env$j, .hex_jigsaw_env$j)
  .hex_jigsaw_env$c <- hex_uniform(-.hex_jigsaw_env$j, .hex_jigsaw_env$j)
  .hex_jigsaw_env$d <- hex_uniform(-.hex_jigsaw_env$j, .hex_jigsaw_env$j)
  .hex_jigsaw_env$e <- hex_uniform(-.hex_jigsaw_env$j, .hex_jigsaw_env$j)
}

hex_l <- function(v) {
  return(v)
}

hex_w <- function(v) {
  return(v * (if (.hex_jigsaw_env$flip) -1.0 else 1.0))
}

# Control point functions (exact JS translation)
hex_p0 <- function() list(l = hex_l(0.0), w = hex_w(0.0))
hex_p1 <- function() list(l = hex_l(0.2), w = hex_w(.hex_jigsaw_env$a))
hex_p2 <- function() list(l = hex_l(0.5 + .hex_jigsaw_env$b + .hex_jigsaw_env$d), w = hex_w(-.hex_jigsaw_env$t + .hex_jigsaw_env$c))
hex_p3 <- function() list(l = hex_l(0.5 - .hex_jigsaw_env$t + .hex_jigsaw_env$b), w = hex_w(.hex_jigsaw_env$t + .hex_jigsaw_env$c))
hex_p4 <- function() list(l = hex_l(0.5 - 2.0 * .hex_jigsaw_env$t + .hex_jigsaw_env$b - .hex_jigsaw_env$d), w = hex_w(3.0 * .hex_jigsaw_env$t + .hex_jigsaw_env$c))
hex_p5 <- function() list(l = hex_l(0.5 + 2.0 * .hex_jigsaw_env$t + .hex_jigsaw_env$b - .hex_jigsaw_env$d), w = hex_w(3.0 * .hex_jigsaw_env$t + .hex_jigsaw_env$c))
hex_p6 <- function() list(l = hex_l(0.5 + .hex_jigsaw_env$t + .hex_jigsaw_env$b), w = hex_w(.hex_jigsaw_env$t + .hex_jigsaw_env$c))
hex_p7 <- function() list(l = hex_l(0.5 + .hex_jigsaw_env$b + .hex_jigsaw_env$d), w = hex_w(-.hex_jigsaw_env$t + .hex_jigsaw_env$c))
hex_p8 <- function() list(l = hex_l(0.8), w = hex_w(.hex_jigsaw_env$e))
hex_p9 <- function() list(l = hex_l(1.0), w = hex_w(0.0))

# Coordinate transformation functions (exact JS translation)
hex_scale <- function(x, y) {
  n <- .hex_jigsaw_env$n
  radius <- .hex_jigsaw_env$radius
  return(list(
    x = x * (1.0 / (2 * n - 4.0/3)) * radius,
    y = y * (1.0 / (2 * n - 4.0/3)) * radius * sqrt(0.75)
  ))
}

hex_rotate <- function(vec, rot) {
  cs <- cos(rot)
  sn <- sin(rot)
  return(list(
    x = vec$x * cs - vec$y * sn,
    y = vec$x * sn + vec$y * cs
  ))
}

hex_warp <- function(vec) {
  if (!.hex_jigsaw_env$do_warp) {
    return(vec)
  }
  angl <- atan2(vec$y, vec$x) + pi
  angl60 <- angl %% (pi / 3)
  angl30 <- abs((pi / 6) - angl60)
  l <- sqrt(0.75) / cos(angl30)
  return(list(
    x = vec$x / l,
    y = vec$y / l
  ))
}

hex_translate <- function(vec) {
  radius <- .hex_jigsaw_env$radius
  offset <- .hex_jigsaw_env$offset
  return(list(
    x = vec$x + radius + offset,
    y = vec$y + radius + offset
  ))
}

hex_process_r <- function(x, y, rot) {
  return(hex_translate(hex_warp(hex_rotate(hex_scale(x, y), rot))))
}

hex_process <- function(x, y) {
  return(hex_process_r(x, y, 0))
}

# Vector operations (exact JS translation)
hex_sub <- function(v1, v2) {
  return(list(
    x = v1$x - v2$x,
    y = v1$y - v2$y
  ))
}

hex_rot90 <- function(v) {
  return(list(
    x = -v$y,
    y = v$x
  ))
}

hex_add <- function(v1, v2) {
  return(list(
    x = v1$x + v2$x,
    y = v1$y + v2$y
  ))
}

hex_mul <- function(s, v) {
  return(list(
    x = s * v$x,
    y = s * v$y
  ))
}

# Linear interpolation function (exact JS translation)
hex_lerp <- function(p, v1, v2, op) {
  dl <- hex_sub(v2, v1)
  dw <- hex_rot90(dl)
  vec <- hex_add(v1, hex_mul(p$l, dl))
  vec <- hex_add(vec, hex_mul(p$w, dw))
  return(paste0(op, round(vec$x, 2), " ", round(vec$y, 2), " "))
}

# Generate tab function (exact JS translation)
hex_gentab <- function(v1, v2, isnew) {
  str <- ""
  hex_next()

  if (isnew) {
    str <- paste0(str, hex_lerp(hex_p0(), v1, v2, "M "))
  }
  str <- paste0(str, hex_lerp(hex_p1(), v1, v2, "C "))
  str <- paste0(str, hex_lerp(hex_p2(), v1, v2, ""))
  str <- paste0(str, hex_lerp(hex_p3(), v1, v2, ""))
  str <- paste0(str, hex_lerp(hex_p4(), v1, v2, "C "))
  str <- paste0(str, hex_lerp(hex_p5(), v1, v2, ""))
  str <- paste0(str, hex_lerp(hex_p6(), v1, v2, ""))
  str <- paste0(str, hex_lerp(hex_p7(), v1, v2, "C "))
  str <- paste0(str, hex_lerp(hex_p8(), v1, v2, ""))
  str <- paste0(str, hex_lerp(hex_p9(), v1, v2, ""))

  return(str)
}

# Line segment functions (exact JS translation)
hex_hlineseg <- function(x, y, isnew) {
  yeven <- ((y + 1) %% 4 == 0)
  xeven <- (x %% 2 == 0)
  yoff <- if ((yeven && xeven) || (!yeven && !xeven)) (-1.0/3) else (1.0/3)

  return(hex_gentab(hex_process(x, y + yoff), hex_process(x + 1, y - yoff), isnew))
}

hex_vlineseg <- function(x, y) {
  return(hex_gentab(hex_process(x, y + (1.0/3)), hex_process(x, y + (5.0/3)), TRUE))
}

hex_blineseg <- function(x, y, isnew, rot) {
  yeven <- ((y + 1) %% 4 == 0)
  xeven <- (x %% 2 == 0)
  yoff <- if ((yeven && xeven) || (!yeven && !xeven)) (-1.0/3) else (1.0/3)

  str <- ""
  if (isnew) {
    vec <- hex_process_r(x, y + yoff, rot)
    str <- paste0(str, "M ", round(vec$x, 2), " ", round(vec$y, 2), " ")
  }
  vec <- hex_process_r(x + 1, y - yoff, rot)
  str <- paste0(str, "L ", round(vec$x, 2), " ", round(vec$y, 2), " ")

  return(str)
}

# Main generation functions (exact JS translation)
hex_gen_dh <- function() {
  str <- ""
  n <- .hex_jigsaw_env$n
  yl <- 2 * n - 1

  for (yi in seq(-yl + 2, yl - 2, by = 2)) {
    isnew <- TRUE
    xl <- 2 * n - 1 - (abs(yi) - 1) / 2
    for (xi in seq(-xl + 1, xl - 2, by = 1)) {
      str <- paste0(str, hex_hlineseg(xi, yi, isnew))
      isnew <- FALSE
    }
  }
  return(str)
}

hex_gen_dv <- function() {
  str <- ""
  n <- .hex_jigsaw_env$n
  yl <- 2 * n - 1

  for (yi in seq(-yl, yl - 1, by = 2)) {
    xl <- 2 * n - 1 - (abs(yi + 1)) / 2
    for (xi in seq(-xl + 2, xl - 2, by = 2)) {
      str <- paste0(str, hex_vlineseg(xi, yi))
    }
  }
  return(str)
}

hex_gen_db <- function() {
  str <- ""
  n <- .hex_jigsaw_env$n
  radius <- .hex_jigsaw_env$radius
  offset <- .hex_jigsaw_env$offset

  if (!.hex_jigsaw_env$do_trunc) {
    yi <- 1 - 2 * n
    isnew <- TRUE

    for (rot in seq(0.0, 2 * pi - 0.1, by = pi/3)) {
      for (xi in seq(-n, n - 2, by = 1)) {
        str <- paste0(str, hex_blineseg(xi, yi, isnew, rot))
        isnew <- FALSE
      }
    }
  } else {
    if (.hex_jigsaw_env$do_warp) {
      str <- paste0(str, "M ", offset, " ", (radius + offset), " ")
      str <- paste0(str, "a ", radius, " ", radius, " 0 1 0 ", (2 * radius), " 0 ")
      str <- paste0(str, "a ", radius, " ", radius, " 0 1 0 ", (-2 * radius), " 0 ")
    } else {
      str <- paste0(str, "M ", offset, " ", (radius + offset), " ")
      str <- paste0(str, "L ", (radius/2 + offset), " ", (radius + offset - radius * sqrt(0.75)), " ")
      str <- paste0(str, "L ", (radius * 1.5 + offset), " ", (radius + offset - radius * sqrt(0.75)), " ")
      str <- paste0(str, "L ", (radius * 2 + offset), " ", (radius + offset), " ")
      str <- paste0(str, "L ", (radius * 1.5 + offset), " ", (radius + offset + radius * sqrt(0.75)), " ")
      str <- paste0(str, "L ", (radius/2 + offset), " ", (radius + offset + radius * sqrt(0.75)), " ")
      str <- paste0(str, "L ", offset, " ", (radius + offset), " ")
    }
  }
  return(str)
}

#' Generate hexagonal jigsaw puzzle SVG (main function)
#' @param seed Random seed
#' @param tabsize Tab size percentage (15-35)
#' @param jitter Jitter percentage (0-13)
#' @param diameter Puzzle diameter in mm
#' @param rings Number of rings
#' @param do_warp Circle warp (creates circular puzzle)
#' @param do_trunc Truncate edge pieces
#' @param stroke_color Color for puzzle lines (default: "black")
#' @param stroke_width Width of puzzle lines (default: 1)
#' @param background Background color or "none" (default: "none")
#' @param opacity Opacity of puzzle pieces (0.0 to 1.0, default 1.0 = fully opaque)
#' @return List containing SVG path data
generate_hex_jigsaw_svg <- function(seed = NULL, tabsize = 27, jitter = 5,
                                    diameter = 240, rings = 6,
                                    do_warp = FALSE, do_trunc = FALSE,
                                    stroke_color = "black", stroke_width = 1,
                                    background = "none", opacity = 1.0) {

  # Initialize environment
  init_hex_jigsaw(seed, tabsize, jitter, diameter, rings, do_warp, do_trunc)

  # Generate path data
  horizontal_paths <- hex_gen_dh()
  vertical_paths <- hex_gen_dv()
  border_paths <- hex_gen_db()

  # Calculate final dimensions
  radius <- .hex_jigsaw_env$radius
  offset <- .hex_jigsaw_env$offset
  width <- 2.0 * (radius + offset)
  height <- 2.0 * (radius + offset)

  # Create complete SVG with styling
  svg_lines <- c(
    sprintf('<svg xmlns="http://www.w3.org/2000/svg" version="1.0" width="%.1fmm" height="%.1fmm" viewBox="0 0 %.1f %.1f">',
            width, height, width, height)
  )
  
  # Add background based on type
  # Background can be: "none", a color string, or a list with gradient colors
  if (is.list(background) && !is.null(background$type) && background$type == "gradient") {
    # Custom gradient with user-specified colors
    center_color <- background$center
    middle_color <- background$middle
    edge_color <- background$edge
    svg_lines <- c(svg_lines,
      '  <defs>',
      '    <radialGradient id="bg-gradient" cx="50%" cy="50%" r="50%">',
      sprintf('      <stop offset="0%%" style="stop-color:%s;stop-opacity:1" />', center_color),
      sprintf('      <stop offset="50%%" style="stop-color:%s;stop-opacity:1" />', middle_color),
      sprintf('      <stop offset="100%%" style="stop-color:%s;stop-opacity:1" />', edge_color),
      '    </radialGradient>',
      '  </defs>',
      '<rect width="100%" height="100%" fill="url(#bg-gradient)"/>'
    )
  } else if (is.character(background) && background == "gradient") {
    # Legacy: default gradient colors for backward compatibility
    svg_lines <- c(svg_lines,
      '  <defs>',
      '    <radialGradient id="bg-gradient" cx="50%" cy="50%" r="50%">',
      '      <stop offset="0%" style="stop-color:#e3f2fd;stop-opacity:1" />',
      '      <stop offset="50%" style="stop-color:#bbdefb;stop-opacity:1" />',
      '      <stop offset="100%" style="stop-color:#90caf9;stop-opacity:1" />',
      '    </radialGradient>',
      '  </defs>',
      '<rect width="100%" height="100%" fill="url(#bg-gradient)"/>'
    )
  } else if (is.character(background) && (background == "none" || background == "")) {
    # No background rect
  } else if (is.character(background)) {
    # Solid color background
    svg_lines <- c(svg_lines,
      sprintf('<rect width="100%%" height="100%%" fill="%s"/>', background)
    )
  }
  
  # Add paths with consistent styling
  if (nchar(horizontal_paths) > 0) {
    svg_lines <- c(svg_lines,
      sprintf('<path fill="none" stroke="%s" stroke-width="%.1f" opacity="%.2f" d="%s"/>',
              stroke_color, stroke_width, opacity, horizontal_paths)
    )
  }

  if (nchar(vertical_paths) > 0) {
    svg_lines <- c(svg_lines,
      sprintf('<path fill="none" stroke="%s" stroke-width="%.1f" opacity="%.2f" d="%s"/>',
              stroke_color, stroke_width, opacity, vertical_paths)
    )
  }

  if (nchar(border_paths) > 0) {
    svg_lines <- c(svg_lines,
      sprintf('<path fill="none" stroke="%s" stroke-width="%.1f" opacity="%.2f" d="%s"/>',
              stroke_color, stroke_width * 1.5, opacity, border_paths)  # Slightly thicker border
    )
  }
  
  svg_lines <- c(svg_lines, '</svg>')
  svg_content <- paste(svg_lines, collapse = "")

  return(list(
    svg = svg_content,
    horizontal = horizontal_paths,
    vertical = vertical_paths,
    border = border_paths,
    parameters = list(
      seed = .hex_jigsaw_env$seed_initial,
      tabsize = tabsize,
      jitter = jitter,
      diameter = diameter,
      rings = rings,
      do_warp = do_warp,
      do_trunc = do_trunc
    )
  ))
}

#' Save hexagonal SVG to file
#' @param puzzle_data Output from generate_hex_jigsaw_svg()
#' @param filename Output filename (default: "hex_jigsaw.svg")
save_hex_jigsaw_svg <- function(puzzle_data, filename = "hex_jigsaw.svg") {
  # Ensure output directory exists
  if (!dir.exists("output")) {
    dir.create("output", recursive = TRUE)
  }
  
  # Add output/ prefix if not already present
  if (!grepl("^output/", filename)) {
    filename <- file.path("output", filename)
  }
  
  writeLines(puzzle_data$svg, filename)
  log_success("Saved hexagonal jigsaw puzzle to: {.file {filename}}")
}

#' Print hexagonal puzzle parameters
#' @param puzzle_data Output from generate_hex_jigsaw_svg()
print_hex_puzzle_info <- function(puzzle_data) {
  params <- puzzle_data$parameters
  shape_type <- if (params$do_warp) "Circular" else "Hexagonal"
  edge_type <- if (params$do_trunc) " (truncated edges)" else ""

  log_subheader("Hexagonal Jigsaw Puzzle Parameters")
  log_params("Puzzle Configuration", list(
    Seed = params$seed,
    Shape = paste0(shape_type, edge_type),
    Diameter = paste0(params$diameter, " mm"),
    Rings = params$rings,
    "Tab size" = paste0(params$tabsize, "%"),
    Jitter = paste0(params$jitter, "%"),
    "Circle warp" = params$do_warp,
    "Truncate edges" = params$do_trunc
  ))
}

# Example usage (exact equivalent to JS interface)
# Commented out to prevent execution when sourced by Shiny app
# Uncomment and run interactively for testing
if (FALSE) {
  log_header("Hexagonal Jigsaw Puzzle Generator (R Translation)")
  log_info("Original JavaScript by Draradech")
  log_info("GitHub: {.url https://github.com/Draradech/jigsaw}")

  # Generate standard hexagonal puzzle
  hex_puzzle <- generate_hex_jigsaw_svg(
    seed = 1234,
    tabsize = 27,   # 27% (JS default)
    jitter = 5,     # 5% (JS default)
    diameter = 240, # 240mm (JS default)
    rings = 6,      # 6 rings (JS default)
    do_warp = FALSE,
    do_trunc = FALSE
  )

  print_hex_puzzle_info(hex_puzzle)
  save_hex_jigsaw_svg(hex_puzzle, "hexagonal_puzzle.svg")

  # Generate circular puzzle with warp
  circular_puzzle <- generate_hex_jigsaw_svg(
    seed = 5678,
    tabsize = 30,
    jitter =3,
    diameter = 200,
    rings = 3,
    do_warp = TRUE,    # Enable circular warp
    do_trunc = TRUE    # Truncate edge pieces for clean circle
  )

  print_hex_puzzle_info(circular_puzzle)
  save_hex_jigsaw_svg(circular_puzzle, "circular_puzzle.svg")

  # Display first few characters of each path type
  log_subheader("Generated SVG paths (hexagonal)")
  log_info("Horizontal (first 100 chars): {substr(hex_puzzle$horizontal, 1, 100)}...")
  log_info("Vertical (first 100 chars): {substr(hex_puzzle$vertical, 1, 100)}...")
  log_info("Border (first 100 chars): {substr(hex_puzzle$border, 1, 100)}...")

  log_success("Both hexagonal and circular puzzles generated successfully!")
}
