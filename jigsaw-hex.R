# Direct R Translation of Draradech's Hexagonal JavaScript Jigsaw Puzzle Generator
# Original source: https://draradech.github.io/jigsaw/jigsaw-hex.html
# License: CC0 (Public Domain)

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
}

# Random number generator (exact JS translation)
hex_random <- function() {
  x <- sin(.hex_jigsaw_env$seed) * 10000
  .hex_jigsaw_env$seed <- .hex_jigsaw_env$seed + 1
  return(x - floor(x))
}

hex_uniform <- function(min_val, max_val) {
  r <- hex_random()
  return(min_val + r * (max_val - min_val))
}

hex_rbool <- function() {
  return(hex_random() > 0.5)
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
#' @return List containing SVG path data
generate_hex_jigsaw_svg <- function(seed = NULL, tabsize = 27, jitter = 5,
                                    diameter = 240, rings = 6,
                                    do_warp = FALSE, do_trunc = FALSE) {

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

  # Create complete SVG
  svg_content <- paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" version="1.0" ',
    'width="', width, 'mm" height="', height, 'mm" ',
    'viewBox="0 0 ', width, ' ', height, '">',
    '<path fill="none" stroke="DarkBlue" stroke-width="0.2" d="',
    horizontal_paths,
    '"></path>',
    '<path fill="none" stroke="DarkRed" stroke-width="0.2" d="',
    vertical_paths,
    '"></path>',
    '<path fill="none" stroke="Black" stroke-width="0.2" d="',
    border_paths,
    '"></path>',
    '</svg>'
  )

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
  writeLines(puzzle_data$svg, filename)
  cat("Saved hexagonal jigsaw puzzle to:", filename, "\n")
}

#' Print hexagonal puzzle parameters
#' @param puzzle_data Output from generate_hex_jigsaw_svg()
print_hex_puzzle_info <- function(puzzle_data) {
  params <- puzzle_data$parameters
  shape_type <- if (params$do_warp) "Circular" else "Hexagonal"
  edge_type <- if (params$do_trunc) " (truncated edges)" else ""

  cat("Hexagonal Jigsaw Puzzle Parameters:\n")
  cat("  Seed:", params$seed, "\n")
  cat("  Shape:", paste0(shape_type, edge_type), "\n")
  cat("  Diameter:", params$diameter, "mm\n")
  cat("  Rings:", params$rings, "\n")
  cat("  Tab size:", params$tabsize, "%\n")
  cat("  Jitter:", params$jitter, "%\n")
  cat("  Circle warp:", params$do_warp, "\n")
  cat("  Truncate edges:", params$do_trunc, "\n")
}

# Example usage (exact equivalent to JS interface)
cat("Hexagonal Jigsaw Puzzle Generator (R Translation)\n")
cat("Original JavaScript by Draradech\n")
cat("GitHub: https://github.com/Draradech/jigsaw\n\n")

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
cat("\nGenerated SVG paths (hexagonal):\n")
cat("Horizontal (first 100 chars):", substr(hex_puzzle$horizontal, 1, 100), "...\n")
cat("Vertical (first 100 chars):", substr(hex_puzzle$vertical, 1, 100), "...\n")
cat("Border (first 100 chars):", substr(hex_puzzle$border, 1, 100), "...\n")

cat("\nBoth hexagonal and circular puzzles generated successfully!\n")
