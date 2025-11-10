# Direct R Translation of Draradech's JavaScript Jigsaw Puzzle Generator
# Original source: https://gist.github.com/Draradech/35d36347312ca6d0887aa7d55f366e30
# License: CC0 (Public Domain)

# Global variables (matching JS implementation exactly)
.jigsaw_env <- new.env()

#' Initialize jigsaw environment with global variables
#' @param seed Random seed (default: random)
#' @param tabsize Tab size percentage (default: 20)
#' @param jitter Jitter percentage (default: 4)
#' @param width Puzzle width in mm (default: 300)
#' @param height Puzzle height in mm (default: 200)
#' @param unit Unit specification: "mm" or "px"
#' @param dpi DPI for conversion (default: 96)
#' @param radius Corner radius in mm (default: 2.0)
#' @param xn Number of columns (default: 15)
#' @param yn Number of rows (default: 10)
init_jigsaw <- function(seed = NULL, tabsize = 20, jitter = 4,
                        width = 300, height = 200, 
                        unit = "mm", dpi = 96, radius = 2.0,
                        xn = 15, yn = 10) {

  if (is.null(seed)) {
    seed <- as.integer(runif(1) * 10000)
  }

  # Store all parameters in environment
  .jigsaw_env$seed_initial <- seed
  .jigsaw_env$seed <- seed
  .jigsaw_env$tabsize <- tabsize
  .jigsaw_env$jitter <- jitter
  .jigsaw_env$width <- width
  .jigsaw_env$height <- height
  .jigsaw_env$unit <- unit
  .jigsaw_env$dpi <- dpi
  .jigsaw_env$radius <- radius
  .jigsaw_env$xn <- xn
  .jigsaw_env$yn <- yn
  .jigsaw_env$offset <- 0.0

  # Parse input (equivalent to parse_input() in JS)
  .jigsaw_env$t <- tabsize / 200.0
  .jigsaw_env$j <- jitter / 100.0
}

# Random number generator (exact JS translation)
random <- function() {
  x <- sin(.jigsaw_env$seed) * 10000
  .jigsaw_env$seed <- .jigsaw_env$seed + 1
  return(x - floor(x))
}

uniform <- function(min_val, max_val) {
  r <- random()
  return(min_val + r * (max_val - min_val))
}

rbool <- function() {
  return(random() > 0.5)
}

# Tab generation functions (exact JS translation)
first <- function() {
  .jigsaw_env$e <- uniform(-.jigsaw_env$j, .jigsaw_env$j)
  .jigsaw_env$flip <- NULL  # Initialize flip for first call
  next_tab()
}

next_tab <- function() {
  flipold <- if (exists("flip", envir = .jigsaw_env)) .jigsaw_env$flip else NULL
  .jigsaw_env$flip <- rbool()
  .jigsaw_env$a <- if (is.null(flipold) || .jigsaw_env$flip == flipold) -.jigsaw_env$e else .jigsaw_env$e
  .jigsaw_env$b <- uniform(-.jigsaw_env$j, .jigsaw_env$j)
  .jigsaw_env$c <- uniform(-.jigsaw_env$j, .jigsaw_env$j)
  .jigsaw_env$d <- uniform(-.jigsaw_env$j, .jigsaw_env$j)
  .jigsaw_env$e <- uniform(-.jigsaw_env$j, .jigsaw_env$j)
}

# Coordinate calculation functions (exact JS translation)
sl <- function() {
  return(if (.jigsaw_env$vertical) .jigsaw_env$height / .jigsaw_env$yn else .jigsaw_env$width / .jigsaw_env$xn)
}

sw <- function() {
  return(if (.jigsaw_env$vertical) .jigsaw_env$width / .jigsaw_env$xn else .jigsaw_env$height / .jigsaw_env$yn)
}

ol <- function() {
  return(.jigsaw_env$offset + sl() * (if (.jigsaw_env$vertical) .jigsaw_env$yi else .jigsaw_env$xi))
}

ow <- function() {
  return(.jigsaw_env$offset + sw() * (if (.jigsaw_env$vertical) .jigsaw_env$xi else .jigsaw_env$yi))
}

l <- function(v) {
  ret <- ol() + sl() * v
  return(round(ret * 100) / 100)
}

w <- function(v) {
  ret <- ow() + sw() * v * (if (.jigsaw_env$flip) -1.0 else 1.0)
  return(round(ret * 100) / 100)
}

# Bézier control point functions (exact JS translation)
p0l <- function() l(0.0)
p0w <- function() w(0.0)
p1l <- function() l(0.2)
p1w <- function() w(.jigsaw_env$a)
p2l <- function() l(0.5 + .jigsaw_env$b + .jigsaw_env$d)
p2w <- function() w(-.jigsaw_env$t + .jigsaw_env$c)
p3l <- function() l(0.5 - .jigsaw_env$t + .jigsaw_env$b)
p3w <- function() w(.jigsaw_env$t + .jigsaw_env$c)
p4l <- function() l(0.5 - 2.0 * .jigsaw_env$t + .jigsaw_env$b - .jigsaw_env$d)
p4w <- function() w(3.0 * .jigsaw_env$t + .jigsaw_env$c)
p5l <- function() l(0.5 + 2.0 * .jigsaw_env$t + .jigsaw_env$b - .jigsaw_env$d)
p5w <- function() w(3.0 * .jigsaw_env$t + .jigsaw_env$c)
p6l <- function() l(0.5 + .jigsaw_env$t + .jigsaw_env$b)
p6w <- function() w(.jigsaw_env$t + .jigsaw_env$c)
p7l <- function() l(0.5 + .jigsaw_env$b + .jigsaw_env$d)
p7w <- function() w(-.jigsaw_env$t + .jigsaw_env$c)
p8l <- function() l(0.8)
p8w <- function() w(.jigsaw_env$e)
p9l <- function() l(1.0)
p9w <- function() w(0.0)

# Main generation functions (exact JS translation)
gen_dh <- function() {
  str <- ""
  .jigsaw_env$vertical <- 0

  for (yi in 1:(.jigsaw_env$yn - 1)) {
    .jigsaw_env$yi <- yi
    .jigsaw_env$xi <- 0
    first()
    str <- paste0(str, "M ", p0l(), ",", p0w(), " ")

    while (.jigsaw_env$xi < .jigsaw_env$xn) {
      str <- paste0(str, "C ", p1l(), " ", p1w(), " ", p2l(), " ", p2w(), " ", p3l(), " ", p3w(), " ")
      str <- paste0(str, "C ", p4l(), " ", p4w(), " ", p5l(), " ", p5w(), " ", p6l(), " ", p6w(), " ")
      str <- paste0(str, "C ", p7l(), " ", p7w(), " ", p8l(), " ", p8w(), " ", p9l(), " ", p9w(), " ")
      next_tab()
      .jigsaw_env$xi <- .jigsaw_env$xi + 1
    }
  }
  return(str)
}

gen_dv <- function() {
  str <- ""
  .jigsaw_env$vertical <- 1

  for (xi in 1:(.jigsaw_env$xn - 1)) {
    .jigsaw_env$xi <- xi
    .jigsaw_env$yi <- 0
    first()
    str <- paste0(str, "M ", p0w(), ",", p0l(), " ")

    while (.jigsaw_env$yi < .jigsaw_env$yn) {
      str <- paste0(str, "C ", p1w(), " ", p1l(), " ", p2w(), " ", p2l(), " ", p3w(), " ", p3l(), " ")
      str <- paste0(str, "C ", p4w(), " ", p4l(), " ", p5w(), " ", p5l(), " ", p6w(), " ", p6l(), " ")
      str <- paste0(str, "C ", p7w(), " ", p7l(), " ", p8w(), " ", p8l(), " ", p9w(), " ", p9l(), " ")
      next_tab()
      .jigsaw_env$yi <- .jigsaw_env$yi + 1
    }
  }
  return(str)
}

gen_db <- function() {
  offset <- .jigsaw_env$offset
  width <- .jigsaw_env$width
  height <- .jigsaw_env$height
  radius <- .jigsaw_env$radius

  str <- ""
  str <- paste0(str, "M ", (offset + radius), " ", (offset), " ")
  str <- paste0(str, "L ", (offset + width - radius), " ", (offset), " ")
  str <- paste0(str, "A ", (radius), " ", (radius), " 0 0 1 ", (offset + width), " ", (offset + radius), " ")
  str <- paste0(str, "L ", (offset + width), " ", (offset + height - radius), " ")
  str <- paste0(str, "A ", (radius), " ", (radius), " 0 0 1 ", (offset + width - radius), " ", (offset + height), " ")
  str <- paste0(str, "L ", (offset + radius), " ", (offset + height), " ")
  str <- paste0(str, "A ", (radius), " ", (radius), " 0 0 1 ", (offset), " ", (offset + height - radius), " ")
  str <- paste0(str, "L ", (offset), " ", (offset + radius), " ")
  str <- paste0(str, "A ", (radius), " ", (radius), " 0 0 1 ", (offset + radius), " ", (offset), " ")
  return(str)
}

#' Generate jigsaw puzzle SVG (main function)
#' @param seed Random seed
#' @param tabsize Tab size percentage (10-30)
#' @param jitter Jitter percentage (0-13)
#' @param width Puzzle width in mm
#' @param height Puzzle height in mm
#' @param radius Corner radius in mm
#' @param xn Number of columns
#' @param yn Number of rows
#' @return List containing SVG path data
generate_jigsaw_svg <- function(seed = NULL, tabsize = 20, jitter = 4,
                                width = 300, height = 200, 
                                unit = "mm", dpi = 96, radius = 2.0,
                                xn = 15, yn = 10) {

  # Convert to mm for internal calculations if needed
  if (unit == "px") {
    width_mm <- width * 25.4 / dpi
    height_mm <- height * 25.4 / dpi
    radius_mm <- radius * 25.4 / dpi
  } else {
    width_mm <- width
    height_mm <- height
    radius_mm <- radius
  }

  # Initialize environment
  init_jigsaw(seed, tabsize, jitter, width_mm, height_mm, unit, dpi, radius_mm, xn, yn)

  # Generate path data
  horizontal_paths <- gen_dh()
  vertical_paths <- gen_dv()
  border_paths <- gen_db()

  # Create SVG dimensions based on unit and DPI
  if (unit == "px") {
    svg_width <- sprintf("%.0f", width)
    svg_height <- sprintf("%.0f", height)
    # ViewBox uses mm units for consistency in path coordinates
    viewbox_w <- width_mm
    viewbox_h <- height_mm
  } else {
    svg_width <- sprintf("%.0fmm", width)
    svg_height <- sprintf("%.0fmm", height)
    viewbox_w <- width
    viewbox_h <- height
  }

  # Create complete SVG
  svg_content <- paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" version="1.0" ',
    'width="', svg_width, '" height="', svg_height, '" ',
    'viewBox="0 0 ', viewbox_w, ' ', viewbox_h, '">',
    '<path fill="none" stroke="DarkBlue" stroke-width="0.1" d="',
    horizontal_paths,
    '"></path>',
    '<path fill="none" stroke="DarkRed" stroke-width="0.1" d="',
    vertical_paths,
    '"></path>',
    '<path fill="none" stroke="Black" stroke-width="0.1" d="',
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
      seed = .jigsaw_env$seed_initial,
      tabsize = tabsize,
      jitter = jitter,
      width = width,
      height = height,
      unit = unit,
      dpi = dpi,
      width_mm = width_mm,
      height_mm = height_mm,
      radius = radius,
      xn = xn,
      yn = yn
    )
  ))
}

#' Save SVG to file
#' @param puzzle_data Output from generate_jigsaw_svg()
#' @param filename Output filename (default: "jigsaw.svg")
save_jigsaw_svg <- function(puzzle_data, filename = "jigsaw.svg") {
  # Ensure output directory exists
  if (!dir.exists("output")) {
    dir.create("output", recursive = TRUE)
  }
  
  # Add output/ prefix if not already present
  if (!grepl("^output/", filename)) {
    filename <- file.path("output", filename)
  }
  
  writeLines(puzzle_data$svg, filename)
  cat("Saved jigsaw puzzle to:", filename, "\n")
}

#' Print puzzle parameters
#' @param puzzle_data Output from generate_jigsaw_svg()
print_puzzle_info <- function(puzzle_data) {
  params <- puzzle_data$parameters
  cat("Jigsaw Puzzle Parameters:\n")
  cat("  Seed:", params$seed, "\n")
  cat("  Size:", params$width, "x", params$height, "mm\n")
  cat("  Pieces:", params$xn, "x", params$yn, "=", params$xn * params$yn, "total\n")
  cat("  Tab size:", params$tabsize, "%\n")
  cat("  Jitter:", params$jitter, "%\n")
  cat("  Corner radius:", params$radius, "mm\n")
}

# Note: Example usage moved to inst/examples/rectangular_puzzle_example.R
# to avoid auto-execution when sourcing this file for function definitions
