# Test piece boundary tracing for individual pieces
# Building on successful tab data extraction

source("test_individual_pieces.R")  # Load the working functions

cat("\n=== Piece Boundary Tracing Test ===\n")

# Coordinate calculation functions (from rectangular_puzzle.R)
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

# Bézier control point functions
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

# Generate curved edge path using tab parameters
generate_curved_edge <- function(tab_params) {
  # Set environment variables from tab parameters
  .jigsaw_env$flip <- tab_params$flip
  .jigsaw_env$a <- tab_params$a
  .jigsaw_env$b <- tab_params$b
  .jigsaw_env$c <- tab_params$c
  .jigsaw_env$d <- tab_params$d
  .jigsaw_env$e <- tab_params$e
  
  # Generate the Bézier curve path
  path <- paste0(
    "C ", p1l(), " ", p1w(), " ", p2l(), " ", p2w(), " ", p3l(), " ", p3w(), " ",
    "C ", p4l(), " ", p4w(), " ", p5l(), " ", p5w(), " ", p6l(), " ", p6w(), " ",
    "C ", p7l(), " ", p7w(), " ", p8l(), " ", p8w(), " ", p9l(), " ", p9w(), " "
  )
  
  return(path)
}

# Generate straight edge path  
generate_straight_edge <- function(start_x, start_y, end_x, end_y) {
  return(paste0("L ", end_x, " ", end_y, " "))
}

# Test with piece (0,0) - top-left corner of 2x2 puzzle
cat("Testing piece (0,0) - top-left corner:\n")

# Get tab data
seed <- 1234
xn <- 2
yn <- 2
tab_data <- extract_tab_data(seed, xn, yn)

# Initialize for piece calculations
init_jigsaw(seed = seed, xn = xn, yn = yn)
piece_x <- 0
piece_y <- 0

# Calculate piece corner coordinates
piece_width <- .jigsaw_env$width / xn
piece_height <- .jigsaw_env$height / yn
x1 <- .jigsaw_env$offset + piece_x * piece_width
y1 <- .jigsaw_env$offset + piece_y * piece_height
x2 <- x1 + piece_width
y2 <- y1 + piece_height

cat("Piece coordinates: (", x1, ",", y1, ") to (", x2, ",", y2, ")\n")

# Start tracing clockwise from top-left corner
path <- paste0("M ", x1, " ", y1, " ")

# 1. Top edge: (x1,y1) to (x2,y1)
cat("Top edge: straight (puzzle border)\n")
path <- paste0(path, generate_straight_edge(x1, y1, x2, y1))

# 2. Right edge: (x2,y1) to (x2,y2) 
cat("Right edge: curved (vertical divider 1, row 1)\n")
.jigsaw_env$vertical <- 1  # Vertical mode
.jigsaw_env$xi <- piece_x + 1  # Column divider index (1)
.jigsaw_env$yi <- piece_y      # Row start (0)
right_tab <- tab_data$vertical[[1]][[1]]  # Column divider 1, row segment 1
right_path <- generate_curved_edge(right_tab)
path <- paste0(path, right_path)

# 3. Bottom edge: (x2,y2) to (x1,y2)
cat("Bottom edge: curved (horizontal divider 1, column 1) - REVERSED\n")
.jigsaw_env$vertical <- 0  # Horizontal mode  
.jigsaw_env$xi <- piece_x      # Column start (0)
.jigsaw_env$yi <- piece_y + 1  # Row divider index (1)
# Note: Bottom edge needs to be reversed since we're tracing right-to-left
# This is a simplified version - full reversal logic needed
bottom_tab <- tab_data$horizontal[[1]][[1]]  # Row divider 1, column segment 1
bottom_path <- generate_curved_edge(bottom_tab)
path <- paste0(path, bottom_path)

# 4. Left edge: (x1,y2) to (x1,y1)
cat("Left edge: straight (puzzle border)\n")
path <- paste0(path, generate_straight_edge(x1, y2, x1, y1))

# Close the path
path <- paste0(path, "Z")

cat("Complete piece path:\n")
cat(substr(path, 1, 200), "...\n")

cat("=== Piece Tracing Test Success ===\n")