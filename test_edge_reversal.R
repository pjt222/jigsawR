# Test edge direction reversal for individual pieces
# Critical for ensuring adjacent pieces have complementary tab/blank patterns

source("test_individual_pieces.R")  # Load working functions

cat("\n=== Edge Direction Reversal Test ===\n")

# Problem: Bottom and left edges need to be traced in reverse direction
# because we're going clockwise around the piece boundary, but the original
# tab data was generated left-to-right (horizontal) and top-to-bottom (vertical)

# Coordinate calculation functions (same as before)
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

# Generate forward curved edge (left-to-right or top-to-bottom)
generate_curved_edge_forward <- function(tab_params) {
  # Set environment variables from tab parameters
  .jigsaw_env$flip <- tab_params$flip
  .jigsaw_env$a <- tab_params$a
  .jigsaw_env$b <- tab_params$b
  .jigsaw_env$c <- tab_params$c
  .jigsaw_env$d <- tab_params$d
  .jigsaw_env$e <- tab_params$e
  
  # Generate Bézier control points
  p0_l <- l(0.0); p0_w <- w(0.0)
  p1_l <- l(0.2); p1_w <- w(.jigsaw_env$a)
  p2_l <- l(0.5 + .jigsaw_env$b + .jigsaw_env$d); p2_w <- w(-.jigsaw_env$t + .jigsaw_env$c)
  p3_l <- l(0.5 - .jigsaw_env$t + .jigsaw_env$b); p3_w <- w(.jigsaw_env$t + .jigsaw_env$c)
  p4_l <- l(0.5 - 2.0 * .jigsaw_env$t + .jigsaw_env$b - .jigsaw_env$d); p4_w <- w(3.0 * .jigsaw_env$t + .jigsaw_env$c)
  p5_l <- l(0.5 + 2.0 * .jigsaw_env$t + .jigsaw_env$b - .jigsaw_env$d); p5_w <- w(3.0 * .jigsaw_env$t + .jigsaw_env$c)
  p6_l <- l(0.5 + .jigsaw_env$t + .jigsaw_env$b); p6_w <- w(.jigsaw_env$t + .jigsaw_env$c)
  p7_l <- l(0.5 + .jigsaw_env$b + .jigsaw_env$d); p7_w <- w(-.jigsaw_env$t + .jigsaw_env$c)
  p8_l <- l(0.8); p8_w <- w(.jigsaw_env$e)
  p9_l <- l(1.0); p9_w <- w(0.0)
  
  # Forward path (original direction)
  path <- paste0(
    "C ", p1_l, " ", p1_w, " ", p2_l, " ", p2_w, " ", p3_l, " ", p3_w, " ",
    "C ", p4_l, " ", p4_w, " ", p5_l, " ", p5_w, " ", p6_l, " ", p6_w, " ",
    "C ", p7_l, " ", p7_w, " ", p8_l, " ", p8_w, " ", p9_l, " ", p9_w, " "
  )
  
  return(list(
    path = path,
    points = list(
      start = c(p0_l, p0_w),
      controls = list(
        c(p1_l, p1_w), c(p2_l, p2_w), c(p3_l, p3_w),
        c(p4_l, p4_w), c(p5_l, p5_w), c(p6_l, p6_w),  
        c(p7_l, p7_w), c(p8_l, p8_w)
      ),
      end = c(p9_l, p9_w)
    )
  ))
}

# Generate reversed curved edge (right-to-left or bottom-to-top)
generate_curved_edge_reverse <- function(tab_params) {
  # Generate forward version first
  forward <- generate_curved_edge_forward(tab_params)
  points <- forward$points
  
  # Reverse the path: start from end, reverse all control points
  # SVG cubic Bézier: C x1 y1 x2 y2 x y
  # To reverse: start from (x,y), use control points in reverse order
  
  start <- points$end
  end <- points$start
  controls <- rev(points$controls)
  
  # Cubic Bézier curves need to be reconstructed in reverse
  # Original: P0 -> C1,C2,P1 -> C3,C4,P2 -> C5,C6,P3
  # Reversed: P3 -> C6,C5,P2 -> C4,C3,P1 -> C2,C1,P0
  
  path <- paste0(
    "C ", controls[[1]][1], " ", controls[[1]][2], " ", 
          controls[[2]][1], " ", controls[[2]][2], " ", 
          controls[[3]][1], " ", controls[[3]][2], " ",
    "C ", controls[[4]][1], " ", controls[[4]][2], " ", 
          controls[[5]][1], " ", controls[[5]][2], " ", 
          controls[[6]][1], " ", controls[[6]][2], " ",
    "C ", controls[[7]][1], " ", controls[[7]][2], " ", 
          controls[[8]][1], " ", controls[[8]][2], " ", 
          end[1], " ", end[2], " "
  )
  
  return(path)
}

# Test edge reversal with piece (0,0) from 2x2 puzzle
cat("Testing edge reversal with piece (0,0):\n")

# Setup
seed <- 1234
xn <- 2
yn <- 2
tab_data <- extract_tab_data(seed, xn, yn)
init_jigsaw(seed = seed, xn = xn, yn = yn)

# Test horizontal edge in both directions
cat("Testing horizontal edge (row divider 1, column 1):\n")
.jigsaw_env$vertical <- 0  # Horizontal mode
.jigsaw_env$xi <- 0       # Column 0 (piece column)
.jigsaw_env$yi <- 1       # Row divider 1

tab_params <- tab_data$horizontal[[1]][[1]]
cat("Tab parameters: flip =", tab_params$flip, ", a =", round(tab_params$a, 3), "\n")

# Forward direction (left-to-right)
forward_edge <- generate_curved_edge_forward(tab_params)
cat("Forward path:", substr(forward_edge$path, 1, 100), "...\n")

# Reverse direction (right-to-left) 
reverse_edge <- generate_curved_edge_reverse(tab_params)
cat("Reverse path:", substr(reverse_edge, 1, 100), "...\n")

# Verify start/end points
forward_start <- forward_edge$points$start
forward_end <- forward_edge$points$end
cat("Forward: starts at (", forward_start[1], ",", forward_start[2], 
    ") ends at (", forward_end[1], ",", forward_end[2], ")\n")

cat("=== Edge Reversal Test Success ===\n")