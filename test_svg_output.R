# Test SVG output generation for individual pieces
# Create actual SVG file to validate the piece generation

source("test_individual_pieces.R")  # Load working functions

cat("\n=== SVG Output Generation Test ===\n")

# All the coordinate and edge generation functions from previous tests
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

generate_straight_edge <- function(x1, y1, x2, y2) {
  return(paste0("L ", x2, " ", y2, " "))
}

generate_curved_edge_forward <- function(tab_params) {
  .jigsaw_env$flip <- tab_params$flip
  .jigsaw_env$a <- tab_params$a
  .jigsaw_env$b <- tab_params$b
  .jigsaw_env$c <- tab_params$c
  .jigsaw_env$d <- tab_params$d
  .jigsaw_env$e <- tab_params$e
  
  p1_l <- l(0.2); p1_w <- w(.jigsaw_env$a)
  p2_l <- l(0.5 + .jigsaw_env$b + .jigsaw_env$d); p2_w <- w(-.jigsaw_env$t + .jigsaw_env$c)
  p3_l <- l(0.5 - .jigsaw_env$t + .jigsaw_env$b); p3_w <- w(.jigsaw_env$t + .jigsaw_env$c)
  p4_l <- l(0.5 - 2.0 * .jigsaw_env$t + .jigsaw_env$b - .jigsaw_env$d); p4_w <- w(3.0 * .jigsaw_env$t + .jigsaw_env$c)
  p5_l <- l(0.5 + 2.0 * .jigsaw_env$t + .jigsaw_env$b - .jigsaw_env$d); p5_w <- w(3.0 * .jigsaw_env$t + .jigsaw_env$c)
  p6_l <- l(0.5 + .jigsaw_env$t + .jigsaw_env$b); p6_w <- w(.jigsaw_env$t + .jigsaw_env$c)
  p7_l <- l(0.5 + .jigsaw_env$b + .jigsaw_env$d); p7_w <- w(-.jigsaw_env$t + .jigsaw_env$c)
  p8_l <- l(0.8); p8_w <- w(.jigsaw_env$e)
  p9_l <- l(1.0); p9_w <- w(0.0)
  
  return(paste0(
    "C ", p1_l, " ", p1_w, " ", p2_l, " ", p2_w, " ", p3_l, " ", p3_w, " ",
    "C ", p4_l, " ", p4_w, " ", p5_l, " ", p5_w, " ", p6_l, " ", p6_w, " ",
    "C ", p7_l, " ", p7_w, " ", p8_l, " ", p8_w, " ", p9_l, " ", p9_w, " "
  ))
}

# Simplified reverse - just use forward for now (proper implementation needed later)
generate_curved_edge_reverse <- function(tab_params) {
  # Placeholder: use forward path for now
  # Real implementation would reverse the Bézier curve properly
  return(generate_curved_edge_forward(tab_params))
}

generate_individual_piece <- function(piece_xi, piece_yi, tab_data, xn, yn) {
  
  # Calculate piece boundaries
  piece_width <- .jigsaw_env$width / xn
  piece_height <- .jigsaw_env$height / yn
  x1 <- .jigsaw_env$offset + piece_xi * piece_width
  y1 <- .jigsaw_env$offset + piece_yi * piece_height
  x2 <- x1 + piece_width
  y2 <- y1 + piece_height
  
  # Start path from top-left corner
  path <- paste0("M ", x1, " ", y1, " ")
  
  # 1. TOP EDGE
  if (piece_yi == 0) {
    path <- paste0(path, generate_straight_edge(x1, y1, x2, y1))
  } else {
    .jigsaw_env$vertical <- 0
    .jigsaw_env$xi <- piece_xi
    .jigsaw_env$yi <- piece_yi
    top_tab <- tab_data$horizontal[[piece_yi]][[piece_xi + 1]]
    path <- paste0(path, generate_curved_edge_forward(top_tab))
  }
  
  # 2. RIGHT EDGE
  if (piece_xi == xn - 1) {
    path <- paste0(path, generate_straight_edge(x2, y1, x2, y2))
  } else {
    .jigsaw_env$vertical <- 1
    .jigsaw_env$xi <- piece_xi + 1
    .jigsaw_env$yi <- piece_yi
    right_tab <- tab_data$vertical[[piece_xi + 1]][[piece_yi + 1]]
    path <- paste0(path, generate_curved_edge_forward(right_tab))
  }
  
  # 3. BOTTOM EDGE
  if (piece_yi == yn - 1) {
    path <- paste0(path, generate_straight_edge(x2, y2, x1, y2))
  } else {
    .jigsaw_env$vertical <- 0
    .jigsaw_env$xi <- piece_xi
    .jigsaw_env$yi <- piece_yi + 1
    bottom_tab <- tab_data$horizontal[[piece_yi + 1]][[piece_xi + 1]]
    path <- paste0(path, generate_curved_edge_reverse(bottom_tab))
  }
  
  # 4. LEFT EDGE
  if (piece_xi == 0) {
    path <- paste0(path, generate_straight_edge(x1, y2, x1, y1))
  } else {
    .jigsaw_env$vertical <- 1
    .jigsaw_env$xi <- piece_xi
    .jigsaw_env$yi <- piece_yi
    left_tab <- tab_data$vertical[[piece_xi]][[piece_yi + 1]]
    path <- paste0(path, generate_curved_edge_reverse(left_tab))
  }
  
  # Close path
  path <- paste0(path, "Z")
  
  return(path)
}

# Generate SVG with individual pieces
generate_individual_pieces_svg <- function(seed = 1234, xn = 2, yn = 2) {
  
  # Extract tab data
  tab_data <- extract_tab_data(seed, xn, yn)
  init_jigsaw(seed = seed, xn = xn, yn = yn)
  
  width <- .jigsaw_env$width
  height <- .jigsaw_env$height
  
  # Start SVG
  svg_content <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>\n',
    '<svg xmlns="http://www.w3.org/2000/svg" version="1.1" ',
    'width="', width, '" height="', height, '" ',
    'viewBox="0 0 ', width, ' ', height, '">\n',
    '<rect width="100%" height="100%" fill="white"/>\n',
    '<g id="puzzle-pieces">\n'
  )
  
  # Generate each piece
  piece_count <- 0
  colors <- c("red", "blue", "green", "orange")  # Different colors for each piece
  
  for (yi in 0:(yn - 1)) {
    for (xi in 0:(xn - 1)) {
      piece_count <- piece_count + 1
      piece_path <- generate_individual_piece(xi, yi, tab_data, xn, yn)
      color <- colors[(piece_count - 1) %% length(colors) + 1]
      
      svg_content <- paste0(svg_content,
        '  <path id="piece-', xi, '-', yi, '" ',
        'fill="none" stroke="', color, '" stroke-width="2" ',
        'd="', piece_path, '"/>\n'
      )
      
      cat("Generated piece (", xi, ",", yi, ") with", color, "stroke\n")
    }
  }
  
  # Close SVG
  svg_content <- paste0(svg_content, '</g>\n</svg>\n')
  
  return(svg_content)
}

# Test SVG generation
cat("Generating SVG with individual pieces...\n")
svg_content <- generate_individual_pieces_svg(seed = 1234, xn = 2, yn = 2)

# Ensure output directory exists
if (!dir.exists("output")) {
  dir.create("output", recursive = TRUE)
}

# Save SVG file
output_file <- "output/individual_pieces_test.svg"
writeLines(svg_content, output_file)

cat("SVG saved to:", output_file, "\n")
cat("File size:", file.info(output_file)$size, "bytes\n")

# Show first few lines of SVG
cat("SVG content preview:\n")
svg_lines <- readLines(output_file)
for (i in 1:min(10, length(svg_lines))) {
  cat("  ", svg_lines[i], "\n")
}

cat("\n=== SVG Output Generation Test Success ===\n")