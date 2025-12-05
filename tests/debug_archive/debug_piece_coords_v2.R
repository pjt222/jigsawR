# Debug piece coordinates with correct arc parsing v2

cat("=" , rep("=", 70), "\n", sep = "")
cat("Debug: Correct Piece Coordinate Parsing v2\n")
cat("=" , rep("=", 70), "\n\n", sep = "")

source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")

# Function to extract actual coordinates from path (handling arc commands)
extract_coordinates <- function(path) {
  all_x <- c()
  all_y <- c()

  # Split path into tokens
  # First, add spaces around command letters
  clean_path <- gsub("([MLCAZ])", " \\1 ", path)
  tokens <- unlist(strsplit(clean_path, "\\s+"))
  tokens <- tokens[nchar(tokens) > 0]

  i <- 1
  while (i <= length(tokens)) {
    token <- tokens[i]

    if (token == "M" || token == "L") {
      # M x y or L x y
      if (i + 2 <= length(tokens)) {
        x <- as.numeric(tokens[i + 1])
        y <- as.numeric(tokens[i + 2])
        if (!is.na(x) && !is.na(y)) {
          all_x <- c(all_x, x)
          all_y <- c(all_y, y)
        }
        i <- i + 3
      } else {
        i <- i + 1
      }
    } else if (token == "C") {
      # C x1 y1 x2 y2 x3 y3
      if (i + 6 <= length(tokens)) {
        x1 <- as.numeric(tokens[i + 1])
        y1 <- as.numeric(tokens[i + 2])
        x2 <- as.numeric(tokens[i + 3])
        y2 <- as.numeric(tokens[i + 4])
        x3 <- as.numeric(tokens[i + 5])
        y3 <- as.numeric(tokens[i + 6])
        all_x <- c(all_x, x1, x2, x3)
        all_y <- c(all_y, y1, y2, y3)
        i <- i + 7
      } else {
        i <- i + 1
      }
    } else if (token == "A") {
      # A rx ry rotation large-arc sweep x y
      # We need to skip 5 parameters and get the last 2
      if (i + 7 <= length(tokens)) {
        x <- as.numeric(tokens[i + 6])
        y <- as.numeric(tokens[i + 7])
        if (!is.na(x) && !is.na(y)) {
          all_x <- c(all_x, x)
          all_y <- c(all_y, y)
        }
        i <- i + 8
      } else {
        i <- i + 1
      }
    } else if (token == "Z") {
      i <- i + 1
    } else {
      # Skip unknown tokens
      i <- i + 1
    }
  }

  return(list(x = all_x, y = all_y))
}

# Test the parser on piece 8's path
test_path <- "M -45.36 39.28 C -48.26 44.26 -49.17 54.92 -54.59 45.07 C -60.00 35.21 -70.79 51.84 -59.98 53.38 C -49.17 54.92 -57.76 58.67 -61.02 63.41 A 88.00 88.00 0 0 1 -70.66 52.45 A 88.00 88.00 0 0 1 -80.75 34.97 A 88.00 88.00 0 0 1 -85.42 21.14 C -79.65 21.28 -71.79 14.55 -77.30 27.74 C -82.81 40.93 -57.08 39.58 -64.43 27.06 C -71.79 14.55 -62.42 20.40 -56.69 19.64 C -54.42 23.56 -54.97 31.41 -49.57 23.12 C -44.17 14.83 -35.34 30.11 -45.16 30.76 C -54.97 31.41 -48.00 35.57 -45.36 39.28 Z"

coords <- extract_coordinates(test_path)
cat("Test parsing piece 8 path:\n")
cat(sprintf("  X values: %s\n", paste(round(coords$x, 2), collapse=", ")))
cat(sprintf("  Y values: %s\n", paste(round(coords$y, 2), collapse=", ")))
cat(sprintf("  X range: [%.2f, %.2f] (span: %.2f)\n", min(coords$x), max(coords$x), max(coords$x) - min(coords$x)))
cat(sprintf("  Y range: [%.2f, %.2f] (span: %.2f)\n", min(coords$y), max(coords$y), max(coords$y) - min(coords$y)))
cat("\n")

# Now run on actual generated pieces
rings <- 3
diameter <- 240
seed <- 1234

pieces <- generate_hex_pieces_with_edge_map(
  rings = rings,
  seed = seed,
  diameter = diameter,
  tabsize = 20,
  jitter = 4,
  separated = FALSE,
  separation_factor = 1.0,
  do_warp = TRUE,
  do_trunc = TRUE
)

cat("\nPiece analysis with CORRECT coordinate parsing:\n\n")

ring_widths <- list("0" = c(), "1" = c(), "2" = c())
ring_heights <- list("0" = c(), "1" = c(), "2" = c())

for (piece in pieces) {
  coords <- extract_coordinates(piece$path)

  if (length(coords$x) > 0 && length(coords$y) > 0) {
    width <- max(coords$x) - min(coords$x)
    height <- max(coords$y) - min(coords$y)
  } else {
    width <- NA
    height <- NA
  }

  ring <- as.character(piece$ring)
  ring_widths[[ring]] <- c(ring_widths[[ring]], width)
  ring_heights[[ring]] <- c(ring_heights[[ring]], height)

  if (piece$id <= 3 || piece$ring == 2) {
    cat(sprintf("Piece %2d (ring %d): width=%.2f, height=%.2f, center=(%.2f, %.2f)\n",
                piece$id, piece$ring, width, height, piece$center_x, piece$center_y))
  }
}

cat("\n\nSummary by ring:\n")
cat(sprintf("%-8s %-10s %-12s %-12s\n", "Ring", "Count", "Avg Width", "Avg Height"))
cat(sprintf("%-8s %-10s %-12s %-12s\n", "----", "-----", "---------", "----------"))

for (ring in c("0", "1", "2")) {
  cat(sprintf("%-8s %-10d %-12.2f %-12.2f\n",
              ring,
              length(ring_widths[[ring]]),
              mean(ring_widths[[ring]], na.rm = TRUE),
              mean(ring_heights[[ring]], na.rm = TRUE)))
}

# Calculate ratio
center_area <- mean(ring_widths[["0"]], na.rm = TRUE) * mean(ring_heights[["0"]], na.rm = TRUE)
outer_area <- mean(ring_widths[["2"]], na.rm = TRUE) * mean(ring_heights[["2"]], na.rm = TRUE)
cat(sprintf("\nOuter/Center area ratio: %.2f\n", outer_area / center_area))
