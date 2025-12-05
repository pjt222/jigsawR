# Debug piece coordinates with correct arc parsing

cat("=" , rep("=", 70), "\n", sep = "")
cat("Debug: Correct Piece Coordinate Parsing\n")
cat("=" , rep("=", 70), "\n\n", sep = "")

source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")

# Function to extract actual coordinates from path (handling arc commands)
extract_coordinates <- function(path) {
  all_x <- c()
  all_y <- c()

  # Split by commands
  tokens <- unlist(strsplit(path, "(?=[MLCAZ])", perl = TRUE))
  tokens <- tokens[nchar(trimws(tokens)) > 0]

  for (token in tokens) {
    token <- trimws(token)
    if (nchar(token) == 0) next

    cmd <- substr(token, 1, 1)
    rest <- substr(token, 2, nchar(token))

    numbers <- as.numeric(unlist(strsplit(rest, "\\s+")))
    numbers <- numbers[!is.na(numbers)]

    if (cmd == "M" || cmd == "L") {
      # M x y or L x y
      if (length(numbers) >= 2) {
        all_x <- c(all_x, numbers[1])
        all_y <- c(all_y, numbers[2])
      }
    } else if (cmd == "C") {
      # C x1 y1 x2 y2 x3 y3
      if (length(numbers) >= 6) {
        all_x <- c(all_x, numbers[1], numbers[3], numbers[5])
        all_y <- c(all_y, numbers[2], numbers[4], numbers[6])
      }
    } else if (cmd == "A") {
      # A rx ry rotation large-arc sweep x y
      # Only the last two are coordinates!
      if (length(numbers) >= 7) {
        all_x <- c(all_x, numbers[6])
        all_y <- c(all_y, numbers[7])
      }
    }
    # Z has no coordinates
  }

  return(list(x = all_x, y = all_y))
}

rings <- 3
diameter <- 240
seed <- 1234

# Generate pieces with warp+trunc
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

# Analyze all pieces by ring
cat("\nPiece analysis with CORRECT coordinate parsing:\n\n")

ring_widths <- list("0" = c(), "1" = c(), "2" = c())
ring_heights <- list("0" = c(), "1" = c(), "2" = c())

for (piece in pieces) {
  coords <- extract_coordinates(piece$path)

  width <- max(coords$x) - min(coords$x)
  height <- max(coords$y) - min(coords$y)

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
              mean(ring_widths[[ring]]),
              mean(ring_heights[[ring]])))
}

# Calculate ratio
center_area <- mean(ring_widths[["0"]]) * mean(ring_heights[["0"]])
outer_area <- mean(ring_widths[["2"]]) * mean(ring_heights[["2"]])
cat(sprintf("\nOuter/Center area ratio: %.2f\n", outer_area / center_area))
