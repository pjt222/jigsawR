# Compare piece sizes with complete mode

cat("=" , rep("=", 70), "\n", sep = "")
cat("Debug: Compare with Complete Mode\n")
cat("=" , rep("=", 70), "\n\n", sep = "")

source("R/hexagonal_puzzle.R")
source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")

# Function to extract actual coordinates from path (handling arc commands)
extract_coordinates <- function(path) {
  all_x <- c()
  all_y <- c()

  clean_path <- gsub("([MLCAZ])", " \\1 ", path)
  tokens <- unlist(strsplit(clean_path, "\\s+"))
  tokens <- tokens[nchar(tokens) > 0]

  i <- 1
  while (i <= length(tokens)) {
    token <- tokens[i]

    if (token == "M" || token == "L") {
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
      if (i + 6 <= length(tokens)) {
        all_x <- c(all_x, as.numeric(tokens[i + 1]), as.numeric(tokens[i + 3]), as.numeric(tokens[i + 5]))
        all_y <- c(all_y, as.numeric(tokens[i + 2]), as.numeric(tokens[i + 4]), as.numeric(tokens[i + 6]))
        i <- i + 7
      } else {
        i <- i + 1
      }
    } else if (token == "A") {
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
      i <- i + 1
    }
  }

  return(list(x = all_x, y = all_y))
}

rings <- 3
diameter <- 240
seed <- 1234
tabsize <- 20
jitter <- 4

# Generate using complete mode (deprecated but canonical)
cat("Generating complete mode puzzle (deprecated)...\n")
complete_result <- generate_hex_jigsaw_svg(
  seed = seed,
  rings = rings,
  diameter = diameter,
  tabsize = tabsize,
  jitter = jitter,
  do_warp = TRUE,
  do_trunc = TRUE
)

# Parse piece paths from complete mode
# The complete mode returns SVG with all pieces in a single path
# Let's look at what it generates
cat("\nComplete mode SVG snippet:\n")
cat(substr(complete_result$svg, 1, 2000), "...\n\n")

# Now compare with separated mode
cat("Generating separated mode puzzle...\n")
pieces <- generate_hex_pieces_with_edge_map(
  rings = rings,
  seed = seed,
  diameter = diameter,
  tabsize = tabsize,
  jitter = jitter,
  separated = FALSE,
  separation_factor = 1.0,
  do_warp = TRUE,
  do_trunc = TRUE
)

# Analyze pieces
cat("\nSeparated mode piece analysis:\n")
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
}

cat("\nSummary by ring:\n")
cat(sprintf("%-8s %-10s %-12s %-12s\n", "Ring", "Count", "Avg Width", "Avg Height"))
cat(sprintf("%-8s %-10s %-12s %-12s\n", "----", "-----", "---------", "----------"))

for (ring in c("0", "1", "2")) {
  cat(sprintf("%-8s %-10d %-12.2f %-12.2f\n",
              ring,
              length(ring_widths[[ring]]),
              mean(ring_widths[[ring]], na.rm = TRUE),
              mean(ring_heights[[ring]], na.rm = TRUE)))
}

center_area <- mean(ring_widths[["0"]], na.rm = TRUE) * mean(ring_heights[["0"]], na.rm = TRUE)
outer_area <- mean(ring_widths[["2"]], na.rm = TRUE) * mean(ring_heights[["2"]], na.rm = TRUE)
cat(sprintf("\nOuter/Center area ratio: %.2f\n", outer_area / center_area))
