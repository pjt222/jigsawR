# Debug piece size variation across rings
# When warp+trunc are both enabled, pieces should be similar sizes

cat("=" , rep("=", 70), "\n", sep = "")
cat("Debug: Piece Size Analysis for Hexagonal Puzzles\n")
cat("=" , rep("=", 70), "\n\n", sep = "")

# Source required functions
source("R/logging.R")
source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")

# Test parameters
rings <- 3
diameter <- 240
seed <- 1234

cat("Test parameters:\n")
cat(sprintf("  rings: %d\n", rings))
cat(sprintf("  diameter: %d mm\n", diameter))
cat(sprintf("  seed: %d\n", seed))

piece_radius <- diameter / (rings * 4)
circle_radius <- diameter / 2
cat(sprintf("  piece_radius: %.2f mm\n", piece_radius))
cat(sprintf("  circle_radius: %.2f mm\n", circle_radius))
cat("\n")

# Function to calculate piece area from path
calculate_piece_area <- function(path) {
  # Extract coordinates
  numbers <- as.numeric(unlist(regmatches(path, gregexpr("-?[0-9]+\\.?[0-9]*", path))))
  numbers <- numbers[!is.na(numbers)]

  if (length(numbers) < 4) return(NA)

  x_coords <- numbers[seq(1, length(numbers), by = 2)]
  y_coords <- numbers[seq(2, length(numbers), by = 2)]

  # Calculate bounding box area (approximate)
  width <- max(x_coords) - min(x_coords)
  height <- max(y_coords) - min(y_coords)

  return(list(
    width = width,
    height = height,
    bbox_area = width * height,
    min_x = min(x_coords),
    max_x = max(x_coords),
    min_y = min(y_coords),
    max_y = max(y_coords)
  ))
}

# Function to calculate distance from origin to piece center
calculate_center_distance <- function(piece) {
  sqrt(piece$center_x^2 + piece$center_y^2)
}

# Test all four modes
test_cases <- list(
  list(do_warp = FALSE, do_trunc = FALSE, name = "neither"),
  list(do_warp = FALSE, do_trunc = TRUE,  name = "trunc_only"),
  list(do_warp = TRUE,  do_trunc = FALSE, name = "warp_only"),
  list(do_warp = TRUE,  do_trunc = TRUE,  name = "both (warp+trunc)")
)

for (test in test_cases) {
  cat("-", rep("-", 70), "\n", sep = "")
  cat(sprintf("Mode: %s\n", test$name))
  cat("-", rep("-", 70), "\n", sep = "")

  # Generate pieces
  pieces <- generate_hex_pieces_with_edge_map(
    rings = rings,
    seed = seed,
    diameter = diameter,
    tabsize = 20,
    jitter = 4,
    separated = FALSE,
    separation_factor = 1.0,
    do_warp = test$do_warp,
    do_trunc = test$do_trunc
  )

  # Analyze by ring
  ring_stats <- list()

  for (piece in pieces) {
    ring <- piece$ring
    area_info <- calculate_piece_area(piece$path)
    center_dist <- calculate_center_distance(piece)

    if (is.null(ring_stats[[as.character(ring)]])) {
      ring_stats[[as.character(ring)]] <- list(
        areas = c(),
        widths = c(),
        heights = c(),
        center_dists = c(),
        piece_ids = c()
      )
    }

    ring_stats[[as.character(ring)]]$areas <- c(ring_stats[[as.character(ring)]]$areas, area_info$bbox_area)
    ring_stats[[as.character(ring)]]$widths <- c(ring_stats[[as.character(ring)]]$widths, area_info$width)
    ring_stats[[as.character(ring)]]$heights <- c(ring_stats[[as.character(ring)]]$heights, area_info$height)
    ring_stats[[as.character(ring)]]$center_dists <- c(ring_stats[[as.character(ring)]]$center_dists, center_dist)
    ring_stats[[as.character(ring)]]$piece_ids <- c(ring_stats[[as.character(ring)]]$piece_ids, piece$id)
  }

  cat("\nPiece sizes by ring:\n")
  cat(sprintf("%-8s %-10s %-12s %-12s %-15s %-10s\n",
              "Ring", "Count", "Avg Width", "Avg Height", "Avg BBox Area", "Center Dist"))
  cat(sprintf("%-8s %-10s %-12s %-12s %-15s %-10s\n",
              "----", "-----", "---------", "----------", "-------------", "-----------"))

  for (ring_name in sort(names(ring_stats))) {
    stats <- ring_stats[[ring_name]]
    cat(sprintf("%-8s %-10d %-12.2f %-12.2f %-15.2f %-10.2f\n",
                ring_name,
                length(stats$areas),
                mean(stats$widths),
                mean(stats$heights),
                mean(stats$areas),
                mean(stats$center_dists)))
  }

  # Calculate size ratio between outer and center rings
  if ("0" %in% names(ring_stats) && "2" %in% names(ring_stats)) {
    center_area <- mean(ring_stats[["0"]]$areas)
    outer_area <- mean(ring_stats[["2"]]$areas)
    ratio <- outer_area / center_area
    cat(sprintf("\nOuter/Center area ratio: %.2f (ideal: ~1.0)\n", ratio))

    if (ratio > 1.5) {
      cat("⚠️  WARNING: Outer pieces are significantly larger than center pieces!\n")
    } else if (ratio < 0.7) {
      cat("⚠️  WARNING: Outer pieces are significantly smaller than center pieces!\n")
    } else {
      cat("✓ Piece sizes are reasonably consistent\n")
    }
  }

  cat("\n")
}

cat("=" , rep("=", 70), "\n", sep = "")
cat("Analysis complete.\n")
