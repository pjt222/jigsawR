# Debug circle path order issue

cat("=== Debug Circle Path Order ===\n\n")

source("R/logging.R")
source("R/concentric_geometry.R")
source("R/hexagonal_bezier_generation.R")
source("R/concentric_edge_generation.R")

rings <- 3
diameter <- 200
seed <- 42
tabsize <- diameter * 0.10

edge_data <- generate_concentric_edge_map(
  rings = rings,
  seed = seed,
  diameter = diameter,
  tabsize = tabsize,
  jitter = 4,
  center_shape = "circle"
)

piece1_edges <- edge_data$piece_edges[[1]]
radial_edges <- piece1_edges[sapply(piece1_edges, function(e) e$type == "radial")]

cat("Circle center should go around counter-clockwise:\n")
cat("  0° -> 60° -> 120° -> 180° -> 240° -> 300° -> 0°\n\n")

cat("Current edge traversal:\n")
for (i in seq_along(radial_edges)) {
  edge_info <- radial_edges[[i]]
  edge <- edge_data$edge_map[[edge_info$edge_ref]]

  if (edge_info$is_forward) {
    start_pt <- edge$start
    end_pt <- edge$end
    direction <- "forward (start->end)"
  } else {
    start_pt <- edge$end
    end_pt <- edge$start
    direction <- "reverse (end->start)"
  }

  start_angle <- atan2(start_pt[2], start_pt[1]) * 180 / pi
  end_angle <- atan2(end_pt[2], end_pt[1]) * 180 / pi

  cat(sprintf("Edge %d (%s): %.1f° -> %.1f° [%s]\n",
              i, edge_info$edge_ref, start_angle, end_angle, direction))
}

cat("\n\nThe problem: Using 'reverse' goes from v2 to v1, but the edges\n")
cat("are stored with v1=inner-start (lower angle) and v2=inner-end (higher angle)\n")
cat("So reverse goes from HIGH angle to LOW angle.\n\n")

cat("For piece 2 (neighbor): V1 is at 0°, V2 is at 60°\n")
cat("  Edge forward: 0° -> 60° (correct for ring 1 piece)\n")
cat("  Edge reverse: 60° -> 0° (correct for circle going clockwise)\n\n")

cat("But circle should go COUNTER-CLOCKWISE (increasing angles)!\n")
cat("So circle should use FORWARD, not REVERSE.\n\n")

cat("Let's verify the expected traversal order:\n")
cat("  Start at 0° (V1 of piece 2)\n")
cat("  Go to 60° (V2 of piece 2 = V1 of piece 3)\n")
cat("  Go to 120° (V2 of piece 3 = V1 of piece 4)\n")
cat("  etc.\n\n")

cat("This means circle should use is_forward = TRUE, not FALSE!\n")
