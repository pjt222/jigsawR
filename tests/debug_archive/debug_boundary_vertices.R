#!/usr/bin/env Rscript
# Debug boundary vertex positions after warp

source("R/logging.R")
source("R/config_utils.R")
source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")

cat("=== Debug Boundary Vertices After Warp ===\n\n")

rings <- 3
seed <- 42
diameter <- 200
tabsize <- 27
jitter <- 5

cat("Generating edge map with do_warp=TRUE, do_trunc=TRUE...\n\n")

# Suppress normal output
capture.output({
  edge_data <- generate_hex_edge_map(rings, seed, diameter, tabsize, jitter,
                                      do_warp = TRUE, do_trunc = TRUE)
}, type = "output")

cat("Looking at border edges and their vertex distances...\n\n")

# Find border edges
border_edges <- list()
for (key in names(edge_data$piece_edge_map)) {
  edge <- edge_data$piece_edge_map[[key]]
  if (edge$type == "border") {
    v1 <- edge$start
    v2 <- edge$end
    d1 <- sqrt(v1[1]^2 + v1[2]^2)
    d2 <- sqrt(v2[1]^2 + v2[2]^2)

    cat(sprintf("Border edge %s:\n", key))
    cat(sprintf("  v1 = (%.2f, %.2f), distance = %.4f\n", v1[1], v1[2], d1))
    cat(sprintf("  v2 = (%.2f, %.2f), distance = %.4f\n", v2[1], v2[2], d2))
    cat(sprintf("  Average distance = %.4f\n", (d1 + d2) / 2))

    border_edges[[key]] <- list(v1 = v1, v2 = v2, d1 = d1, d2 = d2)
  }
}

# Collect all unique boundary vertex distances
all_dists <- c()
for (key in names(border_edges)) {
  all_dists <- c(all_dists, border_edges[[key]]$d1, border_edges[[key]]$d2)
}

cat("\n=== Summary ===\n")
cat(sprintf("Min boundary distance: %.4f\n", min(all_dists)))
cat(sprintf("Max boundary distance: %.4f\n", max(all_dists)))
cat(sprintf("Range: %.4f\n", max(all_dists) - min(all_dists)))

cat("\n=== What radius should be used for arcs? ===\n")
# The original complete mode uses radius = diameter / 2 = 100
# But that's for the TRANSLATED coordinates (centered at 120, 120)
# In our centered-at-origin coordinates, the radius should be...
cat(sprintf("Original diameter: %d\n", diameter))
cat(sprintf("Expected radius: %.2f\n", diameter / 2))

# Let's also check the original complete mode's radius
cat("\n=== Checking original complete mode ===\n")
source("R/hexagonal_puzzle.R")
init_hex_jigsaw(seed = 42, tabsize = 27, jitter = 5, diameter = 200, rings = 3,
                do_warp = TRUE, do_trunc = TRUE)
cat(sprintf("Radius in .hex_jigsaw_env: %.2f\n", .hex_jigsaw_env$radius))
cat(sprintf("Offset in .hex_jigsaw_env: %.2f\n", .hex_jigsaw_env$offset))

cat("\n")
