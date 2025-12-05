#!/usr/bin/env Rscript
# Detailed debug of border edge structure

source("R/logging.R")
source("R/config_utils.R")
source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")

cat("=== Detailed Border Edge Analysis ===\n\n")

rings <- 3
seed <- 42
diameter <- 200
tabsize <- 27
jitter <- 5

# Test both (warp+trunc) - the case that should have circular arcs
cat("Testing BOTH mode (warp=TRUE, trunc=TRUE):\n\n")

capture.output({
  edge_data <- generate_hex_edge_map(rings, seed, diameter, tabsize, jitter,
                                      do_warp = TRUE, do_trunc = TRUE)
}, type = "output")

# Print ALL border edges
cat("All border edges:\n")
border_keys <- c()
for (key in names(edge_data$piece_edge_map)) {
  edge <- edge_data$piece_edge_map[[key]]
  if (edge$type == "border") {
    border_keys <- c(border_keys, key)
  }
}
cat(sprintf("  Found %d border edges\n\n", length(border_keys)))

# Show first 5 border edges in detail
cat("First 5 border edges:\n")
for (i in 1:min(5, length(border_keys))) {
  key <- border_keys[i]
  edge <- edge_data$piece_edge_map[[key]]

  cat(sprintf("\n[%d] Edge %s:\n", i, key))
  cat(sprintf("  forward: %s\n", edge$forward))
  cat(sprintf("  reverse: %s\n", edge$reverse))
  cat(sprintf("  start: (%.4f, %.4f)\n", edge$start[1], edge$start[2]))
  cat(sprintf("  end: (%.4f, %.4f)\n", edge$end[1], edge$end[2]))

  # Calculate distances from origin
  d1 <- sqrt(edge$start[1]^2 + edge$start[2]^2)
  d2 <- sqrt(edge$end[1]^2 + edge$end[2]^2)
  cat(sprintf("  distances: start=%.4f, end=%.4f\n", d1, d2))
}

cat("\n\n=== Testing TRUNC_ONLY mode (warp=FALSE, trunc=TRUE) ===\n\n")

capture.output({
  edge_data2 <- generate_hex_edge_map(rings, seed, diameter, tabsize, jitter,
                                       do_warp = FALSE, do_trunc = TRUE)
}, type = "output")

# Show first 3 border edges
border_keys2 <- c()
for (key in names(edge_data2$piece_edge_map)) {
  edge <- edge_data2$piece_edge_map[[key]]
  if (edge$type == "border") {
    border_keys2 <- c(border_keys2, key)
  }
}

cat("First 3 border edges (should have L commands, not A):\n")
for (i in 1:min(3, length(border_keys2))) {
  key <- border_keys2[i]
  edge <- edge_data2$piece_edge_map[[key]]

  cat(sprintf("\n[%d] Edge %s:\n", i, key))
  cat(sprintf("  forward: %s\n", edge$forward))
  cat(sprintf("  reverse: %s\n", edge$reverse))
}

cat("\n=== Done ===\n")
