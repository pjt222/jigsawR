#!/usr/bin/env Rscript
# Debug border edge structure

source("R/logging.R")
source("R/config_utils.R")
source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")

cat("=== Debug Border Edge Structure ===\n\n")

rings <- 3
seed <- 42
diameter <- 200
tabsize <- 27
jitter <- 5

# Test trunc_only
cat("Testing trunc_only (warp=FALSE, trunc=TRUE):\n\n")

capture.output({
  edge_data <- generate_hex_edge_map(rings, seed, diameter, tabsize, jitter,
                                      do_warp = FALSE, do_trunc = TRUE)
}, type = "output")

cat("Sample border edge structure:\n")
for (key in names(edge_data$piece_edge_map)[1:3]) {
  edge <- edge_data$piece_edge_map[[key]]
  if (edge$type == "border") {
    cat(sprintf("\nEdge %s:\n", key))
    cat(sprintf("  type: %s\n", edge$type))
    cat(sprintf("  forward: %s\n", if (is.null(edge$forward)) "NULL" else edge$forward))
    cat(sprintf("  reverse: %s\n", if (is.null(edge$reverse)) "NULL" else edge$reverse))
    cat(sprintf("  start: (%.2f, %.2f)\n", edge$start[1], edge$start[2]))
    cat(sprintf("  end: (%.2f, %.2f)\n", edge$end[1], edge$end[2]))
    break
  }
}

# Test both (warp+trunc)
cat("\n\nTesting both (warp=TRUE, trunc=TRUE):\n\n")

capture.output({
  edge_data2 <- generate_hex_edge_map(rings, seed, diameter, tabsize, jitter,
                                       do_warp = TRUE, do_trunc = TRUE)
}, type = "output")

cat("Sample border edge structure (with arcs):\n")
for (key in names(edge_data2$piece_edge_map)[1:20]) {
  edge <- edge_data2$piece_edge_map[[key]]
  if (edge$type == "border") {
    cat(sprintf("\nEdge %s:\n", key))
    cat(sprintf("  type: %s\n", edge$type))
    cat(sprintf("  forward: %s\n", if (is.null(edge$forward)) "NULL" else edge$forward))
    cat(sprintf("  reverse: %s\n", if (is.null(edge$reverse)) "NULL" else edge$reverse))
    cat(sprintf("  start: (%.2f, %.2f)\n", edge$start[1], edge$start[2]))
    cat(sprintf("  end: (%.2f, %.2f)\n", edge$end[1], edge$end[2]))

    # Verify arc has radius 100
    if (!is.null(edge$forward) && grepl("^A ", edge$forward)) {
      cat("  ✓ Has arc command (radius 100.00)\n")
    }
    break
  }
}

# Count arc commands
arc_count <- 0
line_count <- 0
for (key in names(edge_data2$piece_edge_map)) {
  edge <- edge_data2$piece_edge_map[[key]]
  if (edge$type == "border") {
    if (!is.null(edge$forward) && grepl("^A ", edge$forward)) {
      arc_count <- arc_count + 1
    } else if (!is.null(edge$forward) && grepl("^L ", edge$forward)) {
      line_count <- line_count + 1
    }
  }
}

cat(sprintf("\n\nSummary for 'both' mode:\n"))
cat(sprintf("  Border edges with arcs: %d\n", arc_count))
cat(sprintf("  Border edges with lines: %d\n", line_count))

cat("\n=== Done ===\n")
