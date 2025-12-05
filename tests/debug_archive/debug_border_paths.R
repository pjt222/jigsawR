#!/usr/bin/env Rscript
# Debug border edge paths

source("R/logging.R")
source("R/config_utils.R")
source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")

cat("=== Debug Border Edge Paths ===\n\n")

rings <- 3
seed <- 42
diameter <- 200
tabsize <- 27
jitter <- 5

cat("Testing trunc_only (warp=FALSE, trunc=TRUE):\n\n")

capture.output({
  edge_data <- generate_hex_edge_map(rings, seed, diameter, tabsize, jitter,
                                      do_warp = FALSE, do_trunc = TRUE)
}, type = "output")

cat("Checking all border edges for path content:\n")
for (key in names(edge_data$piece_edge_map)) {
  edge <- edge_data$piece_edge_map[[key]]
  if (edge$type == "border") {
    path_val <- edge$path
    path_desc <- if (is.null(path_val)) {
      "NULL"
    } else if (length(path_val) == 0) {
      "EMPTY (length 0)"
    } else {
      paste0("length ", nchar(path_val))
    }
    cat(sprintf("  Edge %s: path = %s\n", key, path_desc))

    # Show first 100 chars if path exists
    if (!is.null(path_val) && length(path_val) > 0 && nchar(path_val) > 0) {
      cat(sprintf("    First 100 chars: %s\n", substr(path_val, 1, 100)))
    }
  }
}

cat("\n=== Done ===\n")
