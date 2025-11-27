#!/usr/bin/env Rscript
# Final test of hexagonal lattice coordinate system

source("R/hexagonal_topology.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")

cat("Generating 3-ring hexagonal puzzle with lattice coordinates...\n")

tryCatch({
  result <- generate_hex_edge_map(seed = 42, rings = 3, diameter = 240)

  cat("\n✓✓✓ SUCCESS! Hex Hexagonal lattice coordinate system is working!\n\n")
  cat("Results:\n")
  cat("  - Number of unique edges:", length(result$edge_map), "\n")
  cat("  - Number of pieces:", length(result$piece_paths), "\n")

  cat("\n  - Sample piece 9 path (first 150 chars):\n")
  cat("   ", substr(result$piece_paths[[9]]$path, 1, 150), "...\n")

  cat("\n  - Piece 9 neighbors:\n")
  for (side in 0:5) {
    key <- sprintf("9-%d", side)
    edge_info <- result$piece_paths[[9]]$edges[[key]]
    if (!is.null(edge_info) && edge_info$type != "border") {
      neighbor <- if (edge_info$piece1 == 9) edge_info$piece2 else edge_info$piece1
      cat(sprintf("     Side %d: Piece %d\n", side, neighbor))
    } else {
      cat(sprintf("     Side %d: Border\n", side))
    }
  }

}, error = function(e) {
  cat("\n✗ ERROR:\n")
  cat(conditionMessage(e), "\n")
  traceback()
})
