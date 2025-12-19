# Profile cache building operations
suppressMessages(devtools::load_all(quiet = TRUE))

cat("=== Cache Building Profiler ===\n\n")

rings <- 5
diameter <- 200
tabsize <- 15
jitter <- 4
num_pieces <- 3 * rings * (rings - 1) + 1

cat("Testing with", rings, "rings (", num_pieces, "pieces)\n\n")

# 1. Time the overall edge mapping generation
cat("1. generate_hex_edge_map() [includes neighbor discovery]:\n")
t1 <- system.time({
  for (i in 1:3) {
    edge_data <- generate_hex_edge_map(
      seed = 42,
      rings = rings,
      diameter = diameter,
      tabsize = tabsize,
      jitter = jitter,
      do_warp = TRUE,
      do_trunc = TRUE
    )
  }
})
cat("   3 calls:", round(t1[["elapsed"]] * 1000), "ms\n")
cat("   Per call:", round(t1[["elapsed"]] / 3 * 1000), "ms\n\n")

# 2. Time adjacency matrix building (the fast version)
cat("2. get_hex_adjacency_matrix() [O(1) lookup infrastructure]:\n")
clear_hex_neighbor_cache()  # Clear any existing cache
t2 <- system.time({
  for (i in 1:100) {
    adj <- get_hex_adjacency_matrix(rings)
  }
})
cat("   100 calls:", round(t2[["elapsed"]] * 1000), "ms\n")
cat("   Per call:", round(t2[["elapsed"]] / 100 * 1000, 2), "ms\n\n")

# 3. Time get_hex_neighbors_for_fusion (uses adjacency matrix)
cat("3. get_hex_neighbors_for_fusion() [uses cached adjacency]:\n")
t3 <- system.time({
  for (rep in 1:100) {
    for (piece_id in 1:num_pieces) {
      nb <- get_hex_neighbors_for_fusion(piece_id, rings)
    }
  }
})
cat("   ", num_pieces, "x100 calls:", round(t3[["elapsed"]] * 1000), "ms\n")
cat("   Per call:", round(t3[["elapsed"]] / (num_pieces * 100) * 1000, 3), "ms\n\n")

# Summary
cat("=== Analysis ===\n")
cat("Edge map generation:", round(t1[["elapsed"]] / 3 * 1000), "ms\n")
cat("Adjacency matrix build:", round(t2[["elapsed"]] / 100 * 1000, 2), "ms\n")
cat("Neighbor lookup (cached):", round(t3[["elapsed"]] / (num_pieces * 100) * 1000, 3), "ms\n")
cat("\nThe edge map uses brute-force neighbor search.\n")
cat("The adjacency cache provides O(1) lookups but isn't used in edge generation.\n")
