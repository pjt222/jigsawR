# Benchmark: Old vs New Adjacency/Fusion Computation
# Tests the performance improvement from cached adjacency matrix

devtools::load_all(quiet = TRUE)

cat("=== Performance Benchmark: Adjacency Cache ===\n\n")

# Test 1: 5 rings with fusion
cat("Test 1: 5 rings (61 pieces) with fusion group (1,2,3,4,5,6,7)\n")
fusion_groups <- list(c(1,2,3,4,5,6,7))

# Old method
start <- Sys.time()
old_result <- compute_hex_fused_edges(fusion_groups, list(parameters = list(rings = 5)))
old_time <- as.numeric(difftime(Sys.time(), start, units = "secs"))

# Clear cache for fair comparison
clear_hex_adjacency_cache()

# New method
start <- Sys.time()
new_result <- compute_hex_fused_edges_fast(fusion_groups, 5)
new_time <- as.numeric(difftime(Sys.time(), start, units = "secs"))

cat(sprintf("  Old method: %.4fs\n", old_time))
cat(sprintf("  New method: %.4fs\n", new_time))
if (new_time > 0) {
  cat(sprintf("  Speedup: %.1fx\n\n", old_time / new_time))
}

# Test 2: 7 rings with larger fusion
cat("Test 2: 7 rings (127 pieces) with fusion group (1:13)\n")
fusion_groups <- list(c(1:13))

start <- Sys.time()
old_result <- compute_hex_fused_edges(fusion_groups, list(parameters = list(rings = 7)))
old_time <- as.numeric(difftime(Sys.time(), start, units = "secs"))

clear_hex_adjacency_cache()

start <- Sys.time()
new_result <- compute_hex_fused_edges_fast(fusion_groups, 7)
new_time <- as.numeric(difftime(Sys.time(), start, units = "secs"))

cat(sprintf("  Old method: %.4fs\n", old_time))
cat(sprintf("  New method: %.4fs\n", new_time))
if (new_time > 0) {
  cat(sprintf("  Speedup: %.1fx\n\n", old_time / new_time))
}

# Test 3: Full puzzle generation with fusion
cat("Test 3: Full puzzle generation - 5 rings with fusion\n")
start <- Sys.time()
result <- generate_puzzle(
  type = "hexagonal",
  grid = c(5),
  size = c(200),
  seed = 42,
  fusion_groups = "(1,2,3,4,5,6,7)",
  save_files = FALSE
)
total_time <- as.numeric(difftime(Sys.time(), start, units = "secs"))
cat(sprintf("  Total time: %.4fs\n\n", total_time))

# Test 4: Full puzzle generation 7 rings with fusion
cat("Test 4: Full puzzle generation - 7 rings with fusion\n")
clear_hex_adjacency_cache()
start <- Sys.time()
result <- generate_puzzle(
  type = "hexagonal",
  grid = c(7),
  size = c(200),
  seed = 42,
  fusion_groups = "(1,2,3,4,5,6,7,8,9,10,11,12,13)",
  save_files = FALSE
)
total_time <- as.numeric(difftime(Sys.time(), start, units = "secs"))
cat(sprintf("  Total time: %.4fs\n", total_time))

cat("\nBenchmark complete.\n")
