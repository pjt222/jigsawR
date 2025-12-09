# Quick test for topology cache optimization
devtools::load_all(quiet = TRUE)

cat("Testing topology cache optimization...\n")

# Test 1: Basic fusion
cat("\n=== Test 1: Basic 3-ring fusion ===\n")
start <- Sys.time()
result <- generate_puzzle(
  type = "hexagonal",
  grid = c(3),
  size = c(200),
  seed = 42,
  fusion_groups = list(c(1, 2)),
  save_files = FALSE
)
elapsed <- as.numeric(Sys.time() - start, units = "secs")
cat(sprintf("Generated in %.2f seconds\n", elapsed))
cat(sprintf("Pieces: %d\n", length(result$pieces)))

# Test 2: Medium puzzle with fusion
cat("\n=== Test 2: 5-ring with 6 fusion groups ===\n")
start <- Sys.time()
result2 <- generate_puzzle(
  type = "hexagonal",
  grid = c(5),
  size = c(200),
  seed = 42,
  fusion_groups = list(c(1, 2), c(6, 11), c(16, 21), c(26, 31), c(36, 41), c(46, 51)),
  save_files = FALSE
)
elapsed <- as.numeric(Sys.time() - start, units = "secs")
cat(sprintf("Generated in %.2f seconds\n", elapsed))
cat(sprintf("Pieces: %d\n", length(result2$pieces)))

# Test 3: Verify correctness - check that piece 1 has piece 2 as fused neighbor
p1 <- result$pieces[[1]]
fused_neighbor_ids <- unlist(p1$fused_neighbor_ids)
if (2 %in% fused_neighbor_ids) {
  cat("\nCorrectness check: PASSED (piece 1 has piece 2 as fused neighbor)\n")
} else {
  cat("\nCorrectness check: FAILED (piece 1 does NOT have piece 2 as fused neighbor)\n")
  cat("fused_neighbor_ids:", paste(fused_neighbor_ids, collapse = ", "), "\n")
}

cat("\nAll quick tests completed.\n")
