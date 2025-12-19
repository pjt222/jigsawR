# Benchmark geometry processing optimization
suppressMessages(devtools::load_all(quiet = TRUE))

cat("=== Geometry Processing Optimization Benchmark ===\n\n")

# Run 5 iterations
cat("5-ring hexagonal puzzle (61 pieces):\n")
times <- numeric(5)
for (i in 1:5) {
  t <- system.time({
    result <- generate_puzzle(
      type = "hexagonal",
      grid = c(5),
      size = c(200),
      seed = 42,
      save_files = FALSE
    )
  })
  times[i] <- t[["elapsed"]]
}

cat("\nResults:\n")
cat("  Times:", paste(round(times, 2), "s", collapse = ", "), "\n")
cat("  Average:", round(mean(times), 2), "s\n")
cat("  Min:", round(min(times), 2), "s\n")
cat("  Max:", round(max(times), 2), "s\n")

# Compare with baseline (before optimization: ~2.36s average)
baseline <- 2.36
improvement <- (baseline - mean(times)) / baseline * 100
cat("\n  vs baseline (2.36s):", sprintf("%.1f%% faster", improvement), "\n")
