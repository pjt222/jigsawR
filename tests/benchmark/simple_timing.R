#!/usr/bin/env Rscript
# Simple timing script for jigsawR performance measurement

devtools::load_all(quiet = TRUE)

cat("\n=== jigsawR Performance Timing (Post-Optimization) ===\n\n")

# Helper function to time multiple runs
time_puzzle <- function(expr_str, n_runs = 3) {
  expr <- parse(text = expr_str)
  times <- numeric(n_runs)
  for (i in seq_len(n_runs)) {
    times[i] <- system.time(eval(expr))[["elapsed"]]
  }
  list(median = median(times), mean = mean(times), times = times)
}

# Rectangular 5x5
cat("Rectangular 5x5 puzzle (25 pieces):\n")
result <- time_puzzle('generate_puzzle(type = "rectangular", seed = 42, grid = c(5,5), size = c(500,500))', n_runs = 5)
cat(sprintf("  Median: %.1fms, Mean: %.1fms\n", result$median * 1000, result$mean * 1000))

# Hexagonal 3 rings
cat("\nHexagonal 3 rings puzzle (19 pieces):\n")
result <- time_puzzle('generate_puzzle(type = "hexagonal", seed = 42, grid = c(3), size = c(300))', n_runs = 5)
cat(sprintf("  Median: %.1fms, Mean: %.1fms\n", result$median * 1000, result$mean * 1000))

# Concentric 3 rings
cat("\nConcentric 3 rings puzzle (19 pieces):\n")
result <- time_puzzle('generate_puzzle(type = "concentric", seed = 42, grid = c(3), size = c(300))', n_runs = 5)
cat(sprintf("  Median: %.1fms, Mean: %.1fms\n", result$median * 1000, result$mean * 1000))

# Larger hexagonal
cat("\nHexagonal 5 rings puzzle (61 pieces):\n")
result <- time_puzzle('generate_puzzle(type = "hexagonal", seed = 42, grid = c(5), size = c(500))', n_runs = 3)
cat(sprintf("  Median: %.1fms, Mean: %.1fms\n", result$median * 1000, result$mean * 1000))

# Rectangular 8x8
cat("\nRectangular 8x8 puzzle (64 pieces):\n")
result <- time_puzzle('generate_puzzle(type = "rectangular", seed = 42, grid = c(8,8), size = c(800,800))', n_runs = 3)
cat(sprintf("  Median: %.1fms, Mean: %.1fms\n", result$median * 1000, result$mean * 1000))

cat("\n=== Timing Complete ===\n")
cat("\nBaseline comparison (from docs/performance-optimization-plan.md):\n")
cat("  Rectangular 5x5:  50.3ms\n")
cat("  Hexagonal 3 rings: 302ms\n")
cat("  Concentric 3 rings: 92.4ms\n")
cat("  Hexagonal 5 rings: 1.06s\n")
cat("  Rectangular 8x8:   104ms\n")
