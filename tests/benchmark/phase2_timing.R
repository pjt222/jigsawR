#!/usr/bin/env Rscript
# Phase 2 Performance Timing - Compare against baseline

devtools::load_all(quiet = TRUE)

cat("\n=== Phase 2 Performance Timing ===\n\n")

# Helper function to time multiple runs
time_puzzle <- function(expr_str, n_runs = 3) {
  expr <- parse(text = expr_str)
  times <- numeric(n_runs)
  for (i in seq_len(n_runs)) {
    times[i] <- system.time(eval(expr))[["elapsed"]]
  }
  list(median = median(times) * 1000, mean = mean(times) * 1000)  # Convert to ms
}

# Baseline values from docs/performance-optimization-plan.md
baseline <- list(
  rect_5x5 = 50.3,
  rect_8x8 = 104,
  hex_3 = 302,
  hex_5 = 1060,
  conc_3 = 92.4
)

results <- list()

# Rectangular 5x5
cat("Rectangular 5x5 puzzle (25 pieces):\n")
result <- time_puzzle('generate_puzzle(type = "rectangular", seed = 42, grid = c(5,5), size = c(500,500))', n_runs = 5)
results$rect_5x5 <- result$median
cat(sprintf("  Current: %.1fms (Baseline: %.1fms) - %.1fx\n", result$median, baseline$rect_5x5, baseline$rect_5x5 / result$median))

# Hexagonal 3 rings
cat("\nHexagonal 3 rings puzzle (19 pieces):\n")
result <- time_puzzle('generate_puzzle(type = "hexagonal", seed = 42, grid = c(3), size = c(300))', n_runs = 5)
results$hex_3 <- result$median
cat(sprintf("  Current: %.1fms (Baseline: %.1fms) - %.1fx\n", result$median, baseline$hex_3, baseline$hex_3 / result$median))

# Concentric 3 rings
cat("\nConcentric 3 rings puzzle (19 pieces):\n")
result <- time_puzzle('generate_puzzle(type = "concentric", seed = 42, grid = c(3), size = c(300))', n_runs = 5)
results$conc_3 <- result$median
cat(sprintf("  Current: %.1fms (Baseline: %.1fms) - %.1fx\n", result$median, baseline$conc_3, baseline$conc_3 / result$median))

# Larger hexagonal
cat("\nHexagonal 5 rings puzzle (61 pieces):\n")
result <- time_puzzle('generate_puzzle(type = "hexagonal", seed = 42, grid = c(5), size = c(500))', n_runs = 3)
results$hex_5 <- result$median
cat(sprintf("  Current: %.1fms (Baseline: %.1fms) - %.1fx\n", result$median, baseline$hex_5, baseline$hex_5 / result$median))

# Rectangular 8x8
cat("\nRectangular 8x8 puzzle (64 pieces):\n")
result <- time_puzzle('generate_puzzle(type = "rectangular", seed = 42, grid = c(8,8), size = c(800,800))', n_runs = 3)
results$rect_8x8 <- result$median
cat(sprintf("  Current: %.1fms (Baseline: %.1fms) - %.1fx\n", result$median, baseline$rect_8x8, baseline$rect_8x8 / result$median))

# Summary
cat("\n=== Summary ===\n")
cat("\n| Puzzle Type | Baseline | Current | Speedup |\n")
cat("|-------------|----------|---------|----------|\n")
cat(sprintf("| Rect 5x5    | %.1fms   | %.1fms  | %.2fx   |\n", baseline$rect_5x5, results$rect_5x5, baseline$rect_5x5 / results$rect_5x5))
cat(sprintf("| Rect 8x8    | %.1fms   | %.1fms  | %.2fx   |\n", baseline$rect_8x8, results$rect_8x8, baseline$rect_8x8 / results$rect_8x8))
cat(sprintf("| Hex 3 rings | %.1fms   | %.1fms  | %.2fx   |\n", baseline$hex_3, results$hex_3, baseline$hex_3 / results$hex_3))
cat(sprintf("| Hex 5 rings | %.1fms  | %.1fms | %.2fx   |\n", baseline$hex_5, results$hex_5, baseline$hex_5 / results$hex_5))
cat(sprintf("| Conc 3 rings| %.1fms   | %.1fms  | %.2fx   |\n", baseline$conc_3, results$conc_3, baseline$conc_3 / results$conc_3))

cat("\n=== Phase 2 Complete ===\n")
