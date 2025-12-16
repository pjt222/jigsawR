# Profile Random puzzle generation for small n - Issue #60
# Purpose: Understand the n=10 anomaly (7.76x slower than Voronoi)

suppressPackageStartupMessages({
  devtools::load_all(quiet = TRUE)
})

cat("=== Profiling Random Puzzle (n=10) ===\n\n")

# Run multiple times to identify consistent bottleneck
cat("Running 5 iterations to measure consistency...\n")
times <- numeric(5)
for (i in 1:5) {
  times[i] <- system.time({
    result <- generate_puzzle(
      type = "random", seed = 42 + i,
      grid = c(10), size = c(300, 300), offset = 0
    )
  })["elapsed"]
  cat(sprintf("  Run %d: %.3fs (%d pieces)\n", i, times[i], length(result$pieces)))
}
cat(sprintf("\nMean: %.3fs, SD: %.3fs\n", mean(times), sd(times)))

# Profile using Rprof
cat("\n=== Detailed Profiling ===\n")
Rprof(tmp <- tempfile())
result <- generate_puzzle(
  type = "random", seed = 42,
  grid = c(10), size = c(300, 300), offset = 0
)
Rprof(NULL)

# Read and summarize profile
prof_summary <- summaryRprof(tmp)
cat("\nTop 10 functions by total time:\n")
top_total <- head(prof_summary$by.total[order(-prof_summary$by.total$total.pct), ], 10)
print(top_total[, c("total.time", "total.pct", "self.time", "self.pct")])

cat("\nTop 10 functions by self time:\n")
top_self <- head(prof_summary$by.self[order(-prof_summary$by.self$self.pct), ], 10)
print(top_self[, c("self.time", "self.pct")])

unlink(tmp)

cat("\n=== Profile Complete ===\n")
