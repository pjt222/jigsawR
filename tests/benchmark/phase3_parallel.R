#!/usr/bin/env Rscript
# Phase 3 Performance Benchmark - Parallel Batch Generation

# For parallel execution, jigsawR must be installed (not just loaded via load_all)
# Sequential mode works with devtools::load_all()

pkg_installed <- requireNamespace("jigsawR", quietly = TRUE)

if (pkg_installed) {
  cat("Using installed jigsawR package\n")
  suppressPackageStartupMessages(library(jigsawR))
} else {
  cat("jigsawR not installed - using devtools::load_all()\n")
  cat("NOTE: Parallel mode requires package installation\n")
  cat("      Install with: devtools::install()\n\n")
  suppressPackageStartupMessages(devtools::load_all(quiet = TRUE))
}

cat("\n=== Phase 3: Parallel Batch Generation Benchmark ===\n\n")

# Check if furrr is available
has_furrr <- requireNamespace("furrr", quietly = TRUE) &&
             requireNamespace("future", quietly = TRUE)

if (!has_furrr) {
  cat("WARNING: furrr/future not available. Install with:\n")
  cat("  install.packages(c('furrr', 'future'))\n\n")
}

n_cores <- parallel::detectCores()
cat(sprintf("System has %d cores available\n\n", n_cores))

# Create test variations (8 puzzles of varying complexity)
variations <- list(
  list(name = "rect_3x3_1", seed = 1, type = "rectangular", grid = c(3, 3), size = c(300, 300)),
  list(name = "rect_4x4_2", seed = 2, type = "rectangular", grid = c(4, 4), size = c(400, 400)),
  list(name = "rect_5x5_3", seed = 3, type = "rectangular", grid = c(5, 5), size = c(500, 500)),
  list(name = "hex_3_4", seed = 4, type = "hexagonal", grid = c(3), size = c(300)),
  list(name = "hex_3_5", seed = 5, type = "hexagonal", grid = c(3), size = c(300)),
  list(name = "conc_3_6", seed = 6, type = "concentric", grid = c(3), size = c(300)),
  list(name = "rect_6x6_7", seed = 7, type = "rectangular", grid = c(6, 6), size = c(600, 600)),
  list(name = "hex_4_8", seed = 8, type = "hexagonal", grid = c(4), size = c(400))
)

cat(sprintf("Testing with %d puzzle variations\n\n", length(variations)))

# Create temp directory for output
temp_dir <- tempfile("batch_bench_")
dir.create(temp_dir, recursive = TRUE)

# Benchmark sequential
cat("1. Sequential execution:\n")
time_seq <- system.time({
  suppressMessages({
    results_seq <- generate_puzzle_batch(variations, base_dir = temp_dir, parallel = FALSE)
  })
})["elapsed"]
cat(sprintf("   Time: %.2f seconds\n", time_seq))

# Clean up temp files
unlink(temp_dir, recursive = TRUE)

# Benchmark parallel (if available AND package is installed)
if (has_furrr && pkg_installed) {
  temp_dir <- tempfile("batch_bench_parallel_")
  dir.create(temp_dir, recursive = TRUE)

  # Test with different worker counts
  for (workers in c(2, 4, min(8, n_cores - 1))) {
    if (workers > n_cores - 1) next

    cat(sprintf("\n2. Parallel execution (%d workers):\n", workers))
    time_par <- system.time({
      suppressMessages({
        results_par <- generate_puzzle_batch(
          variations,
          base_dir = temp_dir,
          parallel = TRUE,
          workers = workers
        )
      })
    })["elapsed"]
    cat(sprintf("   Time: %.2f seconds\n", time_par))
    cat(sprintf("   Speedup: %.2fx\n", time_seq / time_par))

    # Clean up
    unlink(temp_dir, recursive = TRUE)
    dir.create(temp_dir, recursive = TRUE)
  }

  unlink(temp_dir, recursive = TRUE)
} else if (!pkg_installed) {
  cat("\n2. Parallel execution: SKIPPED (jigsawR package not installed)\n")
  cat("   Install with: devtools::install()\n")
} else {
  cat("\n2. Parallel execution: SKIPPED (furrr not available)\n")
}

cat("\n=== Phase 3 Benchmark Complete ===\n")
