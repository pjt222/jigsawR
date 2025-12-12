#!/usr/bin/env Rscript

# jigsawR Baseline Benchmark Script
# =================================
# Comprehensive performance benchmarking for all puzzle types

# Setup -------------------------------------------------------------------

# Load package
devtools::load_all()

# Load required packages
if (!requireNamespace("bench", quietly = TRUE)) {
  stop("Package 'bench' is required. Install with: install.packages('bench')")
}
if (!requireNamespace("profvis", quietly = TRUE)) {
  stop("Package 'profvis' is required. Install with: install.packages('profvis')")
}

library(bench)

# Create output directory for results
benchmark_dir <- file.path("tests", "benchmark", "results")
dir.create(benchmark_dir, showWarnings = FALSE, recursive = TRUE)

# Benchmark Configuration -------------------------------------------------

log_info("Starting jigsawR benchmark suite")
log_info("Timestamp: {Sys.time()}")

# Define test configurations
rectangular_configs <- list(
  small = list(grid = c(3, 3), size = c(300, 300), label = "3x3"),
  medium = list(grid = c(5, 5), size = c(500, 500), label = "5x5"),
  large = list(grid = c(8, 8), size = c(800, 800), label = "8x8")
)

hexagonal_configs <- list(
  small = list(grid = c(2), size = c(200), label = "2 rings"),
  medium = list(grid = c(3), size = c(300), label = "3 rings"),
  large = list(grid = c(5), size = c(500), label = "5 rings")
)

concentric_configs <- list(
  small = list(grid = c(2), size = c(200), label = "2 rings"),
  medium = list(grid = c(3), size = c(300), label = "3 rings"),
  large = list(grid = c(5), size = c(500), label = "5 rings")
)

# Fixed seed for reproducibility
SEED <- 42

# Helper Functions --------------------------------------------------------

#' Run benchmark for a single configuration
#' @param type Puzzle type
#' @param config Configuration list
#' @param label Description label
run_single_benchmark <- function(type, config, label) {
  log_info("Benchmarking {type} - {label}")

  result <- bench::mark(
    generate_puzzle(
      type = type,
      seed = SEED,
      grid = config$grid,
      size = config$size,
      offset = 0
    ),
    iterations = 10,
    check = FALSE,
    memory = TRUE
  )

  # Add metadata
  result$type <- type
  result$label <- label
  result$grid <- paste(config$grid, collapse = "x")
  result$size <- paste(config$size, collapse = "x")

  return(result)
}

#' Print benchmark results in a readable format
print_results <- function(results_df) {
  cat("\n")
  cat("=" , rep("=", 70), "\n", sep = "")
  cat("Benchmark Results Summary\n")
  cat("=" , rep("=", 70), "\n", sep = "")
  cat("\n")

  for (i in seq_len(nrow(results_df))) {
    row <- results_df[i, ]
    cat(sprintf("%-20s %-12s\n", "Puzzle Type:", row$type))
    cat(sprintf("%-20s %-12s\n", "Configuration:", row$label))
    cat(sprintf("%-20s %-12s\n", "Grid:", row$grid))
    cat(sprintf("%-20s %-12s\n", "Size (mm):", row$size))
    cat(sprintf("%-20s %s\n", "Median Time:", format(row$median)))
    cat(sprintf("%-20s %s\n", "Mean Time:", format(row$mean)))
    cat(sprintf("%-20s %s\n", "Min Time:", format(row$min)))
    cat(sprintf("%-20s %s\n", "Max Time:", format(row$max)))
    cat(sprintf("%-20s %s\n", "Memory Allocated:", format(row$mem_alloc)))
    cat(sprintf("%-20s %.2f\n", "GC/Iteration:", row$n_gc / row$n_itr))
    cat("\n")
  }

  cat("=" , rep("=", 70), "\n", sep = "")
  cat("\n")
}

# Run Benchmarks ----------------------------------------------------------

log_info("Running rectangular puzzle benchmarks")
rect_results <- lapply(names(rectangular_configs), function(name) {
  config <- rectangular_configs[[name]]
  run_single_benchmark("rectangular", config, config$label)
})
rect_df <- do.call(rbind, rect_results)

log_info("Running hexagonal puzzle benchmarks")
hex_results <- lapply(names(hexagonal_configs), function(name) {
  config <- hexagonal_configs[[name]]
  run_single_benchmark("hexagonal", config, config$label)
})
hex_df <- do.call(rbind, hex_results)

log_info("Running concentric puzzle benchmarks")
conc_results <- lapply(names(concentric_configs), function(name) {
  config <- concentric_configs[[name]]
  run_single_benchmark("concentric", config, config$label)
})
conc_df <- do.call(rbind, conc_results)

# Combine all results
all_results <- rbind(rect_df, hex_df, conc_df)

# Print Results -----------------------------------------------------------

print_results(all_results)

# Save detailed results
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
results_file <- file.path(benchmark_dir, paste0("benchmark_", timestamp, ".rds"))
saveRDS(all_results, results_file)
log_success("Detailed results saved to: {results_file}")

# Create summary CSV
summary_df <- all_results[, c("type", "label", "grid", "size", "median", "mean", "mem_alloc", "n_gc")]
summary_file <- file.path(benchmark_dir, paste0("summary_", timestamp, ".csv"))
write.csv(summary_df, summary_file, row.names = FALSE)
log_success("Summary CSV saved to: {summary_file}")

# Profiling ---------------------------------------------------------------

log_info("Running profvis profiling for medium-sized puzzles")

# Profile rectangular 5x5
log_info("Profiling rectangular 5x5")
prof_rect <- profvis::profvis({
  generate_puzzle(
    type = "rectangular",
    seed = SEED,
    grid = c(5, 5),
    size = c(500, 500),
    offset = 0
  )
})
prof_rect_file <- file.path(benchmark_dir, paste0("profile_rect_5x5_", timestamp, ".html"))
htmlwidgets::saveWidget(prof_rect, prof_rect_file)
log_success("Rectangular profile saved to: {prof_rect_file}")

# Profile hexagonal 3 rings
log_info("Profiling hexagonal 3 rings")
prof_hex <- profvis::profvis({
  generate_puzzle(
    type = "hexagonal",
    seed = SEED,
    grid = c(3),
    size = c(300),
    offset = 0
  )
})
prof_hex_file <- file.path(benchmark_dir, paste0("profile_hex_3rings_", timestamp, ".html"))
htmlwidgets::saveWidget(prof_hex, prof_hex_file)
log_success("Hexagonal profile saved to: {prof_hex_file}")

# Profile concentric 3 rings
log_info("Profiling concentric 3 rings")
prof_conc <- profvis::profvis({
  generate_puzzle(
    type = "concentric",
    seed = SEED,
    grid = c(3),
    size = c(300),
    offset = 0
  )
})
prof_conc_file <- file.path(benchmark_dir, paste0("profile_conc_3rings_", timestamp, ".html"))
htmlwidgets::saveWidget(prof_conc, prof_conc_file)
log_success("Concentric profile saved to: {prof_conc_file}")

# Comparative Analysis ----------------------------------------------------

log_info("Generating comparative analysis")

cat("\n")
cat("=" , rep("=", 70), "\n", sep = "")
cat("Comparative Analysis: Medium-Sized Puzzles\n")
cat("=" , rep("=", 70), "\n", sep = "")
cat("\n")

medium_results <- all_results[all_results$label %in% c("5x5", "3 rings"), ]

rect_medium <- medium_results[medium_results$type == "rectangular", ]
hex_medium <- medium_results[medium_results$type == "hexagonal", ]
conc_medium <- medium_results[medium_results$type == "concentric", ]

rect_time <- as.numeric(rect_medium$median)
hex_time <- as.numeric(hex_medium$median)
conc_time <- as.numeric(conc_medium$median)

cat(sprintf("Rectangular (5x5):  %s\n", format(rect_medium$median)))
cat(sprintf("Hexagonal (3 rings): %s\n", format(hex_medium$median)))
cat(sprintf("Concentric (3 rings): %s\n", format(conc_medium$median)))
cat("\n")

fastest_type <- c("Rectangular", "Hexagonal", "Concentric")[which.min(c(rect_time, hex_time, conc_time))]
cat(sprintf("Fastest type (medium): %s\n", fastest_type))
cat("\n")

# Memory comparison
rect_mem <- as.numeric(rect_medium$mem_alloc)
hex_mem <- as.numeric(hex_medium$mem_alloc)
conc_mem <- as.numeric(conc_medium$mem_alloc)

cat(sprintf("Memory - Rectangular:  %s\n", format(rect_medium$mem_alloc)))
cat(sprintf("Memory - Hexagonal:    %s\n", format(hex_medium$mem_alloc)))
cat(sprintf("Memory - Concentric:   %s\n", format(conc_medium$mem_alloc)))
cat("\n")

most_efficient <- c("Rectangular", "Hexagonal", "Concentric")[which.min(c(rect_mem, hex_mem, conc_mem))]
cat(sprintf("Most memory efficient: %s\n", most_efficient))
cat("\n")

cat("=" , rep("=", 70), "\n", sep = "")
cat("\n")

# Scaling Analysis --------------------------------------------------------

log_info("Analyzing scaling behavior")

cat("\n")
cat("=" , rep("=", 70), "\n", sep = "")
cat("Scaling Analysis\n")
cat("=" , rep("=", 70), "\n", sep = "")
cat("\n")

# Rectangular scaling
rect_small <- as.numeric(rect_results[[1]]$median)
rect_medium <- as.numeric(rect_results[[2]]$median)
rect_large <- as.numeric(rect_results[[3]]$median)

cat("Rectangular Scaling:\n")
cat(sprintf("  3x3 (9 pieces):   %s\n", format(rect_results[[1]]$median)))
cat(sprintf("  5x5 (25 pieces):  %s (%.2fx)\n", format(rect_results[[2]]$median), rect_medium / rect_small))
cat(sprintf("  8x8 (64 pieces):  %s (%.2fx)\n", format(rect_results[[3]]$median), rect_large / rect_small))
cat("\n")

# Hexagonal scaling
hex_small <- as.numeric(hex_results[[1]]$median)
hex_medium <- as.numeric(hex_results[[2]]$median)
hex_large <- as.numeric(hex_results[[3]]$median)

cat("Hexagonal Scaling:\n")
cat(sprintf("  2 rings (7 pieces):   %s\n", format(hex_results[[1]]$median)))
cat(sprintf("  3 rings (19 pieces):  %s (%.2fx)\n", format(hex_results[[2]]$median), hex_medium / hex_small))
cat(sprintf("  5 rings (61 pieces):  %s (%.2fx)\n", format(hex_results[[3]]$median), hex_large / hex_small))
cat("\n")

# Concentric scaling
conc_small <- as.numeric(conc_results[[1]]$median)
conc_medium <- as.numeric(conc_results[[2]]$median)
conc_large <- as.numeric(conc_results[[3]]$median)

cat("Concentric Scaling:\n")
cat(sprintf("  2 rings (7 pieces):   %s\n", format(conc_results[[1]]$median)))
cat(sprintf("  3 rings (13 pieces):  %s (%.2fx)\n", format(conc_results[[2]]$median), conc_medium / conc_small))
cat(sprintf("  5 rings (25 pieces):  %s (%.2fx)\n", format(conc_results[[3]]$median), conc_large / conc_small))
cat("\n")

cat("=" , rep("=", 70), "\n", sep = "")
cat("\n")

# Final Summary -----------------------------------------------------------

log_success("Benchmark suite completed!")
log_info("Results saved to: {benchmark_dir}")
log_info("View profiling results by opening HTML files in a browser")

cat("\n")
cat("Files generated:\n")
cat(sprintf("  - %s\n", results_file))
cat(sprintf("  - %s\n", summary_file))
cat(sprintf("  - %s\n", prof_rect_file))
cat(sprintf("  - %s\n", prof_hex_file))
cat(sprintf("  - %s\n", prof_conc_file))
cat("\n")

log_info("To view profiling results:")
cat("  browseURL('", prof_rect_file, "')\n", sep = "")
cat("  browseURL('", prof_hex_file, "')\n", sep = "")
cat("  browseURL('", prof_conc_file, "')\n", sep = "")
cat("\n")
