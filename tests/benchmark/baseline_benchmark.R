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

# Voronoi puzzle configurations (requires deldir package)
voronoi_configs <- list(
  small = list(grid = c(10), size = c(200, 200), label = "vor_10cells"),
  medium = list(grid = c(25), size = c(300, 300), label = "vor_25cells"),
  large = list(grid = c(50), size = c(400, 400), label = "vor_50cells")
)

# Random shape puzzle configurations (requires RCDT package)
random_configs <- list(
  small = list(grid = c(10), size = c(200, 200), label = "rnd_10pts"),
  medium = list(grid = c(25), size = c(300, 300), label = "rnd_25pts"),
  large = list(grid = c(50), size = c(400, 400), label = "rnd_50pts")
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

# Voronoi benchmarks (if deldir available)
vor_df <- NULL
if (requireNamespace("deldir", quietly = TRUE)) {
  log_info("Running Voronoi puzzle benchmarks")
  vor_results <- lapply(names(voronoi_configs), function(name) {
    config <- voronoi_configs[[name]]
    run_single_benchmark("voronoi", config, config$label)
  })
  vor_df <- do.call(rbind, vor_results)
} else {
  log_warn("Skipping Voronoi benchmarks - deldir package not available")
}

# Random shape benchmarks (if RCDT available)
rnd_df <- NULL
if (requireNamespace("RCDT", quietly = TRUE)) {
  log_info("Running Random shape puzzle benchmarks")
  rnd_results <- lapply(names(random_configs), function(name) {
    config <- random_configs[[name]]
    run_single_benchmark("random", config, config$label)
  })
  rnd_df <- do.call(rbind, rnd_results)
} else {
  log_warn("Skipping Random shape benchmarks - RCDT package not available")
}

# Combine all results
all_results <- rbind(rect_df, hex_df, conc_df, vor_df, rnd_df)

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

# Profile Voronoi 25 cells (if available)
prof_vor_file <- NULL
if (!is.null(vor_df)) {
  log_info("Profiling Voronoi 25 cells")
  prof_vor <- profvis::profvis({
    generate_puzzle(
      type = "voronoi",
      seed = SEED,
      grid = c(25),
      size = c(300, 300),
      offset = 0
    )
  })
  prof_vor_file <- file.path(benchmark_dir, paste0("profile_vor_25cells_", timestamp, ".html"))
  htmlwidgets::saveWidget(prof_vor, prof_vor_file)
  log_success("Voronoi profile saved to: {prof_vor_file}")
}

# Profile Random 25 points (if available)
prof_rnd_file <- NULL
if (!is.null(rnd_df)) {
  log_info("Profiling Random 25 points")
  prof_rnd <- profvis::profvis({
    generate_puzzle(
      type = "random",
      seed = SEED,
      grid = c(25),
      size = c(300, 300),
      offset = 0
    )
  })
  prof_rnd_file <- file.path(benchmark_dir, paste0("profile_rnd_25pts_", timestamp, ".html"))
  htmlwidgets::saveWidget(prof_rnd, prof_rnd_file)
  log_success("Random profile saved to: {prof_rnd_file}")
}

# Comparative Analysis ----------------------------------------------------

log_info("Generating comparative analysis")

cat("\n")
cat("=" , rep("=", 70), "\n", sep = "")
cat("Comparative Analysis: Medium-Sized Puzzles\n")
cat("=" , rep("=", 70), "\n", sep = "")
cat("\n")

medium_results <- all_results[all_results$label %in% c("5x5", "3 rings", "vor_25cells", "rnd_25pts"), ]

rect_medium <- medium_results[medium_results$type == "rectangular", ]
hex_medium <- medium_results[medium_results$type == "hexagonal", ]
conc_medium <- medium_results[medium_results$type == "concentric", ]
vor_medium <- medium_results[medium_results$type == "voronoi", ]
rnd_medium <- medium_results[medium_results$type == "random", ]

# Build comparison vectors (handle missing types)
type_names <- c("Rectangular", "Hexagonal", "Concentric")
type_times <- c(
  as.numeric(rect_medium$median),
  as.numeric(hex_medium$median),
  as.numeric(conc_medium$median)
)
type_mems <- c(
  as.numeric(rect_medium$mem_alloc),
  as.numeric(hex_medium$mem_alloc),
  as.numeric(conc_medium$mem_alloc)
)

cat(sprintf("Rectangular (5x5):     %s\n", format(rect_medium$median)))
cat(sprintf("Hexagonal (3 rings):   %s\n", format(hex_medium$median)))
cat(sprintf("Concentric (3 rings):  %s\n", format(conc_medium$median)))

# Include tessellation types if available
if (nrow(vor_medium) > 0) {
  type_names <- c(type_names, "Voronoi")
  type_times <- c(type_times, as.numeric(vor_medium$median))
  type_mems <- c(type_mems, as.numeric(vor_medium$mem_alloc))
  cat(sprintf("Voronoi (25 cells):    %s\n", format(vor_medium$median)))
}
if (nrow(rnd_medium) > 0) {
  type_names <- c(type_names, "Random")
  type_times <- c(type_times, as.numeric(rnd_medium$median))
  type_mems <- c(type_mems, as.numeric(rnd_medium$mem_alloc))
  cat(sprintf("Random (25 points):    %s\n", format(rnd_medium$median)))
}
cat("\n")

fastest_type <- type_names[which.min(type_times)]
cat(sprintf("Fastest type (medium): %s\n", fastest_type))
cat("\n")

# Memory comparison
cat(sprintf("Memory - Rectangular:  %s\n", format(rect_medium$mem_alloc)))
cat(sprintf("Memory - Hexagonal:    %s\n", format(hex_medium$mem_alloc)))
cat(sprintf("Memory - Concentric:   %s\n", format(conc_medium$mem_alloc)))
if (nrow(vor_medium) > 0) {
  cat(sprintf("Memory - Voronoi:      %s\n", format(vor_medium$mem_alloc)))
}
if (nrow(rnd_medium) > 0) {
  cat(sprintf("Memory - Random:       %s\n", format(rnd_medium$mem_alloc)))
}
cat("\n")

most_efficient <- type_names[which.min(type_mems)]
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

# Voronoi scaling (if available)
if (!is.null(vor_df) && length(vor_results) >= 3) {
  vor_small <- as.numeric(vor_results[[1]]$median)
  vor_medium_time <- as.numeric(vor_results[[2]]$median)
  vor_large <- as.numeric(vor_results[[3]]$median)

  cat("Voronoi Scaling:\n")
  cat(sprintf("  10 cells:  %s\n", format(vor_results[[1]]$median)))
  cat(sprintf("  25 cells:  %s (%.2fx)\n", format(vor_results[[2]]$median), vor_medium_time / vor_small))
  cat(sprintf("  50 cells:  %s (%.2fx)\n", format(vor_results[[3]]$median), vor_large / vor_small))
  cat("\n")
}

# Random scaling (if available)
if (!is.null(rnd_df) && length(rnd_results) >= 3) {
  rnd_small <- as.numeric(rnd_results[[1]]$median)
  rnd_medium_time <- as.numeric(rnd_results[[2]]$median)
  rnd_large <- as.numeric(rnd_results[[3]]$median)

  cat("Random Scaling:\n")
  cat(sprintf("  10 points:  %s\n", format(rnd_results[[1]]$median)))
  cat(sprintf("  25 points:  %s (%.2fx)\n", format(rnd_results[[2]]$median), rnd_medium_time / rnd_small))
  cat(sprintf("  50 points:  %s (%.2fx)\n", format(rnd_results[[3]]$median), rnd_large / rnd_small))
  cat("\n")
}

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
if (!is.null(prof_vor_file)) {
  cat(sprintf("  - %s\n", prof_vor_file))
}
if (!is.null(prof_rnd_file)) {
  cat(sprintf("  - %s\n", prof_rnd_file))
}
cat("\n")

log_info("To view profiling results:")
cat("  browseURL('", prof_rect_file, "')\n", sep = "")
cat("  browseURL('", prof_hex_file, "')\n", sep = "")
cat("  browseURL('", prof_conc_file, "')\n", sep = "")
if (!is.null(prof_vor_file)) {
  cat("  browseURL('", prof_vor_file, "')\n", sep = "")
}
if (!is.null(prof_rnd_file)) {
  cat("  browseURL('", prof_rnd_file, "')\n", sep = "")
}
cat("\n")
