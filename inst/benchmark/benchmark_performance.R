# High-Performance Benchmark for jigsawR
# Uses data.table, future/furrr, and cli for verbose output
#
# Usage: Rscript inst/benchmark/benchmark_performance.R
#
# Inspired by Quake III's optimization philosophy:
# "Measure first, optimize what matters"

devtools::load_all(quiet = TRUE)
library(cli)

# =============================================================================
# SETUP: Configure High-Performance Environment
# =============================================================================

cli_h1("jigsawR High-Performance Benchmark Suite")

# Check for optional high-performance packages
has_data_table <- requireNamespace("data.table", quietly = TRUE)
has_future <- requireNamespace("future", quietly = TRUE)
has_furrr <- requireNamespace("furrr", quietly = TRUE)
has_bench <- requireNamespace("bench", quietly = TRUE)

# Display environment info
cli_h2("Environment Configuration")

r_version <- R.version.string
platform_type <- .Platform$OS.type
sysname <- Sys.info()["sysname"]
n_cores <- parallel::detectCores()

cli_alert_info("R version: {r_version}")
cli_alert_info("Platform: {platform_type} ({sysname})")
cli_alert_info("CPU cores available: {n_cores}")

# Configure data.table if available
if (has_data_table) {
  library(data.table)

  # Set optimal data.table threads
  dt_threads <- parallel::detectCores() - 1L
  dt_threads <- max(dt_threads, 1L)
  setDTthreads(dt_threads)

  cli_alert_success("data.table: {cli::col_green('ENABLED')} ({getDTthreads()} threads)")

  # Display data.table options
  cli_bullets(c(
    "i" = "data.table.verbose: {getOption('datatable.verbose', FALSE)}",
    "i" = "data.table.print.nrows: {getOption('datatable.print.nrows', 100)}"
  ))
} else {
  cli_alert_warning("data.table: {cli::col_yellow('NOT INSTALLED')} (using base R)")
}

# Configure future/furrr if available
if (has_future && has_furrr) {
  library(future)
  library(furrr)

  # Set up multicore plan (use multicore on Unix, multisession on Windows)
  if (.Platform$OS.type == "windows") {
    plan(multisession, workers = parallel::detectCores() - 1L)
    cli_alert_success("future: {cli::col_green('multisession')} ({availableWorkers()} workers)")
  } else {
    plan(multicore, workers = parallel::detectCores() - 1L)
    cli_alert_success("future: {cli::col_green('multicore')} ({availableWorkers()} workers)")
  }
} else {
  cli_alert_warning("future/furrr: {cli::col_yellow('NOT INSTALLED')} (using sequential)")
}

# Configure bench if available
if (has_bench) {
  library(bench)
  cli_alert_success("bench: {cli::col_green('ENABLED')} (precise timing)")
} else {
  cli_alert_warning("bench: {cli::col_yellow('NOT INSTALLED')} (using Sys.time())")
}

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Time a function with high precision
#' Uses bench::mark if available, falls back to Sys.time()
time_function <- function(fn, name = "operation", iterations = 3, verbose = TRUE) {
  if (has_bench) {
    result <- bench::mark(
      fn(),
      iterations = iterations,
      check = FALSE,
      filter_gc = FALSE
    )
    median_time <- as.numeric(result$median, "seconds") * 1000  # ms
    min_time <- as.numeric(result$min, "seconds") * 1000
    max_time <- as.numeric(result$max, "seconds") * 1000
    mem_alloc <- result$mem_alloc
  } else {
    times <- numeric(iterations)
    for (i in seq_len(iterations)) {
      start <- Sys.time()
      fn()
      end <- Sys.time()
      times[i] <- as.numeric(end - start, units = "secs") * 1000
    }
    median_time <- median(times)
    min_time <- min(times)
    max_time <- max(times)
    mem_alloc <- NA
  }

  if (verbose) {
    if (!is.na(mem_alloc)) {
      cli_alert_info("{name}: {cli::col_cyan(sprintf('%.1f ms', median_time))} (min: {sprintf('%.1f', min_time)}, max: {sprintf('%.1f', max_time)}, mem: {format(mem_alloc)})")
    } else {
      cli_alert_info("{name}: {cli::col_cyan(sprintf('%.1f ms', median_time))} (min: {sprintf('%.1f', min_time)}, max: {sprintf('%.1f', max_time)})")
    }
  }

  return(list(
    median_ms = median_time,
    min_ms = min_time,
    max_ms = max_time,
    mem_alloc = mem_alloc
  ))
}

#' Create fusion groups for testing
make_fusion_groups <- function(num_pieces, coverage = 0.2) {
  n_groups <- max(1, as.integer(num_pieces * coverage / 2))
  groups <- list()
  for (i in seq_len(n_groups)) {
    base_piece <- (i - 1) * 5 + 1
    if (base_piece + 1 <= num_pieces) {
      groups[[i]] <- c(base_piece, base_piece + 1)
    }
  }
  groups[lengths(groups) > 0]
}

# =============================================================================
# BENCHMARK 1: Neighbor Lookup Performance (O(1) vs O(n²))
# =============================================================================

cli_h1("Benchmark 1: Neighbor Lookup Performance")

test_rings <- c(3, 5, 7, 10)

cli_alert_info("Testing O(1) axial-coordinate neighbor lookup")

for (rings in test_rings) {
  num_pieces <- 3 * rings * (rings - 1) + 1

  cli_h3("{rings} rings ({num_pieces} pieces)")

  # Time cache building
  clear_hex_neighbor_cache()
  t_build <- time_function(
    function() get_hex_neighbor_data(rings),
    name = "Build neighbor cache",
    iterations = 1
  )

  # Time cached lookups (should be O(1))
  t_lookup <- time_function(
    function() {
      for (piece_id in 1:num_pieces) {
        get_hex_neighbors_for_fusion(piece_id, rings)
      }
    },
    name = sprintf("Lookup all %d pieces", num_pieces),
    iterations = 3
  )

  # Calculate per-piece lookup time
  per_piece_us <- (t_lookup$median_ms / num_pieces) * 1000
  cli_alert_success("Per-piece lookup: {sprintf('%.2f', per_piece_us)} microseconds")
}

# =============================================================================
# BENCHMARK 2: Puzzle Generation Performance
# =============================================================================

cli_h1("Benchmark 2: Puzzle Generation (End-to-End)")

puzzle_configs <- list(
  list(name = "Small Hex (3 rings)", type = "hexagonal", rings = 3),
  list(name = "Medium Hex (5 rings)", type = "hexagonal", rings = 5),
  list(name = "Large Hex (7 rings)", type = "hexagonal", rings = 7),
  list(name = "Small Rect (3x4)", type = "rectangular", grid = c(3, 4)),
  list(name = "Medium Rect (5x6)", type = "rectangular", grid = c(5, 6)),
  list(name = "Concentric (3 rings)", type = "concentric", rings = 3)
)

results <- list()

for (config in puzzle_configs) {
  cli_h3(config$name)

  if (config$type == "hexagonal") {
    num_pieces <- 3 * config$rings * (config$rings - 1) + 1
    fusion_groups <- make_fusion_groups(num_pieces)

    t_result <- time_function(
      function() {
        generate_puzzle(
          type = "hexagonal",
          grid = c(config$rings),
          size = c(200),
          seed = 42,
          fusion_groups = fusion_groups,
          save_files = FALSE
        )
      },
      name = sprintf("Generate %d pieces with %d fusion groups", num_pieces, length(fusion_groups)),
      iterations = 1
    )
  } else if (config$type == "rectangular") {
    num_pieces <- config$grid[1] * config$grid[2]

    t_result <- time_function(
      function() {
        generate_puzzle(
          type = "rectangular",
          grid = config$grid,
          size = c(400, 300),
          seed = 42,
          save_files = FALSE
        )
      },
      name = sprintf("Generate %d pieces", num_pieces),
      iterations = 1
    )
  } else if (config$type == "concentric") {
    t_result <- time_function(
      function() {
        generate_puzzle(
          type = "concentric",
          grid = c(config$rings),
          size = c(240),
          seed = 42,
          center_shape = "hexagon",
          save_files = FALSE
        )
      },
      name = "Generate concentric puzzle",
      iterations = 1
    )
  }

  results[[config$name]] <- t_result
}

# =============================================================================
# BENCHMARK 3: Parallel Processing (if available)
# =============================================================================

if (has_future && has_furrr) {
  cli_h1("Benchmark 3: Parallel Processing Comparison")

  # Generate multiple puzzles - compare sequential vs parallel
  n_puzzles <- 8
  rings <- 5

  cli_alert_info("Generating {n_puzzles} puzzles ({rings}-ring hexagonal)")

  # Sequential
  t_sequential <- time_function(
    function() {
      lapply(1:n_puzzles, function(seed) {
        generate_puzzle(
          type = "hexagonal",
          grid = c(rings),
          size = c(200),
          seed = seed,
          save_files = FALSE
        )
      })
    },
    name = "Sequential (lapply)",
    iterations = 1
  )

  # Parallel with furrr
  t_parallel <- time_function(
    function() {
      future_map(1:n_puzzles, function(seed) {
        generate_puzzle(
          type = "hexagonal",
          grid = c(rings),
          size = c(200),
          seed = seed,
          save_files = FALSE
        )
      }, .options = furrr_options(seed = TRUE))
    },
    name = "Parallel (future_map)",
    iterations = 1
  )

  speedup <- t_sequential$median_ms / t_parallel$median_ms
  cli_alert_success("Parallel speedup: {sprintf('%.2fx', speedup)} ({availableWorkers()} workers)")
}

# =============================================================================
# BENCHMARK 4: Memory Profile (if data.table available)
# =============================================================================

if (has_data_table) {
  cli_h1("Benchmark 4: data.table Operations")

  # Compare base R data.frame vs data.table for neighbor lookups
  rings <- 7
  num_pieces <- 3 * rings * (rings - 1) + 1

  cli_alert_info("Comparing data.frame vs data.table for {num_pieces} pieces")

  # Build neighbor data using base R
  t_base_r <- time_function(
    function() {
      neighbors <- data.frame(
        piece_id = integer(),
        side = integer(),
        neighbor_id = integer(),
        stringsAsFactors = FALSE
      )

      for (piece_id in 1:num_pieces) {
        for (side in 0:5) {
          nb <- get_hex_neighbors_for_fusion(piece_id, rings)
          neighbor_id <- nb$neighbor_id[side + 1]
          neighbors <- rbind(neighbors, data.frame(
            piece_id = piece_id,
            side = side,
            neighbor_id = neighbor_id,
            stringsAsFactors = FALSE
          ))
        }
      }
      neighbors
    },
    name = "Base R (data.frame + rbind)",
    iterations = 1
  )

  # Build neighbor data using data.table
  t_data_table <- time_function(
    function() {
      # Pre-allocate data.table
      n_rows <- num_pieces * 6
      neighbors <- data.table(
        piece_id = integer(n_rows),
        side = integer(n_rows),
        neighbor_id = integer(n_rows)
      )

      row_idx <- 1L
      for (piece_id in 1:num_pieces) {
        nb <- get_hex_neighbors_for_fusion(piece_id, rings)
        for (side in 0:5) {
          set(neighbors, row_idx, "piece_id", as.integer(piece_id))
          set(neighbors, row_idx, "side", as.integer(side))
          set(neighbors, row_idx, "neighbor_id", as.integer(nb$neighbor_id[side + 1]))
          row_idx <- row_idx + 1L
        }
      }
      neighbors
    },
    name = "data.table (pre-allocated + set)",
    iterations = 3
  )

  speedup <- t_base_r$median_ms / t_data_table$median_ms
  cli_alert_success("data.table speedup: {sprintf('%.1fx', speedup)}")
}

# =============================================================================
# SUMMARY
# =============================================================================

cli_h1("Benchmark Summary")

cli_alert_success("All benchmarks completed!")

cli_bullets(c(
  "i" = "O(1) axial neighbor lookup: Enabled",
  "i" = sprintf("data.table: %s", if (has_data_table) "Enabled" else "Not available"),
  "i" = sprintf("future/furrr: %s", if (has_future && has_furrr) "Enabled" else "Not available")
))

# Performance recommendations
cli_h2("Optimization Recommendations")

cli_bullets(c(
  "*" = "Pre-build neighbor cache before heavy fusion operations",
  "*" = "Use data.table for large neighbor maps (>100 pieces)",
  "*" = "Use future_map for batch puzzle generation",
  "*" = "Consider Rcpp for hot paths (bezier, SVG generation)"
))

cli_alert_info("For Quake III-style bit hacks, see: {.url https://en.wikipedia.org/wiki/Fast_inverse_square_root}")
