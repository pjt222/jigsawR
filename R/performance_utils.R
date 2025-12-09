# Performance Utilities for jigsawR
#
# Provides optimized functions using data.table and future/furrr when available.
# Falls back gracefully to base R implementations.

# =============================================================================
# Package Detection
# =============================================================================

#' Check if data.table is available
#' @return Logical
#' @keywords internal
has_data_table <- function() {

  requireNamespace("data.table", quietly = TRUE)
}

#' Check if future/furrr are available
#' @return Logical
#' @keywords internal
has_parallel <- function() {
  requireNamespace("future", quietly = TRUE) &&
    requireNamespace("furrr", quietly = TRUE)
}

# =============================================================================
# data.table Configuration
# =============================================================================

#' Configure data.table for optimal performance
#'
#' Sets data.table threads based on available cores.
#' Should be called at package load or before intensive operations.
#'
#' @param threads Number of threads (NULL = auto-detect)
#' @param verbose Print configuration info
#' @return Invisible number of threads configured
#'
#' @export
configure_data_table <- function(threads = NULL, verbose = FALSE) {
  if (!has_data_table()) {
    if (verbose) {
      message("data.table not available - using base R")
    }
    return(invisible(1L))
  }

  if (is.null(threads)) {
    threads <- max(1L, parallel::detectCores() - 1L)
  }

  data.table::setDTthreads(threads)


  if (verbose) {
    message(sprintf("data.table configured with %d threads", threads))
  }

  invisible(threads)
}

# =============================================================================
# High-Performance Neighbor Map Building
# =============================================================================

#' Build hexagonal neighbor map using data.table (if available)
#'
#' Uses data.table's fast set() operations for ~10x speedup over rbind.
#'
#' @param rings Number of rings in hexagonal puzzle
#' @return data.frame (or data.table if available) with piece_id, side, neighbor_id
#'
#' @export
build_hex_neighbor_map_fast <- function(rings) {
  num_pieces <- 3 * rings * (rings - 1) + 1
  n_rows <- num_pieces * 6L

  if (has_data_table()) {
    # Use data.table with pre-allocation and set()
    dt <- data.table::data.table(
      piece_id = integer(n_rows),
      side = integer(n_rows),
      neighbor_id = integer(n_rows)
    )

    row_idx <- 1L
    for (piece_id in seq_len(num_pieces)) {
      for (side in 0:5) {
        neighbor_id <- get_hex_neighbor(piece_id, side, rings)

        data.table::set(dt, row_idx, "piece_id", as.integer(piece_id))
        data.table::set(dt, row_idx, "side", as.integer(side))
        data.table::set(dt, row_idx, "neighbor_id",
                        if (is.na(neighbor_id)) NA_integer_ else as.integer(neighbor_id))
        row_idx <- row_idx + 1L
      }
    }

    return(dt)
  }

  # Fallback to optimized base R
  build_hex_neighbor_map(rings)
}

# =============================================================================
# Parallel Puzzle Generation
# =============================================================================

#' Generate multiple puzzles in parallel
#'
#' Uses future/furrr for parallel execution when available.
#' Falls back to sequential lapply otherwise.
#'
#' @param n_puzzles Number of puzzles to generate
#' @param type Puzzle type ("hexagonal", "rectangular", "concentric")
#' @param grid Grid specification
#' @param size Size specification
#' @param seeds Vector of seeds (length must equal n_puzzles)
#' @param n_workers Number of parallel workers (NULL = auto)
#' @param ... Additional arguments passed to generate_puzzle()
#'
#' @return List of puzzle results
#'
#' @export
generate_puzzles_parallel <- function(n_puzzles, type, grid, size,
                                      seeds = seq_len(n_puzzles),
                                      n_workers = NULL, ...) {
  if (length(seeds) != n_puzzles) {
    stop("Length of seeds must equal n_puzzles")
  }

  if (has_parallel() && n_puzzles > 1) {
    # Configure parallel backend
    if (is.null(n_workers)) {
      n_workers <- min(4L, parallel::detectCores() - 1L)
    }

    # Set up plan
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)

    if (.Platform$OS.type == "windows") {
      future::plan(future::multisession, workers = n_workers)
    } else {
      future::plan(future::multicore, workers = n_workers)
    }

    # Parallel generation
    results <- furrr::future_map(seeds, function(seed) {
      generate_puzzle(
        type = type,
        grid = grid,
        size = size,
        seed = seed,
        save_files = FALSE,
        ...
      )
    }, .options = furrr::furrr_options(seed = TRUE))

  } else {
    # Sequential fallback
    results <- lapply(seeds, function(seed) {
      generate_puzzle(
        type = type,
        grid = grid,
        size = size,
        seed = seed,
        save_files = FALSE,
        ...
      )
    })
  }

  results
}

# =============================================================================
# Vectorized String Operations
# =============================================================================

#' Vectorized coordinate formatting
#'
#' Faster than sprintf in loops for coordinate pairs.
#'
#' @param x Numeric vector of x coordinates
#' @param y Numeric vector of y coordinates
#' @param digits Number of decimal places
#' @return Character vector of formatted coordinate pairs
#'
#' @keywords internal
format_coords <- function(x, y, digits = 2) {
  paste0(round(x, digits), ",", round(y, digits))
}

#' Build SVG path string efficiently
#'
#' Concatenates path commands without repeated paste0 calls.
#'
#' @param commands Character vector of SVG path commands
#' @return Single path string
#'
#' @keywords internal
build_svg_path <- function(commands) {
  paste(commands, collapse = " ")
}

# =============================================================================
# Performance Information
# =============================================================================
#' Get performance configuration status
#'
#' Reports which performance optimizations are available.
#'
#' @return List with configuration details
#'
#' @export
get_performance_config <- function() {
  list(
    data_table = list(
      available = has_data_table(),
      threads = if (has_data_table()) data.table::getDTthreads() else NA
    ),
    parallel = list(
      available = has_parallel(),
      cores = parallel::detectCores()
    ),
    r_version = R.version.string,
    platform = .Platform$OS.type
  )
}

#' Print performance configuration
#'
#' @export
print_performance_config <- function() {
  config <- get_performance_config()

  if (requireNamespace("cli", quietly = TRUE)) {
    cli::cli_h2("jigsawR Performance Configuration")

    if (config$data_table$available) {
      cli::cli_alert_success("data.table: {config$data_table$threads} threads")
    } else {
      cli::cli_alert_warning("data.table: not available")
    }

    if (config$parallel$available) {
      cli::cli_alert_success("future/furrr: {config$parallel$cores} cores available")
    } else {
      cli::cli_alert_warning("future/furrr: not available")
    }

    cli::cli_alert_info("R: {config$r_version}")
    cli::cli_alert_info("Platform: {config$platform}")
  } else {
    cat("jigsawR Performance Configuration\n")
    cat("==================================\n")
    cat(sprintf("data.table: %s\n",
                if (config$data_table$available)
                  sprintf("%d threads", config$data_table$threads)
                else "not available"))
    cat(sprintf("future/furrr: %s\n",
                if (config$parallel$available)
                  sprintf("%d cores", config$parallel$cores)
                else "not available"))
    cat(sprintf("R: %s\n", config$r_version))
    cat(sprintf("Platform: %s\n", config$platform))
  }

  invisible(config)
}
