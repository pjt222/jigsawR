# RNG Iterator for Batch Optimization
# Provides pre-generated random values with same interface as per-call RNG
# Uses C++ uniform_batch() when available for ~27x speedup

#' Create an RNG iterator with pre-generated batch values
#'
#' Creates an iterator that pre-generates all random values using the
#' deterministic sine-based RNG matching the JavaScript jigsaw generator.
#' Uses C++ implementation when available for better performance.
#'
#' @param seed Starting seed value
#' @param count Number of random values to pre-generate
#' @return List with iterator functions: next_val(), uniform(), rbool(),
#'         get_index(), get_count()
#' @keywords internal
create_rng_iterator <- function(seed, count) {
  # Generate all values upfront using batch function
  values <- uniform_batch(seed, count)
  idx <- 0L

  list(
    #' Get next raw random value [0, 1)
    next_val = function() {
      idx <<- idx + 1L
      values[idx]
    },
    #' Get uniform random value in [min_val, max_val)
    uniform = function(min_val, max_val) {
      idx <<- idx + 1L
      min_val + values[idx] * (max_val - min_val)
    },
    #' Get random boolean (TRUE if value > 0.5)
    rbool = function() {
      idx <<- idx + 1L
      values[idx] > 0.5
    },
    #' Get current index (for debugging)
    get_index = function() idx,
    #' Get total count (for debugging)
    get_count = function() count
  )
}

#' Calculate total RNG calls needed for rectangular puzzle
#'
#' Calculates the exact number of random values needed to generate
#' a rectangular jigsaw puzzle with the given grid dimensions.
#'
#' @param xn Number of columns
#' @param yn Number of rows
#' @return Integer count of RNG values needed
#' @keywords internal
#'
#' @details
#' For rectangular puzzles:
#' - Each row of horizontal edges: first() = 6 calls, then xn * next_tab() = xn * 5 calls
#' - Each column of vertical edges: first() = 6 calls, then yn * next_tab() = yn * 5 calls
#' - Horizontal: (yn - 1) rows
#' - Vertical: (xn - 1) columns
calc_rect_rng_count <- function(xn, yn) {
  # Horizontal: (yn-1) rows, each row: first()=6 + xn*5 (next_tab calls)
  horizontal <- (yn - 1L) * (6L + xn * 5L)
  # Vertical: (xn-1) cols, each col: first()=6 + yn*5 (next_tab calls)
  vertical <- (xn - 1L) * (6L + yn * 5L)
  as.integer(horizontal + vertical)
}

#' Calculate total RNG calls needed for hexagonal puzzle
#'
#' Calculates the exact number of random values needed to generate
#' a hexagonal jigsaw puzzle with the given number of rings.
#'
#' @param n_rings Number of rings in the hexagonal puzzle
#' @return Integer count of RNG values needed
#' @keywords internal
#'
#' @details
#' For hexagonal puzzles:
#' - Each tab generation (hex_gentab) uses 6 RNG calls: flip, a, b, c, d, e
#' - The iteration pattern follows hex_gen_dh and hex_gen_dv
calc_hex_rng_count <- function(n_rings) {
  count <- 0L
  n <- n_rings
  yl <- 2L * n - 1L


  # Horizontal edges (hex_gen_dh pattern)
  # for (yi in seq(-yl + 2, yl - 2, by = 2))
  yi_seq <- seq(-yl + 2L, yl - 2L, by = 2L)
  for (yi in yi_seq) {
    xl <- 2L * n - 1L - (abs(yi) - 1L) / 2L
    # for (xi in seq(-xl + 1, xl - 2, by = 1))
    xi_seq <- seq(-xl + 1, xl - 2, by = 1)
    n_tabs <- length(xi_seq)
    count <- count + n_tabs * 6L
  }

  # Vertical edges (hex_gen_dv pattern)
  # for (yi in seq(-yl, yl - 1, by = 2))
  yi_seq_v <- seq(-yl, yl - 1L, by = 2L)
  for (yi in yi_seq_v) {
    xl <- 2L * n - 1L - abs(yi + 1L) / 2L
    # for (xi in seq(-xl + 2, xl - 2, by = 2))
    xi_seq <- seq(-xl + 2, xl - 2, by = 2)
    n_tabs <- length(xi_seq)
    count <- count + n_tabs * 6L
  }

  as.integer(count)
}
