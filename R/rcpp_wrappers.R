# R wrappers for Rcpp functions with graceful fallback
# These functions use C++ implementations when available,
# falling back to pure R implementations otherwise.

#' @useDynLib jigsawR, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

# =============================================================================
# Rcpp Availability Detection
# =============================================================================

#' Check if Rcpp functions are available
#'
#' Returns TRUE if the C++ implementations are loaded and available.
#' This is used internally to decide whether to use C++ or R fallback.
#'
#' Actually tests the C++ symbol, not just the R wrapper function,
#' because the R wrapper exists in RcppExports.R but the C++ symbol
#' may not be available (e.g., on shinyapps.io where the package
#' isn't compiled with Rcpp).
#'
#' @return Logical indicating if Rcpp functions are available
#' @keywords internal
.rcpp_available <- function() {
  # Must actually TEST the C++ function works, not just check if

  # the R wrapper exists. The R wrapper in RcppExports.R always exists,

  # but .Call() will fail if the shared library isn't loaded.
  tryCatch({
    # Try a minimal call to verify the C++ symbol is available
    result <- random_batch_cpp(seed = 1, count = 1, min_val = 0, max_val = 1)
    is.numeric(result) && length(result) == 1
  }, error = function(e) FALSE)
}

# Cache the availability check result
.rcpp_cache <- new.env(parent = emptyenv())

#' Get cached Rcpp availability status
#' @keywords internal
.rcpp_status <- function() {
  if (is.null(.rcpp_cache$available)) {
    .rcpp_cache$available <- .rcpp_available()
  }
  .rcpp_cache$available
}

# =============================================================================
# Priority 1: RNG Batch Generation Wrapper
# =============================================================================

#' Generate batch of uniform random numbers (R fallback)
#'
#' Pure R implementation of the deterministic sine-based RNG.
#' Used when C++ implementation is not available.
#'
#' @param seed Starting seed value
#' @param count Number of random values to generate
#' @param min_val Minimum value (default 0.0)
#' @param max_val Maximum value (default 1.0)
#' @return Numeric vector of random values
#' @keywords internal
.random_batch_r <- function(seed, count, min_val = 0.0, max_val = 1.0) {
  result <- numeric(count)
  range_val <- max_val - min_val

  for (i in seq_len(count)) {
    x <- sin(seed + i - 1) * 10000
    r <- x - floor(x)
    result[i] <- min_val + r * range_val
  }

  result
}

#' Generate batch of uniform random numbers
#'
#' Uses C++ implementation when available, falling back to R otherwise.
#' Produces deterministic results matching the JavaScript jigsaw generator.
#'
#' @param seed Starting seed value
#' @param count Number of random values to generate
#' @param min_val Minimum value (default 0.0)
#' @param max_val Maximum value (default 1.0)
#' @return Numeric vector of random values
#' @keywords internal
uniform_batch <- function(seed, count, min_val = 0.0, max_val = 1.0) {
  if (.rcpp_status()) {
    return(random_batch_cpp(seed, count, min_val, max_val))
  }
  .random_batch_r(seed, count, min_val, max_val)
}

# =============================================================================
# Priority 2: Bezier Interpolation Wrapper
# =============================================================================

#' Compute cubic Bezier curve points (R fallback)
#'
#' Pure R implementation of cubic Bezier interpolation.
#' Used when C++ implementation is not available.
#'
#' @param p0 Start point as c(x, y)
#' @param cp1 First control point as c(x, y)
#' @param cp2 Second control point as c(x, y)
#' @param p1 End point as c(x, y)
#' @param n_points Number of points to generate
#' @return Data frame with x and y columns
#' @keywords internal
.bezier_batch_r <- function(p0, cp1, cp2, p1, n_points) {
  t <- seq(0, 1, length.out = n_points)

  one_minus_t <- 1 - t
  one_minus_t_sq <- one_minus_t^2
  one_minus_t_cu <- one_minus_t^3
  t_sq <- t^2
  t_cu <- t^3

  x <- one_minus_t_cu * p0[1] +
       3 * one_minus_t_sq * t * cp1[1] +
       3 * one_minus_t * t_sq * cp2[1] +
       t_cu * p1[1]

  y <- one_minus_t_cu * p0[2] +
       3 * one_minus_t_sq * t * cp1[2] +
       3 * one_minus_t * t_sq * cp2[2] +
       t_cu * p1[2]

  data.frame(x = x, y = y)
}

#' Compute cubic Bezier curve points
#'
#' Uses C++ implementation when available, falling back to R otherwise.
#' Computes points along the curve defined by P0, CP1, CP2, P1.
#'
#' @param p0 Start point as c(x, y)
#' @param cp1 First control point as c(x, y)
#' @param cp2 Second control point as c(x, y)
#' @param p1 End point as c(x, y)
#' @param n_points Number of points to generate (default 20)
#' @return Data frame with x and y columns
#' @keywords internal
bezier_batch <- function(p0, cp1, cp2, p1, n_points = 20) {
  if (.rcpp_status()) {
    mat <- bezier_batch_cpp(p0, cp1, cp2, p1, n_points)
    return(data.frame(x = mat[, 1], y = mat[, 2]))
  }
  .bezier_batch_r(p0, cp1, cp2, p1, n_points)
}

# =============================================================================
# Priority 3: SVG Path Translation Wrapper
# =============================================================================

#' Translate SVG path coordinates (R fallback)
#'
#' Pure R implementation using regex-based parsing.
#' Used when C++ implementation is not available.
#'
#' @param path_string SVG path d attribute string
#' @param dx X translation
#' @param dy Y translation
#' @return Translated SVG path string
#' @keywords internal
.svg_translate_r <- function(path_string, dx, dy) {
  if (dx == 0 && dy == 0) {
    return(path_string)
  }

  # Inline implementation matching translate_svg_path
  # (avoids circular dependency and keeps fallback self-contained)
  tokens <- unlist(strsplit(path_string, "(?<=\\s)|(?=\\s)|(?<=[MLCAZ])|(?=[MLCAZ])",
                            perl = TRUE))
  tokens <- tokens[tokens != "" & tokens != " "]

  result <- character()
  i <- 1

  while (i <= length(tokens)) {
    token <- trimws(tokens[i])

    if (token == "M" || token == "L") {
      result <- c(result, token)
      if (i + 2 <= length(tokens)) {
        x <- as.numeric(tokens[i + 1]) + dx
        y <- as.numeric(tokens[i + 2]) + dy
        result <- c(result, sprintf("%.2f", x), sprintf("%.2f", y))
        i <- i + 3
      } else {
        i <- i + 1
      }

    } else if (token == "C") {
      result <- c(result, token)
      if (i + 6 <= length(tokens)) {
        for (j in 0:2) {
          x <- as.numeric(tokens[i + 1 + j * 2]) + dx
          y <- as.numeric(tokens[i + 2 + j * 2]) + dy
          result <- c(result, sprintf("%.2f", x), sprintf("%.2f", y))
        }
        i <- i + 7
      } else {
        i <- i + 1
      }

    } else if (token == "A") {
      result <- c(result, token)
      if (i + 7 <= length(tokens)) {
        result <- c(result,
                    tokens[i + 1],
                    tokens[i + 2],
                    tokens[i + 3],
                    tokens[i + 4],
                    tokens[i + 5])
        x <- as.numeric(tokens[i + 6]) + dx
        y <- as.numeric(tokens[i + 7]) + dy
        result <- c(result, sprintf("%.2f", x), sprintf("%.2f", y))
        i <- i + 8
      } else {
        i <- i + 1
      }

    } else if (token == "Z") {
      result <- c(result, token)
      i <- i + 1

    } else {
      result <- c(result, token)
      i <- i + 1
    }
  }

  paste(result, collapse = " ")
}

#' Translate SVG path coordinates
#'
#' Uses C++ implementation when available, falling back to R otherwise.
#' Applies (dx, dy) translation to all coordinates in an SVG path.
#'
#' @param path_string SVG path d attribute string
#' @param dx X translation
#' @param dy Y translation
#' @return Translated SVG path string
#' @keywords internal
svg_translate <- function(path_string, dx, dy) {
  if (.rcpp_status()) {
    return(svg_translate_cpp(path_string, dx, dy))
  }
  .svg_translate_r(path_string, dx, dy)
}
