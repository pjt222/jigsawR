# Minimal test of tab extraction functionality

cat("=== Minimal Tab Extraction Test ===\n")

# Environment setup
.jigsaw_env <- new.env()

# Core initialization function
init_jigsaw <- function(seed = NULL, tabsize = 20, jitter = 4,
                        width = 300, height = 200, radius = 2.0,
                        xn = 15, yn = 10) {
  if (is.null(seed)) {
    seed <- as.integer(runif(1) * 10000)
  }
  .jigsaw_env$seed_initial <- seed
  .jigsaw_env$seed <- seed
  .jigsaw_env$tabsize <- tabsize
  .jigsaw_env$jitter <- jitter
  .jigsaw_env$width <- width
  .jigsaw_env$height <- height
  .jigsaw_env$radius <- radius
  .jigsaw_env$xn <- xn
  .jigsaw_env$yn <- yn
  .jigsaw_env$offset <- 0.0
  .jigsaw_env$t <- tabsize / 200.0
  .jigsaw_env$j <- jitter / 100.0
}

# Random functions
random <- function() {
  x <- sin(.jigsaw_env$seed) * 10000
  .jigsaw_env$seed <- .jigsaw_env$seed + 1
  return(x - floor(x))
}

uniform <- function(min_val, max_val) {
  r <- random()
  return(min_val + r * (max_val - min_val))
}

rbool <- function() {
  return(random() > 0.5)
}

# Tab generation functions
first <- function() {
  .jigsaw_env$e <- uniform(-.jigsaw_env$j, .jigsaw_env$j)
  .jigsaw_env$flip <- NULL
  next_tab()
}

next_tab <- function() {
  flipold <- if (exists("flip", envir = .jigsaw_env)) .jigsaw_env$flip else NULL
  .jigsaw_env$flip <- rbool()
  .jigsaw_env$a <- if (is.null(flipold) || .jigsaw_env$flip == flipold) -.jigsaw_env$e else .jigsaw_env$e
  .jigsaw_env$b <- uniform(-.jigsaw_env$j, .jigsaw_env$j)
  .jigsaw_env$c <- uniform(-.jigsaw_env$j, .jigsaw_env$j)
  .jigsaw_env$d <- uniform(-.jigsaw_env$j, .jigsaw_env$j)
  .jigsaw_env$e <- uniform(-.jigsaw_env$j, .jigsaw_env$j)
}

# Test the tab generation
cat("Testing with 2x2 puzzle...\n")
init_jigsaw(seed = 1234, xn = 2, yn = 2)

cat("Testing tab generation:\n")
.jigsaw_env$vertical <- 0  # horizontal mode
.jigsaw_env$yi <- 1
.jigsaw_env$xi <- 0

first()
cat("First tab - flip:", .jigsaw_env$flip, "a:", round(.jigsaw_env$a, 3), "\n")

next_tab()
cat("Second tab - flip:", .jigsaw_env$flip, "a:", round(.jigsaw_env$a, 3), "\n")

cat("=== Test Success ===\n")