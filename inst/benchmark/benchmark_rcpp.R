# Benchmark: C++ vs R Implementation Comparison
# Tests the speedup from Rcpp optimizations
#
# Usage: Rscript inst/benchmark/benchmark_rcpp.R

devtools::load_all(quiet = TRUE)
library(cli)

cli_h1("Rcpp vs R Implementation Benchmark")

# =============================================================================
# Check Rcpp availability
# =============================================================================

cli_h2("Environment Check")

rcpp_available <- .rcpp_status()
cli_alert_info("Rcpp functions available: {if (rcpp_available) cli::col_green('YES') else cli::col_red('NO')}")

if (!rcpp_available) {
  cli_alert_danger("C++ functions not available - cannot run comparison")
  quit(status = 1)
}

# =============================================================================
# Benchmark helper
# =============================================================================

benchmark_comparison <- function(name, cpp_fn, r_fn, iterations = 100) {
  cli_h3(name)
  
  # Time C++ version
  t_cpp <- system.time({
    for (i in seq_len(iterations)) cpp_fn()
  })[["elapsed"]]
  
  # Time R version
  t_r <- system.time({
    for (i in seq_len(iterations)) r_fn()
  })[["elapsed"]]
  
  speedup <- t_r / t_cpp
  
  cli_alert_info("C++ time:  {sprintf('%.3f', t_cpp)} seconds ({iterations} iterations)")
  cli_alert_info("R time:    {sprintf('%.3f', t_r)} seconds ({iterations} iterations)")
  cli_alert_success("Speedup:   {cli::col_cyan(sprintf('%.1fx', speedup))}")
  
  return(list(cpp = t_cpp, r = t_r, speedup = speedup))
}

# =============================================================================
# Benchmark 1: RNG Batch Generation
# =============================================================================

cli_h1("Benchmark 1: RNG Batch Generation")

seed <- 42
count <- 10000

result_rng <- benchmark_comparison(
  name = sprintf("Generate %d random values", count),
  cpp_fn = function() random_batch_cpp(seed, count),
  r_fn = function() .random_batch_r(seed, count),
  iterations = 100
)

# =============================================================================
# Benchmark 2: Bezier Point Interpolation
# =============================================================================

cli_h1("Benchmark 2: Bezier Point Interpolation")

p0 <- c(0, 0)
cp1 <- c(10, 30)
cp2 <- c(40, 20)
p1 <- c(50, 50)
n_points <- 100

result_bezier <- benchmark_comparison(
  name = sprintf("Generate %d bezier points", n_points),
  cpp_fn = function() bezier_batch_cpp(p0, cp1, cp2, p1, n_points),
  r_fn = function() .bezier_batch_r(p0, cp1, cp2, p1, n_points),
  iterations = 1000
)

# =============================================================================
# Benchmark 3: SVG Path Translation
# =============================================================================

cli_h1("Benchmark 3: SVG Path Translation")

# Generate a realistic puzzle path (complex path with many commands)
test_path <- paste0(
  "M 0.00 0.00 ",
  paste(replicate(50, "C 10.5 20.3 30.7 40.9 50.1 60.2"), collapse = " "),
  " Z"
)

result_svg <- benchmark_comparison(
  name = sprintf("Translate path with %d C commands", 50),
  cpp_fn = function() svg_translate_cpp(test_path, 100, 200),
  r_fn = function() .svg_translate_r(test_path, 100, 200),
  iterations = 500
)

# =============================================================================
# Benchmark 4: End-to-End Puzzle Generation
# =============================================================================

cli_h1("Benchmark 4: End-to-End Puzzle Generation")

cli_h3("Hexagonal puzzle (5 rings, 61 pieces)")

# With C++ (wrapper uses C++ automatically)
t_cpp <- system.time({
  for (i in 1:5) {
    generate_puzzle(
      type = "hexagonal",
      grid = c(5),
      size = c(200),
      seed = 42,
      save_files = FALSE
    )
  }
})[["elapsed"]]

cli_alert_info("With C++:  {sprintf('%.3f', t_cpp)} seconds (5 puzzles)")

# Temporarily disable C++ by setting cache to FALSE
old_cache <- .rcpp_cache$available
.rcpp_cache$available <- FALSE

t_r <- system.time({
  for (i in 1:5) {
    generate_puzzle(
      type = "hexagonal",
      grid = c(5),
      size = c(200),
      seed = 42,
      save_files = FALSE
    )
  }
})[["elapsed"]]

# Restore C++ availability
.rcpp_cache$available <- old_cache

cli_alert_info("R only:    {sprintf('%.3f', t_r)} seconds (5 puzzles)")
cli_alert_success("Speedup:   {cli::col_cyan(sprintf('%.1fx', t_r / t_cpp))}")

# =============================================================================
# Summary
# =============================================================================

cli_h1("Summary")

cli_alert_success("RNG batch speedup:    {sprintf('%.1fx', result_rng$speedup)}")
cli_alert_success("Bezier speedup:       {sprintf('%.1fx', result_bezier$speedup)}")
cli_alert_success("SVG translate speedup: {sprintf('%.1fx', result_svg$speedup)}")

overall_target <- 5
achieved <- all(c(result_rng$speedup, result_bezier$speedup, result_svg$speedup) >= 2)

if (achieved) {
  cli_alert_success("All C++ functions provide significant speedup!")
} else {
  cli_alert_warning("Some functions may not meet target speedup")
}
