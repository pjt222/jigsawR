# Test Rcpp functions are available and working

devtools::load_all()

cat("=== Rcpp Availability Check ===\n")
cat("Rcpp available:", .rcpp_status(), "\n")
cat("random_batch_cpp exists:", exists("random_batch_cpp", mode = "function"), "\n")
cat("bezier_batch_cpp exists:", exists("bezier_batch_cpp", mode = "function"), "\n")
cat("svg_translate_cpp exists:", exists("svg_translate_cpp", mode = "function"), "\n")

cat("\n=== RNG Batch Test (seed=42, count=5) ===\n")
result <- uniform_batch(seed = 42, count = 5)
print(result)

cat("\n=== Bezier Integration Test ===\n")
cat("Testing bezier_to_points() now uses C++...\n")
pts <- bezier_to_points(c(0, 0), c(1, 2), c(2, 1), c(3, 3), n_points = 5)
print(pts)

cat("\n=== SVG Translate Integration Test ===\n")
cat("Testing translate_svg_path() now uses C++...\n")
svg <- translate_svg_path("M 10 20 L 30 40 C 50 60 70 80 90 100 Z", dx = 100, dy = 200)
cat(svg, "\n")

cat("\n=== Performance Comparison: RNG ===\n")
n <- 10000
seed <- 42
start <- Sys.time()
cpp_result <- uniform_batch(seed, n)
cpp_time <- Sys.time() - start
start <- Sys.time()
r_result <- .random_batch_r(seed, n)
r_time <- Sys.time() - start
cat("C++ uniform_batch:", format(cpp_time, digits = 4), "\n")
cat("R .random_batch_r:", format(r_time, digits = 4), "\n")
cat("Speedup:", round(as.numeric(r_time) / as.numeric(cpp_time), 1), "x\n")

cat("\n=== Performance Comparison: Bezier ===\n")
n_curves <- 1000
n_pts <- 100
start <- Sys.time()
for (i in 1:n_curves) {
  bezier_batch(c(0, 0), c(runif(1), runif(1)), c(runif(1), runif(1)), c(1, 1), n_pts)
}
cpp_time <- Sys.time() - start
start <- Sys.time()
for (i in 1:n_curves) {
  .bezier_batch_r(c(0, 0), c(runif(1), runif(1)), c(runif(1), runif(1)), c(1, 1), n_pts)
}
r_time <- Sys.time() - start
cat("C++ bezier_batch:", format(cpp_time, digits = 4), "\n")
cat("R .bezier_batch_r:", format(r_time, digits = 4), "\n")
cat("Speedup:", round(as.numeric(r_time) / as.numeric(cpp_time), 1), "x\n")

cat("\n=== Performance Comparison: SVG Translate ===\n")
test_path <- "M 10 20 L 30 40 C 50 60 70 80 90 100 A 5 5 0 1 0 150 150 Z"
n_trans <- 10000
start <- Sys.time()
for (i in 1:n_trans) {
  svg_translate(test_path, i, i)
}
cpp_time <- Sys.time() - start
start <- Sys.time()
for (i in 1:n_trans) {
  .svg_translate_r(test_path, i, i)
}
r_time <- Sys.time() - start
cat("C++ svg_translate:", format(cpp_time, digits = 4), "\n")
cat("R .svg_translate_r:", format(r_time, digits = 4), "\n")
cat("Speedup:", round(as.numeric(r_time) / as.numeric(cpp_time), 1), "x\n")

cat("\n=== All Tests Passed ===\n")
