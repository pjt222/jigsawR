#!/usr/bin/env Rscript
# Profile Phase 2 - Verify caching effectiveness

suppressPackageStartupMessages({
  devtools::load_all(quiet = TRUE)
})

cat("\n=== Phase 2 Cache Verification ===\n\n")

# Test 1: Verify parsed_segments exists in pieces
cat("Test 1: Verify parsed_segments caching\n")
rect <- suppressMessages(generate_puzzle(type = "rectangular", seed = 42, grid = c(3,3), size = c(300,300)))
hex <- suppressMessages(generate_puzzle(type = "hexagonal", seed = 42, grid = c(2), size = c(200)))
conc <- suppressMessages(generate_puzzle(type = "concentric", seed = 42, grid = c(2), size = c(200)))

cat(sprintf("  Rectangular: parsed_segments present = %s (segments: %d)\n",
            !is.null(rect$pieces[[1]]$parsed_segments),
            length(rect$pieces[[1]]$parsed_segments)))
cat(sprintf("  Hexagonal: parsed_segments present = %s (segments: %d)\n",
            !is.null(hex$pieces[[1]]$parsed_segments),
            length(hex$pieces[[1]]$parsed_segments)))
cat(sprintf("  Concentric: parsed_segments present = %s (segments: %d)\n",
            !is.null(conc$pieces[[1]]$parsed_segments),
            length(conc$pieces[[1]]$parsed_segments)))

# Test 2: Time parse_svg_path calls directly
cat("\nTest 2: Direct parse_svg_path timing\n")

# Get a sample path
sample_path <- rect$pieces[[1]]$path
cached_segments <- rect$pieces[[1]]$parsed_segments

# Time parsing 100 times
n_iter <- 100
time_parse <- system.time({
  for (i in 1:n_iter) {
    jigsawR:::parse_svg_path(sample_path)
  }
})["elapsed"]

cat(sprintf("  Time to parse path %d times: %.1fms (%.3fms/call)\n",
            n_iter, time_parse * 1000, time_parse * 1000 / n_iter))

# Test 3: Simulate rendering with/without cache
cat("\nTest 3: Split path with vs without cache\n")

# Without cache (piece = NULL)
time_no_cache <- system.time({
  for (i in 1:n_iter) {
    jigsawR:::split_rect_path_into_edges(sample_path, piece = NULL)
  }
})["elapsed"]

# With cache (piece has parsed_segments)
time_with_cache <- system.time({
  for (i in 1:n_iter) {
    jigsawR:::split_rect_path_into_edges(sample_path, piece = rect$pieces[[1]])
  }
})["elapsed"]

cat(sprintf("  Without cache: %.1fms (%d calls)\n", time_no_cache * 1000, n_iter))
cat(sprintf("  With cache:    %.1fms (%d calls)\n", time_with_cache * 1000, n_iter))
cat(sprintf("  Speedup:       %.2fx\n", time_no_cache / time_with_cache))

# Test 4: Full puzzle generation timing (with suppressed output)
cat("\nTest 4: Full puzzle generation (quiet mode)\n")

time_runs <- function(expr, n = 5) {
  times <- numeric(n)
  for (i in 1:n) {
    times[i] <- system.time(suppressMessages(eval(expr)))["elapsed"]
  }
  median(times) * 1000
}

cat(sprintf("  Rect 5x5:    %.1fms\n",
            time_runs(quote(generate_puzzle(type = "rectangular", seed = 42, grid = c(5,5), size = c(500,500))))))
cat(sprintf("  Hex 3 rings: %.1fms\n",
            time_runs(quote(generate_puzzle(type = "hexagonal", seed = 42, grid = c(3), size = c(300))))))
cat(sprintf("  Conc 3 rings:%.1fms\n",
            time_runs(quote(generate_puzzle(type = "concentric", seed = 42, grid = c(3), size = c(300))))))

cat("\n=== Profile Complete ===\n")
