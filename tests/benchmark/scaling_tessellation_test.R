# Scaling test for tessellation puzzles - Issue #60
# Purpose: Understand how performance scales with puzzle size

suppressPackageStartupMessages({
  devtools::load_all(quiet = TRUE)
})

cat("=== Tessellation Scaling Test ===\n\n")

# Test configurations at different scales
test_sizes <- c(10, 20, 30, 40)

results <- data.frame()

for (n in test_sizes) {
  cat(sprintf("\n--- Testing with n = %d ---\n", n))

  # Voronoi (n cells)
  cat(sprintf("  Voronoi (%d cells)...", n))
  vor_time <- system.time({
    vor_result <- generate_puzzle(
      type = "voronoi", seed = 42,
      grid = c(n), size = c(300, 300), offset = 0
    )
  })["elapsed"]
  cat(sprintf(" %.3fs (%d pieces)\n", vor_time, length(vor_result$pieces)))
  results <- rbind(results, data.frame(
    type = "voronoi", n = n,
    pieces = length(vor_result$pieces),
    time = vor_time
  ))

  # Random (n seed points)
  cat(sprintf("  Random (%d points)...", n))
  rnd_time <- system.time({
    rnd_result <- generate_puzzle(
      type = "random", seed = 42,
      grid = c(n), size = c(300, 300), offset = 0
    )
  })["elapsed"]
  cat(sprintf(" %.3fs (%d pieces)\n", rnd_time, length(rnd_result$pieces)))
  results <- rbind(results, data.frame(
    type = "random", n = n,
    pieces = length(rnd_result$pieces),
    time = rnd_time
  ))

  # Hexagonal (approximate similar piece count)
  # For hex: pieces = 3*r*(r-1) + 1, so r=2 gives 7, r=3 gives 19, r=4 gives 37
  hex_rings <- ceiling(sqrt(n / 3))
  if (hex_rings < 2) hex_rings <- 2
  cat(sprintf("  Hexagonal (%d rings)...", hex_rings))
  hex_time <- system.time({
    hex_result <- generate_puzzle(
      type = "hexagonal", seed = 42,
      grid = c(hex_rings), size = c(200), offset = 0
    )
  })["elapsed"]
  cat(sprintf(" %.3fs (%d pieces)\n", hex_time, length(hex_result$pieces)))
  results <- rbind(results, data.frame(
    type = "hexagonal", n = n,
    pieces = length(hex_result$pieces),
    time = hex_time
  ))
}

cat("\n=== Summary Table ===\n")
print(results)

cat("\n=== Time per Piece ===\n")
results$time_per_piece <- results$time / results$pieces
for (type in unique(results$type)) {
  sub <- results[results$type == type, ]
  cat(sprintf("%s: avg %.4f sec/piece\n", type, mean(sub$time_per_piece)))
}

cat("\n=== Random vs Voronoi Ratio ===\n")
for (n in test_sizes) {
  vor <- results[results$type == "voronoi" & results$n == n, "time"]
  rnd <- results[results$type == "random" & results$n == n, "time"]
  cat(sprintf("n=%d: Random is %.2fx slower than Voronoi\n", n, rnd/vor))
}

cat("\n=== Test Complete ===\n")
