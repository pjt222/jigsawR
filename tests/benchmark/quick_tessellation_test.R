# Quick tessellation performance test for Issue #60
# Purpose: Establish baseline timing for Voronoi and Random puzzle generation

suppressPackageStartupMessages({
  devtools::load_all(quiet = TRUE)
})

cat("=== Quick Tessellation Performance Test ===\n\n")

# Small test configuration
test_grid <- c(15)  # 15 cells/points
test_size <- c(250, 250)

# Test Voronoi
cat("Testing Voronoi (15 cells)...\n")
vor_time <- system.time({
  vor_result <- generate_puzzle(
    type = "voronoi",
    seed = 42,
    grid = test_grid,
    size = test_size,
    offset = 0
  )
})
cat(sprintf("  Voronoi: %.3f seconds (%.0f pieces)\n",
            vor_time["elapsed"], length(vor_result$pieces)))

# Test Random
cat("Testing Random (15 points)...\n")
rnd_time <- system.time({
  rnd_result <- generate_puzzle(
    type = "random",
    seed = 42,
    grid = test_grid,
    size = test_size,
    offset = 0
  )
})
cat(sprintf("  Random: %.3f seconds (%.0f pieces)\n",
            rnd_time["elapsed"], length(rnd_result$pieces)))

# Test Hexagonal for comparison (similar piece count)
cat("Testing Hexagonal (3 rings, ~19 pieces)...\n")
hex_time <- system.time({
  hex_result <- generate_puzzle(
    type = "hexagonal",
    seed = 42,
    grid = c(3),
    size = c(200),
    offset = 0
  )
})
cat(sprintf("  Hexagonal: %.3f seconds (%.0f pieces)\n",
            hex_time["elapsed"], length(hex_result$pieces)))

# Test Rectangular for comparison
cat("Testing Rectangular (4x4 = 16 pieces)...\n")
rect_time <- system.time({
  rect_result <- generate_puzzle(
    type = "rectangular",
    seed = 42,
    grid = c(4, 4),
    size = c(200, 200),
    offset = 0
  )
})
cat(sprintf("  Rectangular: %.3f seconds (%.0f pieces)\n",
            rect_time["elapsed"], length(rect_result$pieces)))

# Summary comparison
cat("\n=== Performance Summary ===\n")
times <- c(
  Rectangular = rect_time["elapsed"],
  Hexagonal = hex_time["elapsed"],
  Voronoi = vor_time["elapsed"],
  Random = rnd_time["elapsed"]
)
baseline <- times["Hexagonal"]
cat(sprintf("Baseline (Hexagonal): %.3f seconds\n", baseline))
cat("\nRelative to Hexagonal:\n")
for (name in names(times)) {
  ratio <- times[name] / baseline
  cat(sprintf("  %s: %.2fx\n", name, ratio))
}

cat("\n=== Test Complete ===\n")
