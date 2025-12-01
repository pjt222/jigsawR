#!/usr/bin/env Rscript
# Detailed debug of warp formula

cat("=== Understanding the hex_warp transformation ===\n\n")

# Original formula from hex_warp (no flat-top offset)
hex_warp_original <- function(x, y) {
  angl <- atan2(y, x) + pi
  angl60 <- angl %% (pi / 3)
  angl30 <- abs((pi / 6) - angl60)
  l <- sqrt(0.75) / cos(angl30)
  list(x = x / l, y = y / l)
}

# The original jigsaw-hex.js generates POINTY-TOP hexagons
# Vertices at: 30°, 90°, 150°, 210°, 270°, 330° (offset by 30° from the standard)
# Edge midpoints at: 0°, 60°, 120°, 180°, 240°, 300°

cat("Original hex_warp (pointy-top orientation):\n")
cat("Expected: corners (30°, 90°, etc.) should be warped inward, edge midpoints stay same\n\n")

# Test at various angles
for (deg in seq(0, 90, by = 30)) {
  rad <- deg * pi / 180
  x <- 30 * cos(rad)
  y <- 30 * sin(rad)
  w <- hex_warp_original(x, y)
  orig_dist <- sqrt(x^2 + y^2)
  warp_dist <- sqrt(w$x^2 + w$y^2)
  cat(sprintf("  %3d°: dist %.2f -> %.2f (ratio: %.3f)\n", deg, orig_dist, warp_dist, warp_dist/orig_dist))
}

cat("\n=== Our puzzle uses FLAT-TOP hexagons ===\n")
cat("Vertices at: 0°, 60°, 120°, 180°, 240°, 300°\n")
cat("Edge midpoints at: 30°, 90°, 150°, 210°, 270°, 330°\n\n")

# To convert: add 30° (pi/6) offset
hex_warp_flat_top <- function(x, y) {
  angl <- atan2(y, x) + pi + (pi / 6)  # Added pi/6 for flat-top
  angl60 <- angl %% (pi / 3)
  angl30 <- abs((pi / 6) - angl60)
  l <- sqrt(0.75) / cos(angl30)
  list(x = x / l, y = y / l)
}

cat("Adjusted hex_warp (flat-top orientation):\n")
cat("Expected: corners (0°, 60°, etc.) should be warped inward, edge midpoints stay same\n\n")

for (deg in seq(0, 90, by = 30)) {
  rad <- deg * pi / 180
  x <- 30 * cos(rad)
  y <- 30 * sin(rad)
  w <- hex_warp_flat_top(x, y)
  orig_dist <- sqrt(x^2 + y^2)
  warp_dist <- sqrt(w$x^2 + w$y^2)
  cat(sprintf("  %3d°: dist %.2f -> %.2f (ratio: %.3f)\n", deg, orig_dist, warp_dist, warp_dist/orig_dist))
}

# Let's also compute L values at each angle
cat("\n=== L values at different angles (flat-top) ===\n")
for (deg in seq(0, 90, by = 15)) {
  rad <- deg * pi / 180
  angl <- rad + pi + (pi / 6)
  angl60 <- angl %% (pi / 3)
  angl30 <- abs((pi / 6) - angl60)
  l <- sqrt(0.75) / cos(angl30)
  cat(sprintf("  %3d°: L = %.4f (1/L = %.4f, x/L ratio)\n", deg, l, 1/l))
}

cat("\n=== What we want ===\n")
cat("- At corners (0°, 60°): L > 1, so x/L < x (corners move inward)\n")
cat("- At edge midpoints (30°, 90°): L = 1, so x/L = x (unchanged)\n")
cat("\n=== Checking if flat-top formula achieves this ===\n")
cat("At 0° (corner): L should be 2/sqrt(3) ≈ 1.155\n")
cat("At 30° (edge midpoint): L should be 1.0\n")
