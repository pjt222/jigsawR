#!/usr/bin/env Rscript
# Debug warp formula math

cat("=== Debug Warp Formula ===\n\n")

# Test point
x <- 50
y <- 0
cat(sprintf("Test point: (%0.2f, %0.2f)\n", x, y))
cat(sprintf("Distance: %0.2f\n\n", sqrt(x^2 + y^2)))

# Original hex_warp formula from hexagonal_puzzle.R
cat("=== Original hex_warp (from hexagonal_puzzle.R) ===\n")
angl <- atan2(y, x) + pi
cat(sprintf("atan2(y, x) = %0.4f rad = %0.2f deg\n", atan2(y, x), atan2(y, x) * 180 / pi))
cat(sprintf("angl = atan2 + pi = %0.4f rad = %0.2f deg\n", angl, angl * 180 / pi))

angl60 <- angl %% (pi / 3)
cat(sprintf("angl60 = angl mod 60 = %0.4f rad = %0.2f deg\n", angl60, angl60 * 180 / pi))

angl30 <- abs((pi / 6) - angl60)
cat(sprintf("angl30 = |30 - angl60| = %0.4f rad = %0.2f deg\n", angl30, angl30 * 180 / pi))

l <- sqrt(0.75) / cos(angl30)
cat(sprintf("l = sqrt(0.75) / cos(angl30) = %0.4f / %0.4f = %0.4f\n",
            sqrt(0.75), cos(angl30), l))

new_x <- x / l
new_y <- y / l
cat(sprintf("Result: (%0.2f / %0.4f, %0.2f / %0.4f) = (%0.2f, %0.2f)\n",
            x, l, y, l, new_x, new_y))
cat(sprintf("New distance: %0.2f\n", sqrt(new_x^2 + new_y^2)))
cat(sprintf("Ratio: %0.4f\n\n", sqrt(new_x^2 + new_y^2) / sqrt(x^2 + y^2)))

# My apply_hex_warp formula
cat("=== My apply_hex_warp (from hexagonal_topology.R) ===\n")
angl2 <- atan2(y, x) + pi + (pi / 6)  # I added pi/6 offset
cat(sprintf("angl2 = atan2 + pi + pi/6 = %0.4f rad = %0.2f deg\n", angl2, angl2 * 180 / pi))

angl60_2 <- angl2 %% (pi / 3)
cat(sprintf("angl60 = %0.4f rad = %0.2f deg\n", angl60_2, angl60_2 * 180 / pi))

angl30_2 <- abs((pi / 6) - angl60_2)
cat(sprintf("angl30 = %0.4f rad = %0.2f deg\n", angl30_2, angl30_2 * 180 / pi))

L <- sqrt(0.75) / cos(angl30_2)
cat(sprintf("L = %0.4f\n", L))

# I used multiplication instead of division!
new_x2_mult <- x * L
new_y2_mult <- y * L
cat(sprintf("MULTIPLY result: (%0.2f, %0.2f) - distance %0.2f\n",
            new_x2_mult, new_y2_mult, sqrt(new_x2_mult^2 + new_y2_mult^2)))

new_x2_div <- x / L
new_y2_div <- y / L
cat(sprintf("DIVIDE result: (%0.2f, %0.2f) - distance %0.2f\n",
            new_x2_div, new_y2_div, sqrt(new_x2_div^2 + new_y2_div^2)))

cat("\n=== Testing Different Points ===\n")
test_points <- list(
  list(x = 50, y = 0, desc = "Right (0 deg)"),
  list(x = 25, y = 43.3, desc = "Upper-right (60 deg)"),
  list(x = 0, y = 50, desc = "Up (90 deg)"),
  list(x = -25, y = 43.3, desc = "Upper-left (120 deg)"),
  list(x = 43.3, y = 25, desc = "30 deg - edge midpoint"),
  list(x = 35.36, y = 35.36, desc = "45 deg")
)

for (pt in test_points) {
  x <- pt$x
  y <- pt$y
  orig_dist <- sqrt(x^2 + y^2)

  # Original formula
  angl <- atan2(y, x) + pi
  angl60 <- angl %% (pi / 3)
  angl30 <- abs((pi / 6) - angl60)
  l <- sqrt(0.75) / cos(angl30)
  warp_dist <- sqrt((x/l)^2 + (y/l)^2)
  ratio <- warp_dist / orig_dist

  cat(sprintf("\n%s: angle=%.1f deg, l=%.4f, ratio=%.4f\n",
              pt$desc, atan2(y, x) * 180/pi, l, ratio))
}

cat("\n\n=== Key Insight ===\n")
cat("The original formula DIVIDES by l, so:\n")
cat("  When l > 1 (at corners where angl30=0): distance DECREASES\n")
cat("  When l < 1 (at edge midpoints): distance INCREASES\n")
cat("\nBUT WAIT: Let me check what l actually is at different angles...\n")

cat("\nAt 0 deg (horizontal right):\n")
angl <- atan2(0, 1) + pi  # = pi
angl60 <- angl %% (pi / 3)  # = pi mod (pi/3) = 0
angl30 <- abs((pi/6) - angl60)  # = pi/6
l <- sqrt(0.75) / cos(pi/6)  # = sqrt(0.75) / sqrt(0.75) = 1
cat(sprintf("  angl=%0.2f, angl60=%0.2f, angl30=%0.2f, l=%0.4f\n",
            angl*180/pi, angl60*180/pi, angl30*180/pi, l))

cat("\nAt 30 deg:\n")
angl <- atan2(1, sqrt(3)) + pi  # about pi + 0.5236 = 3.66
angl60 <- angl %% (pi / 3)
angl30 <- abs((pi/6) - angl60)
l <- sqrt(0.75) / cos(angl30)
cat(sprintf("  angl=%0.2f, angl60=%0.2f, angl30=%0.2f, l=%0.4f\n",
            angl*180/pi, angl60*180/pi, angl30*180/pi, l))

cat("\n")
