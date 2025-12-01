#!/usr/bin/env Rscript
# Debug the warp formula

cat("=== Debug Warp Formula ===\n\n")

x <- 30
y <- 0
cat("Input point: (", x, ",", y, ")\n")

angl <- atan2(y, x) + pi
cat("atan2(y, x) =", atan2(y, x), "\n")
cat("angl (after +pi) =", angl, "radians =", angl * 180 / pi, "degrees\n")

angl60 <- angl %% (pi / 3)
cat("angl60 = angl mod (pi/3) =", angl60, "radians =", angl60 * 180 / pi, "degrees\n")

angl30 <- abs((pi / 6) - angl60)
cat("angl30 = |pi/6 - angl60| =", angl30, "radians =", angl30 * 180 / pi, "degrees\n")

L <- sqrt(0.75) / cos(angl30)
cat("L = sqrt(0.75) / cos(angl30) =", sqrt(0.75), "/", cos(angl30), "=", L, "\n")

cat("\nResult: L =", L, " -> point divided by L gives (", x/L, ",", y/L, ")\n")

# The issue: At angle pi (180 deg from the +pi offset):
# angl60 = pi mod (pi/3) = 0 (since pi = 3 * (pi/3))
# angl30 = |pi/6 - 0| = pi/6 = 30 degrees
# L = sqrt(0.75) / cos(30°) = 0.866 / 0.866 = 1.0
# So L = 1 means NO warp at this angle!

# But for a flat-top hexagon, the corner IS at angle 0 (on the +x axis)
# The formula from the original jigsaw-hex.js works with pointy-top hexagons

# Let's check what happens at angle 30 degrees (which should be an edge midpoint)
cat("\n=== Test at angle 30 degrees ===\n")
angle_deg <- 30
angle_rad <- angle_deg * pi / 180
x2 <- 30 * cos(angle_rad)
y2 <- 30 * sin(angle_rad)
cat("Input point: (", x2, ",", y2, ")\n")

angl2 <- atan2(y2, x2) + pi
cat("angl (after +pi) =", angl2 * 180 / pi, "degrees\n")

angl60_2 <- angl2 %% (pi / 3)
cat("angl60 =", angl60_2 * 180 / pi, "degrees\n")

angl30_2 <- abs((pi / 6) - angl60_2)
cat("angl30 =", angl30_2 * 180 / pi, "degrees\n")

L2 <- sqrt(0.75) / cos(angl30_2)
cat("L =", L2, "\n")

# For the warp to work correctly with flat-top hexagons:
# - Corners are at 0°, 60°, 120°, 180°, 240°, 300°
# - Edge midpoints are at 30°, 90°, 150°, 210°, 270°, 330°

# The original formula assumes pointy-top where:
# - Corners are at 30°, 90°, 150°, 210°, 270°, 330°
# - Edge midpoints are at 0°, 60°, 120°, 180°, 240°, 300°

# For flat-top, we need to adjust by rotating 30 degrees
cat("\n=== Adjusted formula for flat-top hexagon ===\n")
cat("We need to add pi/6 (30 deg) offset to account for flat-top orientation\n")

# Let's verify what L should be:
# At corner (angle 0 for flat-top): L should be > 1 (corner protrudes beyond inscribed circle)
# For a regular hexagon with circumradius R:
# - Circumradius (corner distance) = R
# - Inradius (edge midpoint distance) = R * sqrt(3)/2 ≈ 0.866*R
# So L at corners should be 1/0.866 ≈ 1.155 to map corners inward to circle

cat("\n=== Expected L values ===\n")
cat("At corner: L should be ~", 1 / (sqrt(3)/2), "= 2/sqrt(3) ≈ 1.155\n")
cat("At edge midpoint: L should be 1.0\n")
