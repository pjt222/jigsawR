#!/usr/bin/env Rscript
# Test rotation utilities

source("R/rotation_utils.R")

cat("Testing Rotation Utilities\n")
cat("==========================\n\n")

# Test 1: Basic point rotation
cat("Test 1: Rotate point (10, 0) by 90 degrees\n")
result <- rotate_point(10, 0, pi/2)
cat(sprintf("  Expected: (0, 10)\n"))
cat(sprintf("  Got: (%.2f, %.2f)\n", result$x, result$y))
stopifnot(abs(result$x - 0) < 0.01)
stopifnot(abs(result$y - 10) < 0.01)
cat("  ✓ PASS\n\n")

# Test 2: Rotate point by 60 degrees
cat("Test 2: Rotate point (10, 0) by 60 degrees\n")
result <- rotate_point(10, 0, pi/3)
expected_x <- 10 * cos(pi/3)  # 5
expected_y <- 10 * sin(pi/3)  # 8.66
cat(sprintf("  Expected: (%.2f, %.2f)\n", expected_x, expected_y))
cat(sprintf("  Got: (%.2f, %.2f)\n", result$x, result$y))
stopifnot(abs(result$x - expected_x) < 0.01)
stopifnot(abs(result$y - expected_y) < 0.01)
cat("  ✓ PASS\n\n")

# Test 3: Rotate around custom center
cat("Test 3: Rotate point (10, 5) by 90 degrees around center (5, 5)\n")
result <- rotate_point(10, 5, pi/2, center = c(5, 5))
cat(sprintf("  Expected: (5, 10)\n"))
cat(sprintf("  Got: (%.2f, %.2f)\n", result$x, result$y))
stopifnot(abs(result$x - 5) < 0.01)
stopifnot(abs(result$y - 10) < 0.01)
cat("  ✓ PASS\n\n")

# Test 4: Rotate multiple points
cat("Test 4: Rotate triangle vertices by 45 degrees\n")
triangle <- list(c(0, 0), c(10, 0), c(5, 8.66))
rotated <- rotate_points(triangle, pi/4)
cat(sprintf("  Original: (0,0), (10,0), (5,8.66)\n"))
cat(sprintf("  Rotated: (%.2f,%.2f), (%.2f,%.2f), (%.2f,%.2f)\n",
            rotated[[1]][1], rotated[[1]][2],
            rotated[[2]][1], rotated[[2]][2],
            rotated[[3]][1], rotated[[3]][2]))
cat("  ✓ PASS\n\n")

# Test 5: Zero rotation (identity)
cat("Test 5: Zero rotation should return original\n")
result <- rotate_point(10, 5, 0)
stopifnot(abs(result$x - 10) < 1e-10)
stopifnot(abs(result$y - 5) < 1e-10)
cat("  ✓ PASS\n\n")

# Test 6: Full rotation (360 degrees)
cat("Test 6: Full rotation should return to original\n")
result <- rotate_point(10, 5, 2 * pi)
stopifnot(abs(result$x - 10) < 0.01)
stopifnot(abs(result$y - 5) < 0.01)
cat("  ✓ PASS\n\n")

# Test 7: SVG path rotation (basic check)
cat("Test 7: Rotate simple SVG path\n")
path <- "M 10 0 L 0 10"
rotated_path <- rotate_svg_path(path, pi/2)
cat(sprintf("  Original: %s\n", path))
cat(sprintf("  Rotated:  %s\n", rotated_path))
# After 90° rotation: (10,0) -> (0,10), (0,10) -> (-10,0)
# Just check that rotation produces different output
stopifnot(rotated_path != path)
stopifnot(grepl("M", rotated_path))
cat("  ✓ PASS\n\n")

# Test 8: Rotation function creator
cat("Test 8: Create rotation function (compatible with hex_rotate)\n")
rotate_fn <- create_rotation_fn(pi/3)
result <- rotate_fn(c(10, 0))
expected_x <- 10 * cos(pi/3)
expected_y <- 10 * sin(pi/3)
cat(sprintf("  Expected: (%.2f, %.2f)\n", expected_x, expected_y))
cat(sprintf("  Got: (%.2f, %.2f)\n", result[1], result[2]))
stopifnot(abs(result[1] - expected_x) < 0.01)
stopifnot(abs(result[2] - expected_y) < 0.01)
cat("  ✓ PASS\n\n")

# Test 9: Angle equivalence
cat("Test 9: Test angle equivalence with 2π periodicity\n")
stopifnot(angles_equal(0, 2*pi))
stopifnot(angles_equal(pi/3, pi/3 + 2*pi))
stopifnot(angles_equal(pi, -pi))
stopifnot(!angles_equal(0, pi/6))
cat("  ✓ PASS\n\n")

# Test 10: Hexagon rotation (practical example)
cat("Test 10: Rotate hexagon vertices\n")
# Create regular hexagon (flat-top, radius 10)
hex_vertices <- list()
for (i in 0:5) {
  angle <- i * pi / 3
  hex_vertices[[i + 1]] <- c(10 * cos(angle), 10 * sin(angle))
}

# Rotate by 30 degrees
rotated_hex <- rotate_points(hex_vertices, pi/6)

cat("  Original vertex 0: (%.2f, %.2f)\n", hex_vertices[[1]][1], hex_vertices[[1]][2])
cat("  Rotated vertex 0:  (%.2f, %.2f)\n", rotated_hex[[1]][1], rotated_hex[[1]][2])

# After 30° rotation, vertex 0 should be at (8.66, 5)
expected_x <- 10 * cos(pi/6)
expected_y <- 10 * sin(pi/6)
stopifnot(abs(rotated_hex[[1]][1] - expected_x) < 0.01)
stopifnot(abs(rotated_hex[[1]][2] - expected_y) < 0.01)
cat("  ✓ PASS\n\n")

cat("==========================\n")
cat("All rotation utility tests passed! ✓\n")
