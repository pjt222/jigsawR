#!/usr/bin/env Rscript
# Test the apply_hex_trunc function

source("R/hexagonal_topology.R")

cat("=== Testing apply_hex_trunc ===\n\n")

# Test with a target radius of 100
target_radius <- 100

# Test points at different angles
test_angles <- c(0, 30, 45, 60, 90, 120, 150, 180) * pi / 180
test_distances <- c(80, 90, 100)  # Points at various distances

cat("Testing truncation to POINTY-TOP hexagon with corner radius = 100\n")
cat("Expected:\n")
cat("  - At 0, 60, 120, 180: CORNER, hex boundary = 100\n")
cat("  - At 30, 90, 150: EDGE MIDPOINT, hex boundary = 86.6\n\n")

cat(sprintf("%6s | %8s | %12s | %12s | %10s\n",
            "Angle", "Input R", "Input (x,y)", "Output (x,y)", "Output R"))
cat(paste(rep("-", 60), collapse=""), "\n")

for (angle in test_angles) {
  for (r in test_distances) {
    x <- r * cos(angle)
    y <- r * sin(angle)

    result <- apply_hex_trunc(x, y, target_radius)
    output_r <- sqrt(result$x^2 + result$y^2)

    cat(sprintf("%5.0f° | %8.1f | (%5.1f,%5.1f) | (%5.1f,%5.1f) | %10.2f\n",
                angle * 180 / pi, r, x, y, result$x, result$y, output_r))
  }
}

# Verify specific cases
cat("\n=== Verification ===\n")

# Corner at 0° should project to target_radius
corner_x <- 80
corner_y <- 0
corner_result <- apply_hex_trunc(corner_x, corner_y, target_radius)
corner_r <- sqrt(corner_result$x^2 + corner_result$y^2)
cat(sprintf("Corner (0°): input dist=%.1f -> output dist=%.2f (expected: 100)\n",
            sqrt(corner_x^2 + corner_y^2), corner_r))

# Edge midpoint at 30° should project to target_radius * sqrt(0.75)
edge_x <- 80 * cos(30 * pi / 180)
edge_y <- 80 * sin(30 * pi / 180)
edge_result <- apply_hex_trunc(edge_x, edge_y, target_radius)
edge_r <- sqrt(edge_result$x^2 + edge_result$y^2)
expected_edge <- target_radius * sqrt(0.75)
cat(sprintf("Edge (30°): input dist=%.1f -> output dist=%.2f (expected: %.2f)\n",
            sqrt(edge_x^2 + edge_y^2), edge_r, expected_edge))

# Corner at 60° should project to target_radius
corner60_x <- 80 * cos(60 * pi / 180)
corner60_y <- 80 * sin(60 * pi / 180)
corner60_result <- apply_hex_trunc(corner60_x, corner60_y, target_radius)
corner60_r <- sqrt(corner60_result$x^2 + corner60_result$y^2)
cat(sprintf("Corner (60°): input dist=%.1f -> output dist=%.2f (expected: 100)\n",
            sqrt(corner60_x^2 + corner60_y^2), corner60_r))

# Edge midpoint at 90° should project to target_radius * sqrt(0.75)
edge90_x <- 80 * cos(90 * pi / 180)
edge90_y <- 80 * sin(90 * pi / 180)
edge90_result <- apply_hex_trunc(edge90_x, edge90_y, target_radius)
edge90_r <- sqrt(edge90_result$x^2 + edge90_result$y^2)
cat(sprintf("Edge (90°): input dist=%.1f -> output dist=%.2f (expected: %.2f)\n",
            sqrt(edge90_x^2 + edge90_y^2), edge90_r, expected_edge))
