# Compare complete mode scaling with separated mode scaling

setwd("D:/dev/p/jigsawR")

library(devtools)
load_all()

cat("==========================================\n")
cat("DEBUG: Compare scaling methods\n")
cat("==========================================\n\n")

# Test for 3-ring and 7-ring
for (rings in c(3, 7)) {
  diameter <- if (rings == 3) 240 else 400
  radius <- diameter / 2
  n <- rings

  cat(sprintf("\n=== %d-ring puzzle (diameter = %d) ===\n", rings, diameter))

  # Complete mode scaling factor (from hexagonal_puzzle.R line 94)
  complete_scale <- (1.0 / (2 * n - 4.0/3)) * radius
  cat(sprintf("Complete mode scale factor: %.4f\n", complete_scale))

  # Separated mode piece_radius
  sep_piece_radius <- diameter / (rings * 4)
  cat(sprintf("Separated mode piece_radius: %.4f\n", sep_piece_radius))

  # Now calculate the actual max vertex distance in separated mode
  cat("\nActual boundary vertex distances (separated mode calculation):\n")

  num_pieces <- 3 * n * (n - 1) + 1
  all_dists <- c()

  for (piece_id in 1:num_pieces) {
    axial_coords <- map_piece_id_to_axial(piece_id, n)
    cart_coords <- axial_to_cartesian(
      q = axial_coords$q,
      r = axial_coords$r,
      hex_size = sep_piece_radius
    )

    for (i in 0:5) {
      vertex_angle <- i * pi / 3
      vx <- cart_coords$x + sep_piece_radius * cos(vertex_angle)
      vy <- cart_coords$y + sep_piece_radius * sin(vertex_angle)
      dist <- sqrt(vx^2 + vy^2)
      all_dists <- c(all_dists, dist)
    }
  }

  cat(sprintf("  Max vertex distance: %.3f\n", max(all_dists)))
  cat(sprintf("  Min vertex distance: %.3f\n", min(all_dists)))
  cat(sprintf("  Target (radius): %.3f\n", radius))
  cat(sprintf("  Ratio max/radius: %.3f\n", max(all_dists) / radius))

  # The scaling factor we need
  scaling_needed <- radius / max(all_dists)
  cat(sprintf("\nScaling factor needed to fit diameter: %.4f\n", scaling_needed))
  cat(sprintf("Corrected piece_radius: %.4f\n", sep_piece_radius * scaling_needed))
}
