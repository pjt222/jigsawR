#!/usr/bin/env Rscript
source("R/hexagonal_topology.R")

# Check if vertices of adjacent pieces actually touch
rings <- 3
piece_radius <- 240 / (rings * 4)  # diameter / (rings * 4)
ring_spacing <- sqrt(3) * piece_radius

cat("Piece radius:", piece_radius, "\n")
cat("Ring spacing:", ring_spacing, "\n\n")

# Calculate center positions for piece 2 and piece 8
ring_info_2 <- map_piece_id_to_ring(2, rings)
ring_info_8 <- map_piece_id_to_ring(8, rings)

cat("Piece 2 (ring 1, position 0):\n")
cat("  Center:", ring_info_2$ring * ring_spacing * cos(ring_info_2$angle), ",",
    ring_info_2$ring * ring_spacing * sin(ring_info_2$angle), "\n\n")

cat("Piece 8 (ring 2, position 0):\n")
cat("  Center:", ring_info_8$ring * ring_spacing * cos(ring_info_8$angle), ",",
    ring_info_8$ring * ring_spacing * sin(ring_info_8$angle), "\n\n")

# Distance between centers
dist <- ring_spacing  # Should be one ring spacing apart
cat("Distance between ring 1 and ring 2 centers:", dist, "\n")
cat("Distance between adjacent hexagon centers (should be):", sqrt(3) * piece_radius, "\n")
