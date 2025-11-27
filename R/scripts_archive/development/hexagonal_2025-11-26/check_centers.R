#!/usr/bin/env Rscript
source("R/hexagonal_topology.R")

rings <- 3
piece_radius <- 240 / (rings * 4)
hex_size <- 2 / sqrt(3) * piece_radius

# Get axial coords for pieces 2 and 9
ax2 <- map_piece_id_to_axial(2, 3)
ax9 <- map_piece_id_to_axial(9, 3)

cat("Piece 2: q=", ax2$q, ", r=", ax2$r, "\n")
cat("Piece 9: q=", ax9$q, ", r=", ax9$r, "\n")

# Convert to cartesian
c2 <- axial_to_cartesian(ax2$q, ax2$r, hex_size)
c9 <- axial_to_cartesian(ax9$q, ax9$r, hex_size)

cat("\nPiece 2 center: (", c2$x, ",", c2$y, ")\n")
cat("Piece 9 center: (", c9$x, ",", c9$y, ")\n")

# Distance between centers
dist <- sqrt((c9$x - c2$x)^2 + (c9$y - c2$y)^2)
cat("\nDistance between centers:", dist, "\n")
cat("Expected distance (2 * piece_radius):", 2 * piece_radius, "\n")

# Also calculate sqrt(3) * piece_radius for vertical neighbors
vert_dist <- sqrt(3) * piece_radius
cat("Expected distance for vertical neighbors:", vert_dist, "\n")
cat("Expected distance using hex math: sqrt(3) * hex_size:", sqrt(3) * hex_size, "\n")
