#!/usr/bin/env Rscript
source("R/hexagonal_topology.R")

# Piece 2 in 3 rings, compact vs separated
rings <- 3
piece_radius <- 20

# Compact position (separation_factor = 1.0)
compact <- calculate_hex_piece_position(2, rings, piece_radius, separation_factor = 1.0)
cat("Piece 2 compact: (", compact$x, ",", compact$y, ")\n")

# Separated position (separation_factor = 1.5)
separated <- calculate_hex_piece_position(2, rings, piece_radius, separation_factor = 1.5)
cat("Piece 2 separated: (", separated$x, ",", separated$y, ")\n")

# The offset needed
cat("Offset needed: (", separated$x - compact$x, ",", separated$y - compact$y, ")\n")
