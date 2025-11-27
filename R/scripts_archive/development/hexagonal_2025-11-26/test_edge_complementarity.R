#!/usr/bin/env Rscript
# Test edge complementarity between adjacent pieces

source("R/hexagonal_topology.R")
source("R/hexagonal_bezier_generation.R")

cat("Testing edge complementarity...\n\n")

# Generate two adjacent pieces (center and one ring-1 piece)
cat("Generating pieces 1 and 2 (should be adjacent)...\n")
piece1 <- generate_hex_piece_bezier(piece_id = 1, rings = 3, seed = 42, diameter = 240, separated = FALSE)
piece2 <- generate_hex_piece_bezier(piece_id = 2, rings = 3, seed = 42, diameter = 240, separated = FALSE)

cat("Piece 1 (center):\n")
cat("  Ring:", piece1$ring, "\n")
cat("  Position:", piece1$center_x, ",", piece1$center_y, "\n")
cat("  Edges:", length(piece1$edges), "\n\n")

cat("Piece 2 (ring 1, position 0):\n")
cat("  Ring:", piece2$ring, "\n")
cat("  Position:", piece2$center_x, ",", piece2$center_y, "\n")
cat("  Edges:", length(piece2$edges), "\n\n")

# Examine edge structure
cat("Examining edge paths...\n")
cat("Piece 1, Edge 1 (forward):\n")
cat("  ", substring(piece1$edges[[1]]$forward, 1, 80), "...\n\n")

cat("Piece 1, Edge 1 (reverse):\n")
cat("  ", substring(piece1$edges[[1]]$reverse, 1, 80), "...\n\n")

# Key insight: Adjacent pieces should use reverse paths
# For a proper fit:
# - Piece A's forward edge = Piece B's reverse edge (for shared boundary)
#
# Current implementation generates edges independently
# We need to ensure edge_id is calculated based on SHARED edge identity
# not just piece_id

cat("Edge ID calculation:\n")
cat("  Piece 1, side 0: edge_id =", 1 * 10 + 0, "\n")
cat("  Piece 2, side 3: edge_id =", 2 * 10 + 3, "\n")
cat("\n")

cat("ISSUE IDENTIFIED:\n")
cat("  Current: Each piece generates its own edge_id = piece_id * 10 + side\n")
cat("  Problem: Adjacent pieces generate DIFFERENT bezier curves\n")
cat("  Solution needed: Calculate edge_id based on SHARED edge between pieces\n")
cat("\n")

cat("For proper complementarity:\n")
cat("  1. Need to identify which edges are shared between adjacent pieces\n")
cat("  2. Shared edges must use SAME edge_id for deterministic generation\n")
cat("  3. One piece uses forward path, adjacent uses reverse path\n")
