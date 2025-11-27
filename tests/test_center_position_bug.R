#!/usr/bin/env Rscript
# Investigate the doubled center position bug

source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")

cat("=== INVESTIGATING CENTER POSITION DISCREPANCY ===\n\n")

rings <- 2
diameter <- 240
piece_radius <- diameter / (rings * 4)

cat(sprintf("diameter = %d\n", diameter))
cat(sprintf("rings = %d\n", rings))
cat(sprintf("piece_radius = diameter / (rings * 4) = %d / (%d * 4) = %.2f\n\n",
            diameter, rings, piece_radius))

# Test 1: Direct call to calculate_hex_piece_position
cat("=== Direct call to calculate_hex_piece_position ===\n")
for (piece_id in 1:3) {
  pos <- calculate_hex_piece_position(
    piece_id = piece_id,
    rings = rings,
    piece_radius = piece_radius,
    separation_factor = 1.0
  )
  cat(sprintf("Piece %d: (%.2f, %.2f)\n", piece_id, pos$x, pos$y))
}

# Test 2: Check what generate_hex_edge_map uses
cat("\n=== Inside generate_hex_edge_map ===\n")
cat("Code uses: hex_size <- piece_radius\n")
cat("Then: axial_to_cartesian(q, r, hex_size = piece_radius)\n\n")

# Test 3: Check what generate_hex_pieces_with_edge_map uses
cat("=== Inside generate_hex_pieces_with_edge_map ===\n")
cat("At line ~236-241, it calculates compact_pos:\n")
cat("  compact_pos <- calculate_hex_piece_position(\n")
cat("    piece_id = piece_id,\n")
cat("    rings = rings,\n")
cat("    piece_radius = piece_radius,  # This is diameter/(rings*4)\n")
cat("    separation_factor = 1.0\n")
cat("  )\n\n")

# Test 4: Check the separated position calculation
cat("=== Separated Position Calculation ===\n")
cat("If separated=TRUE and base_spacing is provided:\n")
cat("  separated_pos <- calculate_hex_piece_position(\n")
cat("    piece_id = piece_id,\n")
cat("    rings = rings,\n")
cat("    base_spacing = base_spacing,  # This is passed in!\n")
cat("    separation_factor = separation_factor\n")
cat("  )\n\n")

# What is base_spacing when called from generate_separated_hexagonal_svg?
cat("=== In generate_separated_hexagonal_svg (hexagonal_separation.R) ===\n")
cat("Line 191-192:\n")
cat("  base_spacing <- piece_radius * 2\n")
cat("  separation_factor <- 1.0 + (offset / base_spacing)\n\n")

base_spacing_from_caller <- piece_radius * 2
cat(sprintf("So base_spacing passed in = piece_radius * 2 = %.2f * 2 = %.2f\n\n",
            piece_radius, base_spacing_from_caller))

# Test 5: Compare the two
cat("=== COMPARISON ===\n")
cat("When calculate_hex_piece_position is called with:\n")
cat(sprintf("  A) piece_radius = %.2f (direct)\n", piece_radius))
cat(sprintf("  B) base_spacing = %.2f (from generate_hex_pieces_with_edge_map)\n\n", base_spacing_from_caller))

# Case A - what calculate_hex_piece_position does
cat("Case A: calculate_hex_piece_position(piece_radius=30):\n")
pos_a <- calculate_hex_piece_position(piece_id = 2, rings = rings, piece_radius = piece_radius, separation_factor = 1.0)
cat(sprintf("  Piece 2: (%.2f, %.2f)\n\n", pos_a$x, pos_a$y))

# Case B - what happens when base_spacing is used
cat("Case B: calculate_hex_piece_position(base_spacing=60):\n")
cat("The function has backward compat: if piece_radius is NULL and base_spacing is provided,\n")
cat("  piece_radius <- base_spacing\n")
pos_b <- calculate_hex_piece_position(piece_id = 2, rings = rings, base_spacing = base_spacing_from_caller, separation_factor = 1.0)
cat(sprintf("  Piece 2: (%.2f, %.2f)\n\n", pos_b$x, pos_b$y))

cat("=== ROOT CAUSE IDENTIFIED ===\n")
cat("The problem is a NAMING CONFLICT!\n\n")
cat("In generate_separated_hexagonal_svg():\n")
cat("  - It calculates base_spacing = piece_radius * 2 (intended as 'spacing between pieces')\n")
cat("  - It passes this to generate_hex_pieces_with_edge_map(base_spacing=...)\n\n")
cat("In generate_hex_pieces_with_edge_map():\n")
cat("  - At line 220-222: if (separated && is.null(base_spacing)) base_spacing <- diameter/(rings*2)\n")
cat("  - It passes base_spacing to calculate_hex_piece_position(base_spacing=...)\n\n")
cat("In calculate_hex_piece_position():\n")
cat("  - It treats base_spacing as PIECE_RADIUS due to backward compatibility!\n")
cat("  - Line 231-232: if (is.null(piece_radius) && !is.null(base_spacing)) piece_radius <- base_spacing\n\n")
cat("So when:\n")
cat("  - Caller intends: 'space pieces 60mm apart'\n")
cat("  - Function interprets: 'piece_radius is 60mm' (instead of 30mm)\n")
cat("  - Result: positions are DOUBLED!\n")

cat("\n=== VERIFICATION ===\n")
# The positions with base_spacing=60 should be exactly 2x the positions with piece_radius=30
cat(sprintf("Position A (piece_radius=30): (%.2f, %.2f)\n", pos_a$x, pos_a$y))
cat(sprintf("Position B (base_spacing=60): (%.2f, %.2f)\n", pos_b$x, pos_b$y))
cat(sprintf("Ratio B/A: (%.2f, %.2f)\n", pos_b$x/pos_a$x, pos_b$y/pos_a$y))

cat("\nYes! The ratio is exactly 2x because base_spacing (60) is used as piece_radius\n")
cat("instead of the actual piece_radius (30).\n")
