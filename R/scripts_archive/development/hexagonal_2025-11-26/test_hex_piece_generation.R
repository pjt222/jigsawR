#!/usr/bin/env Rscript
# Test hexagonal piece generation

source("R/hexagonal_topology.R")
source("R/rotation_utils.R")
source("R/hexagonal_puzzle.R")
source("R/hexagonal_edge_generation.R")

cat("Testing Hexagonal Piece Generation\n")
cat("===================================\n\n")

# Test 1: Generate center piece
cat("Test 1: Generate center piece (ID 1)\n")
piece1 <- generate_hex_piece(1, rings = 3, seed = 42, diameter = 240)
cat(sprintf("  Piece ID: %d\n", piece1$id))
cat(sprintf("  Ring: %d\n", piece1$ring))
cat(sprintf("  Type: %s\n", piece1$type))
cat(sprintf("  Rotation: %.2f degrees\n", piece1$rotation_degrees))
cat(sprintf("  Position: (%.2f, %.2f)\n", piece1$center_x, piece1$center_y))
stopifnot(piece1$ring == 0)
stopifnot(piece1$type == "center")
stopifnot(piece1$rotation == 0)
cat("  ✓ PASS\n\n")

# Test 2: Generate first ring piece
cat("Test 2: Generate first ring piece (ID 2)\n")
piece2 <- generate_hex_piece(2, rings = 3, seed = 42, diameter = 240)
cat(sprintf("  Piece ID: %d\n", piece2$id))
cat(sprintf("  Ring: %d\n", piece2$ring))
cat(sprintf("  Type: %s\n", piece2$type))
cat(sprintf("  Rotation: %.2f degrees\n", piece2$rotation_degrees))
cat(sprintf("  Position: (%.2f, %.2f)\n", piece2$center_x, piece2$center_y))
stopifnot(piece2$ring == 1)
stopifnot(piece2$type == "inner")
cat("  ✓ PASS\n\n")

# Test 3: Generate with separation
cat("Test 3: Generate separated piece\n")
piece3 <- generate_hex_piece(3, rings = 3, seed = 42, diameter = 240,
                             base_spacing = 60, separation_factor = 1.5)
cat(sprintf("  Piece ID: %d\n", piece3$id))
cat(sprintf("  Rotation: %.2f degrees\n", piece3$rotation_degrees))
cat(sprintf("  Position: (%.2f, %.2f)\n", piece3$center_x, piece3$center_y))
# Position should be non-zero for separated layout
stopifnot(abs(piece3$center_x) > 0.1 || abs(piece3$center_y) > 0.1)
cat("  ✓ PASS\n\n")

# Test 4: Check edge structure
cat("Test 4: Check piece has 6 edges\n")
piece <- generate_hex_piece(5, rings = 3, seed = 42, diameter = 240)
cat(sprintf("  Number of edges: %d\n", length(piece$edges)))
stopifnot(length(piece$edges) == 6)
cat("  ✓ PASS\n\n")

# Test 5: Generate all pieces
cat("Test 5: Generate all pieces for 3-ring puzzle\n")
all_pieces <- generate_all_hex_pieces(rings = 3, seed = 42)
num_pieces <- 3 * 3 * (3 - 1) + 1
cat(sprintf("  Expected pieces: %d\n", num_pieces))
cat(sprintf("  Generated pieces: %d\n", length(all_pieces)))
stopifnot(length(all_pieces) == num_pieces)
cat("  ✓ PASS\n\n")

# Test 6: Check rotation increases
cat("Test 6: Check rotation increases around ring\n")
ring1_pieces <- 2:7  # Ring 1 has 6 pieces
rotations <- sapply(ring1_pieces, function(id) {
  piece <- generate_hex_piece(id, rings = 3, seed = 42, diameter = 240)
  piece$rotation_degrees
})
cat(sprintf("  Rotations: %s\n", paste(sprintf("%.1f°", rotations), collapse = ", ")))
# Check they're all different
stopifnot(length(unique(rotations)) == 6)
# Check they're evenly spaced (60° apart)
for (i in 2:length(rotations)) {
  diff <- rotations[i] - rotations[i-1]
  # Allow for wraparound at 360°
  if (diff < 0) diff <- diff + 360
  stopifnot(abs(diff - 60) < 1)
}
cat("  ✓ PASS\n\n")

# Test 7: Edge piece classification
cat("Test 7: Classify edge pieces correctly\n")
# For 3 rings: ring 0 (center), ring 1 (inner), ring 2 (edge)
piece_ring2 <- generate_hex_piece(8, rings = 3, seed = 42, diameter = 240)
cat(sprintf("  Piece ID 8 ring: %d\n", piece_ring2$ring))
cat(sprintf("  Piece ID 8 type: %s\n", piece_ring2$type))
stopifnot(piece_ring2$ring == 2)
stopifnot(piece_ring2$type == "edge")
cat("  ✓ PASS\n\n")

cat("===================================\n")
cat("All hexagonal piece generation tests passed! ✓\n")
