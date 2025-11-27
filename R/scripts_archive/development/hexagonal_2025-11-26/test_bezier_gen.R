#!/usr/bin/env Rscript
# Test hexagonal bezier piece generation

# Source necessary files
source("R/hexagonal_topology.R")
source("R/hexagonal_bezier_generation.R")

# Test: Generate center piece
cat("Testing center piece generation...\n")
piece1 <- generate_hex_piece_bezier(
  piece_id = 1,
  rings = 3,
  seed = 42,
  diameter = 240
)

cat("\nCenter piece generated successfully!\n")
cat("Piece ID:", piece1$id, "\n")
cat("Ring:", piece1$ring, "\n")
cat("Position: (", piece1$center_x, ",", piece1$center_y, ")\n")
cat("Type:", piece1$type, "\n")
cat("Number of edges:", length(piece1$edges), "\n")
cat("Path length:", nchar(piece1$path), "characters\n")
cat("Path preview:", substring(piece1$path, 1, 100), "...\n")

# Test: Generate all pieces for 2-ring puzzle
cat("\n\nTesting 2-ring puzzle (7 pieces)...\n")
all_pieces <- generate_all_hex_pieces_bezier(
  rings = 2,
  seed = 42,
  diameter = 240
)

cat("Generated", length(all_pieces), "pieces\n")
cat("Piece types:\n")
for (i in 1:length(all_pieces)) {
  p <- all_pieces[[i]]
  cat("  Piece", p$id, "- Ring:", p$ring, "- Type:", p$type, "\n")
}

cat("\nTest completed successfully!\n")
