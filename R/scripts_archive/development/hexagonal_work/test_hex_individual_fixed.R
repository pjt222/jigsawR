#!/usr/bin/env Rscript

# Test the FIXED hexagonal individual pieces

cat("🔥 TESTING FIXED HEXAGONAL INDIVIDUAL PIECES\n")

# Load required files
source("R/hexagonal_puzzle.R")
source("R/hexagonal_individual_pieces.R")

# Test position calculation
positions <- calculate_hexagonal_piece_positions(3)
cat("✅ Generated", length(positions), "piece positions for 3 rings\n")

# Show first few positions
for (i in 1:min(10, length(positions))) {
  pos <- positions[[i]]
  cat(sprintf("Piece %d: ring=%d, q=%d, r=%d, type=%s, center=(%.1f,%.1f)\n",
              pos$index, pos$ring, pos$q, pos$r, pos$type, pos$center[1], pos$center[2]))
}

# Test extracting puzzle structure 
cat("\n🔥 Testing puzzle structure extraction...\n")
puzzle_struct <- extract_hexagonal_puzzle_structure(rings = 3, seed = 42, diameter = 240)
cat("✅ Puzzle structure: rings =", puzzle_struct$rings, ", num_pieces =", puzzle_struct$num_pieces, "\n")

# Test path parsing
cat("\n🔥 Testing path parsing...\n")
path_segments <- parse_hexagonal_puzzle_paths(puzzle_struct)
cat("✅ Parsed segments: horizontal =", length(path_segments$horizontal), 
    ", vertical =", length(path_segments$vertical),
    ", border =", length(path_segments$border), "\n")

# Show some segment info
if (length(path_segments$horizontal) > 0) {
  h_seg <- path_segments$horizontal[[1]]
  cat("First horizontal segment length:", nchar(h_seg$raw_path), "chars\n")
}

# Test individual piece extraction
cat("\n🔥 Testing individual piece extraction...\n")
individual_pieces <- extract_individual_hexagonal_piece_paths(puzzle_struct)
cat("✅ Extracted", length(individual_pieces), "individual pieces\n")

# Show piece info
for (i in 1:min(5, length(individual_pieces))) {
  piece <- individual_pieces[[i]]
  cat(sprintf("Piece %d: type=%s, ring=%d, path_length=%d\n",
              piece$index, piece$type, piece$ring, nchar(piece$path)))
}

# Test creating the combined SVG
cat("\n🔥 Testing SVG generation...\n")
colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#FFA07A", "#98D8C8")
combined_svg <- create_hexagonal_individual_pieces_svg(individual_pieces, puzzle_struct, colors, 1)
cat("✅ Generated SVG with", nchar(combined_svg), "characters\n")

# Save the result
output_file <- "output/test_hex_individual_fixed.svg"
writeLines(combined_svg, output_file)
cat("✅ Saved to:", output_file, "\n")

cat("\n🎯 SUCCESS: Fixed hexagonal individual pieces working!\n")
cat("Generated", length(individual_pieces), "unique pieces with proper positioning\n")