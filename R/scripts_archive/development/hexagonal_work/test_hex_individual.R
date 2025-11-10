#!/usr/bin/env Rscript

# Test script for updated hexagonal individual piece extraction

cat("🔧 Testing UPDATED hexagonal individual piece extraction\n")
cat("Now using actual path parsing and shared edge principle\n")

# Load required functions  
source("R/hexagonal_puzzle.R")
source("R/hexagonal_individual_pieces.R")

cat("Functions loaded successfully\n")

# Test with basic parameters
rings <- 3
seed <- 42
diameter <- 200

cat("Generating hexagonal individual pieces with actual path parsing...\n")
cat("Parameters: rings =", rings, ", seed =", seed, ", diameter =", diameter, "\n")

# Run the updated implementation
result <- generate_hexagonal_individual_pieces(
  rings = rings, 
  seed = seed, 
  diameter = diameter,
  save_files = TRUE
)

cat("✅ SUCCESS!\n")
cat("Generated", length(result$pieces), "individual pieces\n")
cat("Method used:", result$method, "\n")

# Show details of first few pieces
for (i in 1:min(3, length(result$pieces))) {
  piece <- result$pieces[[i]]
  cat("Piece", i, "- Type:", piece$type, "- Center: (", 
      round(piece$center[1], 1), ",", round(piece$center[2], 1), ")\n")
  cat("  Path length:", nchar(piece$path), "characters\n")
}

# Show path parsing results
cat("\nPath parsing verification:\n")
test_struct <- extract_hexagonal_puzzle_structure(rings = 3, seed = 42, diameter = 200)
cat("Original puzzle paths:\n")
cat("- Horizontal path length:", nchar(test_struct$paths$horizontal), "chars\n")
cat("- Vertical path length:", nchar(test_struct$paths$vertical), "chars\n") 
cat("- Border path length:", nchar(test_struct$paths$border), "chars\n")

cat("Files saved:", length(result$files), "files\n")
if (length(result$files) > 0) {
  cat("Main file:", result$files[1], "\n")
}

cat("\n🎯 Updated implementation with path parsing completed!\n")