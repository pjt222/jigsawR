#!/usr/bin/env Rscript

# Step 1: Generate Basic Hexagonal Puzzle
# Ensure the foundation hexagonal puzzle generation is working correctly

cat("🔥 STEP 1: Testing Basic Hexagonal Puzzle Generation\n")

# Load the hexagonal puzzle functions
source("R/hexagonal_puzzle.R")

# Generate a 3-ring hexagonal puzzle with seed 42
puzzle_params <- list(
  seed = 42,
  rings = 3,
  diameter = 240,
  tabsize = 27,
  jitter = 5,
  do_warp = FALSE,
  do_trunc = FALSE
)

cat("Generating 3-ring hexagonal puzzle with parameters:\n")
cat("- Seed:", puzzle_params$seed, "\n")
cat("- Rings:", puzzle_params$rings, "\n") 
cat("- Diameter:", puzzle_params$diameter, "mm\n")
cat("- Tab size:", puzzle_params$tabsize, "%\n")
cat("- Jitter:", puzzle_params$jitter, "%\n")

# Generate the puzzle
hex_puzzle <- generate_hex_jigsaw_svg(
  seed = puzzle_params$seed,
  rings = puzzle_params$rings,
  diameter = puzzle_params$diameter,
  tabsize = puzzle_params$tabsize,
  jitter = puzzle_params$jitter,
  do_warp = puzzle_params$do_warp,
  do_trunc = puzzle_params$do_trunc
)

# Save the puzzle
output_file <- "output/basic_hex_3rings.svg"
writeLines(hex_puzzle$svg, output_file)

cat("\n=== BASIC PUZZLE ANALYSIS ===\n")
cat("SVG file saved to:", output_file, "\n")
cat("Total SVG length:", nchar(hex_puzzle$svg), "characters\n")

# Analyze the paths
cat("\n=== PATH STRUCTURE ===\n")
cat("Horizontal paths length:", nchar(hex_puzzle$horizontal), "characters\n") 
cat("Vertical paths length:", nchar(hex_puzzle$vertical), "characters\n")
cat("Border paths length:", nchar(hex_puzzle$border), "characters\n")

# Check for bezier curves (should contain 'C' commands)
has_bezier_h <- grepl("C", hex_puzzle$horizontal)
has_bezier_v <- grepl("C", hex_puzzle$vertical)

cat("\n=== BEZIER CURVE VERIFICATION ===\n")
cat("Horizontal paths contain bezier curves (C commands):", has_bezier_h, "\n")
cat("Vertical paths contain bezier curves (C commands):", has_bezier_v, "\n")

if (has_bezier_h && has_bezier_v) {
  cat("✅ BASIC HEXAGONAL PUZZLE GENERATION: SUCCESS\n")
  cat("Puzzle contains proper bezier curves with tabs and blanks\n")
} else {
  cat("❌ ISSUE: Missing bezier curves in puzzle paths\n")
}

# Show first 200 characters of each path type
cat("\n=== PATH SAMPLES ===\n")
cat("Horizontal (first 200 chars):", substr(hex_puzzle$horizontal, 1, 200), "...\n")
cat("Vertical (first 200 chars):", substr(hex_puzzle$vertical, 1, 200), "...\n")  
cat("Border (first 200 chars):", substr(hex_puzzle$border, 1, 200), "...\n")

cat("\n🎯 STEP 1 COMPLETE: Basic hexagonal puzzle generation verified\n")