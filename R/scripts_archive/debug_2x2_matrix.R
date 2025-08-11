# Debug 2x2 Puzzle as Matrix - Back to Basics
# Understanding the coordinate system and piece boundaries

cat("=== 2x2 Puzzle Matrix Analysis ===\n")

# Load the working rectangular puzzle functions
source("R/rectangular_puzzle.R")

# Generate a simple 2x2 puzzle to understand the structure
cat("Generating basic 2x2 puzzle...\n")
puzzle_2x2 <- generate_jigsaw_svg(
  seed = 1234,
  xn = 2,   # 2 columns  
  yn = 2,   # 2 rows
  width = 200,  # Keep it simple
  height = 200,
  tabsize = 20,
  jitter = 4
)

# Save the combined version to understand the structure
writeLines(puzzle_2x2$svg, "output/debug_2x2_combined.svg")
cat("Combined 2x2 saved to: output/debug_2x2_combined.svg\n")

# Analyze the puzzle structure
cat("\nPuzzle Parameters:\n")
cat("  Grid: 2x2 = 4 pieces total\n")
cat("  Size: 200x200 mm\n") 
cat("  Piece size: 100x100 mm each\n")

cat("\nMatrix layout:\n")
cat("  [0,0] [1,0]\n")
cat("  [0,1] [1,1]\n")

cat("\nPiece coordinates:\n")
cat("  Piece [0,0]: (0,0) to (100,100) - upper-left\n")
cat("  Piece [1,0]: (100,0) to (200,100) - upper-right\n")
cat("  Piece [0,1]: (0,100) to (200,200) - lower-left\n")
cat("  Piece [1,1]: (100,100) to (200,200) - lower-right\n")

# Show the path data structure
cat("\nGenerated paths:\n")
cat("Horizontal path (dividers between rows):\n")
cat(substr(puzzle_2x2$horizontal, 1, 100), "...\n")

cat("\nVertical path (dividers between columns):\n")  
cat(substr(puzzle_2x2$vertical, 1, 100), "...\n")

cat("\nBorder path:\n")
cat(substr(puzzle_2x2$border, 1, 100), "...\n")

cat("\n=== Analysis Complete ===\n")
cat("Next step: Focus on piece [0,0] boundary identification\n")