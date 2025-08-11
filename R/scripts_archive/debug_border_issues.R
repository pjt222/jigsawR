# Debug Border Issues - Check [1,0] and [1,1] Carefully

cat("=== Debugging Border Issues ===\n")

# Let me re-examine the original 2x2 puzzle to understand the exact coordinates
source("R/rectangular_puzzle.R")

# Generate the reference puzzle again
puzzle_2x2 <- generate_jigsaw_svg(
  seed = 1234,
  xn = 2, yn = 2,
  width = 200, height = 200,
  tabsize = 20, jitter = 4
)

cat("Original puzzle paths:\n")
cat("Horizontal:", substr(puzzle_2x2$horizontal, 1, 200), "...\n")
cat("Vertical:", substr(puzzle_2x2$vertical, 1, 200), "...\n")
cat("Border:", substr(puzzle_2x2$border, 1, 200), "...\n")

# The issue might be that I'm not handling the border radius correctly
# Let me examine the border path more carefully

border_path <- puzzle_2x2$border
cat("\nFull border path:\n")
cat(border_path, "\n")

# The border path includes corner radius: "M 2 0 L 198 0 A 2 2 0 0 1 200 2 L 200 198 A 2 2 0 0 1 198 200 L 2 200 A 2 2 0 0 1 0 198 L 0 2 A 2 2 0 0 1 2 0"
# This means the actual corners are not at (0,0), (200,0), etc. but at (2,0), (198,0), etc.

cat("\nAnalyzing border path components:\n")
cat("- Starts at (2,0) not (0,0)\n")
cat("- Top edge: (2,0) to (198,0)\n") 
cat("- Top-right corner: arc from (198,0) to (200,2)\n")
cat("- Right edge: (200,2) to (200,198)\n")
cat("- Bottom-right corner: arc from (200,198) to (198,200)\n")
cat("- Bottom edge: (198,200) to (2,200)\n")
cat("- Bottom-left corner: arc from (2,200) to (0,198)\n")
cat("- Left edge: (0,198) to (0,2)\n") 
cat("- Top-left corner: arc from (0,2) to (2,0)\n")

# This explains the issue! The puzzle pieces need to account for the corner radius
# Let me check piece [1,0] specifically:

cat("\n=== Piece [1,0] Analysis ===\n")
cat("Expected coordinates: (100,0) to (200,100)\n")
cat("But with corner radius, the actual borders are:\n")
cat("- TOP: (100,0) to (198,0), then arc to (200,2)\n")
cat("- RIGHT: (200,2) to (200,100)\n")

# And for piece [1,1]:
cat("\n=== Piece [1,1] Analysis ===\n") 
cat("Expected coordinates: (100,100) to (200,200)\n")
cat("But with corner radius, the actual borders are:\n")
cat("- RIGHT: (200,100) to (200,198), then arc to (198,200)\n")
cat("- BOTTOM: (198,200) to (100,200)\n")

cat("\n=== Issue Identified ===\n")
cat("The straight border edges need to account for corner radius!\n")
cat("Current pieces use simple L commands, but should include the corner arcs.\n")

# Let me extract the exact border segments for each piece:

# For piece [1,0] - needs top and right border segments
cat("\nPiece [1,0] should have:\n")
cat("- TOP: L 198 0 A 2 2 0 0 1 200 2\n")
cat("- RIGHT: L 200 100\n")

# For piece [1,1] - needs right and bottom border segments  
cat("\nPiece [1,1] should have:\n")
cat("- RIGHT: L 200 198 A 2 2 0 0 1 198 200\n")
cat("- BOTTOM: L 100 200\n")

cat("\n=== Next Step ===\n")
cat("Fix the border segments to include proper corner radius handling\n")