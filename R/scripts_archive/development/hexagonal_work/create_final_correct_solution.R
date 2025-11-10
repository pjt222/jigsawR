# Create the final solution using the correct pieces

# Piece [0,0] - upper left (from FINAL version)
piece_0_0 <- "M 2 0 L 100 0 C 101.09 20 110.47 47.37 90.47 41.14 C 70.47 34.91 70.47 74.91 90.47 61.14 C 110.47 47.37 96.42 80 100 100 C 80 96.5 57.09 113.63 63.17 93.63 C 69.26 73.63 29.26 73.63 43.17 93.63 C 57.09 113.63 20 98.21 0 100 L 0 2 A 2 2 0 0 1 2 0 Z"

# Piece [1,0] - upper right (from FINAL version - this was correct)
piece_1_0 <- "M 100 0 L 198 0 A 2 2 0 0 1 200 2 L 200 100 C 180 98.56 152.05 108.65 160.6 88.65 C 169.16 68.65 129.16 68.65 140.6 88.65 C 152.05 108.65 120 103.5 100 100 C 96.42 80 110.47 47.37 90.47 61.14 C 70.47 74.91 70.47 34.91 90.47 41.14 C 110.47 47.37 101.09 20 100 0 Z"

# Piece [0,1] - lower left (with correct tab from actual puzzle)
# Using the actual horizontal and vertical segments from the puzzle
piece_0_1 <- paste0(
  "M 0 100 ",
  # Top edge: first half of horizontal (from actual puzzle)
  "C 20 97.72 49.22 86.28 36.15 106.28 C 23.09 126.28 63.09 126.28 56.15 106.28 C 49.22 86.28 80 101.85 100 100 ",
  # Right edge: second half of vertical (from actual puzzle)
  "C 99.54 120 89.91 150.7 109.91 138.29 C 129.91 125.89 129.91 165.89 109.91 158.29 C 89.91 150.7 101.79 180 100 200 ",
  # Bottom edge
  "L 2 200 A 2 2 0 0 1 0 198 ",
  # Left edge
  "L 0 100 Z"
)

# Piece [1,1] - lower right
# The left edge needs to be the complement of piece [0,1]'s right edge
# This means reversing the path and flipping x-coordinates
piece_1_1 <- paste0(
  "M 100 100 ",
  # Top edge: second half of horizontal (from actual puzzle)
  "C 120 98.15 148.12 88.01 139.79 108.01 C 131.45 128.01 171.45 128.01 159.79 108.01 C 148.12 88.01 180 98.21 200 100 ",
  # Right edge
  "L 200 198 A 2 2 0 0 1 198 200 ",
  # Bottom edge
  "L 100 200 ",
  # Left edge: reversed and x-flipped second half of vertical (UP from 200 to 100)
  "C 98.21 180 110.09 150.7 90.09 158.29 C 70.09 165.89 70.09 125.89 90.09 138.29 C 110.09 150.7 100.46 120 100 100 ",
  "Z"
)

# Create combined SVG
svg_content <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="200" height="200" viewBox="0 0 200 200">
<rect width="100%%" height="100%%" fill="transparent"/>
<g id="puzzle-pieces">
<path id="piece-0-0" fill="none" stroke="black" stroke-width="1.5" d="%s"/>
<path id="piece-1-0" fill="none" stroke="black" stroke-width="1.5" d="%s"/>
<path id="piece-0-1" fill="none" stroke="black" stroke-width="1.5" d="%s"/>
<path id="piece-1-1" fill="none" stroke="black" stroke-width="1.5" d="%s"/>
</g>
</svg>', piece_0_0, piece_1_0, piece_0_1, piece_1_1)

writeLines(svg_content, "output/2x2_CORRECT_FINAL.svg")

# Also save individual pieces
writeLines(sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="200" height="200" viewBox="0 0 200 200">
<rect width="100%%" height="100%%" fill="transparent"/>
<path id="piece-0_1" fill="none" stroke="black" stroke-width="1.5" d="%s"/>
</svg>', piece_0_1), "output/piece_0_1_CORRECT_FINAL.svg")

writeLines(sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="200" height="200" viewBox="0 0 200 200">
<rect width="100%%" height="100%%" fill="transparent"/>
<path id="piece-1_1" fill="none" stroke="black" stroke-width="1.5" d="%s"/>
</svg>', piece_1_1), "output/piece_1_1_CORRECT_FINAL.svg")

cat("Created:\n")
cat("- output/2x2_CORRECT_FINAL.svg (all four pieces)\n")
cat("- output/piece_0_1_CORRECT_FINAL.svg\n")
cat("- output/piece_1_1_CORRECT_FINAL.svg\n")
cat("\nUsing the original correct pieces [0,0] and [1,0] with the fixed [0,1] and [1,1]\n")