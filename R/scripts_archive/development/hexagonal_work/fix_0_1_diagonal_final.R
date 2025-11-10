# Fix the diagonal issue in piece [0,1]

# The issue identified: piece [0,1] has a diagonal from (100,100) to (2,200)
# This happens because the path construction is wrong

# Let me carefully construct the correct path for piece [0,1]
# Piece [0,1] is the LOWER-LEFT piece

# Looking at the current wrong path:
# M 0 100 - starts at left middle (correct)
# [curves] ending at (100, 100) - this should be the TOP edge going from (0,100) to (100,100)
# [curves] ending at (100, 100) - this should be the RIGHT edge going from (100,100) to (100,200)
# L 2 200 - this diagonal is wrong!

# The correct path should be:
# 1. Start at (0,100)
# 2. Top edge with tab from (0,100) to (100,100)
# 3. Right edge with tab from (100,100) to (100,200)
# 4. Bottom edge straight from (100,200) to (2,200) with corner to (0,198)
# 5. Left edge straight from (0,198) to (0,100)

# Based on the pattern from the working pieces, here's the fix:

piece_0_1_fixed <- paste0(
  "M 0 100 ",
  # Top edge: horizontal divider from (0,100) to (100,100)
  "C 20 98.21 57.09 113.63 43.17 93.63 ",
  "C 29.26 73.63 69.26 73.63 63.17 93.63 ",
  "C 57.09 113.63 80 96.5 100 100 ",
  # Right edge: vertical divider from (100,100) to (100,200)
  "C 99.43 180 107.98 145.59 87.98 157.83 ",
  "C 67.98 170.08 67.98 130.08 87.98 137.83 ",
  "C 107.98 145.59 103.58 120 100 200 ",  # This should end at (100,200) not (100,100)!
  # Bottom edge
  "L 2 200 ",
  "A 2 2 0 0 1 0 198 ",
  # Left edge back to start
  "L 0 100 ",
  "Z"
)

# Create the corrected SVG
svg_content <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="200" height="200" viewBox="0 0 200 200">
<rect width="100%%" height="100%%" fill="transparent"/>
<path id="piece-0_1" fill="none" stroke="black" stroke-width="1.5" d="%s"/>
</svg>', piece_0_1_fixed)

writeLines(svg_content, "output/piece_0_1_DIAGONAL_FIX.svg")
cat("Created output/piece_0_1_DIAGONAL_FIX.svg with corrected end point for vertical edge\n")