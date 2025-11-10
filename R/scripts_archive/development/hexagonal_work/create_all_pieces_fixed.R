# Create all four pieces with the diagonal fix

# Piece [0,0] - upper-left (this one is correct)
piece_0_0 <- "M 2 0 L 100 0 C 101.09 20 110.47 47.37 90.47 41.14 C 70.47 34.91 70.47 74.91 90.47 61.14 C 110.47 47.37 96.42 80 100 100 C 80 96.5 57.09 113.63 63.17 93.63 C 69.26 73.63 29.26 73.63 43.17 93.63 C 57.09 113.63 20 98.21 0 100 L 0 2 A 2 2 0 0 1 2 0 Z"

# Piece [1,0] - upper-right (this one is correct)
piece_1_0 <- "M 100 0 L 198 0 A 2 2 0 0 1 200 2 L 200 100 C 180 98.56 152.05 108.65 160.6 88.65 C 169.16 68.65 129.16 68.65 140.6 88.65 C 152.05 108.65 120 103.5 100 100 C 96.42 80 110.47 47.37 90.47 61.14 C 70.47 74.91 70.47 34.91 90.47 41.14 C 110.47 47.37 101.09 20 100 0 Z"

# Piece [0,1] - lower-left (FIXED diagonal issue)
piece_0_1 <- paste0(
  "M 0 100 ",
  # Top edge: horizontal divider from (0,100) to (100,100)
  "C 20 98.21 57.09 113.63 43.17 93.63 ",
  "C 29.26 73.63 69.26 73.63 63.17 93.63 ",
  "C 57.09 113.63 80 96.5 100 100 ",
  # Right edge: vertical divider from (100,100) to (100,200) - FIXED endpoint
  "C 99.43 180 107.98 145.59 87.98 157.83 ",
  "C 67.98 170.08 67.98 130.08 87.98 137.83 ",
  "C 107.98 145.59 103.58 120 100 200 ",  # Fixed: ends at (100,200)
  # Bottom edge
  "L 2 200 ",
  "A 2 2 0 0 1 0 198 ",
  # Left edge back to start
  "L 0 100 ",
  "Z"
)

# Piece [1,1] - lower-right (this one is correct)
piece_1_1 <- "M 100 100 C 120 103.5 152.05 108.65 140.6 88.65 C 129.16 68.65 169.16 68.65 160.6 88.65 C 152.05 108.65 180 98.56 200 100 L 200 198 A 2 2 0 0 1 198 200 L 100 200 C 103.58 120 107.98 145.59 87.98 137.83 C 67.98 130.08 67.98 170.08 87.98 157.83 C 107.98 145.59 99.43 180 100 200 Z"

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

writeLines(svg_content, "output/2x2_individual_pieces_DIAGONAL_FIXED.svg")

# Also save individual piece [0,1]
svg_0_1 <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="200" height="200" viewBox="0 0 200 200">
<rect width="100%%" height="100%%" fill="transparent"/>
<path id="piece-0_1" fill="none" stroke="black" stroke-width="1.5" d="%s"/>
</svg>', piece_0_1)

writeLines(svg_0_1, "output/piece_0_1_DIAGONAL_FIXED.svg")

cat("Created fixed versions:\n")
cat("- output/2x2_individual_pieces_DIAGONAL_FIXED.svg\n")
cat("- output/piece_0_1_DIAGONAL_FIXED.svg\n")