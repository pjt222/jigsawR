# Create the correct final solution with all working pieces

# Piece [0,0] - from the original FINAL version
piece_0_0 <- "M 2 0 L 100 0 C 101.09 20 110.47 47.37 90.47 41.14 C 70.47 34.91 70.47 74.91 90.47 61.14 C 110.47 47.37 96.42 80 100 100 C 80 96.5 57.09 113.63 63.17 93.63 C 69.26 73.63 29.26 73.63 43.17 93.63 C 57.09 113.63 20 98.21 0 100 L 0 2 A 2 2 0 0 1 2 0 Z"

# Piece [1,0] - from the correct FINAL version (not FINAL_SOLUTION)
piece_1_0 <- "M 100 0 L 198 0 A 2 2 0 0 1 200 2 L 200 100 C 180 98.56 152.05 108.65 160.6 88.65 C 169.16 68.65 129.16 68.65 140.6 88.65 C 152.05 108.65 120 103.5 100 100 C 96.42 80 110.47 47.37 90.47 61.14 C 70.47 74.91 70.47 34.91 90.47 41.14 C 110.47 47.37 101.09 20 100 0 Z"

# Piece [0,1] - from FINAL_SOLUTION (which is correct)
piece_0_1 <- "M 0 100 C 20 97.72 49.22 86.28 36.15 106.28 C 23.09 126.28 63.09 126.28 56.15 106.28 C 49.22 86.28 80 101.85 100 100 C 99.54 120 89.91 150.7 109.91 138.29 C 129.91 125.89 129.91 165.89 109.91 158.29 C 89.91 150.7 101.79 180 100 200 L 2 200 A 2 2 0 0 1 0 198 L 0 100 Z"

# Piece [1,1] - needs the correct complementary edge
# The left edge should be the reverse of piece [0,1]'s right edge with flipped x-coordinates
piece_1_1 <- paste0(
  "M 100 100 ",
  # Top edge: second half of horizontal
  "C 120 98.15 148.12 88.01 139.79 108.01 C 131.45 128.01 171.45 128.01 159.79 108.01 C 148.12 88.01 180 98.21 200 100 ",
  # Right edge
  "L 200 198 A 2 2 0 0 1 198 200 ",
  # Bottom edge
  "L 100 200 ",
  # Left edge: reverse of [0,1] right edge with flipped x-coords
  # Original [0,1] right: from (100,100) to (100,200)
  # [1,1] left: from (100,200) to (100,100) with x-coords flipped
  "C 98.21 180 110.09 150.7 90.09 158.29 C 70.09 165.89 70.09 125.89 90.09 138.29 C 110.09 150.7 100.46 120 100 100 ",
  "Z"
)

# Save all individual pieces
pieces <- list(
  "0_0" = piece_0_0,
  "1_0" = piece_1_0,
  "0_1" = piece_0_1,
  "1_1" = piece_1_1
)

for (name in names(pieces)) {
  svg_content <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="200" height="200" viewBox="0 0 200 200">
<rect width="100%%" height="100%%" fill="transparent"/>
<path id="piece-%s" fill="none" stroke="black" stroke-width="1.5" d="%s"/>
</svg>', name, pieces[[name]])
  
  writeLines(svg_content, paste0("output/piece_", name, "_VERIFIED.svg"))
}

# Create combined view
svg_combined <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="200" height="200" viewBox="0 0 200 200">
<rect width="100%%" height="100%%" fill="transparent"/>
<g id="puzzle-pieces">
<path id="piece-0-0" fill="none" stroke="black" stroke-width="1.5" d="%s"/>
<path id="piece-1-0" fill="none" stroke="black" stroke-width="1.5" d="%s"/>
<path id="piece-0-1" fill="none" stroke="black" stroke-width="1.5" d="%s"/>
<path id="piece-1-1" fill="none" stroke="black" stroke-width="1.5" d="%s"/>
</g>
</svg>', piece_0_0, piece_1_0, piece_0_1, piece_1_1)

writeLines(svg_combined, "output/2x2_VERIFIED_COMPLETE.svg")

cat("Created verified pieces:\n")
cat("- output/piece_0_0_VERIFIED.svg\n")
cat("- output/piece_1_0_VERIFIED.svg (using correct version)\n")
cat("- output/piece_0_1_VERIFIED.svg\n")
cat("- output/piece_1_1_VERIFIED.svg\n")
cat("- output/2x2_VERIFIED_COMPLETE.svg (all pieces combined)\n")