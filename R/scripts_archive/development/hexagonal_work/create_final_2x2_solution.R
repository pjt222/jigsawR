# Create the final 2x2 puzzle solution with all correct pieces

# From the puzzle analysis:
# Horizontal segment: M 0,100 C 20 97.72 49.22 86.28 36.15 106.28 C 23.09 126.28 63.09 126.28 56.15 106.28 C 49.22 86.28 80 101.85 100 100 C 120 98.15 148.12 88.01 139.79 108.01 C 131.45 128.01 171.45 128.01 159.79 108.01 C 148.12 88.01 180 98.21 200 100
# Vertical segment: M 100,0 C 98.19 20 92.58 50.69 112.58 43.15 C 132.58 35.61 132.58 75.61 112.58 63.15 C 92.58 50.69 100.46 80 100 100 C 99.54 120 89.91 150.7 109.91 138.29 C 129.91 125.89 129.91 165.89 109.91 158.29 C 89.91 150.7 101.79 180 100 200

# Break down segments:
# Horizontal first half (0 to 100): C 20 97.72 49.22 86.28 36.15 106.28 C 23.09 126.28 63.09 126.28 56.15 106.28 C 49.22 86.28 80 101.85 100 100
# Horizontal second half (100 to 200): C 120 98.15 148.12 88.01 139.79 108.01 C 131.45 128.01 171.45 128.01 159.79 108.01 C 148.12 88.01 180 98.21 200 100

# Vertical first half (0 to 100): C 98.19 20 92.58 50.69 112.58 43.15 C 132.58 35.61 132.58 75.61 112.58 63.15 C 92.58 50.69 100.46 80 100 100
# Vertical second half (100 to 200): C 99.54 120 89.91 150.7 109.91 138.29 C 129.91 125.89 129.91 165.89 109.91 158.29 C 89.91 150.7 101.79 180 100 200

# Function to reverse a bezier path
reverse_bezier <- function(path_segments) {
  # This is simplified - in reality we need to reverse each segment properly
  # For now, manually construct the reversed paths
}

# Piece [0,0] - upper left
piece_0_0 <- paste0(
  "M 2 0 L 100 0 ",
  # Right edge: first half of vertical (DOWN)
  "C 98.19 20 92.58 50.69 112.58 43.15 C 132.58 35.61 132.58 75.61 112.58 63.15 C 92.58 50.69 100.46 80 100 100 ",
  # Bottom edge: first half of horizontal (LEFT) 
  "C 80 101.85 49.22 86.28 56.15 106.28 C 63.09 126.28 23.09 126.28 36.15 106.28 C 49.22 86.28 20 97.72 0 100 ",
  # Left edge
  "L 0 2 A 2 2 0 0 1 2 0 Z"
)

# Piece [1,0] - upper right
# For the left edge, we need the first half of vertical REVERSED and FLIPPED
vert_first_half_rev_flip <- "C 100.46 80 67.42 50.69 87.42 63.15 C 67.42 75.61 67.42 35.61 87.42 43.15 C 107.42 50.69 101.81 20 100 0"

piece_1_0 <- paste0(
  "M 100 0 L 198 0 A 2 2 0 0 1 200 2 L 200 100 ",
  # Bottom edge: second half of horizontal (LEFT)
  "C 180 98.21 148.12 88.01 159.79 108.01 C 171.45 128.01 131.45 128.01 139.79 108.01 C 148.12 88.01 120 98.15 100 100 ",
  # Left edge: first half vertical reversed and flipped (UP)
  vert_first_half_rev_flip, " Z"
)

# Piece [0,1] - lower left  
piece_0_1 <- paste0(
  "M 0 100 ",
  # Top edge: first half of horizontal (RIGHT)
  "C 20 97.72 49.22 86.28 36.15 106.28 C 23.09 126.28 63.09 126.28 56.15 106.28 C 49.22 86.28 80 101.85 100 100 ",
  # Right edge: second half of vertical (DOWN)
  "C 99.54 120 89.91 150.7 109.91 138.29 C 129.91 125.89 129.91 165.89 109.91 158.29 C 89.91 150.7 101.79 180 100 200 ",
  # Bottom edge
  "L 2 200 A 2 2 0 0 1 0 198 ",
  # Left edge
  "L 0 100 Z"
)

# Piece [1,1] - lower right
# For the left edge, we need second half vertical REVERSED and FLIPPED
vert_second_half_rev_flip <- "C 98.21 180 110.09 150.7 90.09 158.29 C 70.09 165.89 70.09 125.89 90.09 138.29 C 110.09 150.7 100.46 120 100 100"

piece_1_1 <- paste0(
  "M 100 100 ",
  # Top edge: second half of horizontal (RIGHT)
  "C 120 98.15 148.12 88.01 139.79 108.01 C 131.45 128.01 171.45 128.01 159.79 108.01 C 148.12 88.01 180 98.21 200 100 ",
  # Right edge
  "L 200 198 A 2 2 0 0 1 198 200 ",
  # Bottom edge
  "L 100 200 ",
  # Left edge: second half vertical reversed and flipped (UP)
  vert_second_half_rev_flip, " Z"
)

# Create final combined SVG
svg_content <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="200" height="200" viewBox="0 0 200 200">
<rect width="100%%" height="100%%" fill="transparent"/>
<g id="puzzle-pieces">
<path id="piece-0-0" fill="none" stroke="black" stroke-width="1.5" d="%s"/>
<path id="piece-1-0" fill="none" stroke="black" stroke-width="1.5" d="%s"/>
<path id="piece-0-1" fill="none" stroke="black" stroke-width="1.5" d="%s"/>
<path id="piece-1-1" fill="none" stroke="black" stroke-width="1.5" d="%s"/>
</g>
<!-- Reference lines -->
<line x1="100" y1="0" x2="100" y2="200" stroke="red" stroke-width="0.5" opacity="0.3"/>
<line x1="0" y1="100" x2="200" y2="100" stroke="red" stroke-width="0.5" opacity="0.3"/>
</svg>', piece_0_0, piece_1_0, piece_0_1, piece_1_1)

writeLines(svg_content, "output/2x2_FINAL_SOLUTION.svg")

# Also save individual pieces
for (i in 1:4) {
  piece_name <- c("0_0", "1_0", "0_1", "1_1")[i]
  piece_path <- get(paste0("piece_", piece_name))
  
  svg_individual <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="200" height="200" viewBox="0 0 200 200">
<rect width="100%%" height="100%%" fill="transparent"/>
<path id="piece-%s" fill="none" stroke="black" stroke-width="1.5" d="%s"/>
</svg>', piece_name, piece_path)
  
  writeLines(svg_individual, paste0("output/piece_", piece_name, "_FINAL_SOLUTION.svg"))
}

cat("Created final solution:\n")
cat("- output/2x2_FINAL_SOLUTION.svg (all pieces with reference lines)\n")
cat("- output/piece_0_0_FINAL_SOLUTION.svg\n")
cat("- output/piece_1_0_FINAL_SOLUTION.svg\n") 
cat("- output/piece_0_1_FINAL_SOLUTION.svg\n")
cat("- output/piece_1_1_FINAL_SOLUTION.svg\n")