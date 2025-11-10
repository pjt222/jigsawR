# Fix the complementary edges between [0,1] and [1,1]

# The shared edge is at x=100 (vertical line)
# For piece [0,1], the tab protrudes LEFT (x < 100)
# For piece [1,1], the tab should protrude RIGHT (x > 100)

# To get the complementary edge, I need to:
# 1. Reverse the curve direction (which I did)
# 2. Mirror all x-coordinates around x=100 (which I didn't do!)

# Original curve from [0,1] (going DOWN from 100,100 to 100,200):
# C 99.43,180 107.98,145.59 87.98,157.83
# C 67.98,170.08 67.98,130.08 87.98,137.83  
# C 107.98,145.59 103.58,120 100,200

# For [1,1], we need the complementary curve (going UP from 100,200 to 100,100)
# We need to mirror x-coordinates: new_x = 100 + (100 - old_x) = 200 - old_x

mirror_x <- function(x) {
  200 - x
}

# Let's create the properly mirrored curve for [1,1]:
cat("Creating properly mirrored curve for piece [1,1]...\n\n")

# Original points from [0,1] right edge:
# Start: (100,100)
# Seg1: C (99.43,180) (107.98,145.59) to (87.98,157.83)
# Seg2: C (67.98,170.08) (67.98,130.08) to (87.98,137.83)
# Seg3: C (107.98,145.59) (103.58,120) to (100,200)

# Mirrored and reversed for [1,1] left edge:
# Start: (100,200)
# Need to reverse segments and mirror x-coords

# Reversed segments (going from 200 to 100):
# Seg3 reversed: from (100,200) via (mirror(103.58),120) (mirror(107.98),145.59) to (mirror(87.98),137.83)
# Seg2 reversed: from (mirror(87.98),137.83) via (mirror(67.98),130.08) (mirror(67.98),170.08) to (mirror(87.98),157.83)
# Seg1 reversed: from (mirror(87.98),157.83) via (mirror(107.98),145.59) (mirror(99.43),180) to (100,100)

piece_1_1_left_edge_fixed <- paste0(
  "C ", mirror_x(103.58), " 120 ", mirror_x(107.98), " 145.59 ", mirror_x(87.98), " 137.83 ",
  "C ", mirror_x(67.98), " 130.08 ", mirror_x(67.98), " 170.08 ", mirror_x(87.98), " 157.83 ",
  "C ", mirror_x(107.98), " 145.59 ", mirror_x(99.43), " 180 100 100"
)

cat("Fixed left edge for [1,1]:\n")
cat(piece_1_1_left_edge_fixed, "\n\n")

# Calculate mirrored values:
cat("Mirrored x-coordinates:\n")
cat("103.58 -> ", mirror_x(103.58), "\n")
cat("107.98 -> ", mirror_x(107.98), "\n")
cat("87.98 -> ", mirror_x(87.98), "\n")
cat("67.98 -> ", mirror_x(67.98), "\n")
cat("99.43 -> ", mirror_x(99.43), "\n\n")

# Create the complete fixed piece [1,1]
piece_1_1_fixed <- paste0(
  "M 100 100 ",
  # Top edge (horizontal divider) - this stays the same
  "C 120 103.5 152.05 108.65 140.6 88.65 ",
  "C 129.16 68.65 169.16 68.65 160.6 88.65 ",
  "C 152.05 108.65 180 98.56 200 100 ",
  # Right edge
  "L 200 198 A 2 2 0 0 1 198 200 ",
  # Bottom edge
  "L 100 200 ",
  # Left edge (FIXED with mirrored x-coordinates)
  piece_1_1_left_edge_fixed, " ",
  "Z"
)

# Create SVG files
svg_1_1 <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="200" height="200" viewBox="0 0 200 200">
<rect width="100%%" height="100%%" fill="transparent"/>
<path id="piece-1_1" fill="none" stroke="black" stroke-width="1.5" d="%s"/>
</svg>', piece_1_1_fixed)

writeLines(svg_1_1, "output/piece_1_1_COMPLEMENTARY_FIXED.svg")

# Also create a combined view showing both [0,1] and [1,1] with fixed edges
piece_0_1_correct <- "M 0 100 C 20 98.21 57.09 113.63 43.17 93.63 C 29.26 73.63 69.26 73.63 63.17 93.63 C 57.09 113.63 80 96.5 100 100 C 99.43 180 107.98 145.59 87.98 157.83 C 67.98 170.08 67.98 130.08 87.98 137.83 C 107.98 145.59 103.58 120 100 200 L 2 200 A 2 2 0 0 1 0 198 L 0 100 Z"

svg_combined <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="200" height="200" viewBox="0 0 200 200">
<rect width="100%%" height="100%%" fill="transparent"/>
<g id="pieces">
<path id="piece-0_1" fill="none" stroke="blue" stroke-width="1.5" d="%s"/>
<path id="piece-1_1" fill="none" stroke="red" stroke-width="1.5" d="%s"/>
</g>
</svg>', piece_0_1_correct, piece_1_1_fixed)

writeLines(svg_combined, "output/pieces_0_1_and_1_1_complementary.svg")

cat("Created:\n")
cat("- output/piece_1_1_COMPLEMENTARY_FIXED.svg\n")
cat("- output/pieces_0_1_and_1_1_complementary.svg (blue=[0,1], red=[1,1])\n")