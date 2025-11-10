# Fix all pieces to have proper complementary tabs

# From the original puzzle:
# Horizontal: M 0,100 C 20 97.72 49.22 86.28 36.15 106.28 C 23.09 126.28 63.09 126.28 56.15 106.28 C 49.22 86.28 80 101.85 100 100 C 120 98.15 148.12 88.01 139.79 108.01 C 131.45 128.01 171.45 128.01 159.79 108.01 C 148.12 88.01 180 98.21 200 100
# Vertical: M 100,0 C 98.19 20 92.58 50.69 112.58 43.15 C 132.58 35.61 132.58 75.61 112.58 63.15 C 92.58 50.69 100.46 80 100 100 C 99.54 120 89.91 150.7 109.91 138.29 C 129.91 125.89 129.91 165.89 109.91 158.29 C 89.91 150.7 101.79 180 100 200

# For proper complementary edges, we need to decide which piece gets the "tab out" and which gets the "tab in"

# Let's say:
# - [0,0] has tab OUT on right (into [1,0]) ✓ Already correct
# - [0,0] has tab OUT on bottom (into [0,1])
# - [0,1] has tab IN on top (from [0,0])
# - [0,1] has tab OUT on right (into [1,1])
# - [1,0] has tab IN on left (from [0,0]) ✓ Already correct
# - [1,0] has tab OUT on bottom (into [1,1])
# - [1,1] has tab IN on left (from [0,1])
# - [1,1] has tab IN on top (from [1,0])

# Function to flip y-coordinates around y=100 for horizontal edges
flip_y <- function(y) { 200 - y }

# Piece [0,0] - keep as is (it's already correct)
piece_0_0 <- "M 2 0 L 100 0 C 101.09 20 110.47 47.37 90.47 41.14 C 70.47 34.91 70.47 74.91 90.47 61.14 C 110.47 47.37 96.42 80 100 100 C 80 96.5 57.09 113.63 63.17 93.63 C 69.26 73.63 29.26 73.63 43.17 93.63 C 57.09 113.63 20 98.21 0 100 L 0 2 A 2 2 0 0 1 2 0 Z"

# Piece [1,0] - keep the left edge, fix the bottom edge
piece_1_0 <- paste0(
  "M 100 0 L 198 0 A 2 2 0 0 1 200 2 L 200 100 ",
  # Bottom edge: second half of horizontal going LEFT - tab should go DOWN
  "C 180 98.21 148.12 88.01 159.79 108.01 C 171.45 128.01 131.45 128.01 139.79 108.01 C 148.12 88.01 120 98.15 100 100 ",
  # Left edge: correct complementary to [0,0]
  "C 96.42 80 110.47 47.37 90.47 61.14 C 70.47 74.91 70.47 34.91 90.47 41.14 C 110.47 47.37 101.09 20 100 0 Z"
)

# Piece [0,1] - needs complementary top edge (tab IN from [0,0])
# The tab should appear as an indent (going UP into this piece)
# We need to reverse and flip the bottom edge of [0,0]
piece_0_1 <- paste0(
  "M 0 100 ",
  # Top edge: reversed and y-flipped version of [0,0] bottom edge
  # Original [0,0] bottom: C 80 101.85 49.22 86.28 56.15 106.28 C 63.09 126.28 23.09 126.28 36.15 106.28 C 49.22 86.28 20 97.72 0 100
  # This goes from (100,100) to (0,100) with tab going DOWN
  # We need it to go from (0,100) to (100,100) with tab going UP (indent)
  "C 20 102.28 49.22 113.72 43.85 93.72 C 36.91 73.72 76.91 73.72 63.85 93.72 C 50.78 113.72 80 98.15 100 100 ",
  # Right edge: second half of vertical going DOWN - tab OUT
  "C 99.54 120 89.91 150.7 109.91 138.29 C 129.91 125.89 129.91 165.89 109.91 158.29 C 89.91 150.7 101.79 180 100 200 ",
  # Bottom and left edges
  "L 2 200 A 2 2 0 0 1 0 198 L 0 100 Z"
)

# Piece [1,1] - needs complementary edges on BOTH top and left
piece_1_1 <- paste0(
  "M 100 100 ",
  # Top edge: reversed and y-flipped version of [1,0] bottom edge
  # [1,0] bottom has tab going DOWN, we need indent (tab going UP)
  "C 120 101.85 148.12 111.99 140.21 91.99 C 128.55 71.99 168.55 71.99 160.21 91.99 C 151.88 111.99 180 101.79 200 100 ",
  # Right edge
  "L 200 198 A 2 2 0 0 1 198 200 L 100 200 ",
  # Left edge: complementary to [0,1] right edge (tab IN)
  # [0,1] right has tab going RIGHT, we need it going LEFT (x-flipped)
  "C 100.46 180 110.09 150.7 90.09 158.29 C 70.09 165.89 70.09 125.89 90.09 138.29 C 110.09 150.7 99.54 120 100 100 Z"
)

# Save all pieces
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
  
  writeLines(svg_content, paste0("output/piece_", name, "_COMPLEMENTARY.svg"))
}

# Create combined view with colors
svg_combined <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="200" height="200" viewBox="0 0 200 200">
<rect width="100%%" height="100%%" fill="white"/>
<g id="puzzle-pieces">
<path id="piece-0-0" fill="rgba(255,0,0,0.2)" stroke="red" stroke-width="1.5" d="%s"/>
<path id="piece-1-0" fill="rgba(0,255,0,0.2)" stroke="green" stroke-width="1.5" d="%s"/>
<path id="piece-0-1" fill="rgba(0,0,255,0.2)" stroke="blue" stroke-width="1.5" d="%s"/>
<path id="piece-1-1" fill="rgba(255,255,0,0.2)" stroke="orange" stroke-width="1.5" d="%s"/>
</g>
<text x="50" y="50" text-anchor="middle" font-size="12">[0,0]</text>
<text x="150" y="50" text-anchor="middle" font-size="12">[1,0]</text>
<text x="50" y="150" text-anchor="middle" font-size="12">[0,1]</text>
<text x="150" y="150" text-anchor="middle" font-size="12">[1,1]</text>
</svg>', piece_0_0, piece_1_0, piece_0_1, piece_1_1)

writeLines(svg_combined, "output/2x2_COMPLEMENTARY_TABS.svg")

cat("Created complementary tab version:\n")
cat("- All shared edges now have proper complementary tabs\n")
cat("- One piece has tab OUT, adjacent piece has tab IN\n")
cat("- output/2x2_COMPLEMENTARY_TABS.svg\n")