# Fix the vertical segment loop issue

# The problem: I've been using segments that create a loop
# The vertical divider has TWO parts:
# 1. TOP half: (100,0) to (100,100) - for pieces [0,0] and [1,0]
# 2. BOTTOM half: (100,100) to (100,200) - for pieces [0,1] and [1,1]

# From the 2x2 puzzle generation, let me reconstruct the correct segments

# For a 2x2 puzzle, the vertical divider goes from (100,0) to (100,200)
# The tab in the middle will be either on the TOP half or BOTTOM half

# Based on the working pieces [0,0] and [1,0], the tab is in the TOP half
# So the BOTTOM half should be relatively straight or have a complementary shape

# Looking at the horizontal divider for reference:
# The horizontal divider from (0,100) to (100,100) has this curve:
# "C 20 98.21 57.09 113.63 43.17 93.63 C 29.26 73.63 69.26 73.63 63.17 93.63 C 57.09 113.63 80 96.5 100 100"

# For piece [0,1], the correct segments should be:
# 1. Top edge: horizontal segment from (0,100) to (100,100) 
# 2. Right edge: BOTTOM HALF of vertical segment from (100,100) to (100,200)

# Since the tab was in the TOP half, the BOTTOM half might be simpler
# Let me construct a proper monotonic curve for the bottom half

cat("Creating correct vertical segment for piece [0,1]...\n")

# The bottom half should go from (100,100) to (100,200)
# Based on the pattern, if the top half has the main tab, 
# the bottom half should be relatively straight or have a minor curve

# Looking at the pattern from successful pieces, a vertical segment 
# from (100,100) to (100,200) could be:
vertical_bottom_half <- "C 103.58 120 99.43 180 100 200"

# This creates a gentle curve that:
# - Starts at (100,100)
# - Control points at (103.58,120) and (99.43,180)
# - Ends at (100,200)
# - Y-values: 100 → 120 → 180 → 200 (monotonically increasing!)

# Create the corrected piece [0,1]
piece_0_1_no_loop <- paste0(
  "M 0 100 ",
  # Top edge: horizontal segment from (0,100) to (100,100)
  "C 20 98.21 57.09 113.63 43.17 93.63 ",
  "C 29.26 73.63 69.26 73.63 63.17 93.63 ",
  "C 57.09 113.63 80 96.5 100 100 ",
  # Right edge: simple curve from (100,100) to (100,200)
  vertical_bottom_half, " ",
  # Bottom edge
  "L 2 200 ",
  "A 2 2 0 0 1 0 198 ",
  # Left edge
  "L 0 100 ",
  "Z"
)

# Create SVG
svg_content <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="200" height="200" viewBox="0 0 200 200">
<rect width="100%%" height="100%%" fill="transparent"/>
<path id="piece-0_1" fill="none" stroke="black" stroke-width="1.5" d="%s"/>
</svg>', piece_0_1_no_loop)

writeLines(svg_content, "output/piece_0_1_NO_LOOP.svg")

# Also need to update piece [1,1] to have the complementary edge
# For [1,1], the left edge goes from (100,200) UP to (100,100)
# This should be the reverse of the curve above
vertical_bottom_half_reversed <- "C 99.43 180 103.58 120 100 100"

piece_1_1_fixed <- paste0(
  "M 100 100 ",
  # Top edge
  "C 120 103.5 152.05 108.65 140.6 88.65 ",
  "C 129.16 68.65 169.16 68.65 160.6 88.65 ",
  "C 152.05 108.65 180 98.56 200 100 ",
  # Right edge
  "L 200 198 ",
  "A 2 2 0 0 1 198 200 ",
  # Bottom edge
  "L 100 200 ",
  # Left edge: reversed simple curve
  vertical_bottom_half_reversed, " ",
  "Z"
)

# Create combined view
svg_combined <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="200" height="200" viewBox="0 0 200 200">
<rect width="100%%" height="100%%" fill="transparent"/>
<g id="puzzle-pieces">
<path id="piece-0-0" fill="none" stroke="black" stroke-width="1.5" d="%s"/>
<path id="piece-1-0" fill="none" stroke="black" stroke-width="1.5" d="%s"/>
<path id="piece-0-1" fill="none" stroke="green" stroke-width="1.5" d="%s"/>
<path id="piece-1-1" fill="none" stroke="blue" stroke-width="1.5" d="%s"/>
</g>
</svg>', 
  "M 2 0 L 100 0 C 101.09 20 110.47 47.37 90.47 41.14 C 70.47 34.91 70.47 74.91 90.47 61.14 C 110.47 47.37 96.42 80 100 100 C 80 96.5 57.09 113.63 63.17 93.63 C 69.26 73.63 29.26 73.63 43.17 93.63 C 57.09 113.63 20 98.21 0 100 L 0 2 A 2 2 0 0 1 2 0 Z",
  "M 100 0 L 198 0 A 2 2 0 0 1 200 2 L 200 100 C 180 98.56 152.05 108.65 160.6 88.65 C 169.16 68.65 129.16 68.65 140.6 88.65 C 152.05 108.65 120 103.5 100 100 C 96.42 80 110.47 47.37 90.47 61.14 C 70.47 74.91 70.47 34.91 90.47 41.14 C 110.47 47.37 101.09 20 100 0 Z",
  piece_0_1_no_loop,
  piece_1_1_fixed
)

writeLines(svg_combined, "output/2x2_all_pieces_NO_LOOPS.svg")

cat("Created:\n")
cat("- output/piece_0_1_NO_LOOP.svg (fixed vertical segment without loop)\n")
cat("- output/2x2_all_pieces_NO_LOOPS.svg (all four pieces)\n")