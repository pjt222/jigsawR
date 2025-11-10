# Final fix for piece [0,1] diagonal issue

# Load the jigsaw generator to get the original segments
source("jigsaw.R")

# Generate 2x2 puzzle
puzzle <- generate_jigsaw_svg(seed = 42, xn = 2, yn = 2, width = 200, height = 200)

cat("Original horizontal segment 1:\n")
cat(puzzle$data$horizontal[[1]], "\n\n")

cat("Original vertical segment 1:\n")
cat(puzzle$data$vertical[[1]], "\n\n")

# The issue is that I've been using the wrong segment!
# For piece [0,1]:
# - It's the LOWER-LEFT piece
# - Top edge: horizontal segment 1 (reversed) - from (100,100) to (0,100)
# - Right edge: vertical segment 1 - from (100,100) to (100,200)
# - Bottom edge: straight border from (100,200) to (0,200)
# - Left edge: straight border from (0,200) to (0,100)

# Horizontal segment 1 (reversed for top edge of [0,1]):
horiz_seg_1_rev <- "C 80 96.5 57.09 113.63 63.17 93.63 C 69.26 73.63 29.26 73.63 43.17 93.63 C 57.09 113.63 20 98.21 0 100"

# Vertical segment 1 (for right edge of [0,1]):
vert_seg_1 <- "C 99.43 180 107.98 145.59 87.98 157.83 C 67.98 170.08 67.98 130.08 87.98 137.83 C 107.98 145.59 103.58 120 100 100"

# Create corrected piece [0,1]
piece_0_1_corrected <- paste0(
  "M 0 100 ",                    # Start at left middle
  "L 0 198 ",                    # Left edge down
  "A 2 2 0 0 0 2 200 ",          # Bottom-left corner
  "L 100 200 ",                  # Bottom edge
  vert_seg_1, " ",               # Right edge UP from (100,200) to (100,100)
  horiz_seg_1_rev, " ",          # Top edge LEFT from (100,100) to (0,100)
  "Z"                            # Close path
)

# Wait! The vertical segment goes the wrong direction
# It starts at (100,100) and goes to (100,200)
# But I need it to go from (100,200) to (100,100)
# So I need to reverse it!

# Parse and reverse the vertical segment
reverse_bezier_segment <- function(segment) {
  # For piece [0,1], vertical segment needs reversal
  # Original: (100,100) to (100,200)
  # Reversed: (100,200) to (100,100)
  
  # The reversed version (going UP from 200 to 100):
  "C 103.58 120 107.98 145.59 87.98 137.83 C 67.98 130.08 67.98 170.08 87.98 157.83 C 107.98 145.59 99.43 180 100 100"
}

vert_seg_1_reversed <- reverse_bezier_segment(vert_seg_1)

# Final corrected piece
piece_0_1_final <- paste0(
  "M 0 100 ",                    # Start at left middle  
  "L 0 198 ",                    # Left edge down
  "A 2 2 0 0 0 2 200 ",          # Bottom-left corner
  "L 100 200 ",                  # Bottom edge right
  vert_seg_1_reversed, " ",      # Right edge UP to (100,100)
  horiz_seg_1_rev, " ",          # Top edge LEFT to (0,100)
  "Z"                            # Close path
)

# Create SVG
svg_content <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="200" height="200" viewBox="0 0 200 200">
<rect width="100%%" height="100%%" fill="transparent"/>
<path id="piece-0_1" fill="none" stroke="black" stroke-width="1.5" d="%s"/>
</svg>', piece_0_1_final)

writeLines(svg_content, "output/piece_0_1_CORRECTED.svg")
cat("Created output/piece_0_1_CORRECTED.svg\n")