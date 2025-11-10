# Analyze the correct vertical segment from the actual puzzle

cat("Full vertical segment from the puzzle:\n")
vert_seg <- "M 100,0 C 98.19 20 92.58 50.69 112.58 43.15 C 132.58 35.61 132.58 75.61 112.58 63.15 C 92.58 50.69 100.46 80 100 100 C 99.54 120 89.91 150.7 109.91 138.29 C 129.91 125.89 129.91 165.89 109.91 158.29 C 89.91 150.7 101.79 180 100 200"

cat(vert_seg, "\n\n")

cat("Breaking down the vertical segment:\n\n")

cat("FIRST HALF (0 to 100):\n")
cat("Start: (100,0)\n")
cat("Seg 1: C 98.19,20 92.58,50.69 112.58,43.15\n")
cat("       Tab goes RIGHT (x > 100)\n")
cat("Seg 2: C 132.58,35.61 132.58,75.61 112.58,63.15\n")
cat("Seg 3: C 92.58,50.69 100.46,80 100,100\n")
cat("End: (100,100)\n\n")

cat("SECOND HALF (100 to 200):\n")
cat("Start: (100,100)\n")
cat("Seg 4: C 99.54,120 89.91,150.7 109.91,138.29\n")
cat("       Tab goes RIGHT (x > 100)\n")
cat("Seg 5: C 129.91,125.89 129.91,165.89 109.91,158.29\n")
cat("Seg 6: C 89.91,150.7 101.79,180 100,200\n")
cat("End: (100,200)\n\n")

cat("KEY INSIGHTS:\n")
cat("1. The vertical segment has TWO tabs, one in each half\n")
cat("2. Both tabs protrude to the RIGHT (x > 100)\n")
cat("3. For piece [0,1], we need the SECOND HALF (100 to 200)\n")
cat("4. For piece [1,1], we need the SECOND HALF REVERSED with x-coordinates flipped\n\n")

# Extract the second half for piece [0,1]
vertical_second_half <- "C 99.54 120 89.91 150.7 109.91 138.29 C 129.91 125.89 129.91 165.89 109.91 158.29 C 89.91 150.7 101.79 180 100 200"

cat("Second half for piece [0,1] (going DOWN from 100,100 to 100,200):\n")
cat(vertical_second_half, "\n\n")

# For piece [1,1], we need this reversed AND with x-coordinates flipped
# The tab should protrude LEFT (x < 100) for piece [1,1]

# Function to flip x-coordinates around x=100
flip_x <- function(x_str) {
  x <- as.numeric(x_str)
  return(as.character(200 - x))
}

# Create the reversed and flipped version for piece [1,1]
# Going UP from (100,200) to (100,100)
# Original: C 99.54,120 89.91,150.7 109.91,138.29 C 129.91,125.89 129.91,165.89 109.91,158.29 C 89.91,150.7 101.79,180 100,200

# Reversed (UP from 200 to 100):
# Start at (100,200)
# Rev Seg 6: from (100,200) via (flip(101.79),180) (flip(89.91),150.7) to (flip(109.91),158.29)
# Rev Seg 5: from (flip(109.91),158.29) via (flip(129.91),165.89) (flip(129.91),125.89) to (flip(109.91),138.29)
# Rev Seg 4: from (flip(109.91),138.29) via (flip(89.91),150.7) (flip(99.54),120) to (100,100)

vertical_second_half_reversed_flipped <- paste0(
  "C ", flip_x("101.79"), " 180 ", flip_x("89.91"), " 150.7 ", flip_x("109.91"), " 158.29 ",
  "C ", flip_x("129.91"), " 165.89 ", flip_x("129.91"), " 125.89 ", flip_x("109.91"), " 138.29 ",
  "C ", flip_x("89.91"), " 150.7 ", flip_x("99.54"), " 120 100 100"
)

cat("Second half REVERSED and FLIPPED for piece [1,1] (going UP from 100,200 to 100,100):\n")
cat(vertical_second_half_reversed_flipped, "\n\n")

# Now create the corrected pieces
piece_0_1_correct <- paste0(
  "M 0 100 ",
  # Top edge: horizontal segment
  "C 20 97.72 49.22 86.28 36.15 106.28 C 23.09 126.28 63.09 126.28 56.15 106.28 C 49.22 86.28 80 101.85 100 100 ",
  # Right edge: second half of vertical segment
  vertical_second_half, " ",
  # Bottom edge
  "L 2 200 A 2 2 0 0 1 0 198 ",
  # Left edge
  "L 0 100 Z"
)

piece_1_1_correct <- paste0(
  "M 100 100 ",
  # Top edge: second part of horizontal segment
  "C 120 98.15 148.12 88.01 139.79 108.01 C 131.45 128.01 171.45 128.01 159.79 108.01 C 148.12 88.01 180 98.21 200 100 ",
  # Right edge
  "L 200 198 A 2 2 0 0 1 198 200 ",
  # Bottom edge
  "L 100 200 ",
  # Left edge: reversed and flipped second half
  vertical_second_half_reversed_flipped, " ",
  "Z"
)

# Create the final SVGs
writeLines(sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="200" height="200" viewBox="0 0 200 200">
<rect width="100%%" height="100%%" fill="transparent"/>
<path id="piece-0_1" fill="none" stroke="black" stroke-width="1.5" d="%s"/>
</svg>', piece_0_1_correct), "output/piece_0_1_CORRECT_TAB.svg")

writeLines(sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="200" height="200" viewBox="0 0 200 200">
<rect width="100%%" height="100%%" fill="transparent"/>
<path id="piece-1_1" fill="none" stroke="black" stroke-width="1.5" d="%s"/>
</svg>', piece_1_1_correct), "output/piece_1_1_CORRECT_TAB.svg")

cat("Created:\n")
cat("- output/piece_0_1_CORRECT_TAB.svg\n")
cat("- output/piece_1_1_CORRECT_TAB.svg\n")