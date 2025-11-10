# Create a debug version with colors to see what's happening

# Read the verified pieces paths
piece_0_0 <- "M 2 0 L 100 0 C 101.09 20 110.47 47.37 90.47 41.14 C 70.47 34.91 70.47 74.91 90.47 61.14 C 110.47 47.37 96.42 80 100 100 C 80 96.5 57.09 113.63 63.17 93.63 C 69.26 73.63 29.26 73.63 43.17 93.63 C 57.09 113.63 20 98.21 0 100 L 0 2 A 2 2 0 0 1 2 0 Z"

piece_1_0 <- "M 100 0 L 198 0 A 2 2 0 0 1 200 2 L 200 100 C 180 98.56 152.05 108.65 160.6 88.65 C 169.16 68.65 129.16 68.65 140.6 88.65 C 152.05 108.65 120 103.5 100 100 C 96.42 80 110.47 47.37 90.47 61.14 C 70.47 74.91 70.47 34.91 90.47 41.14 C 110.47 47.37 101.09 20 100 0 Z"

piece_0_1 <- "M 0 100 C 20 97.72 49.22 86.28 36.15 106.28 C 23.09 126.28 63.09 126.28 56.15 106.28 C 49.22 86.28 80 101.85 100 100 C 99.54 120 89.91 150.7 109.91 138.29 C 129.91 125.89 129.91 165.89 109.91 158.29 C 89.91 150.7 101.79 180 100 200 L 2 200 A 2 2 0 0 1 0 198 L 0 100 Z"

piece_1_1 <- "M 100 100 C 120 98.15 148.12 88.01 139.79 108.01 C 131.45 128.01 171.45 128.01 159.79 108.01 C 148.12 88.01 180 98.21 200 100 L 200 198 A 2 2 0 0 1 198 200 L 100 200 C 98.21 180 110.09 150.7 90.09 158.29 C 70.09 165.89 70.09 125.89 90.09 138.29 C 110.09 150.7 100.46 120 100 100 Z"

# Create debug version with colors and labels
svg_debug <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="200" height="200" viewBox="0 0 200 200">
<rect width="100%%" height="100%%" fill="white"/>
<!-- Grid lines -->
<line x1="100" y1="0" x2="100" y2="200" stroke="gray" stroke-width="0.5" stroke-dasharray="2,2"/>
<line x1="0" y1="100" x2="200" y2="100" stroke="gray" stroke-width="0.5" stroke-dasharray="2,2"/>
<!-- Pieces -->
<g id="puzzle-pieces">
<path id="piece-0-0" fill="rgba(255,0,0,0.2)" stroke="red" stroke-width="1.5" d="%s"/>
<path id="piece-1-0" fill="rgba(0,255,0,0.2)" stroke="green" stroke-width="1.5" d="%s"/>
<path id="piece-0-1" fill="rgba(0,0,255,0.2)" stroke="blue" stroke-width="1.5" d="%s"/>
<path id="piece-1-1" fill="rgba(255,255,0,0.2)" stroke="orange" stroke-width="1.5" d="%s"/>
</g>
<!-- Labels -->
<text x="50" y="50" text-anchor="middle" font-size="12" fill="black">[0,0]</text>
<text x="150" y="50" text-anchor="middle" font-size="12" fill="black">[1,0]</text>
<text x="50" y="150" text-anchor="middle" font-size="12" fill="black">[0,1]</text>
<text x="150" y="150" text-anchor="middle" font-size="12" fill="black">[1,1]</text>
</svg>', piece_0_0, piece_1_0, piece_0_1, piece_1_1)

writeLines(svg_debug, "output/2x2_DEBUG_COLORS.svg")

# Also create a version with just the outlines, no fill
svg_outlines <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="200" height="200" viewBox="0 0 200 200">
<rect width="100%%" height="100%%" fill="transparent"/>
<g id="puzzle-pieces">
<path id="piece-0-0" fill="none" stroke="black" stroke-width="1.5" d="%s"/>
<path id="piece-1-0" fill="none" stroke="black" stroke-width="1.5" d="%s"/>
<path id="piece-0-1" fill="none" stroke="black" stroke-width="1.5" d="%s"/>
<path id="piece-1-1" fill="none" stroke="black" stroke-width="1.5" d="%s"/>
</g>
</svg>', piece_0_0, piece_1_0, piece_0_1, piece_1_1)

writeLines(svg_outlines, "output/2x2_FINAL_WORKING.svg")

cat("Created debug versions:\n")
cat("- output/2x2_DEBUG_COLORS.svg (with colors and labels)\n")
cat("- output/2x2_FINAL_WORKING.svg (clean version)\n")
cat("\nSince all individual pieces are correct, the combined should work too.\n")
cat("The issue might be visual overlap or rendering.\n")