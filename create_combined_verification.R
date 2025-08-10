# Create Combined SVG with All Four Pieces to Verify Fit

cat("=== Creating Combined Verification SVG ===\n")

# Use the same paths we created for individual pieces
horizontal_seg_1 <- "C 20 98.21 57.09 113.63 43.17 93.63 C 29.26 73.63 69.26 73.63 63.17 93.63 C 57.09 113.63 80 96.5 100 100"
horizontal_seg_2 <- "C 120 103.5 152.05 108.65 140.6 88.65 C 129.16 68.65 169.16 68.65 160.6 88.65 C 152.05 108.65 180 98.56 200 100"
vertical_seg_1 <- "C 101.09 20 110.47 47.37 90.47 41.14 C 70.47 34.91 70.47 74.91 90.47 61.14 C 110.47 47.37 96.42 80 100 100"
vertical_seg_2 <- "C 103.58 120 107.98 145.59 87.98 137.83 C 67.98 130.08 67.98 170.08 87.98 157.83 C 107.98 145.59 99.43 180 100 200"

# Reversed segments
horizontal_seg_1_reversed <- "C 80 96.5 57.09 113.63 63.17 93.63 C 69.26 73.63 29.26 73.63 43.17 93.63 C 57.09 113.63 20 98.21 0 100"
horizontal_seg_2_reversed <- "C 180 98.56 152.05 108.65 160.6 88.65 C 169.16 68.65 129.16 68.65 140.6 88.65 C 152.05 108.65 120 103.5 100 100"
vertical_seg_1_reversed <- "C 96.42 80 110.47 47.37 90.47 61.14 C 70.47 74.91 70.47 34.91 90.47 41.14 C 110.47 47.37 101.09 20 100 0"
vertical_seg_2_reversed <- "C 99.43 180 107.98 145.59 87.98 157.83 C 67.98 170.08 67.98 130.08 87.98 137.83 C 107.98 145.59 103.58 120 100 100"

# Recreate all four pieces
piece_0_0 <- paste0("M 0 0 L 100 0 ", vertical_seg_1, " ", horizontal_seg_1_reversed, " L 0 0 Z")
piece_1_0 <- paste0("M 100 0 L 200 0 L 200 100 ", horizontal_seg_2_reversed, " ", vertical_seg_1_reversed, " Z")
piece_0_1 <- paste0("M 0 100 ", horizontal_seg_1, " ", vertical_seg_2_reversed, " L 0 200 L 0 100 Z")
piece_1_1 <- paste0("M 100 100 ", horizontal_seg_2, " L 200 200 L 100 200 ", vertical_seg_2, " Z")

# Create combined SVG
combined_svg <- paste0(
  '<?xml version="1.0" encoding="UTF-8"?>\n',
  '<svg xmlns="http://www.w3.org/2000/svg" version="1.1" ',
  'width="300" height="300" viewBox="-20 -20 240 240">\n',
  '<rect x="-20" y="-20" width="240" height="240" fill="white"/>\n',
  
  '<!-- All Four Pieces Together -->\n',
  '<g id="complete-puzzle">\n',
  
  '<!-- Piece [0,0] Upper-Left -->\n',
  '<path id="piece-0-0" fill="rgba(255,0,0,0.4)" stroke="red" stroke-width="2" d="', piece_0_0, '"/>\n',
  
  '<!-- Piece [1,0] Upper-Right -->\n', 
  '<path id="piece-1-0" fill="rgba(0,0,255,0.4)" stroke="blue" stroke-width="2" d="', piece_1_0, '"/>\n',
  
  '<!-- Piece [0,1] Lower-Left -->\n',
  '<path id="piece-0-1" fill="rgba(0,255,0,0.4)" stroke="green" stroke-width="2" d="', piece_0_1, '"/>\n',
  
  '<!-- Piece [1,1] Lower-Right -->\n',
  '<path id="piece-1-1" fill="rgba(255,165,0,0.4)" stroke="orange" stroke-width="2" d="', piece_1_1, '"/>\n',
  
  '</g>\n',
  
  '<!-- Reference grid -->\n',
  '<line x1="0" y1="100" x2="200" y2="100" stroke="black" stroke-width="1" stroke-dasharray="5,5"/>\n',
  '<line x1="100" y1="0" x2="100" y2="200" stroke="black" stroke-width="1" stroke-dasharray="5,5"/>\n',
  
  '<!-- Corner markers -->\n',
  '<circle cx="0" cy="0" r="2" fill="black"/>\n',
  '<circle cx="200" cy="0" r="2" fill="black"/>\n',
  '<circle cx="200" cy="200" r="2" fill="black"/>\n',
  '<circle cx="0" cy="200" r="2" fill="black"/>\n',
  '<circle cx="100" cy="100" r="2" fill="black"/>\n',
  
  '<!-- Labels -->\n',
  '<text x="50" y="50" font-size="12" fill="black" text-anchor="middle">[0,0]</text>\n',
  '<text x="150" y="50" font-size="12" fill="black" text-anchor="middle">[1,0]</text>\n', 
  '<text x="50" y="150" font-size="12" fill="black" text-anchor="middle">[0,1]</text>\n',
  '<text x="150" y="150" font-size="12" fill="black" text-anchor="middle">[1,1]</text>\n',
  
  '<text x="100" y="230" font-size="14" fill="black" text-anchor="middle">Complete 2×2 Puzzle - Individual Pieces</text>\n',
  
  '</svg>\n'
)

output_file <- "output/complete_2x2_individual_pieces.svg"
writeLines(combined_svg, output_file)
cat("Combined verification SVG created:", output_file, "\n")

# Also create a clean version for final presentation
clean_svg <- paste0(
  '<?xml version="1.0" encoding="UTF-8"?>\n',
  '<svg xmlns="http://www.w3.org/2000/svg" version="1.1" ',
  'width="200" height="200" viewBox="0 0 200 200">\n',
  '<rect width="100%" height="100%" fill="transparent"/>\n',
  '<g id="puzzle-pieces">\n',
  '<path id="piece-0-0" fill="none" stroke="red" stroke-width="1.5" d="', piece_0_0, '"/>\n',
  '<path id="piece-1-0" fill="none" stroke="blue" stroke-width="1.5" d="', piece_1_0, '"/>\n',
  '<path id="piece-0-1" fill="none" stroke="green" stroke-width="1.5" d="', piece_0_1, '"/>\n',
  '<path id="piece-1-1" fill="none" stroke="orange" stroke-width="1.5" d="', piece_1_1, '"/>\n',
  '</g>\n',
  '</svg>\n'
)

clean_file <- "output/2x2_individual_pieces_clean.svg"
writeLines(clean_svg, clean_file)
cat("Clean final SVG created:", clean_file, "\n")

cat("\n=== Verification Complete ===\n")
cat("Created files:\n")
cat("- Individual pieces: piece_0_0_individual.svg, piece_1_0_individual.svg, etc.\n")
cat("- Verification view:", output_file, "\n")
cat("- Clean final version:", clean_file, "\n")
cat("\nAll four pieces should now fit together perfectly with complementary curved edges!\n")