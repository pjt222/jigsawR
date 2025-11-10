# Fix Shared Edges Between Pieces [0,0] and [1,0]

cat("=== Fixing Shared Edges ===\n")

# The key insight: pieces [0,0] and [1,0] share the vertical divider
# This divider goes from (100,0) to (100,100) 

# From the original 2x2 puzzle:
# Vertical divider: "M 100,0 C 101.09 20 110.47 47.37 90.47 41.14 C 70.47 34.91 70.47 74.91 90.47 61.14 C 110.47 47.37 96.42 80 100 100"

vertical_divider_original <- "C 101.09 20 110.47 47.37 90.47 41.14 C 70.47 34.91 70.47 74.91 90.47 61.14 C 110.47 47.37 96.42 80 100 100"

cat("Original vertical divider (100,0) → (100,100):\n")
cat(vertical_divider_original, "\n")

# For piece [0,0]: right edge goes (100,0) → (100,100) - SAME as original
piece_0_0_right_edge <- vertical_divider_original

# For piece [1,0]: left edge goes (100,100) → (100,0) - REVERSE of original  
# I need to reverse this curve properly

cat("\nFor piece [0,0] right edge: USE original (100,0) → (100,100)\n")
cat("For piece [1,0] left edge: NEED reverse (100,100) → (100,0)\n")

# Create corrected pieces with proper curved edges

# Piece [0,0] corrected
piece_0_0_corrected <- paste0(
  "M 0 0 ",                          # Start
  "L 100 0 ",                       # TOP: straight
  piece_0_0_right_edge, " ",        # RIGHT: curved (original direction)
  "L 0 100 ",                       # BOTTOM: straight (temp)
  "L 0 0 ",                         # LEFT: straight  
  "Z"
)

# Piece [1,0] with straight left edge for now (will fix curve reversal later)
piece_1_0_corrected <- paste0(
  "M 100 0 ",                       # Start
  "L 200 0 ",                       # TOP: straight
  "L 200 100 ",                     # RIGHT: straight
  "L 100 100 ",                     # BOTTOM: straight (temp)
  "L 100 0 ",                       # LEFT: straight (should be reversed curve)
  "Z"
)

cat("\nCorrected piece [0,0] with curved right edge:\n")
cat(piece_0_0_corrected, "\n")

cat("\nPiece [1,0] (still needs curved left edge):\n") 
cat(piece_1_0_corrected, "\n")

# Create SVG showing both pieces with the corrected curved edge
svg_corrected <- paste0(
  '<?xml version="1.0" encoding="UTF-8"?>\n',
  '<svg xmlns="http://www.w3.org/2000/svg" version="1.1" ',
  'width="250" height="150" viewBox="-10 -10 220 120">\n',
  '<rect x="-10" y="-10" width="220" height="120" fill="white"/>\n',
  '<!-- Piece [0,0] with curved right edge -->\n',
  '<path id="piece-0-0" ',
  'fill="rgba(255,0,0,0.2)" stroke="red" stroke-width="2" ',
  'd="', piece_0_0_corrected, '"/>\n',
  '<!-- Piece [1,0] with straight left edge -->\n',
  '<path id="piece-1-0" ',
  'fill="rgba(0,0,255,0.2)" stroke="blue" stroke-width="2" ',
  'd="', piece_1_0_corrected, '"/>\n',
  '<!-- Show the shared vertical line -->\n',
  '<line x1="100" y1="0" x2="100" y2="100" stroke="green" stroke-width="1" stroke-dasharray="3,3"/>\n',
  '<!-- Labels -->\n',
  '<text x="50" y="50" font-size="12" fill="black" text-anchor="middle">[0,0]</text>\n',
  '<text x="150" y="50" font-size="12" fill="black" text-anchor="middle">[1,0]</text>\n',
  '</svg>\n'
)

output_file <- "output/pieces_with_curved_edge.svg"
writeLines(svg_corrected, output_file)
cat("\nCorrected pieces saved to:", output_file, "\n")

cat("\n=== Key Issue Identified ===\n")
cat("- Piece [0,0] right edge: ✅ Uses correct curve\n") 
cat("- Piece [1,0] left edge: ❌ Needs reversed curve to match\n")
cat("Next: Implement proper curve reversal for complementary edges\n")