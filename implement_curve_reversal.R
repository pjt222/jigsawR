# Implement Proper Bézier Curve Reversal

cat("=== Implementing Curve Reversal ===\n")

# Original vertical divider curve: (100,0) → (100,100)
# "C 101.09 20 110.47 47.37 90.47 41.14 C 70.47 34.91 70.47 74.91 90.47 61.14 C 110.47 47.37 96.42 80 100 100"

# This consists of 3 cubic Bézier segments:
# Segment 1: (100,0) → C 101.09,20 110.47,47.37 90.47,41.14
# Segment 2: (90.47,41.14) → C 70.47,34.91 70.47,74.91 90.47,61.14  
# Segment 3: (90.47,61.14) → C 110.47,47.37 96.42,80 100,100

# To reverse: start from (100,100) and go to (100,0)
# Reverse the segments and reverse the control points within each segment

cat("Original curve segments:\n")
cat("Seg 1: (100,0) C 101.09,20 110.47,47.37 90.47,41.14\n")
cat("Seg 2: (90.47,41.14) C 70.47,34.91 70.47,74.91 90.47,61.14\n")
cat("Seg 3: (90.47,61.14) C 110.47,47.37 96.42,80 100,100\n")

cat("\nReversed curve (100,100) → (100,0):\n")
cat("Start at (100,100)\n")
cat("Rev Seg 3: (100,100) C 96.42,80 110.47,47.37 90.47,61.14\n")
cat("Rev Seg 2: (90.47,61.14) C 70.47,74.91 70.47,34.91 90.47,41.14\n") 
cat("Rev Seg 1: (90.47,41.14) C 110.47,47.37 101.09,20 100,0\n")

# Build the reversed curve
reversed_curve <- "C 96.42 80 110.47 47.37 90.47 61.14 C 70.47 74.91 70.47 34.91 90.47 41.14 C 110.47 47.37 101.09 20 100 0"

cat("Reversed curve string:\n")
cat(reversed_curve, "\n")

# Now create piece [1,0] with the properly reversed left edge
piece_1_0_with_reversed_curve <- paste0(
  "M 100 0 ",           # Start at (100,0)
  "L 200 0 ",          # TOP: (100,0) → (200,0)  
  "L 200 100 ",        # RIGHT: (200,0) → (200,100)
  "L 100 100 ",        # BOTTOM: (200,100) → (100,100) (temp straight)
  reversed_curve, " ", # LEFT: (100,100) → (100,0) (reversed curve)
  "Z"
)

cat("\nPiece [1,0] with reversed left curve:\n")
cat(piece_1_0_with_reversed_curve, "\n")

# Create SVG showing both pieces fitting together
svg_with_reversal <- paste0(
  '<?xml version="1.0" encoding="UTF-8"?>\n',
  '<svg xmlns="http://www.w3.org/2000/svg" version="1.1" ',
  'width="280" height="160" viewBox="-20 -20 240 140">\n',
  '<rect x="-20" y="-20" width="240" height="140" fill="white"/>\n',
  '<!-- Piece [0,0] with original curve -->\n',
  '<path id="piece-0-0" ',
  'fill="rgba(255,0,0,0.3)" stroke="red" stroke-width="2" ',
  'd="M 0 0 L 100 0 C 101.09 20 110.47 47.37 90.47 41.14 C 70.47 34.91 70.47 74.91 90.47 61.14 C 110.47 47.37 96.42 80 100 100 L 0 100 L 0 0 Z"/>\n',
  '<!-- Piece [1,0] with reversed curve -->\n',
  '<path id="piece-1-0" ',
  'fill="rgba(0,0,255,0.3)" stroke="blue" stroke-width="2" ',
  'd="', piece_1_0_with_reversed_curve, '"/>\n',
  '<!-- Grid reference -->\n',
  '<line x1="0" y1="100" x2="200" y2="100" stroke="gray" stroke-width="0.5" stroke-dasharray="2,2"/>\n',
  '<line x1="100" y1="0" x2="100" y2="100" stroke="gray" stroke-width="0.5" stroke-dasharray="2,2"/>\n',
  '<!-- Labels -->\n',
  '<text x="50" y="50" font-size="14" fill="black" text-anchor="middle">[0,0]</text>\n',
  '<text x="150" y="50" font-size="14" fill="black" text-anchor="middle">[1,0]</text>\n',
  '<text x="100" y="-5" font-size="10" fill="green" text-anchor="middle">shared edge</text>\n',
  '</svg>\n'
)

output_file <- "output/pieces_with_reversed_curves.svg"
writeLines(svg_with_reversal, output_file)
cat("\nPieces with reversed curves saved to:", output_file, "\n")

cat("\n=== Curve Reversal Implemented ===\n")
cat("Now pieces [0,0] and [1,0] should have complementary curved edges!\n")