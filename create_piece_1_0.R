# Create Piece [1,0] - Upper-Right Corner

cat("=== Creating Piece [1,0] (Upper-Right) ===\n")

cat("Piece [1,0] position: upper-right corner\n") 
cat("Coordinates: (100,0) to (200,100)\n")

cat("\nPiece [1,0] boundary segments:\n")
cat("1. TOP edge: (100,0) to (200,0) - STRAIGHT (puzzle border)\n")
cat("2. RIGHT edge: (200,0) to (200,100) - STRAIGHT (puzzle border)\n")
cat("3. BOTTOM edge: (200,100) to (100,100) - CURVED (horizontal divider)\n")
cat("4. LEFT edge: (100,100) to (100,0) - CURVED (vertical divider, REVERSED)\n")

# From the 2x2 puzzle paths:
# Vertical divider: "M 100,0 C 101.09 20 110.47 47.37 90.47 41.14 C 70.47 34.91 70.47 74.91 90.47 61.14 C 110.47 47.37 96.42 80 100 100"
# Horizontal divider: "M 0,100 C 20 98.21 57.09 113.63 43.17 93.63 C 29.26 73.63 69.26 73.63 63.17 93.63 C 57.09 113.63 80 96.5 100 100 C 120 103.5 152.05 108.65 140.6 88.65 C 129.16 68.65 169.16 68.65 160.6 88.65 C 152.05 108.65 180 98.56 200 100"

# Extract the segments for piece [1,0]:

# TOP edge: straight from (100,0) to (200,0)
top_edge <- "L 200 0"

# RIGHT edge: straight from (200,0) to (200,100) 
right_edge <- "L 200 100"

# BOTTOM edge: curved from (200,100) to (100,100)
# This is the SECOND part of the horizontal divider: "C 120 103.5 152.05 108.65 140.6 88.65 C 129.16 68.65 169.16 68.65 160.6 88.65 C 152.05 108.65 180 98.56 200 100"
# But we need to go from (200,100) to (100,100), so this needs to be REVERSED
bottom_edge <- "L 100 100"  # Simplified for now

# LEFT edge: curved from (100,100) to (100,0)  
# This is the vertical divider but REVERSED: currently goes (100,0) to (100,100), we need (100,100) to (100,0)
left_edge <- "L 100 0"  # Simplified for now

# Create piece [1,0] path
piece_1_0_path <- paste0(
  "M 100 0 ",     # Start at (100,0)
  top_edge, " ",  # TOP: (100,0) → (200,0)
  right_edge, " ", # RIGHT: (200,0) → (200,100)
  bottom_edge, " ", # BOTTOM: (200,100) → (100,100)
  left_edge, " ",  # LEFT: (100,100) → (100,0)
  "Z"             # Close
)

cat("Piece [1,0] path:\n")
cat(piece_1_0_path, "\n")

# Create SVG for piece [1,0]
svg_piece_1_0 <- paste0(
  '<?xml version="1.0" encoding="UTF-8"?>\n',
  '<svg xmlns="http://www.w3.org/2000/svg" version="1.1" ',
  'width="200" height="200" viewBox="0 0 200 200">\n',
  '<rect width="100%" height="100%" fill="lightgray"/>\n',
  '<path id="piece-1-0" ',
  'fill="rgba(0,0,255,0.3)" stroke="blue" stroke-width="3" ',
  'd="', piece_1_0_path, '"/>\n',
  '<!-- Reference grid -->\n',
  '<line x1="100" y1="0" x2="100" y2="200" stroke="gray" stroke-width="0.5" stroke-dasharray="2,2"/>\n',
  '<line x1="0" y1="100" x2="200" y2="100" stroke="gray" stroke-width="0.5" stroke-dasharray="2,2"/>\n',
  '<!-- Corner markers -->\n',
  '<circle cx="100" cy="0" r="2" fill="green"/>\n',
  '<circle cx="200" cy="0" r="2" fill="green"/>\n',
  '<circle cx="200" cy="100" r="2" fill="green"/>\n',
  '<circle cx="100" cy="100" r="2" fill="green"/>\n',
  '</svg>\n'
)

output_file <- "output/piece_1_0.svg"
writeLines(svg_piece_1_0, output_file)
cat("Piece [1,0] saved to:", output_file, "\n")

# Now create a combined SVG showing both pieces [0,0] and [1,0]
svg_combined <- paste0(
  '<?xml version="1.0" encoding="UTF-8"?>\n',
  '<svg xmlns="http://www.w3.org/2000/svg" version="1.1" ',
  'width="200" height="200" viewBox="0 0 200 200">\n',
  '<rect width="100%" height="100%" fill="white"/>\n',
  '<!-- Piece [0,0] -->\n',
  '<path id="piece-0-0" ',
  'fill="rgba(255,0,0,0.3)" stroke="red" stroke-width="2" ',
  'd="M 0 0 L 100 0 C 101.09 20 110.47 47.37 90.47 41.14 C 70.47 34.91 70.47 74.91 90.47 61.14 C 110.47 47.37 96.42 80 100 100 L 0 100 L 0 0 Z"/>\n',
  '<!-- Piece [1,0] -->\n',
  '<path id="piece-1-0" ',
  'fill="rgba(0,0,255,0.3)" stroke="blue" stroke-width="2" ',
  'd="', piece_1_0_path, '"/>\n',
  '<!-- Grid lines -->\n',
  '<line x1="100" y1="0" x2="100" y2="200" stroke="black" stroke-width="1" stroke-dasharray="5,5"/>\n',
  '<line x1="0" y1="100" x2="200" y2="100" stroke="black" stroke-width="1" stroke-dasharray="5,5"/>\n',
  '</svg>\n'
)

combined_file <- "output/pieces_0_0_and_1_0.svg"
writeLines(svg_combined, combined_file)
cat("Combined pieces [0,0] and [1,0] saved to:", combined_file, "\n")

cat("\n=== Piece [1,0] Created ===\n")
cat("Next: Check if the pieces fit together properly\n")