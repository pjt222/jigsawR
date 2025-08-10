# Fix Piece [0,0] Bottom Edge - Use Correct Curved Segment

cat("=== Fixing Piece [0,0] Bottom Edge ===\n")

# The horizontal divider path from the 2x2 puzzle:
# "M 0,100 C 20 98.21 57.09 113.63 43.17 93.63 C 29.26 73.63 69.26 73.63 63.17 93.63 C 57.09 113.63 80 96.5 100 100"

# This path goes from (0,100) to (100,100) - exactly what we need for the bottom edge!
# But since we're tracing clockwise from (100,100) to (0,100), we need to REVERSE it

cat("Original horizontal divider (0,100) → (100,100):\n")
original_bottom <- "C 20 98.21 57.09 113.63 43.17 93.63 C 29.26 73.63 69.26 73.63 63.17 93.63 C 57.09 113.63 80 96.5 100 100"
cat(original_bottom, "\n")

# For clockwise tracing from (100,100) to (0,100), I need to reverse this curve
# For now, let me use the SAME curve but understand it goes in the right direction

cat("\nUsing the horizontal curve for bottom edge:\n")
bottom_edge_curved <- "C 20 98.21 57.09 113.63 43.17 93.63 C 29.26 73.63 69.26 73.63 63.17 93.63 C 57.09 113.63 80 96.5 100 100"

# Wait - this is wrong direction. Let me trace it properly:
# I'm at (100,100) and need to go to (0,100)
# So I need to reverse the original path

cat("Actually, let me use a simpler approach first - straight line to verify structure:\n")
bottom_edge_simple <- "L 0 100"

# Create corrected piece [0,0] path  
piece_0_0_corrected <- paste0(
  "M 0 0 ",                                           # Start at top-left
  "L 100 0 ",                                        # TOP: straight to (100,0)
  "C 101.09 20 110.47 47.37 90.47 41.14 ",          # RIGHT: curved part 1
  "C 70.47 34.91 70.47 74.91 90.47 61.14 ",         # RIGHT: curved part 2
  "C 110.47 47.37 96.42 80 100 100 ",                # RIGHT: curved part 3 → (100,100)
  bottom_edge_simple, " ",                           # BOTTOM: (100,100) → (0,100)
  "L 0 0 ",                                          # LEFT: (0,100) → (0,0)
  "Z"                                                # Close
)

cat("Corrected path:\n")
cat(piece_0_0_corrected, "\n")

# Create SVG
svg_corrected <- paste0(
  '<?xml version="1.0" encoding="UTF-8"?>\n',
  '<svg xmlns="http://www.w3.org/2000/svg" version="1.1" ',
  'width="200" height="200" viewBox="0 0 200 200">\n',
  '<rect width="100%" height="100%" fill="lightgray"/>\n',
  '<path id="piece-0-0" ',
  'fill="rgba(255,0,0,0.3)" stroke="red" stroke-width="3" ',
  'd="', piece_0_0_corrected, '"/>\n',
  '<!-- Reference grid -->\n',
  '<line x1="100" y1="0" x2="100" y2="200" stroke="blue" stroke-width="0.5" stroke-dasharray="2,2"/>\n',
  '<line x1="0" y1="100" x2="200" y2="100" stroke="blue" stroke-width="0.5" stroke-dasharray="2,2"/>\n',
  '<!-- Corner markers -->\n',
  '<circle cx="0" cy="0" r="2" fill="green"/>\n',
  '<circle cx="100" cy="0" r="2" fill="green"/>\n', 
  '<circle cx="100" cy="100" r="2" fill="green"/>\n',
  '<circle cx="0" cy="100" r="2" fill="green"/>\n',
  '</svg>\n'
)

output_file <- "output/piece_0_0_corrected.svg"
writeLines(svg_corrected, output_file)
cat("Corrected piece [0,0] saved to:", output_file, "\n")

cat("\n=== Bottom Edge Fixed (Simple) ===\n")
cat("Next: Verify this looks correct, then work on proper curved bottom\n")