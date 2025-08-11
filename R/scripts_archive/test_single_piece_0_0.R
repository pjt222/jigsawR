# Test Single Piece [0,0] - Upper-Left Corner
# Create SVG containing only this piece to verify boundary

cat("=== Creating Single Piece [0,0] SVG ===\n")

# Use the exact segments identified from the combined 2x2 puzzle
piece_0_0_path <- paste0(
  "M 0 0 ",                                           # Start at top-left
  "L 100 0 ",                                        # TOP: straight to (100,0)
  "C 101.09 20 110.47 47.37 90.47 41.14 ",          # RIGHT: curved part 1
  "C 70.47 34.91 70.47 74.91 90.47 61.14 ",         # RIGHT: curved part 2  
  "C 110.47 47.37 96.42 80 100 100 ",                # RIGHT: curved part 3
  "L 0 100 ",                                        # BOTTOM: straight to (0,100) - TEMP
  "L 0 0 ",                                          # LEFT: straight back to start
  "Z"                                                # Close path
)

cat("Piece [0,0] path constructed\n")

# Create SVG content
svg_content <- paste0(
  '<?xml version="1.0" encoding="UTF-8"?>\n',
  '<svg xmlns="http://www.w3.org/2000/svg" version="1.1" ',
  'width="200" height="200" viewBox="0 0 200 200">\n',
  '<rect width="100%" height="100%" fill="lightgray"/>\n',
  '<!-- Piece [0,0] - Upper-Left Corner -->\n',
  '<path id="piece-0-0" ',
  'fill="none" stroke="red" stroke-width="2" ',
  'd="', piece_0_0_path, '"/>\n',
  '<!-- Show piece bounds for reference -->\n',
  '<rect x="0" y="0" width="100" height="100" ',
  'fill="none" stroke="blue" stroke-width="0.5" stroke-dasharray="5,5"/>\n',
  '</svg>\n'
)

# Save the single piece SVG
output_file <- "output/single_piece_0_0.svg"
writeLines(svg_content, output_file)
cat("Single piece [0,0] saved to:", output_file, "\n")

# Show the path for inspection
cat("\nPiece [0,0] path:\n")
cat(piece_0_0_path, "\n")

# Add reference points for debugging
svg_with_points <- paste0(
  '<?xml version="1.0" encoding="UTF-8"?>\n',
  '<svg xmlns="http://www.w3.org/2000/svg" version="1.1" ',
  'width="200" height="200" viewBox="0 0 200 200">\n',
  '<rect width="100%" height="100%" fill="lightgray"/>\n',
  '<!-- Piece [0,0] -->\n',
  '<path id="piece-0-0" ',
  'fill="rgba(255,0,0,0.2)" stroke="red" stroke-width="2" ',
  'd="', piece_0_0_path, '"/>\n',
  '<!-- Corner points -->\n',
  '<circle cx="0" cy="0" r="3" fill="green"/>\n',      # Top-left
  '<circle cx="100" cy="0" r="3" fill="green"/>\n',    # Top-right  
  '<circle cx="100" cy="100" r="3" fill="green"/>\n',  # Bottom-right
  '<circle cx="0" cy="100" r="3" fill="green"/>\n',    # Bottom-left
  '<!-- Labels -->\n',
  '<text x="5" y="15" font-size="10" fill="black">(0,0)</text>\n',
  '<text x="75" y="15" font-size="10" fill="black">(100,0)</text>\n',
  '<text x="65" y="95" font-size="10" fill="black">(100,100)</text>\n',
  '<text x="5" y="95" font-size="10" fill="black">(0,100)</text>\n',
  '</svg>\n'
)

debug_file <- "output/single_piece_0_0_debug.svg"
writeLines(svg_with_points, debug_file)
cat("Debug version saved to:", debug_file, "\n")

cat("\n=== Single Piece [0,0] Created ===\n")
cat("Files to inspect:\n")
cat("- ", output_file, " (clean version)\n")
cat("- ", debug_file, " (with reference points)\n")