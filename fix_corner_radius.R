# Fix Corner Radius Issues in Border Pieces

cat("=== Fixing Corner Radius Issues ===\n")

# The border path components (with radius=2):
# M 2 0 L 198 0 A 2 2 0 0 1 200 2 L 200 198 A 2 2 0 0 1 198 200 L 2 200 A 2 2 0 0 1 0 198 L 0 2 A 2 2 0 0 1 2 0

# Extract the curved segments (same as before)
horizontal_seg_1 <- "C 20 98.21 57.09 113.63 43.17 93.63 C 29.26 73.63 69.26 73.63 63.17 93.63 C 57.09 113.63 80 96.5 100 100"
horizontal_seg_2 <- "C 120 103.5 152.05 108.65 140.6 88.65 C 129.16 68.65 169.16 68.65 160.6 88.65 C 152.05 108.65 180 98.56 200 100"
vertical_seg_1 <- "C 101.09 20 110.47 47.37 90.47 41.14 C 70.47 34.91 70.47 74.91 90.47 61.14 C 110.47 47.37 96.42 80 100 100"
vertical_seg_2 <- "C 103.58 120 107.98 145.59 87.98 137.83 C 67.98 130.08 67.98 170.08 87.98 157.83 C 107.98 145.59 99.43 180 100 200"

# Reversed segments  
horizontal_seg_1_reversed <- "C 80 96.5 57.09 113.63 63.17 93.63 C 69.26 73.63 29.26 73.63 43.17 93.63 C 57.09 113.63 20 98.21 0 100"
horizontal_seg_2_reversed <- "C 180 98.56 152.05 108.65 160.6 88.65 C 169.16 68.65 129.16 68.65 140.6 88.65 C 152.05 108.65 120 103.5 100 100"
vertical_seg_1_reversed <- "C 96.42 80 110.47 47.37 90.47 61.14 C 70.47 74.91 70.47 34.91 90.47 41.14 C 110.47 47.37 101.09 20 100 0"
vertical_seg_2_reversed <- "C 99.43 180 107.98 145.59 87.98 157.83 C 67.98 170.08 67.98 130.08 87.98 137.83 C 107.98 145.59 103.58 120 100 100"

# Now create the corrected pieces with proper border handling:

# PIECE [0,0] - Upper-Left Corner (has rounded corner at top-left)
# TOP: (2,0) to (100,0) - starts after the corner radius
# RIGHT: vertical_seg_1 (100,0)→(100,100)
# BOTTOM: horizontal_seg_1_reversed (100,100)→(0,100) 
# LEFT: (0,100) to (0,2), then arc to (2,0)
piece_0_0_corrected <- paste0(
  "M 2 0 ",                          # Start after corner radius
  "L 100 0 ",                        # TOP: straight to vertical divider
  vertical_seg_1, " ",               # RIGHT: curved to (100,100)
  horizontal_seg_1_reversed, " ",    # BOTTOM: curved to (0,100)
  "L 0 2 ",                          # LEFT: straight up to corner
  "A 2 2 0 0 1 2 0 ",                # TOP-LEFT corner arc
  "Z"
)

# PIECE [1,0] - Upper-Right Corner (has rounded corner at top-right)
# TOP: (100,0) to (198,0), then arc to (200,2)
# RIGHT: (200,2) to (200,100)  
# BOTTOM: horizontal_seg_2_reversed (200,100)→(100,100)
# LEFT: vertical_seg_1_reversed (100,100)→(100,0)
piece_1_0_corrected <- paste0(
  "M 100 0 ",                        # Start at vertical divider
  "L 198 0 ",                        # TOP: straight to corner
  "A 2 2 0 0 1 200 2 ",              # TOP-RIGHT corner arc
  "L 200 100 ",                      # RIGHT: straight down to horizontal divider
  horizontal_seg_2_reversed, " ",    # BOTTOM: curved to (100,100) 
  vertical_seg_1_reversed, " ",      # LEFT: curved back to (100,0)
  "Z"
)

# PIECE [0,1] - Lower-Left Corner (has rounded corner at bottom-left)
# TOP: horizontal_seg_1 (0,100)→(100,100)
# RIGHT: vertical_seg_2_reversed (100,100)→(100,200)
# BOTTOM: (100,200) to (2,200), then arc to (0,198)
# LEFT: (0,198) to (0,100)
piece_0_1_corrected <- paste0(
  "M 0 100 ",                        # Start at horizontal divider
  horizontal_seg_1, " ",             # TOP: curved to (100,100)
  vertical_seg_2_reversed, " ",      # RIGHT: curved to (100,200)  
  "L 2 200 ",                        # BOTTOM: straight to corner
  "A 2 2 0 0 1 0 198 ",              # BOTTOM-LEFT corner arc
  "L 0 100 ",                        # LEFT: straight up to start
  "Z"
)

# PIECE [1,1] - Lower-Right Corner (has rounded corner at bottom-right)  
# TOP: horizontal_seg_2 (100,100)→(200,100)
# RIGHT: (200,100) to (200,198), then arc to (198,200)
# BOTTOM: (198,200) to (100,200)
# LEFT: vertical_seg_2 (100,200)→(100,100)
piece_1_1_corrected <- paste0(
  "M 100 100 ",                      # Start at intersection
  horizontal_seg_2, " ",             # TOP: curved to (200,100)
  "L 200 198 ",                      # RIGHT: straight down to corner
  "A 2 2 0 0 1 198 200 ",            # BOTTOM-RIGHT corner arc  
  "L 100 200 ",                      # BOTTOM: straight left to vertical divider
  vertical_seg_2, " ",               # LEFT: curved back to (100,100)
  "Z"
)

cat("Corrected pieces with proper corner radius:\n")
cat("Piece [0,0]:", substr(piece_0_0_corrected, 1, 100), "...\n")
cat("Piece [1,0]:", substr(piece_1_0_corrected, 1, 100), "...\n") 
cat("Piece [0,1]:", substr(piece_0_1_corrected, 1, 100), "...\n")
cat("Piece [1,1]:", substr(piece_1_1_corrected, 1, 100), "...\n")

# Create corrected individual SVG files
pieces_corrected <- list(
  "0_0" = list(path = piece_0_0_corrected, color = "red", label = "[0,0] Upper-Left"),
  "1_0" = list(path = piece_1_0_corrected, color = "blue", label = "[1,0] Upper-Right"),
  "0_1" = list(path = piece_0_1_corrected, color = "green", label = "[0,1] Lower-Left"), 
  "1_1" = list(path = piece_1_1_corrected, color = "orange", label = "[1,1] Lower-Right")
)

for (piece_id in names(pieces_corrected)) {
  piece_info <- pieces_corrected[[piece_id]]
  
  svg_content <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>\n',
    '<svg xmlns="http://www.w3.org/2000/svg" version="1.1" ',
    'width="250" height="250" viewBox="-10 -10 220 220">\n',
    '<rect x="-10" y="-10" width="220" height="220" fill="white"/>\n',
    '<!-- ', piece_info$label, ' (Corrected) -->\n',
    '<path id="piece-', piece_id, '-corrected" ',
    'fill="rgba(', 
    ifelse(piece_info$color == "red", "255,0,0", 
           ifelse(piece_info$color == "blue", "0,0,255",
                  ifelse(piece_info$color == "green", "0,255,0", "255,165,0"))), ',0.3)" ',
    'stroke="', piece_info$color, '" stroke-width="2" ',
    'd="', piece_info$path, '"/>\n',
    '<!-- Reference -->\n',
    '<line x1="0" y1="100" x2="200" y2="100" stroke="gray" stroke-width="0.5" stroke-dasharray="3,3"/>\n',
    '<line x1="100" y1="0" x2="100" y2="200" stroke="gray" stroke-width="0.5" stroke-dasharray="3,3"/>\n',
    '<text x="100" y="240" font-size="14" fill="black" text-anchor="middle">',
    piece_info$label, ' (Fixed)</text>\n',
    '</svg>\n'
  )
  
  filename <- paste0("output/piece_", piece_id, "_corrected.svg")
  writeLines(svg_content, filename)
  cat("Created corrected:", filename, "\n")
}

cat("\n=== Corner Radius Issues Fixed ===\n")
cat("Check the corrected files, especially:\n")
cat("- piece_1_0_corrected.svg (right and bottom borders)\n") 
cat("- piece_1_1_corrected.svg (left border)\n")