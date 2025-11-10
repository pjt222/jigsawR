# Create Individual SVGs for All Four Pieces of 2x2 Puzzle

cat("=== Creating All Four Individual Pieces ===\n")

# First, let me re-extract the path data from our 2x2 puzzle
cat("Re-analyzing 2x2 puzzle paths...\n")

# From our previous analysis:
# Horizontal divider: "M 0,100 C 20 98.21 57.09 113.63 43.17 93.63 C 29.26 73.63 69.26 73.63 63.17 93.63 C 57.09 113.63 80 96.5 100 100 C 120 103.5 152.05 108.65 140.6 88.65 C 129.16 68.65 169.16 68.65 160.6 88.65 C 152.05 108.65 180 98.56 200 100"
# Vertical divider: "M 100,0 C 101.09 20 110.47 47.37 90.47 41.14 C 70.47 34.91 70.47 74.91 90.47 61.14 C 110.47 47.37 96.42 80 100 100 C 103.58 120 107.98 145.59 87.98 137.83 C 67.98 130.08 67.98 170.08 87.98 157.83 C 107.98 145.59 99.43 180 100 200"

# Break down the paths into segments:
# Horizontal divider has 2 segments:
# - Segment 1 (0,100)→(100,100): "C 20 98.21 57.09 113.63 43.17 93.63 C 29.26 73.63 69.26 73.63 63.17 93.63 C 57.09 113.63 80 96.5 100 100"
# - Segment 2 (100,100)→(200,100): "C 120 103.5 152.05 108.65 140.6 88.65 C 129.16 68.65 169.16 68.65 160.6 88.65 C 152.05 108.65 180 98.56 200 100"

# Vertical divider has 2 segments:
# - Segment 1 (100,0)→(100,100): "C 101.09 20 110.47 47.37 90.47 41.14 C 70.47 34.91 70.47 74.91 90.47 61.14 C 110.47 47.37 96.42 80 100 100"
# - Segment 2 (100,100)→(100,200): "C 103.58 120 107.98 145.59 87.98 137.83 C 67.98 130.08 67.98 170.08 87.98 157.83 C 107.98 145.59 99.43 180 100 200"

# Define the curved segments
horizontal_seg_1 <- "C 20 98.21 57.09 113.63 43.17 93.63 C 29.26 73.63 69.26 73.63 63.17 93.63 C 57.09 113.63 80 96.5 100 100"
horizontal_seg_2 <- "C 120 103.5 152.05 108.65 140.6 88.65 C 129.16 68.65 169.16 68.65 160.6 88.65 C 152.05 108.65 180 98.56 200 100"
vertical_seg_1 <- "C 101.09 20 110.47 47.37 90.47 41.14 C 70.47 34.91 70.47 74.91 90.47 61.14 C 110.47 47.37 96.42 80 100 100"
vertical_seg_2 <- "C 103.58 120 107.98 145.59 87.98 137.83 C 67.98 130.08 67.98 170.08 87.98 157.83 C 107.98 145.59 99.43 180 100 200"

# Create reversed versions (using the method we established)
# Reverse horizontal_seg_1 (0,100)→(100,100) becomes (100,100)→(0,100)
horizontal_seg_1_reversed <- "C 80 96.5 57.09 113.63 63.17 93.63 C 69.26 73.63 29.26 73.63 43.17 93.63 C 57.09 113.63 20 98.21 0 100"

# Reverse horizontal_seg_2 (100,100)→(200,100) becomes (200,100)→(100,100)  
horizontal_seg_2_reversed <- "C 180 98.56 152.05 108.65 160.6 88.65 C 169.16 68.65 129.16 68.65 140.6 88.65 C 152.05 108.65 120 103.5 100 100"

# Reverse vertical_seg_1 (100,0)→(100,100) becomes (100,100)→(100,0)
vertical_seg_1_reversed <- "C 96.42 80 110.47 47.37 90.47 61.14 C 70.47 74.91 70.47 34.91 90.47 41.14 C 110.47 47.37 101.09 20 100 0"

# Reverse vertical_seg_2 (100,100)→(100,200) becomes (100,200)→(100,100)
vertical_seg_2_reversed <- "C 99.43 180 107.98 145.59 87.98 157.83 C 67.98 170.08 67.98 130.08 87.98 137.83 C 107.98 145.59 103.58 120 100 100"

cat("Path segments extracted and reversed\n")

# Now create each piece:

# PIECE [0,0] - Upper-Left Corner
# TOP: straight (0,0)→(100,0)
# RIGHT: vertical_seg_1 (100,0)→(100,100)  
# BOTTOM: horizontal_seg_1_reversed (100,100)→(0,100)
# LEFT: straight (0,100)→(0,0)
piece_0_0 <- paste0(
  "M 0 0 ",
  "L 100 0 ",
  vertical_seg_1, " ",
  horizontal_seg_1_reversed, " ",
  "L 0 0 ",
  "Z"
)

# PIECE [1,0] - Upper-Right Corner  
# TOP: straight (100,0)→(200,0)
# RIGHT: straight (200,0)→(200,100)
# BOTTOM: horizontal_seg_2_reversed (200,100)→(100,100)
# LEFT: vertical_seg_1_reversed (100,100)→(100,0)
piece_1_0 <- paste0(
  "M 100 0 ",
  "L 200 0 ",
  "L 200 100 ",
  horizontal_seg_2_reversed, " ",
  vertical_seg_1_reversed, " ",
  "Z"
)

# PIECE [0,1] - Lower-Left Corner
# TOP: horizontal_seg_1 (0,100)→(100,100)
# RIGHT: vertical_seg_2_reversed (100,100)→(100,200)
# BOTTOM: straight (100,200)→(0,200)  
# LEFT: straight (0,200)→(0,100)
piece_0_1 <- paste0(
  "M 0 100 ",
  horizontal_seg_1, " ",
  vertical_seg_2_reversed, " ",
  "L 0 200 ",
  "L 0 100 ",
  "Z"
)

# PIECE [1,1] - Lower-Right Corner
# TOP: horizontal_seg_2 (100,100)→(200,100)
# RIGHT: straight (200,100)→(200,200)
# BOTTOM: straight (200,200)→(100,200)
# LEFT: vertical_seg_2 (100,200)→(100,100)
piece_1_1 <- paste0(
  "M 100 100 ",
  horizontal_seg_2, " ",
  "L 200 200 ",
  "L 100 200 ",
  vertical_seg_2, " ",
  "Z"
)

cat("All four piece paths created\n")

# Create individual SVG files
pieces <- list(
  "0_0" = list(path = piece_0_0, color = "red", label = "[0,0] Upper-Left"),
  "1_0" = list(path = piece_1_0, color = "blue", label = "[1,0] Upper-Right"), 
  "0_1" = list(path = piece_0_1, color = "green", label = "[0,1] Lower-Left"),
  "1_1" = list(path = piece_1_1, color = "orange", label = "[1,1] Lower-Right")
)

for (piece_id in names(pieces)) {
  piece_info <- pieces[[piece_id]]
  
  svg_content <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>\n',
    '<svg xmlns="http://www.w3.org/2000/svg" version="1.1" ',
    'width="250" height="250" viewBox="-10 -10 220 220">\n',
    '<rect x="-10" y="-10" width="220" height="220" fill="white"/>\n',
    '<!-- ', piece_info$label, ' -->\n',
    '<path id="piece-', piece_id, '" ',
    'fill="rgba(', 
    ifelse(piece_info$color == "red", "255,0,0", 
           ifelse(piece_info$color == "blue", "0,0,255",
                  ifelse(piece_info$color == "green", "0,255,0", "255,165,0"))), ',0.3)" ',
    'stroke="', piece_info$color, '" stroke-width="3" ',
    'd="', piece_info$path, '"/>\n',
    '<!-- Grid reference -->\n',
    '<line x1="0" y1="100" x2="200" y2="100" stroke="gray" stroke-width="0.5" stroke-dasharray="3,3"/>\n',
    '<line x1="100" y1="0" x2="100" y2="200" stroke="gray" stroke-width="0.5" stroke-dasharray="3,3"/>\n',
    '<!-- Label -->\n',
    '<text x="100" y="240" font-size="14" fill="black" text-anchor="middle">',
    piece_info$label, '</text>\n',
    '</svg>\n'
  )
  
  filename <- paste0("output/piece_", piece_id, "_individual.svg")
  writeLines(svg_content, filename)
  cat("Created:", filename, "\n")
}

cat("\n=== All Four Individual Pieces Created ===\n")
cat("Files:\n")
cat("- output/piece_0_0_individual.svg (Upper-Left, red)\n")
cat("- output/piece_1_0_individual.svg (Upper-Right, blue)\n")  
cat("- output/piece_0_1_individual.svg (Lower-Left, green)\n")
cat("- output/piece_1_1_individual.svg (Lower-Right, orange)\n")