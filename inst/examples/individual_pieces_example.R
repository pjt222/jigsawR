# Individual Puzzle Pieces Example
# Demonstrating the new individual piece export functionality

# Load required functions
source("R/rectangular_puzzle.R")
source("R/individual_pieces_final.R")

cat("=== Individual Puzzle Pieces Example ===\n")

# Example 1: Simple 2x2 puzzle
cat("Generating 2x2 puzzle with individual pieces...\n")
puzzle_2x2 <- generate_individual_pieces_svg(
  seed = 1234,
  xn = 2, yn = 2,
  width = 300, height = 200,
  stroke_width = 2,
  piece_colors = c("red", "blue", "green", "orange")
)

# Save to file
output_file_2x2 <- "output/individual_pieces_2x2.svg"
writeLines(puzzle_2x2$svg, output_file_2x2)
cat("2x2 puzzle saved to:", output_file_2x2, "\n")

# Show piece metadata
cat("Piece metadata:\n")
for (piece_name in names(puzzle_2x2$metadata)) {
  piece <- puzzle_2x2$metadata[[piece_name]]
  cat("  ", piece$id, ": position (", piece$position[1], ",", piece$position[2], 
      "), type:", piece$type, ", color:", piece$color, "\n")
}

# Example 2: Larger 3x3 puzzle  
cat("\nGenerating 3x3 puzzle with individual pieces...\n")
puzzle_3x3 <- generate_individual_pieces_svg(
  seed = 5678,
  xn = 3, yn = 3,
  width = 400, height = 400,
  stroke_width = 1.5,
  piece_colors = c("black")  # Single color for all pieces
)

output_file_3x3 <- "output/individual_pieces_3x3.svg"
writeLines(puzzle_3x3$svg, output_file_3x3)
cat("3x3 puzzle saved to:", output_file_3x3, "\n")

# Count piece types
piece_types <- sapply(puzzle_3x3$metadata, function(x) x$type)
type_counts <- table(piece_types)
cat("Piece type distribution:\n")
for (type in names(type_counts)) {
  cat("  ", type, ":", type_counts[type], "\n")
}

cat("\nParameters for 3x3 puzzle:\n")
params <- puzzle_3x3$parameters
cat("  Seed:", params$seed, "\n")
cat("  Grid:", params$dimensions[1], "x", params$dimensions[2], "\n") 
cat("  Size:", params$size[1], "x", params$size[2], "mm\n")
cat("  Total pieces:", params$total_pieces, "\n")

cat("\n=== Individual Pieces Example Complete ===\n")
cat("Files generated:\n")
cat("  -", output_file_2x2, "(colored 2x2 puzzle)\n")
cat("  -", output_file_3x3, "(black 3x3 puzzle)\n")
cat("\nThese SVGs contain individual <path> elements for each puzzle piece,\n")
cat("addressing the GitHub issue: 'Export SVG as multiple paths; each path being a single piece.'\n")