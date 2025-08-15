devtools::load_all()

# Generate puzzle structure using the clean core implementation
puzzle_structure <- generate_puzzle_core(
  seed = 9999,
  grid = c(6, 5),     # 6 rows, 5 columns (matching yn=6, xn=5)
  size = c(1600, 900), # Same size
  tabsize = 15,       # Same parameters
  jitter = 2
)

# Generate the complete puzzle (traditional approach)
puzzle_complete <- generate_jigsaw_svg(
  seed = 9999,
  tabsize = 15,
  jitter = 2,
  width = 1600,
  height = 900,
  radius = 5.0,
  xn = 5,
  yn = 6
)

# Create output directory
dir.create("output", showWarnings = FALSE)

# Save complete puzzle
save_jigsaw_svg(puzzle_complete, "output/custom_jigsaw_complete.svg")

# Generate separated pieces with kerf offset for laser cutting
kerf_offset <- 100

separated_puzzle <- generate_separated_puzzle_svg(
  puzzle_structure = puzzle_structure,
  offset = kerf_offset,
  colors = "black",           # Black lines for laser cutting
  stroke_width = 1.5,         # Appropriate for laser cutting
  background = "none"         # No background for cutting
)

# Save separated pieces
writeLines(separated_puzzle, "output/custom_jigsaw_separated.svg")

# Print puzzle information
cat("Generated puzzle with", puzzle_structure$grid[1] * puzzle_structure$grid[2], "pieces\n")
cat("Puzzle dimensions:", puzzle_structure$grid[1], "rows x", puzzle_structure$grid[2], "columns\n")
cat("Piece size: ~", round(puzzle_structure$piece_width), "x", round(puzzle_structure$piece_height), "mm\n")
cat("Separation offset:", kerf_offset, "mm\n\n")

# Calculate layout dimensions
original_area <- puzzle_structure$size[1] * puzzle_structure$size[2]
separated_width <- puzzle_structure$size[1] + (puzzle_structure$grid[2] - 1) * kerf_offset
separated_height <- puzzle_structure$size[2] + (puzzle_structure$grid[1] - 1) * kerf_offset
separated_area <- separated_width * separated_height

cat("Files generated:\n")
cat("  • output/custom_jigsaw_complete.svg    - Complete puzzle outline\n")
cat("  • output/custom_jigsaw_separated.svg   - Separated pieces for laser cutting\n\n")

cat("Layout comparison:\n")
cat(sprintf("  Original: %.0f x %.0f mm (%.0f mm²)\n",
           puzzle_structure$size[1], puzzle_structure$size[2], original_area))
cat(sprintf("  Separated: %.0f x %.0f mm (%.0f mm²)\n",
           separated_width, separated_height, separated_area))
cat(sprintf("  Area increase: %.1f%%\n",
           (separated_area - original_area) / original_area * 100))

# Function to save individual pieces as separate SVG files
save_individual_piece_svgs <- function(puzzle_structure,
                                      output_dir = "individual_pieces",
                                      stroke_width = 1.5,
                                      stroke_color = "black",
                                      fill_color = "none",           # Default transparent fill
                                      background_color = "none") {   # Default transparent background

  # Create output directory
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  xn <- puzzle_structure$grid[2]
  yn <- puzzle_structure$grid[1]

  # Generate and save each piece
  for (yi in 0:(yn - 1)) {
    for (xi in 0:(xn - 1)) {
      # Generate piece path
      piece_path <- generate_single_piece(xi, yi, puzzle_structure)

      # Calculate piece bounding box for proper viewBox
      piece_width <- puzzle_structure$piece_width * 1.5  # Add padding
      piece_height <- puzzle_structure$piece_height * 1.5
      x_offset <- xi * puzzle_structure$piece_width - puzzle_structure$piece_width * 0.25
      y_offset <- yi * puzzle_structure$piece_height - puzzle_structure$piece_height * 0.25

      # Build background rect if needed
      background_rect <- ""
      if (background_color != "none" && background_color != "") {
        background_rect <- sprintf('  <rect x="%.2f" y="%.2f" width="%.0f" height="%.0f" fill="%s"/>\n',
                                  x_offset, y_offset, piece_width, piece_height, background_color)
      }

      # Create individual SVG for this piece
      svg_content <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1"
     width="%.0f" height="%.0f" viewBox="%.2f %.2f %.0f %.0f">
%s  <path d="%s" fill="%s" stroke="%s" stroke-width="%.1f" stroke-linecap="round" stroke-linejoin="round"/>
</svg>',
        piece_width, piece_height,
        x_offset, y_offset, piece_width, piece_height,
        background_rect,
        piece_path, fill_color, stroke_color, stroke_width)

      # Save to file
      filename <- sprintf("%s/piece_%02d_%02d.svg", output_dir, xi, yi)
      writeLines(svg_content, filename)
    }
  }

  cat(sprintf("\nSaved %d individual piece files to %s/\n", xn * yn, output_dir))
}

# Generate individual piece files
cat("\nGenerating individual piece files...\n")
save_individual_piece_svgs(puzzle_structure,
                          output_dir = "output/individual_pieces",
                          stroke_width = 1.5,
                          stroke_color = "black")

cat("Individual pieces saved as:\n")
cat("  • output/individual_pieces/piece_00_00.svg through piece_04_05.svg\n")
cat("  • Each file contains one complete puzzle piece\n")
cat("  • Default: transparent fill and background (fill='none', background='none')\n")
cat("  • Suitable for individual laser cutting or manufacturing\n")

# Example: Generate colored individual pieces
cat("\nGenerating colored individual pieces example...\n")
save_individual_piece_svgs(puzzle_structure,
                          output_dir = "output/individual_pieces_colored",
                          stroke_width = 2.0,
                          stroke_color = "darkblue",
                          fill_color = "#E3F2FD",        # Light blue fill
                          background_color = "white")     # White background

cat("Colored pieces saved to output/individual_pieces_colored/\n")
cat("  • White background with light blue fill\n")
cat("  • Dark blue stroke for visibility\n")
