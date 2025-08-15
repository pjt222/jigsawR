devtools::load_all()

# Generate puzzle structure using the clean core implementation
puzzle_structure <- generate_puzzle_core(
  seed = 9999,
  grid = c(6, 4),     # 6 rows, 5 columns (matching yn=6, xn=5)
  size = c(1200, 600), # Same size
  tabsize = 15,       # Same parameters
  jitter = 2
)

# Generate the complete puzzle (traditional approach)
puzzle_complete <- generate_jigsaw_svg(
  seed = 9999,
  tabsize = 15,
  jitter = 2,
  width = 1200,
  height = 600,
  radius = 5.0,
  xn = 4,
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

# Calculate layout dimensions
original_area <- puzzle_structure$size[1] * puzzle_structure$size[2]
separated_width <- puzzle_structure$size[1] + (puzzle_structure$grid[2] - 1) * kerf_offset
separated_height <- puzzle_structure$size[2] + (puzzle_structure$grid[1] - 1) * kerf_offset
separated_area <- separated_width * separated_height

# Print summary
cat(sprintf("Puzzle: %dx%d grid (%d pieces), %.0fx%.0fmm, offset=%dmm\n",
           puzzle_structure$grid[2], puzzle_structure$grid[1],
           puzzle_structure$grid[1] * puzzle_structure$grid[2],
           puzzle_structure$size[1], puzzle_structure$size[2], kerf_offset))
cat(sprintf("Layout: %.0fx%.0fmm → %.0fx%.0fmm (%.1f%% increase)\n",
           puzzle_structure$size[1], puzzle_structure$size[2],
           separated_width, separated_height,
           (separated_area - original_area) / original_area * 100))

# Function to save individual pieces as separate SVG files
save_individual_piece_svgs <- function(puzzle_structure,
                                      output_dir = "individual_pieces",
                                      stroke_width = 1.5,
                                      stroke_color = "black",
                                      fill_color = "transparent",    # Default transparent fill (can be matrix) - PowerPoint compatible
                                      background_color = "transparent", # Default transparent background (can be matrix) - PowerPoint compatible
                                      use_viridis = FALSE) {         # Auto-generate viridis colors

  # Create output directory
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  xn <- puzzle_structure$grid[2]
  yn <- puzzle_structure$grid[1]

  # Helper function to convert 8-char hex to 6-char hex (PowerPoint compatible)
  clean_hex_color <- function(color) {
    if (is.character(color) && nchar(color) == 9 && substr(color, 1, 1) == "#") {
      # Remove alpha channel (last 2 characters)
      return(substr(color, 1, 7))
    }
    return(color)
  }

  # Generate viridis colors if requested
  if (use_viridis && is.character(fill_color) && length(fill_color) == 1 &&
      (fill_color == "transparent" || fill_color == "none")) {
    if (!requireNamespace("viridis", quietly = TRUE)) {
      stop("viridis package is required for use_viridis = TRUE")
    }
    n_pieces <- xn * yn
    viridis_colors <- viridis::viridis(n_pieces)
    # Clean hex colors for PowerPoint compatibility
    viridis_colors <- sapply(viridis_colors, clean_hex_color)
    fill_color <- matrix(viridis_colors, nrow = yn, ncol = xn, byrow = TRUE)
  }

  # Validate matrix dimensions if provided
  if (is.matrix(stroke_color)) {
    if (!all(dim(stroke_color) == c(yn, xn))) {
      stop(sprintf("stroke_color matrix dimensions (%dx%d) must match grid (%dx%d)",
                   nrow(stroke_color), ncol(stroke_color), yn, xn))
    }
  }
  if (is.matrix(fill_color)) {
    if (!all(dim(fill_color) == c(yn, xn))) {
      stop(sprintf("fill_color matrix dimensions (%dx%d) must match grid (%dx%d)",
                   nrow(fill_color), ncol(fill_color), yn, xn))
    }
  }
  if (is.matrix(background_color)) {
    if (!all(dim(background_color) == c(yn, xn))) {
      stop(sprintf("background_color matrix dimensions (%dx%d) must match grid (%dx%d)",
                   nrow(background_color), ncol(background_color), yn, xn))
    }
  }

  # Generate and save each piece
  for (yi in 0:(yn - 1)) {
    for (xi in 0:(xn - 1)) {
      # Generate piece path
      piece_path <- generate_single_piece(xi, yi, puzzle_structure)

      # Extract colors for this piece (matrix or single value)
      current_stroke <- if(is.matrix(stroke_color)) stroke_color[yi+1, xi+1] else stroke_color
      current_fill <- if(is.matrix(fill_color)) fill_color[yi+1, xi+1] else fill_color
      current_bg <- if(is.matrix(background_color)) background_color[yi+1, xi+1] else background_color

      # Clean hex colors for PowerPoint compatibility
      current_stroke <- clean_hex_color(current_stroke)
      current_fill <- clean_hex_color(current_fill)
      current_bg <- clean_hex_color(current_bg)

      # Calculate piece bounding box for proper viewBox
      piece_width <- puzzle_structure$piece_width * 1.5  # Add padding
      piece_height <- puzzle_structure$piece_height * 1.5
      x_offset <- xi * puzzle_structure$piece_width - puzzle_structure$piece_width * 0.25
      y_offset <- yi * puzzle_structure$piece_height - puzzle_structure$piece_height * 0.25

      # Build background rect if needed
      background_rect <- ""
      if (current_bg != "transparent" && current_bg != "none" && current_bg != "") {
        background_rect <- sprintf('  <rect x="%.2f" y="%.2f" width="%.0f" height="%.0f" fill="%s"/>\n',
                                  x_offset, y_offset, piece_width, piece_height, current_bg)
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
        piece_path, current_fill, current_stroke, stroke_width)

      # Save to file
      filename <- sprintf("%s/piece_%02d_%02d.svg", output_dir, xi, yi)
      writeLines(svg_content, filename)
    }
  }

  cat(sprintf("\nSaved %d individual piece files to %s/\n", xn * yn, output_dir))
}

# Generate individual piece files (transparent - PowerPoint compatible)
save_individual_piece_svgs(puzzle_structure,
                          output_dir = "output/individual_pieces",
                          stroke_width = 1.5,
                          stroke_color = "black")

# Generate individual piece files (colored)
save_individual_piece_svgs(puzzle_structure,
                          output_dir = "output/individual_pieces_colored",
                          stroke_width = 2.0,
                          stroke_color = "darkblue",
                          fill_color = "#E3F2FD",        # Light blue fill
                          background_color = "white")     # White background

# Generate individual piece files (viridis colors)
save_individual_piece_svgs(puzzle_structure,
                          output_dir = "output/individual_pieces_viridis",
                          stroke_width = 1.5,
                          stroke_color = "white",
                          use_viridis = TRUE)

# Generate individual piece files with custom color matrix
if (requireNamespace("viridis", quietly = TRUE)) {
  # Create custom color matrix using viridis palette
  n_pieces <- puzzle_structure$grid[1] * puzzle_structure$grid[2]
  custom_colors <- viridis::viridis(n_pieces, option = "plasma")  # plasma palette
  # Clean hex colors for PowerPoint compatibility
  custom_colors <- sapply(custom_colors, function(color) {
    if (is.character(color) && nchar(color) == 9 && substr(color, 1, 1) == "#") {
      return(substr(color, 1, 7))  # Remove alpha channel
    }
    return(color)
  })
  color_matrix <- matrix(custom_colors,
                         nrow = puzzle_structure$grid[1],
                         ncol = puzzle_structure$grid[2],
                         byrow = TRUE)

  save_individual_piece_svgs(puzzle_structure,
                            output_dir = "output/individual_pieces_plasma",
                            stroke_width = 2.0,
                            stroke_color = "white",
                            fill_color = color_matrix)
}

cat("All files saved to output/ directory\n")
