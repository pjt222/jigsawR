devtools::load_all()

# Generate puzzle structure using the clean core implementation
puzzle_structure <- generate_puzzle_core(
  seed = 9999,
  grid = c(6, 4), # 6 rows, 4 columns
  size = c(1200, 600), # Size in specified units
  unit = "mm", # Unit: "mm" or "px"
  dpi = 300, # High-quality print DPI
  tabsize = 15, # Tab parameters
  jitter = 2
)

# Generate the complete puzzle (traditional approach)
puzzle_complete <- generate_jigsaw_svg(
  seed = 9999,
  tabsize = 15,
  jitter = 2,
  width = 1200,
  height = 600,
  unit = "mm", # Same unit as puzzle_structure
  dpi = 300, # Same DPI for consistency
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
  colors = "black", # Black lines for laser cutting
  stroke_width = 1.5, # Appropriate for laser cutting
  background = "none" # No background for cutting
)

# Save separated pieces
writeLines(separated_puzzle, "output/custom_jigsaw_separated.svg")

# Calculate layout dimensions
original_area <- puzzle_structure$size[1] * puzzle_structure$size[2]
separated_width <- puzzle_structure$size[1] + (puzzle_structure$grid[2] - 1) * kerf_offset
separated_height <- puzzle_structure$size[2] + (puzzle_structure$grid[1] - 1) * kerf_offset
separated_area <- separated_width * separated_height

# Print summary
cat(sprintf(
  "Puzzle: %dx%d grid (%d pieces), %.0fx%.0f%s, %d DPI, offset=%d%s\n",
  puzzle_structure$grid[2], puzzle_structure$grid[1],
  puzzle_structure$grid[1] * puzzle_structure$grid[2],
  puzzle_structure$size[1], puzzle_structure$size[2], puzzle_structure$unit,
  puzzle_structure$dpi, kerf_offset,
  if (puzzle_structure$unit == "px") "px" else "mm"
))
cat(sprintf(
  "Layout: %.0fx%.0f%s → %.0fx%.0f%s (%.1f%% increase)\n",
  puzzle_structure$size[1], puzzle_structure$size[2], puzzle_structure$unit,
  separated_width, separated_height, puzzle_structure$unit,
  (separated_area - original_area) / original_area * 100
))

# Function to generate text overlay SVG for PowerPoint editing
generate_text_overlay_svg <- function(puzzle_structure,
                                      label_text = NULL,
                                      label_size = 20,
                                      label_color = "black",
                                      label_font = "Times New Roman",
                                      add_label_background = FALSE,
                                      label_bg_opacity = 0.8,
                                      label_bg_color = "white") {
  xn <- puzzle_structure$grid[2]
  yn <- puzzle_structure$grid[1]

  # Calculate puzzle dimensions
  puzzle_width <- puzzle_structure$size_mm[1]
  puzzle_height <- puzzle_structure$size_mm[2]

  # Start SVG with same dimensions as puzzle
  svg_content <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1"
     width="%.0f" height="%.0f" viewBox="0 0 %.0f %.0f">
', puzzle_width, puzzle_height, puzzle_width, puzzle_height)

  # Generate text elements for each piece position
  for (yi in 0:(yn - 1)) {
    for (xi in 0:(xn - 1)) {
      # Calculate center position for text
      center_x <- xi * puzzle_structure$piece_width + puzzle_structure$piece_width / 2
      center_y <- yi * puzzle_structure$piece_height + puzzle_structure$piece_height / 2

      # Extract label text for this piece
      current_label <- if (is.matrix(label_text)) {
        label_text[yi + 1, xi + 1]
      } else if (is.vector(label_text) && length(label_text) > 1) {
        piece_index <- yi * xn + xi + 1
        if (piece_index <= length(label_text)) label_text[piece_index] else sprintf("%d-%d", xi, yi)
      } else if (!is.null(label_text)) {
        as.character(label_text)
      } else {
        sprintf("%d-%d", xi, yi) # Default: coordinates
      }

      # Optional label background
      if (add_label_background) {
        # Estimate text width (rough approximation)
        text_width <- nchar(current_label) * label_size * 0.6
        text_height <- label_size * 1.2
        bg_x <- center_x - text_width / 2
        bg_y <- center_y - text_height / 2

        svg_content <- paste0(svg_content, sprintf(
          '  <rect x="%.2f" y="%.2f" width="%.2f" height="%.2f" fill="%s" fill-opacity="%.2f" stroke="none"/>\n',
          bg_x, bg_y, text_width, text_height, label_bg_color, label_bg_opacity
        ))
      }

      # Text element
      svg_content <- paste0(svg_content, sprintf(
        '  <text x="%.2f" y="%.2f" font-family="%s" font-size="%d" fill="%s" text-anchor="middle" dominant-baseline="central">%s</text>\n',
        center_x, center_y, label_font, as.integer(label_size), label_color, current_label
      ))
    }
  }

  # Close SVG
  svg_content <- paste0(svg_content, "</svg>")

  return(svg_content)
}

# Function to save individual pieces as separate SVG files
save_individual_piece_svgs <- function(puzzle_structure,
                                       output_dir = "individual_pieces",
                                       stroke_width = 1.5,
                                       stroke_color = "black",
                                       fill_color = "transparent", # Default transparent fill (can be matrix) - PowerPoint compatible
                                       background_color = "transparent", # Default transparent background (can be matrix) - PowerPoint compatible
                                       use_viridis = FALSE, # Auto-generate viridis colors
                                       add_labels = FALSE, # Enable text labels
                                       label_text = NULL, # Custom text (can be matrix)
                                       label_size = 20, # Font size in pixels
                                       label_color = "black", # Text color
                                       label_font = "Times New Roman", # Default font (PowerPoint-friendly)
                                       add_label_background = FALSE, # Optional white background behind text
                                       label_bg_opacity = 0.8, # Background opacity if enabled
                                       text_overlay_file = NULL) { # Generate PowerPoint-editable text overlay SVG

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
      stop(sprintf(
        "stroke_color matrix dimensions (%dx%d) must match grid (%dx%d)",
        nrow(stroke_color), ncol(stroke_color), yn, xn
      ))
    }
  }
  if (is.matrix(fill_color)) {
    if (!all(dim(fill_color) == c(yn, xn))) {
      stop(sprintf(
        "fill_color matrix dimensions (%dx%d) must match grid (%dx%d)",
        nrow(fill_color), ncol(fill_color), yn, xn
      ))
    }
  }
  if (is.matrix(background_color)) {
    if (!all(dim(background_color) == c(yn, xn))) {
      stop(sprintf(
        "background_color matrix dimensions (%dx%d) must match grid (%dx%d)",
        nrow(background_color), ncol(background_color), yn, xn
      ))
    }
  }
  if (is.matrix(label_text)) {
    if (!all(dim(label_text) == c(yn, xn))) {
      stop(sprintf(
        "label_text matrix dimensions (%dx%d) must match grid (%dx%d)",
        nrow(label_text), ncol(label_text), yn, xn
      ))
    }
  }

  # Generate and save each piece
  for (yi in 0:(yn - 1)) {
    for (xi in 0:(xn - 1)) {
      # Generate piece path
      piece_path <- generate_single_piece(xi, yi, puzzle_structure)

      # Extract colors for this piece (matrix or single value)
      current_stroke <- if (is.matrix(stroke_color)) stroke_color[yi + 1, xi + 1] else stroke_color
      current_fill <- if (is.matrix(fill_color)) fill_color[yi + 1, xi + 1] else fill_color
      current_bg <- if (is.matrix(background_color)) background_color[yi + 1, xi + 1] else background_color

      # Clean hex colors for PowerPoint compatibility
      current_stroke <- clean_hex_color(current_stroke)
      current_fill <- clean_hex_color(current_fill)
      current_bg <- clean_hex_color(current_bg)

      # Calculate piece bounding box for proper viewBox
      piece_width <- puzzle_structure$piece_width * 1.5 # Add padding
      piece_height <- puzzle_structure$piece_height * 1.5
      x_offset <- xi * puzzle_structure$piece_width - puzzle_structure$piece_width * 0.25
      y_offset <- yi * puzzle_structure$piece_height - puzzle_structure$piece_height * 0.25

      # Build background rect if needed
      background_rect <- ""
      if (current_bg != "transparent" && current_bg != "none" && current_bg != "") {
        background_rect <- sprintf(
          '  <rect x="%.2f" y="%.2f" width="%.0f" height="%.0f" fill="%s"/>\n',
          x_offset, y_offset, piece_width, piece_height, current_bg
        )
      }

      # Build label text if needed
      label_element <- ""
      if (add_labels) {
        # Calculate center position for text
        center_x <- xi * puzzle_structure$piece_width + puzzle_structure$piece_width / 2
        center_y <- yi * puzzle_structure$piece_height + puzzle_structure$piece_height / 2

        # Extract label text for this piece
        current_label <- if (is.matrix(label_text)) {
          label_text[yi + 1, xi + 1]
        } else if (is.vector(label_text) && length(label_text) > 1) {
          piece_index <- yi * xn + xi + 1
          if (piece_index <= length(label_text)) label_text[piece_index] else sprintf("%d-%d", xi, yi)
        } else if (!is.null(label_text)) {
          as.character(label_text)
        } else {
          sprintf("%d-%d", xi, yi) # Default: coordinates
        }

        # Optional label background
        label_bg <- ""
        if (add_label_background) {
          # Estimate text width (rough approximation)
          text_width <- nchar(current_label) * label_size * 0.6
          text_height <- label_size * 1.2
          bg_x <- center_x - text_width / 2
          bg_y <- center_y - text_height / 2

          label_bg <- sprintf(
            '  <rect x="%.2f" y="%.2f" width="%.2f" height="%.2f" fill="white" fill-opacity="%.2f" stroke="none"/>\n',
            bg_x, bg_y, text_width, text_height, label_bg_opacity
          )
        }

        # Text element
        label_element <- sprintf(
          '%s  <text x="%.2f" y="%.2f" font-family="%s" font-size="%d" fill="%s" text-anchor="middle" dominant-baseline="central">%s</text>\n',
          label_bg, center_x, center_y, label_font, as.integer(label_size), label_color, current_label
        )
      }

      # Create individual SVG for this piece
      svg_content <- sprintf(
        '<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1"
     width="%.0f" height="%.0f" viewBox="%.2f %.2f %.0f %.0f">
%s  <path d="%s" fill="%s" stroke="%s" stroke-width="%.1f" stroke-linecap="round" stroke-linejoin="round"/>
%s</svg>',
        piece_width, piece_height,
        x_offset, y_offset, piece_width, piece_height,
        background_rect,
        piece_path, current_fill, current_stroke, stroke_width,
        label_element
      )

      # Save to file
      filename <- sprintf("%s/piece_%02d_%02d.svg", output_dir, xi, yi)
      writeLines(svg_content, filename)
    }
  }

  cat(sprintf("\nSaved %d individual piece files to %s/\n", xn * yn, output_dir))

  # Generate text overlay SVG if requested (for PowerPoint editing)
  if (!is.null(text_overlay_file)) {
    overlay_content <- generate_text_overlay_svg(
      puzzle_structure = puzzle_structure,
      label_text = label_text,
      label_size = label_size,
      label_color = label_color,
      label_font = label_font,
      add_label_background = add_label_background,
      label_bg_opacity = label_bg_opacity
    )

    # Ensure output/ prefix for overlay file
    if (!grepl("^output/", text_overlay_file)) {
      text_overlay_file <- file.path("output", text_overlay_file)
    }

    writeLines(overlay_content, text_overlay_file)
    cat(sprintf("Saved text overlay for PowerPoint editing: %s\n", text_overlay_file))
  }
}

# Generate individual piece files (transparent - PowerPoint compatible)
save_individual_piece_svgs(puzzle_structure,
  output_dir = "output/individual_pieces",
  stroke_width = 1.5,
  stroke_color = "black"
)

# Generate individual piece files (colored)
save_individual_piece_svgs(puzzle_structure,
  output_dir = "output/individual_pieces_colored",
  stroke_width = 2.0,
  stroke_color = "darkblue",
  fill_color = "#E3F2FD", # Light blue fill
  background_color = "white"
) # White background

# Generate individual piece files (viridis colors)
save_individual_piece_svgs(puzzle_structure,
  output_dir = "output/individual_pieces_viridis",
  stroke_width = 1.5,
  stroke_color = "white",
  use_viridis = TRUE
)

# Generate individual piece files with custom color matrix
if (requireNamespace("viridis", quietly = TRUE)) {
  # Create custom color matrix using viridis palette
  n_pieces <- puzzle_structure$grid[1] * puzzle_structure$grid[2]
  custom_colors <- viridis::viridis(n_pieces, option = "plasma") # plasma palette
  # Clean hex colors for PowerPoint compatibility
  custom_colors <- sapply(custom_colors, function(color) {
    if (is.character(color) && nchar(color) == 9 && substr(color, 1, 1) == "#") {
      return(substr(color, 1, 7)) # Remove alpha channel
    }
    return(color)
  })
  color_matrix <- matrix(custom_colors,
    nrow = puzzle_structure$grid[1],
    ncol = puzzle_structure$grid[2],
    byrow = TRUE
  )

  save_individual_piece_svgs(puzzle_structure,
    output_dir = "output/individual_pieces_plasma",
    stroke_width = 2.0,
    stroke_color = "white",
    fill_color = color_matrix
  )
}

# Generate individual piece files with text labels (coordinate labels)
save_individual_piece_svgs(puzzle_structure,
  output_dir = "output/individual_pieces_labeled",
  stroke_width = 2.0,
  stroke_color = "darkblue",
  fill_color = "#F0F8FF", # AliceBlue fill
  add_labels = TRUE, # Enable labels
  label_size = 24, # Font size
  label_color = "darkblue"
) # Text color matching stroke

# Generate individual piece files with custom text matrix
if (requireNamespace("viridis", quietly = TRUE)) {
  # Create custom label matrix with piece numbers
  label_matrix <- matrix(paste("P", 1:(puzzle_structure$grid[1] * puzzle_structure$grid[2])),
    nrow = puzzle_structure$grid[1],
    ncol = puzzle_structure$grid[2],
    byrow = TRUE
  )

  save_individual_piece_svgs(puzzle_structure,
    output_dir = "output/individual_pieces_numbered",
    stroke_width = 1.5,
    stroke_color = "white",
    use_viridis = TRUE, # Viridis colors
    add_labels = TRUE, # Enable labels
    label_text = label_matrix, # Custom text matrix
    label_size = 20, # Font size
    label_color = "white", # White text on colored background
    add_label_background = TRUE, # Semi-transparent background
    label_bg_opacity = 0.7
  ) # Background opacity
}

# PowerPoint-Compatible Workflow: Separate pieces and editable text overlay
save_individual_piece_svgs(puzzle_structure,
  output_dir = "output/powerpoint_pieces",
  stroke_width = 1.5,
  stroke_color = "black",
  fill_color = "transparent", # No labels on pieces
  add_labels = FALSE, # Important: no embedded labels
  text_overlay_file = "powerpoint_text_overlay.svg"
) # Separate text file

# Alternative: Custom labels with overlay
if (requireNamespace("viridis", quietly = TRUE)) {
  # Create custom label matrix
  custom_labels <- matrix(paste("Piece", 1:(puzzle_structure$grid[1] * puzzle_structure$grid[2])),
    nrow = puzzle_structure$grid[1],
    ncol = puzzle_structure$grid[2],
    byrow = TRUE
  )

  save_individual_piece_svgs(puzzle_structure,
    output_dir = "output/powerpoint_pieces_colored",
    stroke_width = 1.5,
    stroke_color = "white",
    use_viridis = TRUE, # Colored pieces
    add_labels = FALSE, # No embedded labels
    text_overlay_file = "powerpoint_custom_text_overlay.svg",
    label_text = custom_labels, # Custom text for overlay
    label_size = 18,
    label_color = "darkblue",
    add_label_background = TRUE,
    label_bg_opacity = 0.9
  )
}

cat("All files saved to output/ directory\n")
cat("\n=== PowerPoint Workflow Instructions ===\n")
cat("1. Import puzzle pieces (e.g., output/powerpoint_pieces/piece_*.svg)\n")
cat("2. Import text overlay (e.g., output/powerpoint_text_overlay.svg)\n")
cat("3. Align the text overlay exactly over the puzzle pieces\n")
cat("4. Right-click text overlay → 'Convert to Shape' → Ungroup\n")
cat("5. Individual text boxes are now editable in PowerPoint!\n")
cat("6. Tip: Lock puzzle pieces layer to prevent accidental movement\n")
