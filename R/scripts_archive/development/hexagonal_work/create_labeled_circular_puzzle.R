#!/usr/bin/env Rscript

# Create Circular Puzzle with Piece Labels
cat("🏷️ CREATING CIRCULAR PUZZLE WITH PIECE LABELS\n")

# Read the correct circular puzzle
circular_file <- "output/circular_puzzle.svg"
if (!file.exists(circular_file)) {
  stop("Circular puzzle file not found: ", circular_file)
}

svg_content <- readLines(circular_file)
cat("Loaded circular puzzle SVG with", length(svg_content), "lines\n")

# Parse the SVG to understand its structure
cat("\n=== ANALYZING CIRCULAR PUZZLE STRUCTURE ===\n")

# Find all path elements
path_elements <- grep('<path', svg_content, value = TRUE)
cat("Found", length(path_elements), "path elements\n")

# Analyze each path
piece_info <- list()
for (i in seq_along(path_elements)) {
  path_line <- path_elements[i]
  
  # Extract the d attribute
  d_match <- regmatches(path_line, regexpr('d="[^"]*"', path_line))
  if (length(d_match) > 0) {
    path_data <- gsub('d="', '', d_match)
    path_data <- gsub('".*', '', path_data)
    
    # Analyze path characteristics
    has_bezier <- grepl(' C ', path_data)
    has_arc <- grepl(' a ', path_data)
    is_circular_border <- grepl('a \\d+', path_data) && grepl('stroke-width="1.5"', path_line)
    
    # Extract coordinates for center calculation
    coords <- regmatches(path_data, gregexpr("[-\\d\\.]+", path_data))[[1]]
    coords_numeric <- as.numeric(coords[!is.na(as.numeric(coords))])
    
    if (length(coords_numeric) >= 4) {
      x_coords <- coords_numeric[seq(1, length(coords_numeric), by = 2)]
      y_coords <- coords_numeric[seq(2, length(coords_numeric), by = 2)]
      
      center_x <- mean(x_coords, na.rm = TRUE)
      center_y <- mean(y_coords, na.rm = TRUE)
      
      piece_info[[i]] <- list(
        id = i,
        path = path_data,
        center_x = center_x,
        center_y = center_y,
        has_bezier = has_bezier,
        has_arc = has_arc,
        is_border = is_circular_border,
        stroke_width = if(is_circular_border) "1.5" else "1.0"
      )
      
      cat(sprintf("Piece %2d: Center(%.1f, %.1f) %s %s %s\n",
                  i, center_x, center_y,
                  if(has_bezier) "bezier" else "lines ",
                  if(has_arc) "arc" else "   ",
                  if(is_circular_border) "BORDER" else "piece "))
    }
  }
}

# Filter out the circular border - we want to label individual pieces only
puzzle_pieces <- piece_info[sapply(piece_info, function(x) !x$is_border)]
border_piece <- piece_info[sapply(piece_info, function(x) x$is_border)]

cat(sprintf("\nIdentified: %d puzzle pieces + %d border element\n", 
            length(puzzle_pieces), length(border_piece)))

# Create the labeled SVG
cat("\n=== CREATING LABELED SVG ===\n")

# Define colors for pieces
colors <- c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12", "#9B59B6",
           "#1ABC9C", "#E67E22", "#34495E", "#F1C40F", "#E91E63",
           "#00BCD4", "#4CAF50", "#FF9800", "#607D8B", "#795548",
           "#FF5722", "#009688", "#8BC34A", "#FFC107", "#673AB7",
           "#FF4444", "#44FF44", "#4444FF", "#FFFF44", "#FF44FF",
           "#44FFFF", "#888888", "#444444", "#CCCCCC", "#666666")

# Start building the new SVG
labeled_svg <- c(
  '<svg xmlns="http://www.w3.org/2000/svg" version="1.0" width="240.0mm" height="240.0mm" viewBox="0 0 240.0 240.0">',
  '  <title>Circular Puzzle with Piece Labels</title>',
  '  <defs>',
  '    <filter id="shadow" x="-50%" y="-50%" width="200%" height="200%">',
  '      <feDropShadow dx="1" dy="1" stdDeviation="1" flood-opacity="0.3"/>',
  '    </filter>',
  '  </defs>',
  '  <rect width="100%" height="100%" fill="#f8f9fa"/>',
  '  <g id="circular-puzzle-labeled">'
)

# Add each puzzle piece with labels
for (i in seq_along(puzzle_pieces)) {
  piece <- puzzle_pieces[[i]]
  color_idx <- ((i - 1) %% length(colors)) + 1
  color <- colors[color_idx]
  
  # Create piece group
  labeled_svg <- c(labeled_svg,
    sprintf('    <g id="piece-%d">', i),
    sprintf('      <path fill="%s" fill-opacity="0.15" stroke="%s" stroke-width="2" filter="url(#shadow)" d="%s"/>', 
            color, color, piece$path),
    sprintf('      <circle cx="%.1f" cy="%.1f" r="8" fill="white" fill-opacity="0.9" stroke="%s" stroke-width="1"/>',
            piece$center_x, piece$center_y, color),
    sprintf('      <text x="%.1f" y="%.1f" text-anchor="middle" dominant-baseline="central" font-family="Arial, sans-serif" font-size="10" font-weight="bold" fill="%s">%d</text>',
            piece$center_x, piece$center_y + 1, color, i),
    '    </g>'
  )
}

# Add the circular border (if exists)
if (length(border_piece) > 0) {
  border <- border_piece[[1]]
  labeled_svg <- c(labeled_svg,
    sprintf('    <g id="border">'),
    sprintf('      <path fill="none" stroke="#2C3E50" stroke-width="3" d="%s"/>', border$path),
    '    </g>'
  )
}

# Add title and summary
labeled_svg <- c(labeled_svg,
  '    <g id="info">',
  '      <text x="120" y="20" text-anchor="middle" font-family="Arial, sans-serif" font-size="16" font-weight="bold" fill="#2c3e50">Circular Puzzle - Labeled</text>',
  sprintf('      <text x="120" y="35" text-anchor="middle" font-family="Arial, sans-serif" font-size="12" fill="#7f8c8d">%d pieces with tabs and blanks</text>', length(puzzle_pieces)),
  '    </g>'
)

# Close SVG
labeled_svg <- c(labeled_svg, '  </g>', '</svg>')

# Save the labeled circular puzzle
output_file <- "output/circular_puzzle_labeled.svg"
writeLines(labeled_svg, output_file)

# Create a summary
summary_data <- list(
  total_pieces = length(puzzle_pieces),
  has_border = length(border_piece) > 0,
  pieces = puzzle_pieces,
  colors_used = colors[1:min(length(puzzle_pieces), length(colors))],
  output_file = output_file
)

saveRDS(summary_data, "output/circular_puzzle_labeled.rds")

cat("\n✅ LABELED CIRCULAR PUZZLE CREATED\n")
cat("Results:\n")
cat("- Total puzzle pieces:", length(puzzle_pieces), "\n")
cat("- Has circular border:", length(border_piece) > 0, "\n")
cat("- Output file:", output_file, "\n")
cat("- Data file: output/circular_puzzle_labeled.rds\n")

cat("\n🎯 NEXT STEPS:\n")
cat("1. Review the labeled puzzle to verify piece identification\n")
cat("2. Use this as foundation for individual piece extraction\n")
cat("3. Each piece now has a clear ID and center point for reference\n")

cat("\n📊 PIECE BREAKDOWN:\n")
bezier_pieces <- sum(sapply(puzzle_pieces, function(x) x$has_bezier))
cat("- Pieces with bezier curves (real tabs):", bezier_pieces, "\n")
cat("- Success rate:", round(bezier_pieces/length(puzzle_pieces)*100, 1), "%\n")