#!/usr/bin/env Rscript

# Create Circular Puzzle with Piece Labels - Fixed Version
cat("🏷️ CREATING CIRCULAR PUZZLE WITH PIECE LABELS (FIXED)\n")

# Read the circular puzzle SVG
circular_file <- "output/circular_puzzle.svg"
if (!file.exists(circular_file)) {
  stop("Circular puzzle file not found: ", circular_file)
}

# Read as one long string since it's all on one line
svg_content <- paste(readLines(circular_file), collapse = "")
cat("Loaded circular puzzle SVG\n")

# Parse paths from the single-line SVG
path_matches <- gregexpr('<path[^>]*d="[^"]*"[^>]*/?>', svg_content)[[1]]
if (path_matches[1] == -1) {
  stop("No path elements found in SVG")
}

paths <- character()
for (i in seq_along(path_matches)) {
  start <- path_matches[i]
  length_val <- attr(path_matches, "match.length")[i]
  paths[i] <- substr(svg_content, start, start + length_val - 1)
}

cat("Found", length(paths), "path elements\n")

# Analyze each path
cat("\n=== ANALYZING CIRCULAR PUZZLE PATHS ===\n")

piece_info <- list()
for (i in seq_along(paths)) {
  path_line <- paths[i]
  
  # Extract the d attribute value
  d_match <- regmatches(path_line, regexpr('d="[^"]*"', path_line))
  if (length(d_match) > 0) {
    path_data <- gsub('d="', '', d_match)
    path_data <- gsub('".*', '', path_data)
    
    # Check path characteristics
    has_bezier <- grepl(' C ', path_data)
    has_arc <- grepl(' a ', path_data)
    stroke_width <- if(grepl('stroke-width="1.5"', path_line)) "1.5" else "1.0"
    is_border <- has_arc && stroke_width == "1.5"
    
    # Extract first few coordinates to estimate center
    coords_text <- regmatches(path_data, gregexpr("[-\\d\\.]+", path_data))[[1]]
    coords_num <- as.numeric(coords_text)
    coords_num <- coords_num[!is.na(coords_num)]
    
    if (length(coords_num) >= 4) {
      # Take first few coordinate pairs for center estimation
      sample_size <- min(20, length(coords_num))
      sample_coords <- coords_num[1:sample_size]
      
      x_coords <- sample_coords[seq(1, length(sample_coords), by = 2)]
      y_coords <- sample_coords[seq(2, length(sample_coords), by = 2)]
      
      center_x <- mean(x_coords, na.rm = TRUE)
      center_y <- mean(y_coords, na.rm = TRUE)
      
      piece_info[[i]] <- list(
        id = i,
        path = path_data,
        center_x = center_x,
        center_y = center_y,
        has_bezier = has_bezier,
        has_arc = has_arc,
        is_border = is_border,
        stroke_width = stroke_width,
        path_length = nchar(path_data)
      )
      
      type_str <- if(is_border) "BORDER" else if(has_bezier) "piece " else "simple"
      cat(sprintf("Path %2d: Center(%.1f, %.1f) len=%4d %s %s\n",
                  i, center_x, center_y, nchar(path_data),
                  type_str, if(has_bezier) "✓bezier" else "○lines"))
    }
  }
}

# Separate border from puzzle pieces
puzzle_pieces <- list()
border_pieces <- list()

for (piece in piece_info) {
  if (piece$is_border) {
    border_pieces[[length(border_pieces) + 1]] <- piece
  } else {
    puzzle_pieces[[length(puzzle_pieces) + 1]] <- piece
  }
}

cat(sprintf("\nIdentified: %d puzzle pieces + %d border elements\n", 
            length(puzzle_pieces), length(border_pieces)))

# Define colors for pieces
colors <- c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12", "#9B59B6",
           "#1ABC9C", "#E67E22", "#34495E", "#F1C40F", "#E91E63",
           "#00BCD4", "#4CAF50", "#FF9800", "#607D8B", "#795548",
           "#FF5722", "#009688", "#8BC34A", "#FFC107", "#673AB7")

# Create labeled SVG
cat("\n=== CREATING LABELED SVG ===\n")

labeled_svg <- c(
  '<?xml version="1.0" encoding="UTF-8"?>',
  '<svg xmlns="http://www.w3.org/2000/svg" version="1.0" width="240.0mm" height="240.0mm" viewBox="0 0 240.0 240.0">',
  '  <title>Circular Puzzle with Piece Labels</title>',
  '  <defs>',
  '    <filter id="shadow" x="-50%" y="-50%" width="200%" height="200%">',
  '      <feDropShadow dx="2" dy="2" stdDeviation="1" flood-opacity="0.3"/>',
  '    </filter>',
  '    <filter id="glow" x="-50%" y="-50%" width="200%" height="200%">',
  '      <feGaussianBlur stdDeviation="1" result="coloredBlur"/>',
  '      <feMerge>',
  '        <feMergeNode in="coloredBlur"/>',
  '        <feMergeNode in="SourceGraphic"/>',
  '      </feMerge>',
  '    </filter>',
  '  </defs>',
  '  <rect width="100%" height="100%" fill="#f9f9f9"/>',
  '  <g id="circular-puzzle-labeled">'
)

# Add title
labeled_svg <- c(labeled_svg,
  '    <text x="120" y="25" text-anchor="middle" font-family="Arial, sans-serif" font-size="18" font-weight="bold" fill="#2c3e50">Circular Puzzle - Labeled</text>',
  sprintf('    <text x="120" y="42" text-anchor="middle" font-family="Arial, sans-serif" font-size="12" fill="#7f8c8d">%d pieces with individual labels</text>', length(puzzle_pieces))
)

# Add each puzzle piece with enhanced styling and labels
for (i in seq_along(puzzle_pieces)) {
  piece <- puzzle_pieces[[i]]
  color_idx <- ((i - 1) %% length(colors)) + 1
  color <- colors[color_idx]
  
  # Enhanced styling based on bezier content
  if (piece$has_bezier) {
    fill_opacity <- "0.25"
    stroke_width <- "2.5"
    filter_effect <- 'filter="url(#glow)"'
    tab_indicator <- "✓"
  } else {
    fill_opacity <- "0.15"
    stroke_width <- "2"
    filter_effect <- 'filter="url(#shadow)"'
    tab_indicator <- "○"
  }
  
  labeled_svg <- c(labeled_svg,
    sprintf('    <g id="piece-%d">', i),
    sprintf('      <path fill="%s" fill-opacity="%s" stroke="%s" stroke-width="%s" %s d="%s"/>', 
            color, fill_opacity, color, stroke_width, filter_effect, piece$path),
    sprintf('      <circle cx="%.1f" cy="%.1f" r="10" fill="white" fill-opacity="0.9" stroke="%s" stroke-width="2"/>',
            piece$center_x, piece$center_y, color),
    sprintf('      <text x="%.1f" y="%.1f" text-anchor="middle" dominant-baseline="central" font-family="Arial, sans-serif" font-size="10" font-weight="bold" fill="%s">%d</text>',
            piece$center_x, piece$center_y, color, i),
    sprintf('      <text x="%.1f" y="%.1f" text-anchor="middle" font-family="Arial, sans-serif" font-size="7" fill="%s" opacity="0.8">%s</text>',
            piece$center_x, piece$center_y + 18, color, tab_indicator),
    '    </g>'
  )
}

# Add the border (if any)
if (length(border_pieces) > 0) {
  for (border in border_pieces) {
    labeled_svg <- c(labeled_svg,
      '    <g id="border">',
      sprintf('      <path fill="none" stroke="#2C3E50" stroke-width="3" d="%s"/>', border$path),
      '    </g>'
    )
  }
}

# Add legend
bezier_count <- sum(sapply(puzzle_pieces, function(x) x$has_bezier))
line_count <- length(puzzle_pieces) - bezier_count

labeled_svg <- c(labeled_svg,
  '    <g id="legend">',
  '      <rect x="10" y="210" width="220" height="25" fill="white" fill-opacity="0.9" stroke="#bdc3c7" stroke-width="1"/>',
  '      <text x="20" y="225" font-family="Arial, sans-serif" font-size="10" font-weight="bold" fill="#2c3e50">Legend:</text>',
  sprintf('      <text x="70" y="225" font-family="Arial, sans-serif" font-size="9" fill="#27ae60">✓ = Real tabs/blanks (%d pieces)</text>', bezier_count),
  sprintf('      <text x="180" y="225" font-family="Arial, sans-serif" font-size="9" fill="#e74c3c">○ = Simple edges (%d pieces)</text>', line_count),
  '    </g>'
)

# Close SVG
labeled_svg <- c(labeled_svg, '  </g>', '</svg>')

# Save the labeled circular puzzle
output_file <- "output/circular_puzzle_labeled.svg"
writeLines(labeled_svg, output_file)

# Create detailed summary
summary_data <- list(
  total_pieces = length(puzzle_pieces),
  bezier_pieces = bezier_count,
  simple_pieces = line_count,
  has_border = length(border_pieces) > 0,
  pieces = puzzle_pieces,
  borders = border_pieces,
  success_rate = round((bezier_count / length(puzzle_pieces)) * 100, 1),
  output_file = output_file
)

saveRDS(summary_data, "output/circular_puzzle_labeled.rds")

cat("\n✅ LABELED CIRCULAR PUZZLE CREATED\n")
cat("📊 RESULTS:\n")
cat("- Total puzzle pieces:", length(puzzle_pieces), "\n")
cat("- Pieces with real tabs/blanks:", bezier_count, sprintf("(%.1f%%)\n", summary_data$success_rate))
cat("- Simple edge pieces:", line_count, "\n")
cat("- Has circular border:", length(border_pieces) > 0, "\n")
cat("- Output file:", output_file, "\n")

cat("\n🎯 FOUNDATION READY:\n")
cat("✅ Each piece has a unique ID (1 to", length(puzzle_pieces), ")\n")
cat("✅ Each piece has calculated center coordinates\n") 
cat("✅ Visual distinction between tab/blank vs simple pieces\n")
cat("✅ Ready for individual piece extraction\n")

if (bezier_count > 0) {
  cat(sprintf("\n🎉 SUCCESS: %.1f%% of pieces have real tabs and blanks!\n", summary_data$success_rate))
}