#!/usr/bin/env Rscript

# Create Properly Labeled Circular Puzzle using REAL piece centers
# This uses the calculated centers from actual puzzle analysis
cat("🏷️ CREATING PROPERLY LABELED CIRCULAR PUZZLE\n")

# Load the analysis results
if (!file.exists("output/circular_puzzle_analysis.rds")) {
  stop("Run analyze_circular_puzzle_properly.R first to generate analysis")
}

analysis_data <- readRDS("output/circular_puzzle_analysis.rds")
piece_centers <- analysis_data$piece_centers
puzzle_structure <- analysis_data$puzzle_structure

cat("Loaded analysis with", length(piece_centers), "piece centers\n")

# Function to create labeled SVG using REAL piece positions
create_labeled_circular_svg <- function(piece_centers, puzzle_structure) {
  
  # Define colors for pieces
  colors <- c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12", "#9B59B6",
             "#1ABC9C", "#E67E22", "#34495E", "#F1C40F", "#E91E63",
             "#00BCD4", "#4CAF50", "#FF9800", "#607D8B", "#795548",
             "#FF5722", "#009688", "#8BC34A", "#FFC107", "#673AB7")
  
  # Start SVG with proper viewBox (same as original circular puzzle)
  svg_lines <- c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<svg xmlns="http://www.w3.org/2000/svg" version="1.0" width="240.0mm" height="240.0mm" viewBox="0 0 240.0 240.0">',
    '  <title>Circular Puzzle - Properly Labeled with Real Piece Centers</title>',
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
    '  <rect width="100%" height="100%" fill="#fafafa"/>',
    '  <g id="circular-puzzle-with-real-labels">'
  )
  
  # Add the original puzzle paths (semi-transparent) with enhanced styling
  for (i in seq_along(puzzle_structure$puzzle_paths)) {
    path_info <- puzzle_structure$puzzle_paths[[i]]
    
    svg_lines <- c(svg_lines,
      sprintf('    <path fill="none" stroke="#555" stroke-width="2" opacity="0.6" filter="url(#shadow)" d="%s"/>',
              path_info$path_data)
    )
  }
  
  # Add the circular border
  if (length(puzzle_structure$border_paths) > 0) {
    border <- puzzle_structure$border_paths[[1]]
    svg_lines <- c(svg_lines,
      sprintf('    <path fill="none" stroke="#2C3E50" stroke-width="3" d="%s"/>',
              border$path_data)
    )
  }
  
  # Add title
  svg_lines <- c(svg_lines,
    '    <text x="120" y="25" text-anchor="middle" font-family="Arial, sans-serif" font-size="16" font-weight="bold" fill="#2c3e50">Circular Puzzle - Real Piece Centers</text>',
    sprintf('    <text x="120" y="40" text-anchor="middle" font-family="Arial, sans-serif" font-size="12" fill="#7f8c8d">%d pieces extracted from actual puzzle paths</text>', length(piece_centers))
  )
  
  # Add labels at REAL calculated piece centers
  bezier_count <- 0
  for (piece in piece_centers) {
    color_idx <- ((piece$piece_id - 1) %% length(colors)) + 1
    color <- colors[color_idx]
    
    # Enhanced styling for pieces with bezier curves
    if (piece$has_bezier) {
      bezier_count <- bezier_count + 1
      radius <- 12
      stroke_width <- 2.5
      filter_effect <- 'filter="url(#glow)"'
      symbol <- "✓"
    } else {
      radius <- 10
      stroke_width <- 2
      filter_effect <- 'filter="url(#shadow)"'
      symbol <- "○"
    }
    
    svg_lines <- c(svg_lines,
      sprintf('    <g id="piece-%d">', piece$piece_id),
      sprintf('      <circle cx="%.2f" cy="%.2f" r="%d" fill="white" fill-opacity="0.9" stroke="%s" stroke-width="%.1f" %s/>',
              piece$center_x, piece$center_y, radius, color, stroke_width, filter_effect),
      sprintf('      <text x="%.2f" y="%.2f" text-anchor="middle" dominant-baseline="central" font-family="Arial, sans-serif" font-size="10" font-weight="bold" fill="%s">%d</text>',
              piece$center_x, piece$center_y, color, piece$piece_id),
      sprintf('      <text x="%.2f" y="%.2f" text-anchor="middle" font-family="Arial, sans-serif" font-size="8" fill="%s" opacity="0.7">%s</text>',
              piece$center_x, piece$center_y + 20, color, symbol),
      '    </g>'
    )
  }
  
  # Add legend with analysis results
  simple_count <- length(piece_centers) - bezier_count
  svg_lines <- c(svg_lines,
    '    <g id="legend">',
    '      <rect x="10" y="200" width="220" height="30" fill="white" fill-opacity="0.9" stroke="#bdc3c7" stroke-width="1"/>',
    '      <text x="20" y="215" font-family="Arial, sans-serif" font-size="10" font-weight="bold" fill="#2c3e50">Real Piece Analysis:</text>',
    sprintf('      <text x="20" y="225" font-family="Arial, sans-serif" font-size="9" fill="#27ae60">✓ Bezier pieces: %d | ○ Simple pieces: %d | Coverage: %.1f%%</text>', 
            bezier_count, simple_count, (bezier_count/length(piece_centers))*100),
    '    </g>'
  )
  
  # Close SVG
  svg_lines <- c(svg_lines, '  </g>', '</svg>')
  
  return(paste(svg_lines, collapse = "\n"))
}

# Generate the properly labeled SVG
labeled_svg <- create_labeled_circular_svg(piece_centers, puzzle_structure)

# Save the properly labeled circular puzzle
output_file <- "output/circular_puzzle_properly_labeled.svg"
writeLines(labeled_svg, output_file)

# Create summary of the proper approach
summary_data <- list(
  approach = "reproducible_analysis",
  method = "path_based_center_calculation",
  timestamp = Sys.time(),
  piece_centers = piece_centers,
  total_pieces = length(piece_centers),
  pieces_with_bezier = sum(sapply(piece_centers, function(p) p$has_bezier)),
  success_rate = round((sum(sapply(piece_centers, function(p) p$has_bezier)) / length(piece_centers)) * 100, 1),
  output_file = output_file,
  source_analysis = "output/circular_puzzle_analysis.rds"
)

saveRDS(summary_data, "output/circular_puzzle_labeled_properly.rds")

cat("\n✅ PROPERLY LABELED CIRCULAR PUZZLE CREATED\n")
cat("📊 PROPER RESULTS:\n")
cat("- Approach: Reproducible function-based analysis\n")
cat("- Method: Calculated centers from actual puzzle paths\n")
cat("- Total pieces:", length(piece_centers), "\n")
cat("- Pieces with bezier curves:", sum(sapply(piece_centers, function(p) p$has_bezier)), sprintf("(%.1f%%)\n", summary_data$success_rate))
cat("- Labels placed at: REAL piece centers (not manual positions)\n")
cat("- Output file:", output_file, "\n")

cat("\n🎯 WHY THIS IS THE CORRECT APPROACH:\n")
cat("✅ Analyzes EXISTING puzzle structure instead of manual manipulation\n")
cat("✅ Calculates piece centers from ACTUAL paths, not estimated positions\n") 
cat("✅ Uses reproducible functions that can work with any circular puzzle\n")
cat("✅ Preserves original puzzle geometry while adding labels\n")
cat("✅ Foundation for proper individual piece extraction\n")

cat("\n📝 NEXT STEP:\n")
cat("Use these REAL piece centers for accurate individual piece extraction\n")
cat("Each piece center represents an actual segment of the puzzle with real tabs/blanks\n")