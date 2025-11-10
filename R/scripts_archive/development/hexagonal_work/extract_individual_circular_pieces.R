#!/usr/bin/env Rscript

# Extract Individual Pieces from Circular Puzzle
# Uses the real piece centers from the properly labeled circular puzzle
cat("🧩 EXTRACTING INDIVIDUAL PIECES FROM CIRCULAR PUZZLE\n")

# Load the analysis results
if (!file.exists("output/circular_puzzle_analysis.rds")) {
  stop("Run analyze_circular_puzzle_properly.R first to generate analysis")
}

analysis_data <- readRDS("output/circular_puzzle_analysis.rds")
piece_centers <- analysis_data$piece_centers
puzzle_structure <- analysis_data$puzzle_structure

cat("Loaded analysis with", length(piece_centers), "piece centers\n")

# Load required functions for path manipulation
source("R/bezier_utils.R")

# Function to extract individual piece boundary from puzzle paths
extract_piece_boundary <- function(piece_center, puzzle_structure, piece_id) {
  # Find the closest puzzle paths to this piece center
  piece_x <- piece_center$center_x
  piece_y <- piece_center$center_y
  
  # For circular puzzles, we need to identify which path segments belong to this piece
  # This is a complex geometric operation requiring careful analysis of the puzzle structure
  
  relevant_paths <- list()
  
  # Analyze each puzzle path to see if it's near this piece center
  for (i in seq_along(puzzle_structure$puzzle_paths)) {
    path_info <- puzzle_structure$puzzle_paths[[i]]
    path_data <- path_info$path_data
    
    # Parse the path to extract coordinates
    parsed_path <- parse_svg_path(path_data)
    
    # Check if any coordinates in this path are close to our piece center
    min_distance <- Inf
    for (coord in parsed_path$coordinates) {
      distance <- sqrt((coord$x - piece_x)^2 + (coord$y - piece_y)^2)
      min_distance <- min(min_distance, distance)
    }
    
    # If path is close to piece center, include it
    if (min_distance < 30) { # Threshold for proximity
      relevant_paths[[length(relevant_paths) + 1]] <- list(
        path_data = path_data,
        distance = min_distance,
        path_index = i
      )
    }
  }
  
  # Sort paths by distance to piece center
  relevant_paths <- relevant_paths[order(sapply(relevant_paths, function(p) p$distance))]
  
  return(relevant_paths)
}

# Function to create individual piece SVG
create_individual_piece_svg <- function(piece_center, relevant_paths, piece_id) {
  # Define colors for pieces
  colors <- c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12", "#9B59B6",
             "#1ABC9C", "#E67E22", "#34495E", "#F1C40F", "#E91E63",
             "#00BCD4", "#4CAF50", "#FF9800", "#607D8B", "#795548",
             "#FF5722", "#009688", "#8BC34A", "#FFC107", "#673AB7")
  
  color_idx <- ((piece_id - 1) %% length(colors)) + 1
  color <- colors[color_idx]
  
  # Create SVG with appropriate viewBox centered on piece
  margin <- 40
  min_x <- piece_center$center_x - margin
  max_x <- piece_center$center_x + margin
  min_y <- piece_center$center_y - margin  
  max_y <- piece_center$center_y + margin
  
  width <- max_x - min_x
  height <- max_y - min_y
  
  svg_lines <- c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    sprintf('<svg xmlns="http://www.w3.org/2000/svg" version="1.0" width="%.1fmm" height="%.1fmm" viewBox="%.1f %.1f %.1f %.1f">', 
            width, height, min_x, min_y, width, height),
    sprintf('  <title>Circular Puzzle - Piece %d</title>', piece_id),
    '  <defs>',
    '    <filter id="shadow" x="-50%" y="-50%" width="200%" height="200%">',
    '      <feDropShadow dx="1" dy="1" stdDeviation="0.5" flood-opacity="0.3"/>',
    '    </filter>',
    '  </defs>',
    '  <rect width="100%" height="100%" fill="#fafafa"/>',
    sprintf('  <g id="piece-%d">', piece_id)
  )
  
  # Add the relevant paths for this piece
  for (path_info in relevant_paths) {
    svg_lines <- c(svg_lines,
      sprintf('    <path d="%s" fill="none" stroke="%s" stroke-width="2" filter="url(#shadow)"/>',
              path_info$path_data, color)
    )
  }
  
  # Add piece center marker
  svg_lines <- c(svg_lines,
    sprintf('    <circle cx="%.2f" cy="%.2f" r="3" fill="%s" fill-opacity="0.8"/>',
            piece_center$center_x, piece_center$center_y, color),
    sprintf('    <text x="%.2f" y="%.2f" text-anchor="middle" dominant-baseline="central" font-family="Arial" font-size="8" font-weight="bold" fill="%s">%d</text>',
            piece_center$center_x, piece_center$center_y, color, piece_id)
  )
  
  # Close SVG
  svg_lines <- c(svg_lines, '  </g>', '</svg>')
  
  return(paste(svg_lines, collapse = "\n"))
}

# Create output directory
if (!dir.exists("output/individual_circular_pieces")) {
  dir.create("output/individual_circular_pieces", recursive = TRUE)
}

# Extract and create individual pieces
cat("Extracting individual pieces...\n")
individual_pieces_data <- list()

for (i in seq_along(piece_centers)) {
  piece_center <- piece_centers[[i]]
  piece_id <- piece_center$piece_id
  
  cat(sprintf("Processing piece %d (center: %.2f, %.2f)...\n", 
              piece_id, piece_center$center_x, piece_center$center_y))
  
  # Extract boundary paths for this piece
  relevant_paths <- extract_piece_boundary(piece_center, puzzle_structure, piece_id)
  
  # Create individual piece SVG
  piece_svg <- create_individual_piece_svg(piece_center, relevant_paths, piece_id)
  
  # Save individual piece
  piece_filename <- sprintf("output/individual_circular_pieces/circular_piece_%02d.svg", piece_id)
  writeLines(piece_svg, piece_filename)
  
  # Store piece data
  individual_pieces_data[[i]] <- list(
    piece_id = piece_id,
    center = c(piece_center$center_x, piece_center$center_y),
    has_bezier = piece_center$has_bezier,
    path_count = length(relevant_paths),
    filename = piece_filename
  )
  
  cat(sprintf("  ✓ Piece %d: %d relevant paths found\n", piece_id, length(relevant_paths)))
}

# Create combined index SVG showing all pieces
cat("Creating combined index...\n")
create_combined_index <- function(piece_centers, individual_pieces_data) {
  svg_lines <- c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<svg xmlns="http://www.w3.org/2000/svg" version="1.0" width="240.0mm" height="240.0mm" viewBox="0 0 240.0 240.0">',
    '  <title>Circular Puzzle - Individual Pieces Index</title>',
    '  <rect width="100%" height="100%" fill="#f8f9fa"/>',
    '  <g id="pieces-index">'
  )
  
  colors <- c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12", "#9B59B6",
             "#1ABC9C", "#E67E22", "#34495E", "#F1C40F", "#E91E63",
             "#00BCD4", "#4CAF50", "#FF9800", "#607D8B", "#795548",
             "#FF5722", "#009688", "#8BC34A", "#FFC107", "#673AB7")
  
  # Add title
  svg_lines <- c(svg_lines,
    '    <text x="120" y="20" text-anchor="middle" font-family="Arial, sans-serif" font-size="14" font-weight="bold" fill="#2c3e50">Individual Circular Puzzle Pieces</text>',
    sprintf('    <text x="120" y="35" text-anchor="middle" font-family="Arial, sans-serif" font-size="10" fill="#7f8c8d">%d pieces extracted • Each piece saved as separate SVG</text>', length(piece_centers))
  )
  
  # Add piece indicators
  for (i in seq_along(piece_centers)) {
    piece <- piece_centers[[i]]
    piece_data <- individual_pieces_data[[i]]
    
    color_idx <- ((piece$piece_id - 1) %% length(colors)) + 1
    color <- colors[color_idx]
    
    # Piece indicator
    svg_lines <- c(svg_lines,
      sprintf('    <circle cx="%.2f" cy="%.2f" r="8" fill="%s" fill-opacity="0.7" stroke="white" stroke-width="2"/>',
              piece$center_x, piece$center_y, color),
      sprintf('    <text x="%.2f" y="%.2f" text-anchor="middle" dominant-baseline="central" font-family="Arial" font-size="8" font-weight="bold" fill="white">%d</text>',
              piece$center_x, piece$center_y, piece$piece_id)
    )
    
    # Path count indicator
    symbol <- if (piece_data$path_count > 0) "✓" else "○"
    svg_lines <- c(svg_lines,
      sprintf('    <text x="%.2f" y="%.2f" text-anchor="middle" font-family="Arial, sans-serif" font-size="6" fill="%s" opacity="0.8">%s</text>',
              piece$center_x, piece$center_y + 15, color, symbol)
    )
  }
  
  # Add legend
  svg_lines <- c(svg_lines,
    '    <g id="extraction-legend">',
    '      <rect x="10" y="200" width="220" height="35" fill="white" fill-opacity="0.9" stroke="#bdc3c7" stroke-width="1"/>',
    '      <text x="20" y="215" font-family="Arial, sans-serif" font-size="10" font-weight="bold" fill="#2c3e50">Extraction Results:</text>',
    sprintf('      <text x="20" y="225" font-family="Arial, sans-serif" font-size="9" fill="#27ae60">%d pieces extracted | All saved to individual_circular_pieces/</text>', length(piece_centers)),
    '      <text x="20" y="235" font-family="Arial, sans-serif" font-size="8" fill="#7f8c8d">✓ = Paths found | ○ = No paths (center only)</text>',
    '    </g>'
  )
  
  # Close SVG
  svg_lines <- c(svg_lines, '  </g>', '</svg>')
  
  return(paste(svg_lines, collapse = "\n"))
}

# Save combined index
index_svg <- create_combined_index(piece_centers, individual_pieces_data)
writeLines(index_svg, "output/individual_circular_pieces_index.svg")

# Save extraction summary
summary_data <- list(
  timestamp = Sys.time(),
  total_pieces = length(piece_centers),
  pieces_with_paths = sum(sapply(individual_pieces_data, function(p) p$path_count > 0)),
  pieces_extracted = length(individual_pieces_data),
  output_directory = "output/individual_circular_pieces/",
  index_file = "output/individual_circular_pieces_index.svg",
  pieces_data = individual_pieces_data
)

saveRDS(summary_data, "output/individual_circular_pieces_summary.rds")

cat("\n✅ INDIVIDUAL CIRCULAR PIECES EXTRACTION COMPLETED\n")
cat("📊 EXTRACTION RESULTS:\n")
cat("- Total pieces:", summary_data$total_pieces, "\n")
cat("- Pieces with paths found:", summary_data$pieces_with_paths, "\n")
cat("- Individual SVG files created:", length(individual_pieces_data), "\n")
cat("- Output directory:", summary_data$output_directory, "\n")
cat("- Index file:", summary_data$index_file, "\n")

cat("\n📂 FILES CREATED:\n")
for (piece_data in individual_pieces_data) {
  status <- if (piece_data$path_count > 0) sprintf("(%d paths)", piece_data$path_count) else "(center only)"
  cat(sprintf("- %s %s\n", basename(piece_data$filename), status))
}

cat("\n🎯 NEXT STEP:\n")
cat("Review the individual piece SVGs to verify proper extraction\n")
cat("Each piece is saved with its surrounding puzzle geometry\n")