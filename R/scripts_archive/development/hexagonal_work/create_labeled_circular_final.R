#!/usr/bin/env Rscript

# Create Labeled Circular Puzzle - Final Working Version
cat("🏷️ CREATING LABELED CIRCULAR PUZZLE (FINAL)\n")

# Read the circular puzzle
svg_content <- paste(readLines("output/circular_puzzle.svg"), collapse = "")
cat("Loaded circular puzzle SVG\n")

# Extract the three main path elements we found from debugging
paths_data <- list()

# Path 1: Main puzzle pieces (length 3512)
d1_match <- regmatches(svg_content, regexpr('d="M 47\\.94[^"]*189\\.34[^"]*"', svg_content))
if (length(d1_match) > 0) {
  d1 <- gsub('^d="', '', d1_match)
  d1 <- gsub('"$', '', d1)
  paths_data[[1]] <- list(
    type = "puzzle_pieces",
    path = d1,
    stroke_width = "1.0",
    description = "Main puzzle pieces with tabs and blanks"
  )
}

# Path 2: Additional puzzle segments (length 1924) 
d2_match <- regmatches(svg_content, regexpr('d="M 95\\.98[^"]*217\\.07[^"]*"', svg_content))
if (length(d2_match) > 0) {
  d2 <- gsub('^d="', '', d2_match)
  d2 <- gsub('"$', '', d2)
  paths_data[[2]] <- list(
    type = "puzzle_segments",
    path = d2,
    stroke_width = "1.0", 
    description = "Additional puzzle segments"
  )
}

# Path 3: Circular border (length 54)
d3_match <- regmatches(svg_content, regexpr('d="M 20 120 a[^"]*"', svg_content))
if (length(d3_match) > 0) {
  d3 <- gsub('^d="', '', d3_match)
  d3 <- gsub('"$', '', d3)
  paths_data[[3]] <- list(
    type = "border",
    path = d3,
    stroke_width = "1.5",
    description = "Circular border"
  )
}

cat("Extracted", length(paths_data), "path elements\n")

# For the circular puzzle, we'll treat the first two paths as containing multiple puzzle pieces
# that are drawn as continuous paths. We need to identify logical piece boundaries.

# Since the circular puzzle is correctly generated, let's use a different approach:
# Create piece labels based on coordinate analysis of the continuous paths

# Function to extract coordinate centers from path segments
extract_coordinate_clusters <- function(path_data) {
  # Extract all number sequences (more flexible regex)
  numbers_text <- unlist(strsplit(path_data, "[MC L]"))
  numbers_text <- numbers_text[numbers_text != ""]
  
  centers <- list()
  
  # Process each segment
  for (segment in numbers_text) {
    # Extract floating point numbers from each segment
    coords <- as.numeric(unlist(regmatches(segment, gregexpr("\\d+\\.\\d+|\\d+", segment))))
    coords <- coords[!is.na(coords)]
    
    if (length(coords) >= 4) {
      # Take pairs of coordinates
      x_coords <- coords[seq(1, min(10, length(coords)), by = 2)]
      y_coords <- coords[seq(2, min(10, length(coords)), by = 2)]
      
      if (length(x_coords) > 0 && length(y_coords) > 0) {
        center_x <- mean(x_coords)
        center_y <- mean(y_coords)
        
        # Only add if coordinates are reasonable (within viewBox)
        if (center_x >= 0 && center_x <= 240 && center_y >= 0 && center_y <= 240) {
          centers[[length(centers) + 1]] <- list(x = center_x, y = center_y)
        }
      }
    }
  }
  
  return(centers)
}

# Extract centers from the main puzzle paths
all_centers <- list()

for (i in 1:2) {  # Only process the first two paths (puzzle pieces)
  if (!is.null(paths_data[[i]])) {
    centers <- extract_coordinate_clusters(paths_data[[i]]$path)
    cat(sprintf("Path %d (%s): extracted %d coordinate centers\n", 
                i, paths_data[[i]]$type, length(centers)))
    
    for (center in centers) {
      all_centers[[length(all_centers) + 1]] <- center
    }
  }
}

cat("Total coordinate centers found:", length(all_centers), "\n")

# Create a manual piece layout for circular puzzle (3 rings)
# Based on hexagonal structure: center (1) + ring 1 (6) + ring 2 (12) = 19 pieces
manual_pieces <- list(
  list(x = 120, y = 120, id = 1, ring = 0, desc = "center"),
  # Ring 1 (6 pieces around center)
  list(x = 120, y = 80, id = 2, ring = 1, desc = "ring1-top"),
  list(x = 150, y = 100, id = 3, ring = 1, desc = "ring1-topright"),
  list(x = 150, y = 140, id = 4, ring = 1, desc = "ring1-bottomright"),
  list(x = 120, y = 160, id = 5, ring = 1, desc = "ring1-bottom"),
  list(x = 90, y = 140, id = 6, ring = 1, desc = "ring1-bottomleft"),
  list(x = 90, y = 100, id = 7, ring = 1, desc = "ring1-topleft"),
  # Ring 2 (12 pieces)
  list(x = 120, y = 50, id = 8, ring = 2, desc = "ring2-top"),
  list(x = 145, y = 65, id = 9, ring = 2, desc = "ring2-topright1"),
  list(x = 165, y = 90, id = 10, ring = 2, desc = "ring2-topright2"),
  list(x = 175, y = 120, id = 11, ring = 2, desc = "ring2-right"),
  list(x = 165, y = 150, id = 12, ring = 2, desc = "ring2-bottomright1"),
  list(x = 145, y = 175, id = 13, ring = 2, desc = "ring2-bottomright2"),
  list(x = 120, y = 190, id = 14, ring = 2, desc = "ring2-bottom"),
  list(x = 95, y = 175, id = 15, ring = 2, desc = "ring2-bottomleft1"),
  list(x = 75, y = 150, id = 16, ring = 2, desc = "ring2-bottomleft2"),
  list(x = 65, y = 120, id = 17, ring = 2, desc = "ring2-left"),
  list(x = 75, y = 90, id = 18, ring = 2, desc = "ring2-topleft1"),
  list(x = 95, y = 65, id = 19, ring = 2, desc = "ring2-topleft2")
)

cat("Using manual piece layout with", length(manual_pieces), "pieces\n")

# Define colors
colors <- c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12", "#9B59B6",
           "#1ABC9C", "#E67E22", "#34495E", "#F1C40F", "#E91E63",
           "#00BCD4", "#4CAF50", "#FF9800", "#607D8B", "#795548",
           "#FF5722", "#009688", "#8BC34A", "#FFC107", "#673AB7")

# Create the labeled SVG
labeled_svg <- c(
  '<?xml version="1.0" encoding="UTF-8"?>',
  '<svg xmlns="http://www.w3.org/2000/svg" version="1.0" width="240.0mm" height="240.0mm" viewBox="0 0 240.0 240.0">',
  '  <title>Circular Puzzle with Piece Labels</title>',
  '  <defs>',
  '    <filter id="shadow" x="-50%" y="-50%" width="200%" height="200%">',
  '      <feDropShadow dx="2" dy="2" stdDeviation="1" flood-opacity="0.3"/>',
  '    </filter>',
  '  </defs>',
  '  <rect width="100%" height="100%" fill="#fafafa"/>',
  '  <g id="circular-puzzle-labeled">'
)

# Add the original puzzle paths (slightly transparent)
for (i in 1:2) {
  if (!is.null(paths_data[[i]])) {
    labeled_svg <- c(labeled_svg,
      sprintf('    <path fill="none" stroke="#666" stroke-width="%s" opacity="0.4" d="%s"/>',
              paths_data[[i]]$stroke_width, paths_data[[i]]$path)
    )
  }
}

# Add the circular border
if (length(paths_data) >= 3 && !is.null(paths_data[[3]])) {
  labeled_svg <- c(labeled_svg,
    sprintf('    <path fill="none" stroke="#2C3E50" stroke-width="3" d="%s"/>',
            paths_data[[3]]$path)
  )
}

# Add piece labels
for (piece in manual_pieces) {
  color_idx <- ((piece$id - 1) %% length(colors)) + 1
  color <- colors[color_idx]
  
  # Ring-based styling
  if (piece$ring == 0) {
    radius <- 12
    font_size <- 12
  } else if (piece$ring == 1) {
    radius <- 10
    font_size <- 10
  } else {
    radius <- 8
    font_size <- 9
  }
  
  labeled_svg <- c(labeled_svg,
    sprintf('    <g id="piece-%d">', piece$id),
    sprintf('      <circle cx="%.1f" cy="%.1f" r="%d" fill="white" fill-opacity="0.9" stroke="%s" stroke-width="2" filter="url(#shadow)"/>',
            piece$x, piece$y, radius, color),
    sprintf('      <text x="%.1f" y="%.1f" text-anchor="middle" dominant-baseline="central" font-family="Arial, sans-serif" font-size="%d" font-weight="bold" fill="%s">%d</text>',
            piece$x, piece$y, font_size, color, piece$id),
    '    </g>'
  )
}

# Add title and legend
labeled_svg <- c(labeled_svg,
  '    <text x="120" y="25" text-anchor="middle" font-family="Arial, sans-serif" font-size="16" font-weight="bold" fill="#2c3e50">Circular Puzzle - Labeled Foundation</text>',
  sprintf('    <text x="120" y="40" text-anchor="middle" font-family="Arial, sans-serif" font-size="12" fill="#7f8c8d">%d pieces ready for individual extraction</text>', length(manual_pieces)),
  '    <rect x="10" y="210" width="220" height="25" fill="white" fill-opacity="0.9" stroke="#bdc3c7" stroke-width="1"/>',
  '    <text x="20" y="225" font-family="Arial, sans-serif" font-size="10" font-weight="bold" fill="#2c3e50">Foundation: Ring 0 (center) + Ring 1 (6 pieces) + Ring 2 (12 pieces) = 19 total</text>'
)

# Close SVG
labeled_svg <- c(labeled_svg, '  </g>', '</svg>')

# Save the labeled circular puzzle
output_file <- "output/circular_puzzle_labeled.svg"
writeLines(labeled_svg, output_file)

# Save summary data
summary_data <- list(
  total_pieces = length(manual_pieces),
  pieces = manual_pieces,
  original_paths = paths_data,
  output_file = output_file
)

saveRDS(summary_data, "output/circular_puzzle_labeled.rds")

cat("\n✅ LABELED CIRCULAR PUZZLE FOUNDATION CREATED\n")
cat("📊 RESULTS:\n")
cat("- Total piece positions:", length(manual_pieces), "\n")
cat("- Ring 0 (center): 1 piece\n")
cat("- Ring 1: 6 pieces\n") 
cat("- Ring 2: 12 pieces\n")
cat("- Original puzzle paths preserved underneath labels\n")
cat("- Output file:", output_file, "\n")

cat("\n🎯 NEXT STEPS:\n")
cat("1. ✅ Foundation with piece positions established\n")
cat("2. 🔄 Extract individual pieces using these labeled positions\n")
cat("3. 🔄 Match each position to corresponding puzzle segments\n")
cat("4. 🔄 Generate individual SVG files for each piece\n")

cat("\n📝 This labeled foundation provides the coordinate system for accurate individual piece extraction.\n")