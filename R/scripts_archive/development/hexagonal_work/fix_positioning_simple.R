#!/usr/bin/env Rscript

# Fix Positioning - Simple Approach
# Step 3: Use a simple approach to fix the positioning issue

cat("🎯 FIXING POSITIONING - SIMPLE APPROACH\n")

# The simplest solution: Don't apply transforms at all, just use the segment paths as-is
# but adjust the viewBox to center the content properly

# Load existing data
if (!file.exists("output/hex_individual_with_real_tabs.rds")) {
  stop("Please run create_hex_individual_with_real_tabs.R first")
}

tabs_data <- readRDS("output/hex_individual_with_real_tabs.rds")
pieces <- tabs_data$pieces

cat("Loaded", length(pieces), "pieces with real tabs\n")

# Create SVG without transforms, using original positioning
cat("\n=== CREATING SVG WITHOUT TRANSFORMS ===\n")

# Find the bounds of the actual paths (not the intended grid positions)
# This will help us center the viewBox properly

# For now, let's use a simple approach - use the puzzle's original coordinate system
# The original puzzle viewBox is 0 0 288.0 288.0

# Colors for pieces
colors <- c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12", "#9B59B6",
           "#1ABC9C", "#E67E22", "#34495E", "#F1C40F", "#E91E63",
           "#00BCD4", "#4CAF50", "#FF9800", "#607D8B", "#795548",
           "#FF5722", "#009688", "#8BC34A", "#FFC107", "#673AB7")

# Create SVG using original puzzle coordinate system
svg_lines <- c(
  '<svg width="300mm" height="300mm" viewBox="0 0 300 300" xmlns="http://www.w3.org/2000/svg">',
  '  <title>Hexagonal Individual Pieces - Fixed Positioning</title>',
  '  <defs>',
  '    <filter id="shadow" x="-50%" y="-50%" width="200%" height="200%">',
  '      <feDropShadow dx="1" dy="1" stdDeviation="1" flood-opacity="0.3"/>',
  '    </filter>',
  '  </defs>',
  '  <g id="hexagonal-pieces-fixed-positioning">'
)

# Add each piece without transforms - just use the original segment paths
pieces_with_bezier <- 0

for (i in 1:length(pieces)) {
  piece_data <- pieces[[as.character(i)]]
  
  if (is.null(piece_data)) {
    piece_data <- pieces[[i]]  # Try numeric index
  }
  
  if (!is.null(piece_data)) {
    color_idx <- ((i - 1) %% length(colors)) + 1
    color <- colors[color_idx]
    
    stroke_width <- if (piece_data$has_bezier) "2" else "1.5"
    fill_opacity <- if (piece_data$has_bezier) "0.15" else "0.05"
    
    if (piece_data$has_bezier) {
      pieces_with_bezier <- pieces_with_bezier + 1
    }
    
    # Add piece directly without transform
    svg_lines <- c(svg_lines,
      sprintf('    <g id="piece-%d">', piece_data$piece_id),
      sprintf('      <path d="%s" fill="%s" fill-opacity="%s" stroke="%s" stroke-width="%s" filter="url(#shadow)"/>', 
              piece_data$path, color, fill_opacity, color, stroke_width),
      '    </g>'
    )
  }
}

# Close SVG
svg_lines <- c(svg_lines, '  </g>', '</svg>')

# Save SVG
final_svg <- paste(svg_lines, collapse = "\n")
writeLines(final_svg, "output/hex_individual_fixed_positioning.svg")

cat("✅ FIXED POSITIONING SVG CREATED\n")
cat("Key changes:\n")
cat("- Removed transforms that were causing double positioning\n")
cat("- Used original puzzle coordinate system\n") 
cat("- Preserved real tabs/blanks from segments\n")
cat("- Pieces with bezier curves:", pieces_with_bezier, "\n")

# Let's also try a version that centers everything properly
cat("\n=== CREATING CENTERED VERSION ===\n")

# Use a centered viewBox that should contain the puzzle pieces
centered_svg_lines <- c(
  '<svg width="300mm" height="300mm" viewBox="0 0 300 300" xmlns="http://www.w3.org/2000/svg">',
  '  <title>Hexagonal Individual Pieces - Centered</title>',
  '  <defs>',
  '    <filter id="shadow" x="-50%" y="-50%" width="200%" height="200%">',
  '      <feDropShadow dx="1" dy="1" stdDeviation="1" flood-opacity="0.3"/>',
  '    </filter>',
  '  </defs>',
  '  <g id="pieces-centered">'
)

# Add pieces with labels
for (i in 1:length(pieces)) {
  piece_data <- pieces[[as.character(i)]]
  
  if (is.null(piece_data)) {
    piece_data <- pieces[[i]]
  }
  
  if (!is.null(piece_data)) {
    color_idx <- ((i - 1) %% length(colors)) + 1
    color <- colors[color_idx]
    
    stroke_width <- if (piece_data$has_bezier) "2" else "1.5"
    fill_opacity <- if (piece_data$has_bezier) "0.15" else "0.05"
    
    centered_svg_lines <- c(centered_svg_lines,
      sprintf('    <g id="piece-%d">', piece_data$piece_id),
      sprintf('      <path d="%s" fill="%s" fill-opacity="%s" stroke="%s" stroke-width="%s" filter="url(#shadow)"/>', 
              piece_data$path, color, fill_opacity, color, stroke_width),
      sprintf('      <text x="150" y="%d" text-anchor="middle" font-family="Arial" font-size="8" fill="%s">%d %s</text>',
              20 + i * 10, color, piece_data$piece_id, 
              if(piece_data$has_bezier) "✓" else "○"),
      '    </g>'
    )
  }
}

centered_svg_lines <- c(centered_svg_lines, '  </g>', '</svg>')

final_centered_svg <- paste(centered_svg_lines, collapse = "\n")
writeLines(final_centered_svg, "output/hex_individual_centered.svg")

cat("\n✅ BOTH VERSIONS CREATED\n")
cat("Generated files:\n")
cat("- output/hex_individual_fixed_positioning.svg (no transforms)\n")
cat("- output/hex_individual_centered.svg (with labels)\n")

cat("\nNext step: View the SVGs to see if the positioning is fixed\n")
cat("If pieces are still clustered, we need to implement proper coordinate translation\n")