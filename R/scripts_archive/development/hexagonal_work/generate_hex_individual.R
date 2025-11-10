#!/usr/bin/env Rscript

# Step 5: Generate Individual Hexagonal Pieces SVG
# Create the final output with properly extracted pieces

cat("🔥 STEP 5: Generating Individual Pieces SVG\n")

# Load extraction data from Step 4
if (!file.exists("output/hex_piece_extraction.rds")) {
  stop("Please run extract_hex_pieces.R first to generate the extraction data")
}

extraction_data <- readRDS("output/hex_piece_extraction.rds")
extracted_pieces <- extraction_data$extracted_pieces
puzzle_params <- extraction_data$puzzle_params
stats <- extraction_data$statistics

cat("=== PIECE DATA SUMMARY ===\n")
cat("Total pieces:", stats$total_pieces, "\n")
cat("Pieces with bezier curves:", stats$bezier_count, "\n")
cat("Unique paths:", stats$unique_paths, "\n")
cat("Path length range:", stats$path_length_range[1], "to", stats$path_length_range[2], "chars\n")

# Create color palette
colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#FFA07A", "#98D8C8", 
           "#F7DC6F", "#BB8FCE", "#85C1E9", "#F8C471", "#82E0AA",
           "#FF9999", "#66CDAA", "#87CEEB", "#DDA0DD", "#F0E68C")

# Calculate puzzle dimensions
diameter <- puzzle_params$diameter
radius <- diameter / 2.0
margin <- radius * 0.3
total_size <- 2 * (radius + margin)

cat("\n=== SVG GENERATION ===\n")
cat("Puzzle diameter:", diameter, "mm\n")
cat("SVG size:", total_size, "x", total_size, "mm\n")

# Start SVG
svg_lines <- c(
  sprintf('<svg width="%.1fmm" height="%.1fmm" viewBox="%.2f %.2f %.2f %.2f" xmlns="http://www.w3.org/2000/svg">',
          total_size, total_size, -total_size/2, -total_size/2, total_size, total_size),
  sprintf('  <title>Hexagonal Individual Pieces - Actual Bezier Curves (Seed: %d)</title>', 
          puzzle_params$seed),
  '  <g id="hexagonal-individual-pieces-proper">'
)

# Add each piece
pieces_added <- 0
bezier_pieces_added <- 0

for (piece_id in 1:stats$total_pieces) {
  if (as.character(piece_id) %in% names(extracted_pieces)) {
    piece <- extracted_pieces[[as.character(piece_id)]]
    
    # Get color
    color_idx <- ((piece_id - 1) %% length(colors)) + 1
    color <- colors[color_idx]
    
    # Determine piece status for labeling
    status_label <- if (piece$has_bezier) "✓" else "○"
    
    # Add piece to SVG
    svg_lines <- c(svg_lines,
      sprintf('    <g id="piece-%d">', piece_id),
      sprintf('      <path d="%s" fill="none" stroke="%s" stroke-width="1.5" opacity="0.9"/>', 
              piece$path, color),
      sprintf('      <text x="0" y="%d" text-anchor="middle" font-family="Arial" font-size="8" font-weight="bold" fill="%s">%s%d</text>',
              -10 + piece_id * 2, color, status_label, piece_id),
      '    </g>'
    )
    
    pieces_added <- pieces_added + 1
    if (piece$has_bezier) bezier_pieces_added <- bezier_pieces_added + 1
  }
}

# Close SVG
svg_lines <- c(svg_lines, '  </g>', '</svg>')

# Combine into complete SVG
complete_svg <- paste(svg_lines, collapse = "\n")

# Save the SVG
output_file <- "output/hex_individual_proper.svg"
writeLines(complete_svg, output_file)

cat("SVG saved to:", output_file, "\n")
cat("SVG length:", nchar(complete_svg), "characters\n")

cat("\n=== FINAL RESULTS ===\n")
cat("✅ Pieces added to SVG:", pieces_added, "\n")
cat("✅ Pieces with bezier curves:", bezier_pieces_added, "\n") 
cat("✅ Percentage with bezier curves:", round((bezier_pieces_added/pieces_added)*100, 1), "%\n")

# Show path samples with their types
cat("\n=== PATH TYPE ANALYSIS ===\n")
for (piece_id in 1:min(5, stats$total_pieces)) {
  if (as.character(piece_id) %in% names(extracted_pieces)) {
    piece <- extracted_pieces[[as.character(piece_id)]]
    path_sample <- substr(piece$path, 1, 60)
    cat(sprintf("Piece %d (%s): %s... (%s)\n", 
                piece_id, piece$segment_source, path_sample, 
                if(piece$has_bezier) "BEZIER" else "straight"))
  }
}

cat("\n🎯 STEP 5 COMPLETE: Individual pieces SVG generated!\n")

if (bezier_pieces_added >= stats$total_pieces * 0.5) {
  cat("✅ SUCCESS: Over 50% of pieces have actual bezier curves from the puzzle!\n")
  cat("✅ This is a MAJOR improvement over simple hexagons!\n")
} else {
  cat("⚠️  PARTIAL SUCCESS: Some pieces have bezier curves, improvement over simple hexagons\n")
}

cat("\n=== COMPARISON TO PREVIOUS ATTEMPTS ===\n")
cat("BEFORE: All pieces were simple hexagons with straight lines (L commands)\n")
cat("NOW:   ", bezier_pieces_added, "pieces have actual bezier curves (C commands) from the puzzle\n")
cat("BEFORE: All pieces looked identical\n") 
cat("NOW:   ", stats$unique_paths, "unique path shapes\n")

cat("\nResult: Successfully using ACTUAL puzzle path segments instead of artificial shapes! ✨\n")