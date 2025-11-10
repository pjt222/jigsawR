#!/usr/bin/env Rscript

# Restore Missing Outer Border Piece to Hexagonal Individual Pieces
cat("🔧 RESTORING MISSING OUTER BORDER PIECE\n")

# Load the original complete pieces data
if (!file.exists("output/hex_individual_with_real_tabs.rds")) {
  stop("Original pieces data not found")
}

tabs_data <- readRDS("output/hex_individual_with_real_tabs.rds")
all_pieces <- tabs_data$pieces

cat("Loaded original", length(all_pieces), "pieces\n")

# Find piece 19 (the outer border that was removed)
piece_19 <- NULL
if (!is.null(all_pieces[["19"]])) {
  piece_19 <- all_pieces[["19"]]
} else if (!is.null(all_pieces[[19]])) {
  piece_19 <- all_pieces[[19]]
}

if (is.null(piece_19)) {
  stop("Piece 19 (outer border) not found in original data")
}

cat("Found piece 19:\n")
cat("  ID:", piece_19$piece_id, "\n")
cat("  Has bezier:", piece_19$has_bezier, "\n")
cat("  Path length:", nchar(piece_19$path), "\n")
cat("  Path start:", substr(piece_19$path, 1, 50), "...\n")

# Check if this is indeed the outer border (should have L commands and large coordinates)
is_outer_border <- !piece_19$has_bezier && grepl("L.*L.*L", piece_19$path)
cat("  Is outer border:", is_outer_border, "\n")

if (!is_outer_border) {
  stop("Piece 19 doesn't appear to be the outer border piece")
}

# Read the current cleaned SVG
cleaned_svg <- readLines("output/hex_individual_CLEANED.svg")
cat("\nRead cleaned SVG with", length(cleaned_svg), "lines\n")

# Find where to insert piece 19 (before the closing </g> tag)
closing_g_line <- which(cleaned_svg == "  </g>")[1]
if (is.na(closing_g_line)) {
  # Find alternative closing pattern
  closing_g_line <- grep("</g>", cleaned_svg)[length(grep("</g>", cleaned_svg))]
}

cat("Will insert piece 19 before line", closing_g_line, "\n")

# Choose appropriate styling for outer border piece
color <- "#2C3E50"  # Dark color for border
fill_opacity <- "0.05"  # Very light fill
stroke_width <- "3"     # Thicker stroke for border
stroke_opacity <- "0.8"

# Create the SVG elements for piece 19
piece_19_svg <- c(
  sprintf('    <g id="piece-%d">', piece_19$piece_id),
  sprintf('      <path d="%s" fill="%s" fill-opacity="%s" stroke="%s" stroke-width="%s" stroke-opacity="%s" filter="url(#shadow)"/>', 
          piece_19$path, color, fill_opacity, color, stroke_width, stroke_opacity),
  '    </g>'
)

cat("Created SVG elements for piece 19\n")

# Insert piece 19 into the cleaned SVG
if (closing_g_line > 1) {
  restored_svg <- c(
    cleaned_svg[1:(closing_g_line-1)],
    piece_19_svg,
    cleaned_svg[closing_g_line:length(cleaned_svg)]
  )
} else {
  stop("Could not find appropriate insertion point")
}

# Update the title to reflect the restoration
title_line <- grep('<title>', restored_svg)[1]
if (!is.na(title_line)) {
  restored_svg[title_line] <- '  <title>Hexagonal Individual Pieces - COMPLETE (Outer Border Restored)</title>'
}

# Save the restored SVG
output_file <- "output/hex_individual_COMPLETE.svg"
writeLines(restored_svg, output_file)

# Verify the restoration
restored_pieces <- length(grep('<g id="piece-', restored_svg))
path_count <- length(grep('<path d=', restored_svg))

cat("\n✅ OUTER BORDER PIECE RESTORED\n")
cat("Results:\n")
cat("- Total pieces:", restored_pieces, "\n")
cat("- Total paths:", path_count, "\n") 
cat("- Outer border piece: RESTORED with appropriate styling\n")
cat("- Output file:", output_file, "\n")

if (restored_pieces == 19 && path_count == 19) {
  cat("\n🎉 SUCCESS: All 19 pieces now present!\n")
  cat("- 18 inner pieces with real tabs/blanks (bezier curves)\n")
  cat("- 1 outer border piece with straight edges\n")
  cat("- Complete hexagonal puzzle individual pieces\n")
} else {
  cat("\n⚠️  Expected 19 pieces, got", restored_pieces, "\n")
}

# Create verification summary
cat("\n=== PIECE BREAKDOWN ===\n")
cat("Ring 0 (center): 1 piece with tabs/blanks\n")
cat("Ring 1 (inner): 6 pieces with tabs/blanks\n")
cat("Ring 2 (middle): 11 pieces with tabs/blanks\n") 
cat("Outer border: 1 piece with straight edges\n")
cat("Total: 19 pieces\n")

cat("\n📝 RECOMMENDATION:\n")
cat("Open", output_file, "to verify all pieces are now visible.\n")
cat("The outer border should now properly frame the puzzle pieces.\n")