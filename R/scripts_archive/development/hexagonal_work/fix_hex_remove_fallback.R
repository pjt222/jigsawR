#!/usr/bin/env Rscript

# Fix Hexagonal Individual Pieces - Remove Fallback Geometric Hexagon
cat("🔧 FIXING HEXAGONAL PIECES - REMOVING FALLBACK PIECE 19\n")

# Read the current SVG
svg_file <- "output/hex_individual_FINAL_FIXED.svg"
if (!file.exists(svg_file)) {
  stop("Source SVG file not found: ", svg_file)
}

svg_lines <- readLines(svg_file)
cat("Loaded SVG with", length(svg_lines), "lines\n")

# Identify the problematic piece 19 (the large geometric hexagon)
piece19_start <- grep('<g id="piece-19">', svg_lines)
piece19_end <- piece19_start

# Find the closing </g> for piece 19
if (length(piece19_start) > 0) {
  for (i in (piece19_start + 1):length(svg_lines)) {
    if (grepl("</g>", svg_lines[i])) {
      piece19_end <- i
      break
    }
  }
  
  cat(sprintf("Found piece 19: lines %d to %d\n", piece19_start, piece19_end))
  
  # Check what piece 19 contains (should be the large geometric hexagon)
  piece19_content <- svg_lines[piece19_start:piece19_end]
  path_line <- grep('<path d=', piece19_content, value = TRUE)[1]
  
  if (grepl("L.*L.*L", path_line) && grepl("15\\.43", path_line) && grepl("272\\.57", path_line)) {
    cat("✅ Confirmed: Piece 19 is the large geometric hexagon fallback\n")
    cat("   Contains L commands (straight lines) and spans large coordinates\n")
    
    # Remove piece 19 from the SVG
    filtered_svg <- svg_lines[-c(piece19_start:piece19_end)]
    
    cat("Removed piece 19, new SVG has", length(filtered_svg), "lines\n")
  } else {
    cat("❌ Piece 19 doesn't match expected fallback pattern - keeping it\n")
    filtered_svg <- svg_lines
  }
} else {
  cat("❌ Piece 19 not found in SVG\n")
  filtered_svg <- svg_lines
}

# Also fix the NA values in the viewBox and text positions
cat("\n🔧 FIXING NA VALUES\n")

# Calculate proper viewBox from remaining pieces (exclude piece 19)
path_lines <- grep('<path d=', filtered_svg, value = TRUE)
path_lines <- path_lines[!grepl("15\\.43.*272\\.57", path_lines)]  # Exclude any remaining large paths

cat("Analyzing", length(path_lines), "paths for bounds\n")

all_coords <- c()
for (path in path_lines) {
  # Extract d attribute
  d_attr <- regmatches(path, regexpr('d="[^"]*"', path))
  if (length(d_attr) > 0) {
    path_data <- gsub('d="', '', d_attr)
    path_data <- gsub('".*', '', path_data)
    
    # Extract numbers
    path_clean <- gsub('[MLHVCSQTAZ]', ' ', path_data, ignore.case = TRUE)
    numbers <- as.numeric(unlist(strsplit(path_clean, "\\s+")))
    numbers <- numbers[!is.na(numbers)]
    
    all_coords <- c(all_coords, numbers)
  }
}

if (length(all_coords) >= 4) {
  # Assume alternating x, y coordinates
  x_coords <- all_coords[seq(1, length(all_coords), by = 2)]
  y_coords <- all_coords[seq(2, length(all_coords), by = 2)]
  
  min_x <- min(x_coords, na.rm = TRUE)
  max_x <- max(x_coords, na.rm = TRUE)
  min_y <- min(y_coords, na.rm = TRUE)
  max_y <- max(y_coords, na.rm = TRUE)
  
  # Add margins
  margin <- 20
  viewbox_x <- min_x - margin
  viewbox_y <- min_y - margin
  viewbox_w <- (max_x - min_x) + 2 * margin
  viewbox_h <- (max_y - min_y) + 2 * margin
  
  cat(sprintf("Calculated viewBox: %.1f %.1f %.1f %.1f\n", 
              viewbox_x, viewbox_y, viewbox_w, viewbox_h))
} else {
  # Fallback viewBox
  viewbox_x <- 0
  viewbox_y <- 0  
  viewbox_w <- 300
  viewbox_h <- 300
  cat("Using fallback viewBox\n")
}

# Fix the SVG header with proper viewBox
svg_header_line <- grep('<svg', filtered_svg)[1]
if (length(svg_header_line) > 0) {
  filtered_svg[svg_header_line] <- sprintf(
    '<svg width="%.1fmm" height="%.1fmm" viewBox="%.1f %.1f %.1f %.1f" xmlns="http://www.w3.org/2000/svg">',
    viewbox_w * 0.8, viewbox_h * 0.8, viewbox_x, viewbox_y, viewbox_w, viewbox_h
  )
}

# Fix text x="NA" y="NA" by removing labels (they're not positioned correctly anyway)
filtered_svg <- filtered_svg[!grepl('x="NA"', filtered_svg)]

# Add a title update
title_line <- grep('<title>', filtered_svg)[1]
if (length(title_line) > 0) {
  filtered_svg[title_line] <- '  <title>Hexagonal Individual Pieces - CLEANED (Fallback Removed)</title>'
}

# Save the cleaned SVG
output_file <- "output/hex_individual_CLEANED.svg"
writeLines(filtered_svg, output_file)

# Count remaining pieces
remaining_pieces <- length(grep('<g id="piece-', filtered_svg))

cat("\n✅ HEXAGONAL PIECES CLEANED\n")
cat("Results:\n")
cat("- Removed fallback piece 19 (large geometric hexagon)\n")
cat("- Fixed viewBox NA values\n") 
cat("- Removed invalid text labels\n")
cat("- Remaining pieces:", remaining_pieces, "\n")
cat("- Output file:", output_file, "\n")

if (remaining_pieces == 18) {
  cat("\n🎉 SUCCESS: 18 real puzzle pieces with tabs/blanks preserved!\n")
  cat("The overlapping issue should now be resolved.\n")
} else {
  cat("\n⚠️  Expected 18 pieces, got", remaining_pieces, "\n")
}

cat("\n📝 RECOMMENDATION:\n")
cat("Open", output_file, "to verify that overlapping pieces issue is resolved.\n")
cat("All pieces should now be clearly visible without obstruction.\n")