#!/usr/bin/env Rscript

# Debug Circular Puzzle Path Parsing
cat("🔍 DEBUGGING CIRCULAR PUZZLE PATH PARSING\n")

# Read the circular puzzle
svg_content <- paste(readLines("output/circular_puzzle.svg"), collapse = "")
cat("SVG content length:", nchar(svg_content), "\n")

# Show first 200 characters
cat("First 200 chars:", substr(svg_content, 1, 200), "\n")

# Find path elements
path_matches <- gregexpr('<path[^>]*>', svg_content)[[1]]
cat("Found", length(path_matches), "path matches\n")

if (path_matches[1] != -1) {
  for (i in seq_along(path_matches)) {
    start <- path_matches[i]
    length_val <- attr(path_matches, "match.length")[i]
    path_element <- substr(svg_content, start, start + length_val - 1)
    
    cat(sprintf("\nPath %d (length %d):\n", i, length_val))
    cat("First 100 chars:", substr(path_element, 1, 100), "\n")
    
    # Extract d attribute
    d_match <- regmatches(path_element, regexpr('d="[^"]*"', path_element))
    if (length(d_match) > 0) {
      d_content <- gsub('d="', '', d_match)
      d_content <- gsub('".*', '', d_content)
      cat("D attribute length:", nchar(d_content), "\n")
      cat("D starts with:", substr(d_content, 1, 50), "\n")
      
      # Check for patterns
      has_c <- grepl(' C ', d_content)
      has_m <- grepl('^M ', d_content)
      has_l <- grepl(' L ', d_content)
      has_a <- grepl(' a ', d_content)
      
      cat("Contains: M=", has_m, " L=", has_l, " C=", has_c, " a=", has_a, "\n")
    }
    
    # Check stroke width
    stroke_match <- regmatches(path_element, regexpr('stroke-width="[^"]*"', path_element))
    if (length(stroke_match) > 0) {
      cat("Stroke width:", stroke_match, "\n")
    }
  }
} else {
  cat("No path elements found\n")
}

# Try a simpler approach - just extract the d attributes directly
cat("\n=== DIRECT D ATTRIBUTE EXTRACTION ===\n")
d_matches <- gregexpr('d="[^"]*"', svg_content)[[1]]
if (d_matches[1] != -1) {
  for (i in seq_along(d_matches)) {
    start <- d_matches[i]
    length_val <- attr(d_matches, "match.length")[i]
    d_full <- substr(svg_content, start, start + length_val - 1)
    d_content <- gsub('d="', '', d_full)
    d_content <- gsub('"$', '', d_content)
    
    cat(sprintf("D attribute %d: length=%d\n", i, nchar(d_content)))
    cat("  Starts:", substr(d_content, 1, 50), "\n")
    cat("  Ends  :", substr(d_content, nchar(d_content)-50, nchar(d_content)), "\n")
    
    # Count coordinate numbers
    numbers <- regmatches(d_content, gregexpr("[-\\d\\.]+", d_content))[[1]]
    valid_numbers <- sum(!is.na(as.numeric(numbers)))
    cat("  Valid numbers:", valid_numbers, "\n")
    
    if (valid_numbers >= 4) {
      coords <- as.numeric(numbers)
      coords <- coords[!is.na(coords)]
      sample_coords <- coords[1:min(10, length(coords))]
      
      x_coords <- sample_coords[seq(1, length(sample_coords), by = 2)]
      y_coords <- sample_coords[seq(2, length(sample_coords), by = 2)]
      
      if (length(x_coords) > 0 && length(y_coords) > 0) {
        center_x <- mean(x_coords)
        center_y <- mean(y_coords)
        cat(sprintf("  Est center: (%.1f, %.1f)\n", center_x, center_y))
      }
    }
    
    cat("\n")
  }
}