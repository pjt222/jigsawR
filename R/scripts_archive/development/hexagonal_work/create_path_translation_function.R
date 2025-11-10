#!/usr/bin/env Rscript

# Create Path Translation Function
# Step 2: Create function to center path segments at origin

cat("🔧 CREATING PATH TRANSLATION FUNCTION\n")

# Load segment data
if (!file.exists("output/hex_segments_data.rds")) {
  stop("Please run analyze_hex_tabs_simple.R first")
}

segments_data <- readRDS("output/hex_segments_data.rds")
all_segments <- c(segments_data$horizontal_segments, 
                 segments_data$vertical_segments, 
                 segments_data$border_segments)

cat("Loaded", length(all_segments), "segments\n")

# Simple function to extract and translate coordinates
translate_path_to_origin <- function(path_string) {
  
  # Simple approach: find the first M coordinate as the starting point
  # and subtract it from all coordinates in the path
  
  # Find the first M coordinate
  m_match <- regexpr("M\\s+([-\\d\\.]+)\\s+([-\\d\\.]+)", path_string, perl = TRUE)
  
  if (m_match[1] == -1) {
    # No M command found, return path as-is
    return(path_string)
  }
  
  # Extract the first coordinate
  m_text <- regmatches(path_string, m_match)
  coord_matches <- regexpr("([-\\d\\.]+)\\s+([-\\d\\.]+)", m_text)
  coord_text <- regmatches(m_text, coord_matches)
  
  coords <- strsplit(coord_text, "\\s+")[[1]]
  start_x <- as.numeric(coords[1])
  start_y <- as.numeric(coords[2])
  
  cat(sprintf("Original start: (%.2f, %.2f) → ", start_x, start_y))
  
  # Replace all coordinates in the path by subtracting the start coordinates
  
  # Function to replace a coordinate pair
  replace_coords <- function(match_text) {
    # Extract the coordinates from the match
    coord_pattern <- "([-\\d\\.]+)\\s+([-\\d\\.]+)"
    coords_in_match <- regmatches(match_text, gregexpr(coord_pattern, match_text))[[1]]
    
    new_match <- match_text
    for (coord_pair in coords_in_match) {
      coord_nums <- as.numeric(strsplit(coord_pair, "\\s+")[[1]])
      if (length(coord_nums) >= 2) {
        new_x <- coord_nums[1] - start_x
        new_y <- coord_nums[2] - start_y
        new_coord <- sprintf("%.2f %.2f", new_x, new_y)
        new_match <- sub(coord_pair, new_coord, new_match, fixed = TRUE)
      }
    }
    return(new_match)
  }
  
  translated_path <- path_string
  
  # Replace M commands
  m_pattern <- "M\\s+([-\\d\\.\\s]+?)(?=\\s+[CML]|$)"
  m_matches <- regexpr(m_pattern, translated_path, perl = TRUE)
  if (m_matches[1] != -1) {
    m_text <- regmatches(translated_path, m_matches)
    new_m <- replace_coords(m_text)
    regmatches(translated_path, m_matches) <- new_m
  }
  
  # Replace C commands  
  c_pattern <- "C\\s+([-\\d\\.\\s]+?)(?=\\s+[CML]|$)"
  while (TRUE) {
    c_matches <- regexpr(c_pattern, translated_path, perl = TRUE)
    if (c_matches[1] == -1) break
    
    c_text <- regmatches(translated_path, c_matches)
    new_c <- replace_coords(c_text)
    regmatches(translated_path, c_matches) <- new_c
  }
  
  # Replace L commands
  l_pattern <- "L\\s+([-\\d\\.\\s]+?)(?=\\s+[CML]|$)"
  while (TRUE) {
    l_matches <- regexpr(l_pattern, translated_path, perl = TRUE)
    if (l_matches[1] == -1) break
    
    l_text <- regmatches(translated_path, l_matches)
    new_l <- replace_coords(l_text)
    regmatches(translated_path, l_matches) <- new_l
  }
  
  cat("Centered at origin\n")
  return(translated_path)
}

# Test the translation function on a few segments
cat("\n=== TESTING TRANSLATION FUNCTION ===\n")

translated_segments <- list()

for (i in 1:length(all_segments)) {
  segment <- all_segments[[i]]
  
  cat(sprintf("Translating %s: ", segment$id))
  original_path <- segment$segment
  translated_path <- translate_path_to_origin(original_path)
  
  translated_segments[[i]] <- list(
    id = segment$id,
    original = original_path,
    translated = translated_path,
    has_bezier = segment$has_bezier,
    length = nchar(translated_path)
  )
}

cat("\n=== SAMPLE TRANSLATIONS ===\n")
for (i in 1:min(3, length(translated_segments))) {
  seg <- translated_segments[[i]]
  cat(sprintf("\n%s:\n", seg$id))
  cat("BEFORE:", substr(seg$original, 1, 80), "...\n")
  cat("AFTER: ", substr(seg$translated, 1, 80), "...\n")
}

# Save translated segments
translation_data <- list(
  translated_segments = translated_segments,
  original_segments = all_segments
)

saveRDS(translation_data, "output/translated_segments.rds")

cat("\n✅ PATH TRANSLATION COMPLETE\n") 
cat("Created", length(translated_segments), "translated segments\n")
cat("All segments now centered around origin (0,0)\n")
cat("Saved to: output/translated_segments.rds\n")