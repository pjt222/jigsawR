#!/usr/bin/env Rscript

# Step-by-Step Analysis of Hexagonal Puzzle Tabs/Blanks
# Understand how to extract actual bezier curves for individual pieces

cat("🔍 STEP-BY-STEP HEXAGONAL TABS/BLANKS ANALYSIS\n")

# Load hexagonal puzzle functions
source("R/hexagonal_puzzle.R")

# Generate the puzzle with known parameters
cat("=== STEP 1: GENERATE PUZZLE WITH TABS/BLANKS ===\n")
init_hex_jigsaw(seed = 42, rings = 3, diameter = 240)

# Get the three path types
horizontal_path <- hex_gen_dh()
vertical_path <- hex_gen_dv()
border_path <- hex_gen_db()

cat("Horizontal path length:", nchar(horizontal_path), "chars\n")
cat("Vertical path length:", nchar(vertical_path), "chars\n")
cat("Border path length:", nchar(border_path), "chars\n")

# STEP 2: Parse paths to see bezier curves
cat("\n=== STEP 2: EXAMINE BEZIER CURVES IN PATHS ===\n")

# Function to extract all curve commands
extract_bezier_curves <- function(path_string, path_type) {
  
  # Find all C commands (bezier curves) 
  c_pattern <- "C\\s+([0-9\\.-]+\\s+[0-9\\.-]+\\s+[0-9\\.-]+\\s+[0-9\\.-]+\\s+[0-9\\.-]+\\s+[0-9\\.-]+)"
  c_matches <- gregexpr(c_pattern, path_string, perl = TRUE)
  
  curves <- list()
  if (length(c_matches[[1]]) > 0 && c_matches[[1]][1] != -1) {
    
    match_strings <- regmatches(path_string, c_matches)[[1]]
    
    for (i in 1:length(match_strings)) {
      curves[[i]] <- list(
        index = i,
        type = path_type,
        full_command = match_strings[i],
        length = nchar(match_strings[i])
      )
    }
  }
  
  return(curves)
}

# Extract bezier curves from all paths
h_curves <- extract_bezier_curves(horizontal_path, "horizontal")
v_curves <- extract_bezier_curves(vertical_path, "vertical") 
b_curves <- extract_bezier_curves(border_path, "border")

cat("Horizontal bezier curves:", length(h_curves), "\n")
cat("Vertical bezier curves:", length(v_curves), "\n")
cat("Border bezier curves:", length(b_curves), "\n")
cat("Total bezier curves:", length(h_curves) + length(v_curves) + length(b_curves), "\n")

# Show sample curves
cat("\n=== STEP 3: SAMPLE BEZIER CURVES ===\n")
if (length(v_curves) > 0) {
  cat("Sample vertical bezier curve:\n")
  cat(v_curves[[1]]$full_command, "\n")
}

if (length(h_curves) > 0) {
  cat("Sample horizontal bezier curve:\n")
  cat(h_curves[[1]]$full_command, "\n")
}

# STEP 4: Parse complete segments (M to next M)
cat("\n=== STEP 4: PARSE COMPLETE SEGMENTS ===\n")

parse_complete_segments <- function(path_string, path_type) {
  
  # Split on M commands 
  segments <- strsplit(path_string, "M ")[[1]]
  segments <- segments[segments != "" & nchar(trimws(segments)) > 0]
  
  parsed_segments <- list()
  for (i in 1:length(segments)) {
    segment_text <- trimws(segments[i])
    if (segment_text != "") {
      # Add M back if needed
      if (i > 1 || !grepl("^M", path_string)) {
        segment_text <- paste0("M ", segment_text)
      }
      
      # Count bezier curves in this segment
      curve_count <- length(gregmatches(segment_text, gregexpr("C\\s+", segment_text))[[1]])
      if (curve_count == -1 || is.na(curve_count)) curve_count <- 0
      
      parsed_segments[[i]] <- list(
        index = i,
        type = path_type,
        segment = segment_text,
        length = nchar(segment_text),
        bezier_count = curve_count,
        has_tabs = curve_count > 0
      )
    }
  }
  
  return(parsed_segments)
}

# Parse all segments
h_segments <- parse_complete_segments(horizontal_path, "horizontal")
v_segments <- parse_complete_segments(vertical_path, "vertical")
b_segments <- parse_complete_segments(border_path, "border")

cat("Complete segments found:\n")
cat("- Horizontal:", length(h_segments), "segments\n")
cat("- Vertical:", length(v_segments), "segments\n")
cat("- Border:", length(b_segments), "segments\n")
cat("- Total:", length(h_segments) + length(v_segments) + length(b_segments), "segments\n")

# STEP 5: Analyze segments with tabs
cat("\n=== STEP 5: SEGMENTS WITH TABS/BLANKS ===\n")

all_segments <- c(h_segments, v_segments, b_segments)
segments_with_tabs <- 0
segments_without_tabs <- 0

for (seg in all_segments) {
  if (seg$has_tabs) {
    segments_with_tabs <- segments_with_tabs + 1
  } else {
    segments_without_tabs <- segments_without_tabs + 1
  }
}

cat("Segments WITH tabs/blanks:", segments_with_tabs, "\n")
cat("Segments WITHOUT tabs/blanks:", segments_without_tabs, "\n")

# Show detailed breakdown
cat("\n=== STEP 6: DETAILED SEGMENT BREAKDOWN ===\n")

cat("HORIZONTAL SEGMENTS:\n")
for (i in 1:length(h_segments)) {
  seg <- h_segments[[i]]
  cat(sprintf("  H%d: %d chars, %d curves, tabs=%s\n", 
              i, seg$length, seg$bezier_count, seg$has_tabs))
  if (i <= 2) {  # Show first 2 as samples
    cat(sprintf("    Sample: %s\n", substr(seg$segment, 1, 80)))
  }
}

cat("\nVERTICAL SEGMENTS:\n")
for (i in 1:min(5, length(v_segments))) {  # Show first 5
  seg <- v_segments[[i]]
  cat(sprintf("  V%d: %d chars, %d curves, tabs=%s\n", 
              i, seg$length, seg$bezier_count, seg$has_tabs))
  cat(sprintf("    Sample: %s\n", substr(seg$segment, 1, 80)))
}

cat("\nBORDER SEGMENTS:\n")
for (i in 1:length(b_segments)) {
  seg <- b_segments[[i]]
  cat(sprintf("  B%d: %d chars, %d curves, tabs=%s\n", 
              i, seg$length, seg$bezier_count, seg$has_tabs))
  if (seg$has_tabs) {
    cat(sprintf("    Sample: %s\n", substr(seg$segment, 1, 80)))
  }
}

# STEP 7: Understanding piece-to-segment mapping
cat("\n=== STEP 7: PIECE-TO-SEGMENT MAPPING CHALLENGE ===\n")
cat("Key insights:\n")
cat("1. Total pieces expected: 19 (for 3 rings)\n")
cat("2. Total segments available:", length(all_segments), "\n")
cat("3. Each piece needs 6 edges\n")
cat("4. Total edges needed: 19 pieces × 6 edges = 114 edges\n")
cat("5. But edges are SHARED between adjacent pieces\n")
cat("6. So actual unique edges = segments with tabs/blanks\n")

cat("\nThe challenge:\n")
cat("- We have", segments_with_tabs, "segments with actual tabs/blanks\n")
cat("- We need to map these to the 6 edges of each of 19 pieces\n") 
cat("- Adjacent pieces must share the SAME edge (with opposite orientations)\n")

# STEP 8: Save analysis data
analysis_data <- list(
  puzzle_params = list(seed = 42, rings = 3, diameter = 240),
  paths = list(
    horizontal = horizontal_path,
    vertical = vertical_path, 
    border = border_path
  ),
  segments = list(
    horizontal = h_segments,
    vertical = v_segments,
    border = b_segments
  ),
  statistics = list(
    total_segments = length(all_segments),
    segments_with_tabs = segments_with_tabs,
    segments_without_tabs = segments_without_tabs,
    total_bezier_curves = length(h_curves) + length(v_curves) + length(b_curves)
  )
)

saveRDS(analysis_data, "output/hex_tabs_analysis.rds")

cat("\n✅ ANALYSIS COMPLETE\n")
cat("Next steps needed:\n")
cat("1. Map each piece's 6 edges to actual path segments\n")
cat("2. Handle edge sharing between adjacent pieces\n") 
cat("3. Extract and reverse segments as needed\n")
cat("4. Build complete piece boundaries with actual tabs/blanks\n")
cat("Saved analysis to: output/hex_tabs_analysis.rds\n")