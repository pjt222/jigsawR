#!/usr/bin/env Rscript

# Simplified Analysis of Hexagonal Puzzle Tabs/Blanks 
# Fix R function issue and understand the bezier structure

cat("🔍 SIMPLIFIED HEXAGONAL TABS ANALYSIS\n")

# Load hexagonal puzzle functions
source("R/hexagonal_puzzle.R")

# Generate puzzle
cat("=== GENERATING PUZZLE WITH TABS ===\n")
init_hex_jigsaw(seed = 42, rings = 3, diameter = 240)

horizontal_path <- hex_gen_dh()
vertical_path <- hex_gen_dv()
border_path <- hex_gen_db()

cat("Horizontal path:", nchar(horizontal_path), "chars\n")
cat("Vertical path:", nchar(vertical_path), "chars\n")
cat("Border path:", nchar(border_path), "chars\n")

# Simple function to count bezier curves
count_bezier_curves <- function(path_string) {
  # Count C commands
  c_count <- length(gregexpr("\\bC\\s+", path_string)[[1]])
  if (c_count == 1 && gregexpr("\\bC\\s+", path_string)[[1]][1] == -1) {
    c_count <- 0
  }
  return(c_count)
}

h_curves <- count_bezier_curves(horizontal_path)
v_curves <- count_bezier_curves(vertical_path)
b_curves <- count_bezier_curves(border_path)

cat("Bezier curves found:\n")
cat("- Horizontal:", h_curves, "\n")
cat("- Vertical:", v_curves, "\n") 
cat("- Border:", b_curves, "\n")
cat("- Total:", h_curves + v_curves + b_curves, "\n")

# Look at the structure
cat("\n=== PATH STRUCTURE ANALYSIS ===\n")

# Show start of each path type to understand structure
cat("Horizontal path start:\n")
cat(substr(horizontal_path, 1, 200), "\n...\n")

cat("\nVertical path start:\n")
cat(substr(vertical_path, 1, 200), "\n...\n")

cat("\nBorder path start:\n")
cat(substr(border_path, 1, 200), "\n...\n")

# Parse segments by splitting on M
parse_simple_segments <- function(path_string, path_type) {
  if (nchar(path_string) == 0) return(list())
  
  segments <- strsplit(path_string, "M ")[[1]]
  segments <- segments[segments != "" & nchar(trimws(segments)) > 0]
  
  result <- list()
  for (i in 1:length(segments)) {
    segment_text <- trimws(segments[i])
    if (segment_text != "") {
      if (i > 1 || !grepl("^M", path_string)) {
        segment_text <- paste0("M ", segment_text)
      }
      
      # Simple check for curves
      has_curves <- grepl("C ", segment_text)
      curve_count <- length(gregexpr("\\bC\\s+", segment_text)[[1]])
      if (curve_count == 1 && gregexpr("\\bC\\s+", segment_text)[[1]][1] == -1) {
        curve_count <- 0
      }
      
      result[[i]] <- list(
        id = paste0(path_type, i),
        segment = segment_text,
        length = nchar(segment_text),
        has_bezier = has_curves,
        bezier_count = curve_count
      )
    }
  }
  return(result)
}

h_segs <- parse_simple_segments(horizontal_path, "H")
v_segs <- parse_simple_segments(vertical_path, "V") 
b_segs <- parse_simple_segments(border_path, "B")

cat("\n=== SEGMENT BREAKDOWN ===\n")
cat("Horizontal segments:", length(h_segs), "\n")
cat("Vertical segments:", length(v_segs), "\n")
cat("Border segments:", length(b_segs), "\n")

# Show what segments have bezier curves
cat("\n=== SEGMENTS WITH BEZIER CURVES ===\n")

bezier_segments <- 0
straight_segments <- 0

all_segs <- c(h_segs, v_segs, b_segs)
for (seg in all_segs) {
  if (seg$has_bezier) {
    bezier_segments <- bezier_segments + 1
    cat(sprintf("%s: %d curves, %d chars\n", seg$id, seg$bezier_count, seg$length))
    if (bezier_segments <= 3) {  # Show first 3 examples
      cat(sprintf("  → %s\n", substr(seg$segment, 1, 100)))
    }
  } else {
    straight_segments <- straight_segments + 1
  }
}

cat(sprintf("\nSUMMARY:\n"))
cat(sprintf("- Segments with bezier curves (tabs/blanks): %d\n", bezier_segments))
cat(sprintf("- Segments with straight lines: %d\n", straight_segments))
cat(sprintf("- Total segments: %d\n", length(all_segs)))

# Key insight about the individual pieces
cat("\n=== KEY INSIGHT FOR INDIVIDUAL PIECES ===\n")
cat("Expected pieces: 19 (for 3 rings: 1 center + 6 ring1 + 12 ring2)\n")
cat("Total segments available:", length(all_segs), "\n")
cat("Segments with actual tabs/blanks:", bezier_segments, "\n")

cat("\nThe problem with current implementation:\n")
cat("✗ Current: Using geometric hexagons (only L commands)\n")
cat("✓ Needed: Using actual puzzle segments (with C commands for tabs)\n")

cat("\nNext steps:\n")
cat("1. Map each piece to its surrounding segments\n")
cat("2. Extract the segments that form each piece's 6 edges\n") 
cat("3. Handle shared edges between adjacent pieces\n")
cat("4. Construct complete piece paths with actual tabs/blanks\n")

# Save the segments for next step
segments_data <- list(
  horizontal_segments = h_segs,
  vertical_segments = v_segs,
  border_segments = b_segs,
  statistics = list(
    bezier_segments = bezier_segments,
    straight_segments = straight_segments,
    total_segments = length(all_segs)
  ),
  puzzle_params = list(seed = 42, rings = 3, diameter = 240)
)

saveRDS(segments_data, "output/hex_segments_data.rds")

cat("\n✅ SEGMENTS ANALYZED AND SAVED\n")
cat("Saved to: output/hex_segments_data.rds\n")
cat("Ready for piece-to-segment mapping next.\n")