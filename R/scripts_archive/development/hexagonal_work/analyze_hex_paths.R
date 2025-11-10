#!/usr/bin/env Rscript

# Step 2: Analyze Hexagonal Puzzle Path Structure
# Understand how the puzzle paths are organized and map to pieces

cat("🔥 STEP 2: Analyzing Hexagonal Puzzle Path Structure\n")

# Load required functions
source("R/hexagonal_puzzle.R")

# Generate the same puzzle as Step 1
puzzle_params <- list(
  seed = 42,
  rings = 3,
  diameter = 240,
  tabsize = 27,
  jitter = 5,
  do_warp = FALSE,
  do_trunc = FALSE
)

# Generate puzzle and access the raw path data
init_hex_jigsaw(
  seed = puzzle_params$seed,
  rings = puzzle_params$rings,
  diameter = puzzle_params$diameter,
  tabsize = puzzle_params$tabsize,
  jitter = puzzle_params$jitter,
  do_warp = puzzle_params$do_warp,
  do_trunc = puzzle_params$do_trunc
)

# Generate individual path components
horizontal_paths <- hex_gen_dh()
vertical_paths <- hex_gen_dv() 
border_paths <- hex_gen_db()

cat("=== PUZZLE PARAMETERS ===\n")
cat("Rings:", puzzle_params$rings, "\n")
cat("Expected pieces for 3 rings:", 3 * 3 * (3 - 1) + 1, "= 19 pieces\n")
cat("- Ring 0 (center): 1 piece\n")
cat("- Ring 1: 6 pieces\n") 
cat("- Ring 2 (outer): 12 pieces\n")

# Parse path segments by splitting on 'M' commands
parse_path_segments <- function(path_string, path_type) {
  if (nchar(path_string) == 0) {
    return(list())
  }
  
  # Find all 'M' command positions
  m_positions <- gregexpr("M", path_string)[[1]]
  
  if (length(m_positions) == 1 && m_positions[1] == -1) {
    # No M commands found
    return(list())
  }
  
  segments <- list()
  
  if (length(m_positions) > 1) {
    # Multiple segments
    for (i in 1:(length(m_positions) - 1)) {
      start_pos <- m_positions[i]
      end_pos <- m_positions[i + 1] - 1
      segment <- substr(path_string, start_pos, end_pos)
      segments[[length(segments) + 1]] <- list(
        type = path_type,
        index = i,
        path = trimws(segment),
        length = nchar(segment),
        has_bezier = grepl("C", segment)
      )
    }
    
    # Last segment
    last_start <- m_positions[length(m_positions)]
    last_segment <- substr(path_string, last_start, nchar(path_string))
    segments[[length(segments) + 1]] <- list(
      type = path_type,
      index = length(m_positions),
      path = trimws(last_segment),
      length = nchar(last_segment),
      has_bezier = grepl("C", last_segment)
    )
  } else {
    # Single segment
    segments[[1]] <- list(
      type = path_type,
      index = 1,
      path = trimws(path_string),
      length = nchar(path_string),
      has_bezier = grepl("C", path_string)
    )
  }
  
  return(segments)
}

cat("\n=== PATH SEGMENT ANALYSIS ===\n")

# Parse all path types
horizontal_segments <- parse_path_segments(horizontal_paths, "horizontal")
vertical_segments <- parse_path_segments(vertical_paths, "vertical")
border_segments <- parse_path_segments(border_paths, "border")

cat("Horizontal segments:", length(horizontal_segments), "\n")
cat("Vertical segments:", length(vertical_segments), "\n") 
cat("Border segments:", length(border_segments), "\n")
cat("Total segments:", length(horizontal_segments) + length(vertical_segments) + length(border_segments), "\n")

# Analyze horizontal segments
if (length(horizontal_segments) > 0) {
  cat("\n--- HORIZONTAL SEGMENTS ---\n")
  for (i in 1:min(3, length(horizontal_segments))) {
    seg <- horizontal_segments[[i]]
    cat(sprintf("Segment %d: %d chars, bezier: %s\n", i, seg$length, seg$has_bezier))
    cat("  Path:", substr(seg$path, 1, 100), "...\n")
  }
  if (length(horizontal_segments) > 3) {
    cat("  ... and", length(horizontal_segments) - 3, "more horizontal segments\n")
  }
}

# Analyze vertical segments  
if (length(vertical_segments) > 0) {
  cat("\n--- VERTICAL SEGMENTS ---\n")
  for (i in 1:min(3, length(vertical_segments))) {
    seg <- vertical_segments[[i]]
    cat(sprintf("Segment %d: %d chars, bezier: %s\n", i, seg$length, seg$has_bezier))
    cat("  Path:", substr(seg$path, 1, 100), "...\n")
  }
  if (length(vertical_segments) > 3) {
    cat("  ... and", length(vertical_segments) - 3, "more vertical segments\n")
  }
}

# Analyze border segments
if (length(border_segments) > 0) {
  cat("\n--- BORDER SEGMENTS ---\n")
  for (i in 1:length(border_segments)) {
    seg <- border_segments[[i]]
    cat(sprintf("Segment %d: %d chars, bezier: %s\n", i, seg$length, seg$has_bezier))
    cat("  Path:", substr(seg$path, 1, 150), "...\n")
  }
}

cat("\n=== COORDINATE SYSTEM ANALYSIS ===\n")

# Extract some coordinates to understand the coordinate system
extract_first_coordinate <- function(path) {
  # Extract first M command coordinates
  m_match <- regexpr("M\\s+([-\\d\\.]+)\\s+([-\\d\\.]+)", path)
  if (m_match != -1) {
    coords_text <- regmatches(path, m_match)
    coords <- as.numeric(unlist(regmatches(coords_text, gregexpr("[-\\d\\.]+", coords_text))))
    if (length(coords) >= 2) {
      return(c(coords[1], coords[2]))
    }
  }
  return(NULL)
}

# Get starting coordinates for segments
h_coords <- list()
v_coords <- list()

for (i in 1:min(3, length(horizontal_segments))) {
  coord <- extract_first_coordinate(horizontal_segments[[i]]$path)
  if (!is.null(coord)) {
    h_coords[[i]] <- coord
  }
}

for (i in 1:min(3, length(vertical_segments))) {
  coord <- extract_first_coordinate(vertical_segments[[i]]$path) 
  if (!is.null(coord)) {
    v_coords[[i]] <- coord
  }
}

if (length(h_coords) > 0) {
  cat("Sample horizontal segment starting coordinates:\n")
  for (i in 1:length(h_coords)) {
    if (!is.null(h_coords[[i]])) {
      cat(sprintf("  H%d: (%.2f, %.2f)\n", i, h_coords[[i]][1], h_coords[[i]][2]))
    }
  }
} else {
  cat("No horizontal coordinates extracted\n")
}

if (length(v_coords) > 0) {
  cat("Sample vertical segment starting coordinates:\n")
  for (i in 1:length(v_coords)) {
    if (!is.null(v_coords[[i]])) {
      cat(sprintf("  V%d: (%.2f, %.2f)\n", i, v_coords[[i]][1], v_coords[[i]][2]))
    }
  }
} else {
  cat("No vertical coordinates extracted\n")
}

# Calculate puzzle bounds from parameters
radius <- puzzle_params$diameter / 2.0
offset <- radius * 0.2
total_size <- 2.0 * (radius + offset)

cat("\nPuzzle bounds:\n")
cat("- Radius:", radius, "mm\n")
cat("- Offset:", offset, "mm\n") 
cat("- Total size:", total_size, "mm\n")

cat("\n🎯 STEP 2 COMPLETE: Puzzle path structure analyzed\n")
cat("✅ Found", length(horizontal_segments), "horizontal segments with bezier curves\n")
cat("✅ Found", length(vertical_segments), "vertical segments with bezier curves\n") 
cat("✅ Found", length(border_segments), "border segments\n")
cat("✅ Coordinate system and puzzle bounds identified\n")

# Save analysis data for next step
analysis_data <- list(
  horizontal_segments = horizontal_segments,
  vertical_segments = vertical_segments, 
  border_segments = border_segments,
  puzzle_params = puzzle_params,
  total_segments = length(horizontal_segments) + length(vertical_segments) + length(border_segments)
)

saveRDS(analysis_data, "output/hex_path_analysis.rds")
cat("Analysis data saved to: output/hex_path_analysis.rds\n")