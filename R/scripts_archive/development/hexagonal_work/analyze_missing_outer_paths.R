#!/usr/bin/env Rscript

# Analyze Missing Outer Border Paths in Hexagonal Individual Pieces
cat("🔍 ANALYZING MISSING OUTER BORDER PATHS\n")

# First, let's understand the expected hexagonal puzzle structure
# For 3 rings: center (1) + ring 1 (6) + ring 2 (12) = 19 pieces total
# But we only have 18 pieces after removing the fallback

cat("=== EXPECTED HEXAGONAL STRUCTURE (3 rings) ===\n")
expected_pieces <- 3 * 3 * (3 - 1) + 1  # Formula: 3 * rings * (rings - 1) + 1
cat("Expected total pieces:", expected_pieces, "\n")

# Ring structure:
cat("Ring 0 (center): 1 piece\n")
cat("Ring 1: 6 pieces\n") 
cat("Ring 2: 12 pieces\n")
cat("Total: 19 pieces\n")

# Check what we currently have
if (file.exists("output/hex_individual_CLEANED.svg")) {
  cleaned_svg <- readLines("output/hex_individual_CLEANED.svg")
  cleaned_paths <- length(grep('<path d=', cleaned_svg))
  cat("\nCurrent pieces in CLEANED.svg:", cleaned_paths, "\n")
} else {
  cat("\nCLEANED.svg not found\n")
}

# Load the original puzzle segments to understand what should be there
if (file.exists("output/hex_individual_with_real_tabs.rds")) {
  tabs_data <- readRDS("output/hex_individual_with_real_tabs.rds")
  original_pieces <- tabs_data$pieces
  cat("Original pieces data:", length(original_pieces), "\n")
  
  cat("\n=== ORIGINAL PIECES ANALYSIS ===\n")
  for (i in 1:length(original_pieces)) {
    piece_data <- original_pieces[[as.character(i)]]
    
    if (is.null(piece_data)) {
      piece_data <- original_pieces[[i]]
    }
    
    if (!is.null(piece_data)) {
      path_snippet <- substr(piece_data$path, 1, 50)
      has_bezier <- piece_data$has_bezier
      
      # Check if this looks like an outer border piece
      is_outer <- grepl("^[0-9.\\s]+L", piece_data$path) || 
                 (!has_bezier && nchar(piece_data$path) > 200)
      
      cat(sprintf("Piece %2d: %s %s %s\n", 
                  piece_data$piece_id,
                  if(has_bezier) "✓bezier" else "○lines ",
                  if(is_outer) "OUTER" else "inner",
                  substr(path_snippet, 1, 30)))
    }
  }
} else {
  cat("Original pieces data not found\n")
}

# Now check the original hexagonal puzzle to see its segment structure
cat("\n=== CHECKING ORIGINAL HEXAGONAL PUZZLE ===\n")

# Generate a fresh hexagonal puzzle to understand the expected segments
source("R/hexagonal_puzzle.R")

# Generate the same puzzle used for individual pieces extraction
seed_val <- 12345  # Use same seed as was used for individual pieces
rings_val <- 3
diameter_val <- 200

puzzle_result <- generate_hex_jigsaw_svg(
  rings = rings_val,
  diameter = diameter_val, 
  seed = seed_val,
  tab_size = 25,
  jitter = 0.18
)

cat("Generated fresh hexagonal puzzle\n")

# Parse the puzzle SVG to find all segments
puzzle_lines <- strsplit(puzzle_result$svg, "\n")[[1]]
puzzle_paths <- grep('<path', puzzle_lines, value = TRUE)

cat("Original puzzle segments:", length(puzzle_paths), "\n")

# Analyze each segment in the original puzzle
cat("\n=== ORIGINAL PUZZLE SEGMENTS ===\n")
for (i in seq_along(puzzle_paths)) {
  path_line <- puzzle_paths[i]
  
  # Extract the d attribute
  d_match <- regmatches(path_line, regexpr('d="[^"]*"', path_line))
  if (length(d_match) > 0) {
    path_data <- gsub('d="', '', d_match)
    path_data <- gsub('".*', '', path_data)
    
    # Check characteristics
    has_c_commands <- grepl(' C ', path_data)
    has_l_commands <- grepl(' L ', path_data) 
    is_closed <- grepl(' Z', path_data, ignore.case = TRUE)
    path_length <- nchar(path_data)
    
    # Extract some coordinates to identify outer pieces
    coords <- regmatches(path_data, gregexpr("[-\\d\\.]+", path_data))[[1]]
    coords_numeric <- as.numeric(coords[!is.na(as.numeric(coords))])
    
    if (length(coords_numeric) >= 4) {
      x_coords <- coords_numeric[seq(1, length(coords_numeric), by = 2)]
      y_coords <- coords_numeric[seq(2, length(coords_numeric), by = 2)]
      
      # Check if this segment extends to the puzzle boundaries
      min_x <- min(x_coords)
      max_x <- max(x_coords)
      min_y <- min(y_coords)
      max_y <- max(y_coords)
      
      # Estimate if this is an outer border segment
      is_outer_estimate <- (max_x - min_x > 100) || (max_y - min_y > 100)
      
      cat(sprintf("Segment %2d: %s %s %s bounds(%.0f-%.0f, %.0f-%.0f) len=%d\n",
                  i,
                  if(has_c_commands) "bezier" else "lines ",
                  if(is_closed) "closed" else "open  ",
                  if(is_outer_estimate) "OUTER" else "inner",
                  min_x, max_x, min_y, max_y, path_length))
    }
  }
}

cat("\n=== MISSING PIECES ANALYSIS ===\n")
cat("Expected pieces: 19\n")
cat("Current pieces: 18\n")
cat("Missing pieces: 1\n")
cat("\nThis suggests we accidentally removed a real piece along with the fallback.\n")
cat("Need to restore the missing outer border piece(s).\n")