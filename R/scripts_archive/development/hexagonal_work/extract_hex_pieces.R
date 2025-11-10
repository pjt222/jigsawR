#!/usr/bin/env Rscript

# Step 4: Extract Individual Hexagonal Piece Paths
# Use the segment mapping to extract proper bezier curves for each piece

cat("🔥 STEP 4: Extracting Individual Piece Paths\n")

# Load mapping data from Step 3
if (!file.exists("output/hex_piece_mapping.rds")) {
  stop("Please run map_hex_pieces.R first to generate the mapping data")
}

mapping_data <- readRDS("output/hex_piece_mapping.rds")
piece_mapping <- mapping_data$piece_mapping
segment_analysis <- mapping_data$segment_analysis

# Load required functions
source("R/hexagonal_puzzle.R")

cat("=== EXTRACTING PIECE PATHS ===\n")

# Create list of all segments for easy lookup
all_segments <- list()

# Add horizontal segments
for (seg in segment_analysis$horizontal_segments) {
  seg_name <- paste("H", seg$index, sep="")
  all_segments[[seg_name]] <- seg
}

# Add vertical segments
for (seg in segment_analysis$vertical_segments) {
  seg_name <- paste("V", seg$index, sep="")
  all_segments[[seg_name]] <- seg
}

# Add border segments
for (seg in segment_analysis$border_segments) {
  seg_name <- paste("B", seg$index, sep="")
  all_segments[[seg_name]] <- seg
}

cat("Total segments available:", length(all_segments), "\n")

# Extract paths for each piece
extracted_pieces <- list()

for (piece_name in names(piece_mapping)) {
  mapping <- piece_mapping[[piece_name]]
  piece_id <- mapping$piece_id
  assigned_segment <- mapping$segments
  
  cat(sprintf("Processing Piece %d: assigned to %s\n", piece_id, assigned_segment))
  
  # Get the actual segment
  if (assigned_segment %in% names(all_segments)) {
    segment <- all_segments[[assigned_segment]]
    
    # Extract the path
    piece_path <- segment$path
    
    # Verify it has bezier curves
    has_bezier <- segment$has_bezier
    path_length <- nchar(piece_path)
    
    cat(sprintf("  → Path length: %d chars, bezier: %s\n", path_length, has_bezier))
    
    # Store the extracted piece
    extracted_pieces[[as.character(piece_id)]] <- list(
      piece_id = piece_id,
      ring = mapping$ring,
      path = piece_path,
      segment_source = assigned_segment,
      has_bezier = has_bezier,
      path_length = path_length
    )
    
  } else {
    cat(sprintf("  ⚠️  WARNING: Segment %s not found\n", assigned_segment))
    
    # Create a fallback simple hexagon for missing segments
    fallback_path <- "M 20 0 L 10 17.32 L -10 17.32 L -20 0 L -10 -17.32 L 10 -17.32 Z"
    
    extracted_pieces[[as.character(piece_id)]] <- list(
      piece_id = piece_id,
      ring = mapping$ring,
      path = fallback_path,
      segment_source = "fallback",
      has_bezier = FALSE,
      path_length = nchar(fallback_path)
    )
  }
}

cat("\n=== EXTRACTION RESULTS ===\n")
cat("Pieces extracted:", length(extracted_pieces), "\n")

# Analyze the results
bezier_count <- 0
path_lengths <- c()
segment_sources <- c()

for (piece in extracted_pieces) {
  if (piece$has_bezier) bezier_count <- bezier_count + 1
  path_lengths <- c(path_lengths, piece$path_length)
  segment_sources <- c(segment_sources, piece$segment_source)
}

cat("Pieces with bezier curves:", bezier_count, "out of", length(extracted_pieces), "\n")
cat("Path length range:", min(path_lengths), "to", max(path_lengths), "characters\n")
cat("Average path length:", round(mean(path_lengths)), "characters\n")

# Check for unique paths
unique_paths <- length(unique(sapply(extracted_pieces, function(p) p$path)))
cat("Unique paths:", unique_paths, "out of", length(extracted_pieces), "\n")

if (unique_paths == length(extracted_pieces)) {
  cat("✅ SUCCESS: All pieces have unique paths!\n")
} else {
  cat("⚠️  Some pieces share paths\n")
}

# Show sample paths
cat("\n=== SAMPLE EXTRACTED PATHS ===\n")
for (i in 1:min(3, length(extracted_pieces))) {
  piece <- extracted_pieces[[i]]
  cat(sprintf("Piece %d (%s): %s...\n", 
              piece$piece_id, piece$segment_source, 
              substr(piece$path, 1, 80)))
}

# Verify we have the key components for individual pieces
bezier_percentage <- (bezier_count / length(extracted_pieces)) * 100
cat(sprintf("\n=== QUALITY ASSESSMENT ===\n"))
cat(sprintf("Bezier coverage: %.1f%% of pieces\n", bezier_percentage))

if (bezier_percentage >= 70) {
  cat("✅ EXCELLENT: Most pieces have proper bezier curves\n")
} else if (bezier_percentage >= 50) {
  cat("⚠️  GOOD: Many pieces have bezier curves\n") 
} else {
  cat("❌ POOR: Few pieces have bezier curves\n")
}

# Save extracted pieces for next step
extraction_data <- list(
  extracted_pieces = extracted_pieces,
  puzzle_params = mapping_data$puzzle_params,
  statistics = list(
    total_pieces = length(extracted_pieces),
    bezier_count = bezier_count,
    unique_paths = unique_paths,
    path_length_range = c(min(path_lengths), max(path_lengths)),
    average_length = mean(path_lengths)
  )
)

saveRDS(extraction_data, "output/hex_piece_extraction.rds")

cat("\n🎯 STEP 4 COMPLETE: Individual piece paths extracted\n")
cat("✅ Extracted", length(extracted_pieces), "pieces with actual puzzle paths\n")
cat("✅", bezier_count, "pieces have bezier curves (tabs and blanks)\n") 
cat("✅", unique_paths, "unique paths generated\n")
cat("Extraction data saved to: output/hex_piece_extraction.rds\n")

cat("\n=== NEXT STEP PREPARATION ===\n")
cat("Ready to generate individual pieces SVG using extracted paths.\n")
cat("These paths contain the actual bezier curves from the puzzle.\n")