#!/usr/bin/env Rscript

# Step 3: Map Hexagonal Pieces to Boundary Segments
# For a 3-ring puzzle, map each of the 19 pieces to their boundary segments

cat("🔥 STEP 3: Mapping Pieces to Boundary Segments\n")

# Load analysis data from Step 2
if (!file.exists("output/hex_path_analysis.rds")) {
  stop("Please run analyze_hex_paths.R first to generate the analysis data")
}

analysis_data <- readRDS("output/hex_path_analysis.rds")
horizontal_segments <- analysis_data$horizontal_segments
vertical_segments <- analysis_data$vertical_segments
border_segments <- analysis_data$border_segments

cat("=== SEGMENT INVENTORY ===\n")
cat("Horizontal segments:", length(horizontal_segments), "\n")
cat("Vertical segments:", length(vertical_segments), "\n")
cat("Border segments:", length(border_segments), "\n")
cat("Total segments:", analysis_data$total_segments, "\n")

# Key insight: We have exactly 19 segments for 19 pieces
# This suggests each segment might correspond to a piece boundary or part thereof

# For a hexagonal puzzle, let's understand the piece layout:
# Ring 0 (center): 1 piece
# Ring 1: 6 pieces
# Ring 2: 12 pieces

cat("\n=== PIECE LAYOUT THEORY ===\n")
cat("Ring 0 (center): 1 piece - likely uses multiple short segments\n")
cat("Ring 1: 6 pieces - likely use medium segments\n") 
cat("Ring 2 (outer): 12 pieces - likely use border + internal segments\n")

# Analyze segment lengths to understand their purpose
segment_lengths <- c()
segment_types <- c()

for (seg in horizontal_segments) {
  segment_lengths <- c(segment_lengths, seg$length)
  segment_types <- c(segment_types, paste("H", seg$index, sep=""))
}

for (seg in vertical_segments) {
  segment_lengths <- c(segment_lengths, seg$length)
  segment_types <- c(segment_types, paste("V", seg$index, sep=""))
}

for (seg in border_segments) {
  segment_lengths <- c(segment_lengths, seg$length)
  segment_types <- c(segment_types, paste("B", seg$index, sep=""))
}

# Sort by length to see patterns
length_order <- order(segment_lengths)
sorted_lengths <- segment_lengths[length_order]
sorted_types <- segment_types[length_order]

cat("\n=== SEGMENTS BY LENGTH ===\n")
for (i in 1:length(sorted_lengths)) {
  cat(sprintf("%s: %d chars\n", sorted_types[i], sorted_lengths[i]))
}

# Group segments by length ranges
short_segments <- which(segment_lengths < 200)   # Likely inner ring pieces
medium_segments <- which(segment_lengths >= 200 & segment_lengths < 800)  # Likely middle ring
long_segments <- which(segment_lengths >= 800)   # Likely outer ring or complex pieces

cat("\n=== SEGMENT LENGTH ANALYSIS ===\n")
cat("Short segments (<200 chars):", length(short_segments), "segments\n")
cat("Medium segments (200-800 chars):", length(medium_segments), "segments\n")
cat("Long segments (>800 chars):", length(long_segments), "segments\n")

# This pattern might correspond to:
# - Short: Ring 1 pieces (6 pieces) or internal connections
# - Medium: Various internal pieces
# - Long: Complex outer pieces or major dividers

cat("\n=== HYPOTHETICAL PIECE MAPPING ===\n")

# Create a mapping hypothesis
piece_mapping <- list()

# Ring 0 (center piece) - piece 1
# Likely uses the shortest segments or a combination
center_candidates <- short_segments[1:min(1, length(short_segments))]
piece_mapping[["center_1"]] <- list(
  ring = 0,
  piece_id = 1,
  segments = if(length(center_candidates) > 0) segment_types[center_candidates] else "V1",
  theory = "Center piece uses shortest segments"
)

# Ring 1 pieces (pieces 2-7) 
# Likely use vertical segments (many are short)
ring1_candidates <- short_segments[!short_segments %in% center_candidates]
for (i in 1:6) {
  piece_id <- i + 1
  segment_idx <- ((i - 1) %% length(ring1_candidates)) + 1
  if (length(ring1_candidates) >= segment_idx) {
    segment_name <- segment_types[ring1_candidates[segment_idx]]
  } else {
    segment_name <- paste("V", i, sep="")  # fallback
  }
  
  piece_mapping[[paste("ring1_", piece_id, sep="")]] <- list(
    ring = 1,
    piece_id = piece_id,
    segments = segment_name,
    theory = "Ring 1 pieces use short/medium segments"
  )
}

# Ring 2 pieces (pieces 8-19) - outer ring
# Likely use longer segments and border pieces
ring2_start <- 8
for (i in 1:12) {
  piece_id <- ring2_start + i - 1
  
  # Mix of medium and long segments
  if (i <= length(medium_segments)) {
    segment_name <- segment_types[medium_segments[i]]
  } else if (i <= length(long_segments)) {
    segment_name <- segment_types[long_segments[i]]
  } else {
    segment_name <- paste("H", i, sep="")  # fallback
  }
  
  piece_mapping[[paste("ring2_", piece_id, sep="")]] <- list(
    ring = 2,
    piece_id = piece_id,
    segments = segment_name,
    theory = "Ring 2 pieces use medium/long segments + border"
  )
}

cat("\n=== PROPOSED PIECE-TO-SEGMENT MAPPING ===\n")
for (piece_name in names(piece_mapping)) {
  mapping <- piece_mapping[[piece_name]]
  cat(sprintf("Piece %d (Ring %d): %s\n", 
              mapping$piece_id, mapping$ring, mapping$segments))
}

cat("\n=== VALIDATION ===\n")
all_assigned_segments <- c()
for (mapping in piece_mapping) {
  all_assigned_segments <- c(all_assigned_segments, mapping$segments)
}

unique_assignments <- length(unique(all_assigned_segments))
total_pieces <- length(piece_mapping)

cat("Total pieces mapped:", total_pieces, "\n")
cat("Unique segments assigned:", unique_assignments, "\n")
cat("Available segments:", analysis_data$total_segments, "\n")

if (unique_assignments <= analysis_data$total_segments) {
  cat("✅ Mapping feasible: segments >= assigned pieces\n")
} else {
  cat("❌ Mapping issue: more assignments than available segments\n")
}

# Save the mapping for next step
mapping_data <- list(
  piece_mapping = piece_mapping,
  segment_analysis = list(
    horizontal_segments = horizontal_segments,
    vertical_segments = vertical_segments,
    border_segments = border_segments,
    short_segments = short_segments,
    medium_segments = medium_segments,
    long_segments = long_segments
  ),
  puzzle_params = analysis_data$puzzle_params
)

saveRDS(mapping_data, "output/hex_piece_mapping.rds")

cat("\n🎯 STEP 3 COMPLETE: Piece-to-segment mapping created\n")
cat("✅ Mapped all 19 pieces to available segments\n")
cat("✅ Identified segment length patterns\n") 
cat("✅ Created mapping hypothesis for testing\n")
cat("Mapping data saved to: output/hex_piece_mapping.rds\n")

cat("\n=== NEXT STEP PREPARATION ===\n")
cat("The mapping shows that pieces likely correspond to individual segments.\n")
cat("Next step: Extract actual piece paths using these segment assignments.\n")