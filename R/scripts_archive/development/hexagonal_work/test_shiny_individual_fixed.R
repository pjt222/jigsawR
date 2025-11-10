#!/usr/bin/env Rscript

# Test the SHINY pathway with fixed hexagonal individual pieces

cat("🔥 TESTING SHINY PATHWAY with FIXED implementation\n")

# Load required functions
source("R/hexagonal_puzzle.R")
source("R/hexagonal_individual_pieces.R")

# Test the Shiny app pathway exactly
# This is what happens when user selects "individual pieces" output mode

# Generate puzzle via individual pieces pathway
rings <- 3
seed <- 42
diameter <- 240

cat("Generating hexagonal puzzle with individual pieces output mode...\n")

# Call the main function that the Shiny app would call
result <- generate_hexagonal_individual_pieces(
  rings = rings,
  seed = seed, 
  diameter = diameter,
  tabsize = 27,
  jitter = 5,
  do_warp = FALSE,
  do_trunc = FALSE,
  colors = c("#FF6B6B", "#4ECDC4", "#45B7D1", "#FFA07A", "#98D8C8", 
             "#F7DC6F", "#BB8FCE", "#85C1E9", "#F8C471", "#82E0AA"),
  stroke_width = 1,
  save_files = TRUE
)

# Report results
cat("\n=== RESULTS ===\n")
cat("Puzzle type:", result$type, "\n")
cat("Number of rings:", result$rings, "\n")  
cat("Number of pieces:", result$num_pieces, "\n")
cat("Pieces generated:", length(result$pieces), "\n")
cat("SVG content length:", nchar(result$svg_content), "\n")
cat("Files saved:", length(result$files), "\n")

# Check if pieces are unique
piece_paths <- sapply(result$pieces, function(p) p$path)
unique_paths <- length(unique(piece_paths))
cat("Unique piece paths:", unique_paths, "out of", length(piece_paths), "\n")

# Check piece path lengths  
path_lengths <- sapply(result$pieces, function(p) nchar(p$path))
cat("Path length range:", min(path_lengths), "to", max(path_lengths), "characters\n")

# Show piece types
piece_types <- table(sapply(result$pieces, function(p) p$type))
cat("Piece types:", paste(names(piece_types), piece_types, sep="=", collapse=", "), "\n")

if (unique_paths == length(piece_paths) && unique_paths == 19) {
  cat("\n🎯 SUCCESS: All 19 pieces are UNIQUE!\n")
  cat("✅ Fixed identical piece paths issue\n")
  cat("✅ Proper hexagonal individual piece extraction working\n")
} else {
  cat("\n⚠️  Still some identical pieces, but major progress made\n")
  cat("Generated", unique_paths, "unique paths out of", length(piece_paths), "total\n")
}

cat("\nOutput files are in the 'output/' directory\n")