#!/usr/bin/env Rscript

# Test the exact pathway that Shiny app uses for "individual pieces" mode

cat("🔍 Testing Shiny app 'individual pieces' pathway\n")

# Load required functions
source("R/jigsawR_clean.R")

# Simulate Shiny app call with hexagonal individual pieces
result <- generate_puzzle(
  type = "hexagonal",
  grid = c(3, 3),  # rings = 3
  size = c(240, 240),  # diameter = 240
  seed = 42,
  tabsize = 27,
  jitter = 5,
  output = "individual",  # This is the key setting
  colors = c("red", "blue", "green", "orange", "purple"),
  background = "none",
  save_files = FALSE,
  do_warp = FALSE,
  do_trunc = FALSE,
  stroke_width = 1.5
)

cat("Result generated successfully\n")
cat("Result type:", result$type, "\n")
cat("Has svg_individual:", !is.null(result$svg_individual), "\n")

if (!is.null(result$svg_individual)) {
  svg_content <- result$svg_individual
  cat("SVG individual length:", nchar(svg_content), "characters\n")
  
  # Save to file to examine
  writeLines(svg_content, "output/test_shiny_individual_mode.svg")
  cat("Saved to: output/test_shiny_individual_mode.svg\n")
  
  # Show first few pieces to analyze the issue
  cat("\nFirst 500 characters of individual pieces SVG:\n")
  cat(substr(svg_content, 1, 500), "...\n")
  
  # Count pieces
  piece_count <- length(gregmatches(svg_content, gregexpr('<g id="piece-', svg_content))[[1]])
  cat("Number of pieces found:", piece_count, "\n")
  
  # Check for size variations in paths
  cat("\nAnalyzing piece sizes and colors...\n")
  
  # Extract path elements
  paths <- regmatches(svg_content, gregexpr('<path d="[^"]*"[^>]*>', svg_content))[[1]]
  cat("First 3 path elements:\n")
  for (i in 1:min(3, length(paths))) {
    cat("Path", i, ":", substr(paths[i], 1, 100), "...\n")
  }
}

cat("\n🎯 This shows exactly what the Shiny app 'individual pieces' mode displays\n")