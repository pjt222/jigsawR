#!/usr/bin/env Rscript

# Debug script to examine hexagonal puzzle paths in detail

cat("🔍 Examining hexagonal puzzle paths for proper individual piece extraction\n")

# Load required functions  
source("R/hexagonal_puzzle.R")

# Generate a simple puzzle to examine its structure
rings <- 3
seed <- 42
diameter <- 200

cat("Generating hexagonal puzzle with rings =", rings, "\n")

# Get the puzzle structure
puzzle <- generate_hex_jigsaw_svg(rings = rings, seed = seed, diameter = diameter)

cat("Puzzle generated successfully\n")
cat("Number of pieces expected: 3 * rings * (rings - 1) + 1 =", 3 * rings * (rings - 1) + 1, "\n")

cat("\n=== PATH ANALYSIS ===\n")

cat("\n1. HORIZONTAL PATHS:\n")
h_path <- puzzle$horizontal
cat("Length:", nchar(h_path), "characters\n")
cat("First 200 chars:", substr(h_path, 1, 200), "\n")

# Count move commands (M) - each represents a new path segment  
h_moves <- lengths(regmatches(h_path, gregexpr("M", h_path)))
cat("Number of 'M' commands (segments):", h_moves, "\n")

cat("\n2. VERTICAL PATHS:\n")  
v_path <- puzzle$vertical
cat("Length:", nchar(v_path), "characters\n")
cat("First 200 chars:", substr(v_path, 1, 200), "\n")

v_moves <- lengths(regmatches(v_path, gregexpr("M", v_path)))
cat("Number of 'M' commands (segments):", v_moves, "\n")

cat("\n3. BORDER PATHS:\n")
b_path <- puzzle$border
cat("Length:", nchar(b_path), "characters\n")
cat("First 200 chars:", substr(b_path, 1, 200), "\n")

b_moves <- lengths(regmatches(b_path, gregexpr("M", b_path)))
cat("Number of 'M' commands (segments):", b_moves, "\n")

cat("\n=== PATH SEGMENT BREAKDOWN ===\n")

# Split horizontal path by M commands to see individual segments
h_segments <- strsplit(h_path, "M")[[1]]
h_segments <- h_segments[nchar(h_segments) > 0]  # Remove empty elements

cat("Horizontal path segments:\n")
for (i in 1:min(3, length(h_segments))) {
  segment <- paste0("M", h_segments[i])
  cat("Segment", i, "(", nchar(segment), "chars):", substr(segment, 1, 100), "...\n")
}

# Look at vertical segments
v_segments <- strsplit(v_path, "M")[[1]]
v_segments <- v_segments[nchar(v_segments) > 0]

cat("\nVertical path segments:\n")
for (i in 1:min(3, length(v_segments))) {
  segment <- paste0("M", v_segments[i])
  cat("Segment", i, "(", nchar(segment), "chars):", substr(segment, 1, 100), "...\n")
}

cat("\n=== INSIGHT FOR INDIVIDUAL PIECES ===\n")
cat("Each 'M' command starts a new path segment.\n")
cat("Adjacent pieces should share these exact segments.\n") 
cat("The key is mapping piece positions to specific segments.\n")
cat("Total segments available: H =", length(h_segments), ", V =", length(v_segments), ", B =", length(strsplit(b_path, "M")[[1]]), "\n")

cat("\n🎯 Path analysis completed!\n")