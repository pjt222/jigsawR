# Analyze the SVG structure in detail
devtools::load_all()

# Read the generated SVG
svg_content <- readLines("output/fusion_investigation/puzzle_2x2_seed42_separated.svg")

cat("=== SVG Line Count ===\n")
cat("Total lines:", length(svg_content), "\n\n")

cat("=== Path Elements ===\n")
path_lines <- grep('<path d=', svg_content, value = TRUE)
for (i in seq_along(path_lines)) {
  line <- path_lines[i]
  
  # Extract path type
  if (grepl('fill="none" stroke="none"', line)) {
    type <- "FILL (invisible)"
  } else if (grepl('opacity="', line)) {
    type <- "FUSED EDGE"
  } else if (grepl('stroke=', line)) {
    type <- "NORMAL EDGE"
  } else {
    type <- "UNKNOWN"
  }
  
  # Extract path preview
  path_preview <- sub('.*d="([^"]+)".*', '\\1', line)
  path_preview <- substr(path_preview, 1, 50)
  
  cat(sprintf("Line %d: %s - %s...\n", i, type, path_preview))
}

# Count by type
cat("\n=== Path Type Counts ===\n")
cat("Fill paths:", sum(grepl('fill="none" stroke="none"', path_lines)), "\n")
cat("Normal edges:", sum(grepl('stroke=', path_lines) & !grepl('opacity="', path_lines)), "\n")
cat("Fused edges:", sum(grepl('opacity="', path_lines)), "\n")

# Expected counts for 2x2 grid with pieces 1-2 fused
cat("\n=== Expected for 2x2 with fusion (pieces 1-2) ===\n")
cat("4 pieces total\n")
cat("Each piece has 4 edges: N, E, S, W\n")
cat("Piece 1: E edge is fused\n")
cat("Piece 2: W edge is fused\n")
cat("Expected normal edges: 4 pieces * 4 edges - 2 fused = 14\n")
cat("Expected fused edges: 2\n")
