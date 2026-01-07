devtools::load_all()

cat("Testing Fusion Rendering with Large Offset\n")
cat("==========================================\n\n")

# Test 1: Rectangular fusion with large offset
cat("1. Rectangular puzzle with fusion (offset=30)...\n")
result_rect <- generate_puzzle(
  type = "rectangular",
  seed = 42,
  grid = c(2, 2),
  size = c(200, 200),
  offset = 30,
  fusion_groups = list(c(1, 2)),
  fusion_style = "dashed",  # Make fused edges visible with dashed style
  save_files = TRUE,
  output_dir = "output"
)
cat(sprintf("   Generated %d pieces\n", length(result_rect$pieces)))
cat(sprintf("   SVG saved to: %s\n", result_rect$files$svg))

# Check if fused edges are set
p1 <- result_rect$pieces[[1]]
p2 <- result_rect$pieces[[2]]
cat(sprintf("   Piece 1 fused_edges: %s\n", paste(names(which(unlist(p1$fused_edges))), collapse = ", ")))
cat(sprintf("   Piece 2 fused_edges: %s\n", paste(names(which(unlist(p2$fused_edges))), collapse = ", ")))

# Test 2: Hexagonal fusion with large offset
cat("\n2. Hexagonal puzzle with fusion (offset=25)...\n")
result_hex <- generate_puzzle(
  type = "hexagonal",
  seed = 42,
  grid = c(2),
  size = c(200),
  offset = 25,
  fusion_groups = list(c(1, 2)),
  fusion_style = "dashed",  # Make fused edges visible with dashed style
  save_files = TRUE,
  output_dir = "output"
)
cat(sprintf("   Generated %d pieces\n", length(result_hex$pieces)))
cat(sprintf("   SVG saved to: %s\n", result_hex$files$svg))

# Check if fused edges are set
p1_hex <- result_hex$pieces[[1]]
p2_hex <- result_hex$pieces[[2]]
cat(sprintf("   Piece 1 fused_edges: %s\n", paste(names(which(unlist(p1_hex$fused_edges))), collapse = ", ")))
cat(sprintf("   Piece 2 fused_edges: %s\n", paste(names(which(unlist(p2_hex$fused_edges))), collapse = ", ")))

# Test 3: Concentric fusion with large offset
cat("\n3. Concentric puzzle with fusion (offset=25)...\n")
result_conc <- generate_puzzle(
  type = "concentric",
  seed = 42,
  grid = c(2),
  size = c(200),
  offset = 25,
  fusion_groups = list(c(1, 2)),
  fusion_style = "dashed",  # Make fused edges visible with dashed style
  save_files = TRUE,
  output_dir = "output"
)
cat(sprintf("   Generated %d pieces\n", length(result_conc$pieces)))
cat(sprintf("   SVG saved to: %s\n", result_conc$files$svg))

# Check if fused edges are set
p1_conc <- result_conc$pieces[[1]]
p2_conc <- result_conc$pieces[[2]]
cat(sprintf("   Piece 1 fused_edges: %s\n", paste(names(which(unlist(p1_conc$fused_edges))), collapse = ", ")))
cat(sprintf("   Piece 2 fused_edges: %s\n", paste(names(which(unlist(p2_conc$fused_edges))), collapse = ", ")))

# Verify SVG content contains fusion styling
cat("\n4. Checking SVG content for fusion styling...\n")

svg_rect <- readLines(result_rect$files$svg)
svg_hex <- readLines(result_hex$files$svg)
svg_conc <- readLines(result_conc$files$svg)

# Look for fusion styling (opacity and dashed attributes)
rect_opacity_count <- sum(grepl('opacity="0.30"', svg_rect))
hex_opacity_count <- sum(grepl('opacity="0.30"', svg_hex))
conc_opacity_count <- sum(grepl('opacity="0.30"', svg_conc))

rect_dash_count <- sum(grepl('stroke-dasharray', svg_rect))
hex_dash_count <- sum(grepl('stroke-dasharray', svg_hex))
conc_dash_count <- sum(grepl('stroke-dasharray', svg_conc))

cat(sprintf("   Rectangular SVG fused edges (opacity=0.30): %d\n", rect_opacity_count))
cat(sprintf("   Hexagonal SVG fused edges (opacity=0.30): %d\n", hex_opacity_count))
cat(sprintf("   Concentric SVG fused edges (opacity=0.30): %d\n", conc_opacity_count))

cat(sprintf("   Rectangular SVG dashed edges: %d\n", rect_dash_count))
cat(sprintf("   Hexagonal SVG dashed edges: %d\n", hex_dash_count))
cat(sprintf("   Concentric SVG dashed edges: %d\n", conc_dash_count))

# Count path elements in each SVG
rect_paths <- sum(grepl("<path", svg_rect))
hex_paths <- sum(grepl("<path", svg_hex))
conc_paths <- sum(grepl("<path", svg_conc))

cat(sprintf("\n   Rectangular SVG path count: %d\n", rect_paths))
cat(sprintf("   Hexagonal SVG path count: %d\n", hex_paths))
cat(sprintf("   Concentric SVG path count: %d\n", conc_paths))

cat("\n==========================================\n")
cat("Visual test complete!\n")
cat("Please inspect the generated SVG files in the output/ directory:\n")
cat("- output/puzzle_rectangular_*.svg\n")
cat("- output/puzzle_hexagonal_*.svg\n")
cat("- output/puzzle_concentric_*.svg\n")
