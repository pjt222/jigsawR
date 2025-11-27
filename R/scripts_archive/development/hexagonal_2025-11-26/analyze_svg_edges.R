#!/usr/bin/env Rscript
# Analyze if edges in generated SVG are actually scrambled or not

source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")

cat("Detailed Edge Analysis\n")
cat("======================\n\n")

rings <- 2
seed <- 42

# Generate edge map
cat("Step 1: Generating edge map...\n")
edge_data <- generate_hex_edge_map(
  rings = rings,
  seed = seed,
  diameter = 240,
  tabsize = 27,
  jitter = 5
)

cat(sprintf("  Generated %d unique edges\n", edge_data$num_edges))
cat(sprintf("  Mapped %d piece-side combinations\n\n", length(edge_data$piece_edge_map)))

# Check specific edge pairs
cat("Step 2: Checking edge complementarity for adjacent pieces...\n\n")

# Test pair: Piece 1 (side 0) and Piece 2 (side 3)
cat("Test 1: Piece 1 (center) side 0 ←→ Piece 2 side 3\n")
edge_1_0 <- edge_data$piece_edge_map[["1-0"]]
edge_2_3 <- edge_data$piece_edge_map[["2-3"]]

cat("  Piece 1 side 0:\n")
cat("    Type:", edge_1_0$type, "\n")
cat("    Edge key:", edge_1_0$edge_key, "\n")
cat("    Is forward:", edge_1_0$is_forward, "\n")
cat("    Forward path (first 60 chars):", substring(edge_1_0$forward, 1, 60), "...\n")
cat("    Reverse path (first 60 chars):", substring(edge_1_0$reverse, 1, 60), "...\n")

cat("  Piece 2 side 3:\n")
cat("    Type:", edge_2_3$type, "\n")
cat("    Edge key:", edge_2_3$edge_key, "\n")
cat("    Is forward:", edge_2_3$is_forward, "\n")
cat("    Forward path (first 60 chars):", substring(edge_2_3$forward, 1, 60), "...\n")
cat("    Reverse path (first 60 chars):", substring(edge_2_3$reverse, 1, 60), "...\n")

cat("\n  Checking complementarity:\n")
if (edge_1_0$edge_key == edge_2_3$edge_key) {
  cat("    ✓ SAME EDGE KEY:", edge_1_0$edge_key, "\n")
} else {
  cat("    ✗ DIFFERENT EDGE KEYS\n")
}

if (edge_1_0$forward == edge_2_3$reverse) {
  cat("    ✓ COMPLEMENTARY: Piece 1 forward = Piece 2 reverse\n")
} else {
  cat("    ✗ NOT COMPLEMENTARY\n")
  cat("    Piece 1 forward != Piece 2 reverse\n")
}

cat("\n")

# Test another pair
cat("Test 2: Piece 2 side 2 ←→ Piece 7 side 4\n")
edge_2_2 <- edge_data$piece_edge_map[["2-2"]]
edge_7_4 <- edge_data$piece_edge_map[["7-4"]]

cat("  Edge key match:", edge_2_2$edge_key == edge_7_4$edge_key, "\n")
cat("  Complementary:", edge_2_2$forward == edge_7_4$reverse, "\n")

cat("\n")
cat("Step 3: Summary\n")
cat("===============\n")
cat("Edge mapping is working correctly if:\n")
cat("  1. Adjacent pieces reference same edge key ✓/✗\n")
cat("  2. One piece's forward = other piece's reverse ✓/✗\n")
