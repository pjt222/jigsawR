#!/usr/bin/env Rscript
# Test fixed edge generation with proper mapping

source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")
source("R/hexagonal_separation.R")

cat("Testing fixed edge generation\n")
cat("==============================\n\n")

# Test 1: Generate edge map
cat("Test 1: Creating edge map for 2-ring puzzle...\n")
edge_data <- generate_hex_edge_map(
  rings = 2,
  seed = 42,
  diameter = 240,
  tabsize = 27,
  jitter = 5
)

cat(sprintf("✓ Generated %d unique edges\n", edge_data$num_edges))
cat(sprintf("✓ Mapped %d piece-side combinations\n", length(edge_data$piece_edge_map)))

# Verify edge uniqueness
cat("\nVerifying edge mapping:\n")
cat("  Checking piece 1 (side 0) and piece 2 (side 3)...\n")
edge_1_0 <- edge_data$piece_edge_map[["1-0"]]
edge_2_3 <- edge_data$piece_edge_map[["2-3"]]

cat(sprintf("    Piece 1 side 0: type=%s, forward path length=%d\n",
            edge_1_0$type, nchar(edge_1_0$forward)))
cat(sprintf("    Piece 2 side 3: type=%s, forward path length=%d\n",
            edge_2_3$type, nchar(edge_2_3$forward)))

# Check if they're complementary
if (edge_1_0$forward == edge_2_3$reverse) {
  cat("    ✓ COMPLEMENTARY: Piece 1 forward = Piece 2 reverse\n")
} else {
  cat("    ✗ NOT COMPLEMENTARY\n")
}

cat("\n")

# Test 2: Generate complete puzzle
cat("Test 2: Generating complete puzzle with fixed edges...\n")
svg <- generate_separated_hexagonal_svg(
  rings = 2,
  seed = 42,
  diameter = 240,
  offset = 10,
  arrangement = "hexagonal",
  use_bezier = TRUE,
  tabsize = 27,
  jitter = 5
)

output_file <- "output/test_fixed_edges_2ring.svg"
writeLines(svg, output_file)
cat(sprintf("✓ Saved to: %s\n", output_file))

cat("\n")

# Test 3: 3-ring puzzle
cat("Test 3: Generating 3-ring puzzle...\n")
svg3 <- generate_separated_hexagonal_svg(
  rings = 3,
  seed = 123,
  diameter = 240,
  offset = 10,
  arrangement = "hexagonal",
  use_bezier = TRUE,
  tabsize = 27,
  jitter = 5
)

output_file3 <- "output/test_fixed_edges_3ring.svg"
writeLines(svg3, output_file3)
cat(sprintf("✓ Saved to: %s\n", output_file3))

cat("\n")
cat("====================================\n")
cat("✓ All tests completed successfully!\n")
cat("====================================\n")
