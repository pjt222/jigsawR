#!/usr/bin/env Rscript
# Test to investigate hexagonal piece separation parameter flow
# User concern: Does changing separation actually move piece centers?

source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")

cat("=== HEXAGONAL PIECE SEPARATION INVESTIGATION ===\n\n")

# Test parameters
rings <- 2
seed <- 42
diameter <- 240

# Calculate piece_radius (this is what the code uses)
piece_radius <- diameter / (rings * 4)
cat(sprintf("Piece radius: %.2f mm\n", piece_radius))
cat(sprintf("Diameter: %d mm\n", diameter))
cat(sprintf("Rings: %d\n", rings))
cat("\n")

# Test 1: Check how separation_factor is calculated from offset
cat("=== TEST 1: Separation Factor Calculation ===\n")
cat("Formula: separation_factor = 1.0 + (offset / base_spacing)\n")
cat("Where: base_spacing = piece_radius * 2\n\n")

base_spacing <- piece_radius * 2
cat(sprintf("base_spacing = %.2f mm\n", base_spacing))

for (offset in c(0, 5, 10, 20, 50)) {
  sep_factor <- 1.0 + (offset / base_spacing)
  cat(sprintf("  offset=%3dmm -> separation_factor=%.3f\n", offset, sep_factor))
}

cat("\n=== TEST 2: Piece Center Positions at Different Separations ===\n")

# Test piece positions for center and a ring 1 piece
test_pieces <- c(1, 2, 4)  # center, first ring piece, another ring piece

for (offset in c(0, 10, 50)) {
  sep_factor <- 1.0 + (offset / base_spacing)
  cat(sprintf("\nOffset = %d mm (separation_factor = %.3f):\n", offset, sep_factor))

  for (piece_id in test_pieces) {
    pos <- calculate_hex_piece_position(
      piece_id = piece_id,
      rings = rings,
      piece_radius = piece_radius,
      separation_factor = sep_factor
    )
    cat(sprintf("  Piece %d: center=(%.2f, %.2f) ring=%d\n",
                piece_id, pos$x, pos$y, pos$ring))
  }
}

cat("\n=== TEST 3: Full Piece Generation with Edge Map ===\n")
cat("Testing whether generated pieces actually move...\n\n")

# Generate pieces at different separation levels
for (offset in c(0, 10, 50)) {
  sep_factor <- 1.0 + (offset / base_spacing)
  cat(sprintf("Offset = %d mm:\n", offset))

  pieces <- generate_hex_pieces_with_edge_map(
    rings = rings,
    seed = seed,
    diameter = diameter,
    tabsize = 27,
    jitter = 5,
    separated = TRUE,
    base_spacing = base_spacing,
    separation_factor = sep_factor
  )

  # Show center positions
  for (i in 1:min(3, length(pieces))) {
    p <- pieces[[i]]
    cat(sprintf("  Piece %d: center=(%.2f, %.2f)\n", p$id, p$center_x, p$center_y))
  }
  cat("\n")
}

cat("\n=== TEST 4: Check Actual Path Coordinates ===\n")
cat("Comparing first M command coordinates for piece 2 at different offsets...\n\n")

for (offset in c(0, 10, 50)) {
  sep_factor <- 1.0 + (offset / base_spacing)

  pieces <- generate_hex_pieces_with_edge_map(
    rings = rings,
    seed = seed,
    diameter = diameter,
    tabsize = 27,
    jitter = 5,
    separated = TRUE,
    base_spacing = base_spacing,
    separation_factor = sep_factor
  )

  # Extract first M coordinates from piece 2's path
  path <- pieces[[2]]$path
  m_match <- regmatches(path, regexpr("^M [-0-9.]+ [-0-9.]+", path))

  cat(sprintf("Offset = %2d mm: %s (center=%.2f,%.2f)\n",
              offset, m_match, pieces[[2]]$center_x, pieces[[2]]$center_y))
}

cat("\n=== TEST 5: Critical Check - Is offset being passed correctly? ===\n")
cat("Tracing the Shiny app flow:\n")
cat("1. User sets input$offset (e.g., 10mm)\n")
cat("2. generate_separated_hexagonal_svg() receives offset parameter\n")
cat("3. It calculates: separation_factor = 1.0 + (offset / (piece_radius * 2))\n")
cat("4. Calls generate_hex_pieces_with_edge_map(separation_factor=...)\n")
cat("5. That function calls calculate_hex_piece_position(separation_factor=...)\n")
cat("6. Position is scaled: x * separation_factor, y * separation_factor\n\n")

# The key issue: In hexagonal_separation.R line 191-204:
cat("Looking at hexagonal_separation.R:\n")
cat("- For bezier mode, separated = TRUE\n")
cat("- base_spacing = piece_radius * 2\n")
cat("- separation_factor = 1.0 + (offset / base_spacing)\n")
cat("\nThis should work correctly IF:\n")
cat("- offset > 0 produces separation_factor > 1.0\n")
cat("- offset = 0 produces separation_factor = 1.0 (tight fit)\n")

cat("\n=== CONCLUSION ===\n")
sep_0 <- 1.0 + (0 / base_spacing)
sep_10 <- 1.0 + (10 / base_spacing)
cat(sprintf("At offset=0mm:  separation_factor = %.4f\n", sep_0))
cat(sprintf("At offset=10mm: separation_factor = %.4f\n", sep_10))

if (abs(sep_0 - 1.0) < 0.0001) {
  cat("\nSeparation factor of 1.0 at offset=0 SHOULD give tight-fitting pieces.\n")
} else {
  cat("\nWARNING: separation_factor is not 1.0 at offset=0!\n")
}
