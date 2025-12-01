#!/usr/bin/env Rscript
# Test reactive styling workflow for Shiny app

source("R/logging.R")
source("R/config_utils.R")
source("R/rectangular_puzzle.R")
source("R/hexagonal_puzzle.R")
source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")
source("R/unified_piece_generation.R")
source("R/piece_positioning.R")
source("R/unified_renderer.R")
source("R/puzzle_core_clean.R")

cat("\n=== Reactive Styling Test ===\n\n")

# Simulate generate button click
cat("Step 1: Generate pieces (basic settings)\n")
pieces <- generate_pieces_internal(
  type = "rectangular", seed = 42,
  grid = c(2, 2), size = c(200, 200),
  tabsize = 20, jitter = 4
)
cat("  Generated", length(pieces$pieces), "pieces\n")

cat("\nStep 2: Apply positioning\n")
positioned <- apply_piece_positioning(pieces, offset = 10)
cat("  Positioned", length(positioned$pieces), "pieces\n")

# Test re-rendering with different styles (no regeneration needed)
cat("\nStep 3: Re-render with different styling options\n")

cat("  - magma palette, stroke=1.5...")
svg1 <- render_puzzle_svg(positioned, palette = "magma", stroke_width = 1.5)
cat(" OK (", nchar(svg1), " chars)\n", sep = "")

cat("  - viridis palette, stroke=3.0...")
svg2 <- render_puzzle_svg(positioned, palette = "viridis", stroke_width = 3.0)
cat(" OK (", nchar(svg2), " chars)\n", sep = "")

cat("  - plasma palette, opacity=0.5...")
svg3 <- render_puzzle_svg(positioned, palette = "plasma", opacity = 0.5)
cat(" OK (", nchar(svg3), " chars)\n", sep = "")

cat("  - fill=#ff0000, background=#ffffff...")
svg4 <- render_puzzle_svg(positioned, fill = "#ff0000", background = "#ffffff")
cat(" OK (", nchar(svg4), " chars)\n", sep = "")

# Verify outputs differ
cat("\nStep 4: Verify outputs differ\n")
cat("  svg1 != svg2:", svg1 != svg2, "\n")
cat("  svg2 != svg3:", svg2 != svg3, "\n")
cat("  svg3 != svg4:", svg3 != svg4, "\n")

all_different <- (svg1 != svg2) && (svg2 != svg3) && (svg3 != svg4)

if (all_different) {
  cat("\n=== Test PASSED ===\n")
} else {
  cat("\n=== Test FAILED ===\n")
  stop("Styling changes did not produce different outputs")
}
