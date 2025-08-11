# Puzzle Piece Separation Examples
# Demonstrates how to generate puzzles with separated pieces for laser cutting

# Load required functions
source("R/rectangular_puzzle.R")
source("R/puzzle_core_clean.R")
source("R/puzzle_separation.R")

cat("=== Puzzle Piece Separation Examples ===\n\n")
cat("This feature maintains pieces in their original grid positions\n")
cat("but adds configurable gaps between them.\n\n")

# Example 1: Basic separation for laser cutting
cat("Example 1: Laser Cutting Layout (2x2)\n")
cat("-" , rep("-", 40), "\n", sep="")

# Generate puzzle
puzzle_2x2 <- generate_puzzle_core(
  seed = 1234,
  grid = c(2, 2),
  size = c(100, 100)
)

# Calculate optimal offset for laser cutting
optimal_offset <- calculate_optimal_offset(
  piece_width = puzzle_2x2$piece_width,
  piece_height = puzzle_2x2$piece_height,
  kerf = 0.2,  # Typical laser kerf
  min_separation = 3  # Minimum 3mm between pieces
)

cat(sprintf("Piece size: %.0f x %.0f mm\n", 
            puzzle_2x2$piece_width, puzzle_2x2$piece_height))
cat(sprintf("Optimal offset for laser cutting: %.2f mm\n", optimal_offset))

# Generate separated puzzle
svg_laser <- generate_separated_puzzle_svg(
  puzzle_structure = puzzle_2x2,
  offset = optimal_offset,
  colors = "black",  # Black lines for laser cutting
  stroke_width = 0.5  # Thin lines for cutting
)

writeLines(svg_laser, "output/example_laser_cutting_2x2.svg")
cat("✓ Saved: output/example_laser_cutting_2x2.svg\n\n")

# Example 2: Progressive separation visualization
cat("Example 2: Progressive Separation (shows different offsets)\n")
cat("-" , rep("-", 40), "\n", sep="")

offsets <- c(0, 5, 10, 20)
colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#FFA07A")

for (offset in offsets) {
  svg <- generate_separated_puzzle_svg(
    puzzle_structure = puzzle_2x2,
    offset = offset,
    colors = colors,
    stroke_width = 1.5
  )
  
  filename <- sprintf("output/example_separation_%dmm.svg", offset)
  writeLines(svg, filename)
  cat(sprintf("  Offset %2d mm: %s\n", offset, filename))
}

cat("\n")

# Example 3: Larger puzzle for production
cat("Example 3: Production Layout (4x3 puzzle)\n")
cat("-" , rep("-", 40), "\n", sep="")

# Generate larger puzzle
puzzle_4x3 <- generate_puzzle_core(
  seed = 9999,
  grid = c(3, 4),  # 3 rows, 4 columns = 12 pieces
  size = c(200, 150),
  tabsize = 22,
  jitter = 3
)

# Production layout with adequate separation
production_offset <- 8  # 8mm separation for easy handling

svg_production <- generate_separated_puzzle_svg(
  puzzle_structure = puzzle_4x3,
  offset = production_offset,
  colors = "darkblue",
  stroke_width = 1.0
)

writeLines(svg_production, "output/example_production_4x3.svg")

# Calculate material usage
original_area <- puzzle_4x3$size[1] * puzzle_4x3$size[2]
separated_width <- puzzle_4x3$size[1] + (4 - 1) * production_offset
separated_height <- puzzle_4x3$size[2] + (3 - 1) * production_offset
separated_area <- separated_width * separated_height

cat(sprintf("Original puzzle size: %.0f x %.0f mm (area: %.0f mm²)\n",
            puzzle_4x3$size[1], puzzle_4x3$size[2], original_area))
cat(sprintf("Separated layout size: %.0f x %.0f mm (area: %.0f mm²)\n",
            separated_width, separated_height, separated_area))
cat(sprintf("Area increase: %.1f%%\n", 
            (separated_area - original_area) / original_area * 100))
cat("✓ Saved: output/example_production_4x3.svg\n\n")

# Example 4: Educational/Assembly guide
cat("Example 4: Assembly Guide with Color Coding\n")
cat("-" , rep("-", 40), "\n", sep="")

# Generate a 3x3 puzzle
puzzle_3x3 <- generate_puzzle_core(
  seed = 42,
  grid = c(3, 3),
  size = c(150, 150)
)

# Create assembly guide with generous spacing and colors
guide_colors <- c(
  "#FF6B6B",  # Red - corners
  "#4ECDC4",  # Teal - edges
  "#45B7D1",  # Blue - center
  "#FF6B6B",  # Red - corners
  "#4ECDC4",  # Teal - edges
  "#FF6B6B",  # Red - corners
  "#4ECDC4",  # Teal - edges
  "#4ECDC4",  # Teal - edges
  "#FF6B6B"   # Red - corners
)

svg_guide <- generate_separated_puzzle_svg(
  puzzle_structure = puzzle_3x3,
  offset = 15,  # Generous spacing for clarity
  colors = guide_colors,
  stroke_width = 2.0  # Thicker lines for visibility
)

writeLines(svg_guide, "output/example_assembly_guide_3x3.svg")
cat("✓ Saved: output/example_assembly_guide_3x3.svg\n")
cat("  Color coding: Red=corners, Teal=edges, Blue=center\n\n")

# Example 5: Using enhanced function with modes
cat("Example 5: Enhanced Function Demonstration\n")
cat("-" , rep("-", 40), "\n", sep="")

modes <- list(
  list(desc = "Traditional (touching pieces)", 
       mode = "individual", offset = 0),
  list(desc = "Small separation (5mm)", 
       mode = "separated", offset = 5),
  list(desc = "Medium separation (10mm)", 
       mode = "separated", offset = 10),
  list(desc = "Large separation (15mm)", 
       mode = "separated", offset = 15)
)

for (i in seq_along(modes)) {
  m <- modes[[i]]
  svg <- generate_puzzle_svg_enhanced(
    puzzle_structure = puzzle_2x2,
    mode = m$mode,
    offset = m$offset,
    colors = c("red", "blue", "green", "orange")
  )
  
  filename <- sprintf("output/example_enhanced_mode_%d.svg", i)
  writeLines(svg, filename)
  cat(sprintf("  %s\n    → %s\n", m$desc, filename))
}

# Summary
cat("\n=== Summary ===\n")
cat("Generated examples demonstrate:\n")
cat("  1. Laser cutting layout with optimal offset calculation\n")
cat("  2. Progressive separation visualization (0, 5, 10, 20mm)\n")
cat("  3. Production layout with material usage calculation\n")
cat("  4. Assembly guide with color coding\n")
cat("  5. Enhanced function with different modes\n")
cat("\nAll files saved to output/ directory\n")
cat("\nUse cases:\n")
cat("  • Laser cutting: Optimal spacing for kerf and handling\n")
cat("  • Education: Clear piece identification and assembly\n")
cat("  • Production: Efficient material usage with adequate separation\n")
cat("  • Documentation: Visual guides and instructions\n")