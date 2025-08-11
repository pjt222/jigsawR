# Example usage of rectangular puzzle generator
# (moved from R/rectangular_puzzle.R to avoid auto-execution when sourcing)

library(jigsawR)

cat("Jigsaw Puzzle Generator (R Translation)\n")
cat("Original JavaScript by Draradech\n") 
cat("GitHub: https://github.com/Draradech/jigsaw\n\n")

# Generate with default parameters
puzzle <- generate_jigsaw_svg()
print_puzzle_info(puzzle)

# Generate with custom parameters (matching JS defaults)
puzzle_custom <- generate_jigsaw_svg(
  seed = 1234,
  tabsize = 25,  # 25%
  jitter = 6,    # 6%
  width = 300,   # 300mm
  height = 200,  # 200mm
  radius = 2.0,  # 2.0mm
  xn = 15,       # 15 columns
  yn = 10        # 10 rows
)

# Save to file
save_jigsaw_svg(puzzle_custom, "custom_jigsaw.svg")

# Display first few characters of each path type
cat("\nGenerated SVG paths:\n")
cat("Horizontal (first 100 chars):", substr(puzzle_custom$horizontal, 1, 100), "...\n")
cat("Vertical (first 100 chars):", substr(puzzle_custom$vertical, 1, 100), "...\n")
cat("Border (first 100 chars):", substr(puzzle_custom$border, 1, 100), "...\n")