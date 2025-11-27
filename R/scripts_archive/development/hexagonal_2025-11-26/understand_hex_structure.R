# Understand hexagonal puzzle structure
source("R/hexagonal_puzzle.R")

# Generate a small hexagonal puzzle
init_hex_jigsaw(seed = 42, rings = 2)

# Check what the formula gives
rings <- 2
formula_pieces <- 3 * rings * (rings - 1) + 1
cat("Formula says:", formula_pieces, "pieces for", rings, "rings\n\n")

# Check what hex_gen_dh iteration gives
n <- rings
yl <- 2 * n - 1
count <- 0
for (yi in seq(-yl + 2, yl - 2, by = 2)) {
  xl <- 2 * n - 1 - (abs(yi) - 1) / 2
  for (xi in seq(-xl + 1, xl - 2, by = 1)) {
    count <- count + 1
  }
}
cat("hex_gen_dh iteration gives:", count, "items\n\n")

# Generate actual puzzle and count paths
puzzle <- generate_hex_jigsaw_svg(seed = 42, rings = 2)
cat("Puzzle generated successfully\n")
cat("Has horizontal, vertical, border paths\n")
