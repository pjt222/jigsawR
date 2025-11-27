#!/usr/bin/env Rscript
# Proper hexagonal lattice positioning

cat("Hexagonal Lattice vs Polar Coordinates\n")
cat("=======================================\n\n")

cat("WRONG (current): All pieces at same ring use polar coordinates\n")
cat("  Ring 2, position 0: r=2, θ=0° → (x, y)\n")
cat("  Ring 2, position 1: r=2, θ=30° → (x', y')\n")
cat("  Problem: These don't share vertices with ring 1 pieces!\n\n")

cat("CORRECT: Hexagonal lattice coordinates\n")
cat("  Each piece position defined by (q, r) in axial coordinates\n")
cat("  Conversion: x = size * (3/2 * q)\n")
cat("              y = size * (sqrt(3)/2 * q + sqrt(3) * r)\n\n")

cat("For ring 2:\n")
cat("  Even positions: Direct radial extension of ring 1 pieces\n")
cat("  Odd positions: Fill gaps between ring 1 pieces\n")
cat("  All pieces properly tile with shared vertices\n\n")

cat("Solution: Replace polar positioning with hexagonal lattice!\n")
