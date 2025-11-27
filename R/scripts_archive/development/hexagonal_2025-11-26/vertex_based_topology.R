#!/usr/bin/env Rscript
# Vertex-based hexagonal topology (like aromatic ring systems)

cat("Hexagonal Vertex Topology Analysis\n")
cat("===================================\n\n")

cat("Key insight: Each hexagon has 6 vertices\n")
cat("Adjacent hexagons SHARE vertices\n\n")

cat("Ring 0 (center piece 1):\n")
cat("  Vertices: V1, V2, V3, V4, V5, V6\n\n")

cat("Ring 1 (pieces 2-7):\n")
cat("  Each ring 1 piece shares 2 vertices with center\n")
cat("  Piece 2 shares V1-V2 with center\n")
cat("  Piece 3 shares V2-V3 with center\n")
cat("  Piece 4 shares V3-V4 with center\n")
cat("  etc.\n\n")

cat("Ring 2 (pieces 8-19):\n")
cat("  Each ring 2 piece shares vertices with ring 1 pieces\n")
cat("  Piece 8 shares edge with piece 2 (ring 1)\n")
cat("  Piece 9 shares edge with piece 2 (ring 1)\n")
cat("  Piece 10 shares edge with pieces 2 AND 3 (ring 1)\n\n")

cat("The key: Adjacent pieces share an EDGE (2 vertices)\n")
cat("Not just a side number, but actual geometric vertices!\n\n")

cat("For proper neighbor detection:\n")
cat("1. Calculate actual vertex positions for each piece\n")
cat("2. Find which pieces share 2 consecutive vertices\n")
cat("3. Those pieces are neighbors on that shared edge\n")
