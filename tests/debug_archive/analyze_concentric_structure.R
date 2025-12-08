# Detailed analysis of concentric piece structure

library(devtools)
load_all()

result <- generate_puzzle(
  type = "concentric",
  seed = 42,
  grid = c(2),
  size = c(200),
  offset = 10,
  save_files = FALSE
)

piece <- result$pieces[[2]]

cat("=== ACTUAL PATH STRUCTURE ===\n")
cat("Looking at the path:\n")
cat(piece$path, "\n\n")

cat("Breaking down by LOGICAL edges (based on how piece was built):\n")
cat("Trapezoid has 4 edges: INNER (v1->v2), RIGHT (v2->v3), OUTER (v3->v4), LEFT (v4->v1)\n\n")

cat("From build_concentric_piece_path (lines 608-720 in concentric_edge_generation.R):\n")
cat("1. M to v1 (start)\n")
cat("2. INNER edge: v1 -> v2 (may be multiple segments if connecting to multiple inner pieces)\n")
cat("3. RIGHT edge: v2 -> v3 (single edge, may have bezier tab)\n")
cat("4. OUTER edge: v3 -> v4 (may be multiple segments OR border)\n")
cat("5. LEFT edge: v4 -> v1 (single edge, may have bezier tab)\n")
cat("6. Z (close)\n\n")

cat("Analyzing the actual segment sequence:\n")
source("R/bezier_utils.R")
segments <- parse_svg_path(piece$path)

cat("Seg 1: M 62.99 7.50   <- START at v1 (inner-start)\n")
cat("Seg 2-4: C C C        <- INNER edge (3 bezier segments!)\n")
cat("Seg 5-7: C C C        <- RIGHT edge (3 bezier segments!)\n")
cat("Seg 8: L              <- OUTER edge segment 1 (straight line)\n")
cat("Seg 9-11: C C C       <- OUTER edge segment 2 (3 bezier segments!)\n")
cat("Seg 12: (missing!)    <- LEFT edge (should be here!)\n")
cat("Seg 12: Z             <- CLOSE\n\n")

cat("=== THE PROBLEM ===\n")
cat("The algorithm uses ceiling(10/4) = 3 segments per edge\n")
cat("This distributes as: [1-3], [4-6], [7-9], [10-10]\n")
cat("But the ACTUAL structure is:\n")
cat("  INNER:  segments 1-3  (3 C segments) ✓ CORRECT\n")
cat("  RIGHT:  segments 4-6  (3 C segments) ✓ CORRECT\n")
cat("  OUTER:  segments 7-?  (1 L + 3 C = 4 segments!) ✗ WRONG\n")
cat("  LEFT:   segments 11-11 (1 C segment) ✗ WRONG\n\n")

cat("The issue: OUTER edge has 4 segments (L + 3C), not 3!\n")
cat("This is because OUTER may contain:\n")
cat("  - Border edges (straight L segments)\n")
cat("  - Connection to outer ring (bezier tabs with 3 C segments)\n\n")

cat("=== SOLUTION ===\n")
cat("The ceiling(n_segments / n_edges) approach is INCORRECT for concentric pieces.\n")
cat("Concentric pieces don't have uniform segment distribution across edges.\n\n")
cat("We need to use the ACTUAL edge structure from piece generation:\n")
cat("  - Read piece_edge_list from generation (stores edge types)\n")
cat("  - Match segments to edges based on expected structure\n")
cat("  - Account for variable-length edges (INNER and OUTER can be multi-segment)\n\n")

cat("Compare to hexagonal: hexagonal pieces have 6 edges, each with exactly 1 bezier (3 C segments)\n")
cat("So 6 edges × 3 segments = 18 segments total, evenly distributed.\n")
cat("But concentric is NOT evenly distributed!\n")

