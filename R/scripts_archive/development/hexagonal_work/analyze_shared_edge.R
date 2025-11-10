# Analyze the shared edge between [0,1] and [1,1]

cat("Analyzing the shared vertical edge between pieces [0,1] and [1,1]...\n\n")

# Piece [0,1] right edge (from 100,100 to 100,200):
cat("Piece [0,1] RIGHT edge:\n")
cat("Starts at: (100,100)\n")
cat("C 99.43 180 107.98 145.59 87.98 157.83\n")
cat("  Control points: (99.43,180) and (107.98,145.59)\n")
cat("  Ends at: (87.98,157.83)\n")
cat("C 67.98 170.08 67.98 130.08 87.98 137.83\n")
cat("  Control points: (67.98,170.08) and (67.98,130.08)\n")
cat("  Ends at: (87.98,137.83)\n")
cat("C 107.98 145.59 103.58 120 100 200\n")
cat("  Control points: (107.98,145.59) and (103.58,120)\n")
cat("  Ends at: (100,200)\n\n")

# Piece [1,1] left edge (from 100,200 to 100,100):
cat("Piece [1,1] LEFT edge:\n")
cat("Starts at: (100,200)\n")
cat("C 103.58 120 107.98 145.59 87.98 137.83\n")
cat("  Control points: (103.58,120) and (107.98,145.59)\n")
cat("  Ends at: (87.98,137.83)\n")
cat("C 67.98 130.08 67.98 170.08 87.98 157.83\n")
cat("  Control points: (67.98,130.08) and (67.98,170.08)\n")
cat("  Ends at: (87.98,157.83)\n")
cat("C 107.98 145.59 99.43 180 100 100\n")
cat("  Control points: (107.98,145.59) and (99.43,180)\n")
cat("  Ends at: (100,100)\n\n")

cat("ISSUE FOUND!\n")
cat("The curves are NOT properly reversed!\n\n")

cat("For [0,1] right edge:\n")
cat("- Goes from (100,100) to (100,200)\n")
cat("- The tab protrudes to the LEFT (x < 100)\n\n")

cat("For [1,1] left edge:\n")
cat("- Goes from (100,200) to (100,100)\n")
cat("- The tab ALSO protrudes to the LEFT (x < 100)!\n\n")

cat("This is wrong! The tabs should be complementary:\n")
cat("- If [0,1] has a tab protruding LEFT, then [1,1] should have an indent (tab protruding RIGHT)\n")
cat("- The x-coordinates should be mirrored around x=100\n\n")

cat("The issue: When reversing the curve, I only reversed the order and control points,\n")
cat("but didn't mirror the x-coordinates around the vertical line x=100!\n")