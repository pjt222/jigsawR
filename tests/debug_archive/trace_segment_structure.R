# Trace the actual segment structure more carefully

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
source("R/bezier_utils.R")
segments <- parse_svg_path(piece$path)

cat("=== DETAILED SEGMENT TRACE ===\n\n")

cat("Each bezier tab is 3 consecutive C segments.\n")
cat("Counting bezier tabs and straight segments:\n\n")

i <- 1
seg_idx <- 0
while (i <= length(segments)) {
  seg <- segments[[i]]
  
  if (seg$type == "M") {
    cat(sprintf("Segment %d: M (start at %.2f, %.2f)\n", i, seg$x, seg$y))
    i <- i + 1
  } else if (seg$type == "Z") {
    cat(sprintf("Segment %d: Z (close path)\n", i))
    i <- i + 1
  } else if (seg$type == "L") {
    seg_idx <- seg_idx + 1
    cat(sprintf("Segment %d: L (straight line to %.2f, %.2f) - content seg %d\n", 
                i, seg$x, seg$y, seg_idx))
    i <- i + 1
  } else if (seg$type == "C") {
    # Check if this is part of a 3-segment bezier tab
    if (i + 2 <= length(segments) && 
        segments[[i+1]]$type == "C" && 
        segments[[i+2]]$type == "C") {
      seg_idx <- seg_idx + 1
      cat(sprintf("Segments %d-%d: BEZIER TAB (3 C segments) - content seg %d\n", 
                  i, i+2, seg_idx))
      i <- i + 3
    } else {
      seg_idx <- seg_idx + 1
      cat(sprintf("Segment %d: C (single bezier) - content seg %d\n", i, seg_idx))
      i <- i + 1
    }
  } else {
    seg_idx <- seg_idx + 1
    cat(sprintf("Segment %d: %s - content seg %d\n", i, seg$type, seg_idx))
    i <- i + 1
  }
}

cat("\n=== INTERPRETATION ===\n")
cat("If we count BEZIER TABS as single units (not 3 separate segments):\n")
cat("  Content units = (number of edges with tabs) + (number of straight segments)\n\n")

cat("For a trapezoid in ring 1:\n")
cat("  INNER: 1 bezier tab (connects to center hexagon)\n")
cat("  RIGHT: 1 bezier tab (connects to next piece in ring)\n")
cat("  OUTER: may have border (straight L) + connections to outer ring (bezier tabs)\n")
cat("  LEFT: 1 bezier tab (connects to previous piece in ring)\n\n")

cat("This piece has:\n")
cat("  - 1 bezier tab (INNER)\n")
cat("  - 1 bezier tab (RIGHT)\n")
cat("  - 1 straight segment + 1 bezier tab (OUTER border + outer connection)\n")
cat("  - 1 bezier tab (LEFT)\n")
cat("Total: 5 content units (4 bezier tabs + 1 straight)\n\n")

cat("But wait - that's only 4 EDGES, not 5 units!\n")
cat("The problem: OUTER edge has TWO parts (border L + bezier tab)\n")
cat("These get split incorrectly when using segment-based distribution.\n")

