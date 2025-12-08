# Visual demonstration of the edge splitting bug

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
source("R/unified_renderer.R")

cat("=" , rep("=", 70), "=\n", sep="")
cat("VISUAL DEMONSTRATION: Concentric Edge Splitting Bug\n")
cat("=", rep("=", 70), "=\n\n", sep="")

segments <- parse_svg_path(piece$path)
content_segs <- segments[sapply(segments, function(s) !s$type %in% c("M", "Z"))]

cat("Original path has", length(content_segs), "content segments\n\n")

cat("CURRENT ALGORITHM (WRONG):\n")
cat("-", rep("-", 70), "-\n", sep="")
cat(sprintf("segs_per_edge = ceiling(%d / 4) = %d\n\n", length(content_segs), ceiling(length(content_segs)/4)))

edge_paths <- split_concentric_path_into_edges(piece$path, piece)

for (edge_name in c("INNER", "RIGHT", "OUTER", "LEFT")) {
  edge_path <- edge_paths[[edge_name]]
  if (edge_path == "") {
    cat(sprintf("%-6s: EMPTY\n", edge_name))
  } else {
    edge_segs <- parse_svg_path(edge_path)
    edge_content <- edge_segs[sapply(edge_segs, function(s) !s$type %in% c("M", "Z"))]
    
    # Count C and L segments
    n_c <- sum(sapply(edge_content, function(s) s$type == "C"))
    n_l <- sum(sapply(edge_content, function(s) s$type == "L"))
    
    status <- ""
    if (edge_name == "INNER" && n_c == 3) status <- "✓ CORRECT"
    else if (edge_name == "RIGHT" && n_c == 3) status <- "✓ CORRECT"
    else if (edge_name == "OUTER" && n_c < 3) status <- "✗ INCOMPLETE (missing C segments)"
    else if (edge_name == "LEFT" && n_c < 3) status <- "✗ SEVERELY INCOMPLETE"
    
    cat(sprintf("%-6s: %d segs (%dC %dL) %s\n", edge_name, length(edge_content), n_c, n_l, status))
  }
}

cat("\n")
cat("EXPECTED STRUCTURE:\n")
cat("-", rep("-", 70), "-\n", sep="")
cat("INNER:  3C (1 bezier tab)\n")
cat("RIGHT:  3C (1 bezier tab)\n")
cat("OUTER:  1L + 3C (border + 1 bezier tab) = 4 segments\n")
cat("LEFT:   3C (1 bezier tab)\n")
cat("TOTAL:  9C + 1L = 10 segments\n\n")

cat("THE BUG:\n")
cat("-", rep("-", 70), "-\n", sep="")
cat("• OUTER should have 4 segments (1L + 3C), but gets only 3 (1L + 2C)\n")
cat("• LEFT should have 3 segments (3C), but gets only 1 (1C)\n")
cat("• The 'missing' segments: OUTER is missing 1C, LEFT is missing 2C\n")
cat("• This makes OUTER incomplete and LEFT barely visible\n\n")

cat("VISUAL IMPACT:\n")
cat("-", rep("-", 70), "-\n", sep="")
cat("When rendered with edge fusion:\n")
cat("  [INNER] ============ COMPLETE (visible)\n")
cat("  [RIGHT] ============ COMPLETE (visible)\n")
cat("  [OUTER] =========    INCOMPLETE (distorted)\n")
cat("  [LEFT]  ==           SEVERELY INCOMPLETE (barely visible)\n\n")

cat("Result: Only ~2.5 of 4 edges render properly → 'half edges'\n")

