devtools::load_all()

cat("SVG Path Analysis\n")
cat("=================\n\n")

# Read the generated SVG
svg_lines <- readLines("output/puzzle_conc2_seed42_separated.svg")

# Extract path elements
path_lines <- svg_lines[grepl("<path", svg_lines)]

cat(sprintf("Total path elements: %d\n\n", length(path_lines)))

# Parse each path to understand what's being drawn
cat("=== PATH BREAKDOWN ===\n\n")

# Group by stroke color
colors_used <- unique(gsub('.*stroke="([^"]+)".*', '\\1', path_lines))
colors_used <- colors_used[colors_used != "none"]

cat("Colors found:\n")
for (color in colors_used) {
  count <- sum(grepl(sprintf('stroke="%s"', color), path_lines))
  cat(sprintf("  %s: %d paths\n", color, count))
}

cat("\n=== PATHS BY COLOR ===\n")

for (color in colors_used) {
  matching <- path_lines[grepl(sprintf('stroke="%s"', color), path_lines)]
  cat(sprintf("\nColor %s (%d paths):\n", color, length(matching)))

  for (j in seq_along(matching)) {
    line <- matching[j]
    # Check if dashed
    is_dashed <- grepl("stroke-dasharray", line)
    has_opacity <- grepl('opacity="0', line)

    # Get path type
    path_d <- gsub('.*d="([^"]+)".*', '\\1', line)
    # Count segment types
    m_count <- length(gregexpr("M ", path_d)[[1]])
    l_count <- length(gregexpr(" L ", path_d)[[1]])
    c_count <- length(gregexpr(" C ", path_d)[[1]])

    style <- if (is_dashed && has_opacity) "FUSED" else "normal"

    cat(sprintf("  Path %d: %s, M=%d L=%d C=%d\n",
                j, style, m_count, l_count, c_count))

    # Show first 80 chars of path
    path_preview <- substr(path_d, 1, 80)
    cat(sprintf("    d=\"%s...\"\n", path_preview))
  }
}

cat("\n=== EXPECTED vs ACTUAL ===\n")
cat("
Expected edges per piece (concentric 2-ring):
  Piece 1 (center): 6 edges (1 fused with P2)
    - 5 solid edges (2,3,4,5,6)
    - 1 fused edge (1)
  Piece 2 (ring 1, pos 0): 4 edges (INNER fused with P1)
    - 3 solid edges (RIGHT, OUTER, LEFT)
    - 1 fused edge (INNER)
  Piece 3-4: 4 edges each, all solid
  Piece 5: 4 edges (RIGHT fused with P6)
    - 3 solid (INNER, OUTER, LEFT)
    - 1 fused (RIGHT)
  Piece 6: 4 edges (LEFT fused with P5, RIGHT fused with P7)
    - 2 solid (INNER, OUTER)
    - 2 fused (LEFT, RIGHT)
  Piece 7: 4 edges (LEFT fused with P6)
    - 3 solid (INNER, RIGHT, OUTER)
    - 1 fused (LEFT)

Total solid edges: 5+3+4+4+3+2+3 = 24
Total fused edges: 1+1+1+2+1 = 6
")
