# Analyze hexagonal path structure
source('R/hexagonal_puzzle.R')
source('R/bezier_utils.R')

cat("ANALYZING HEXAGONAL PATH STRUCTURE\n")
cat("===================================\n\n")

# Generate 2-ring puzzle
init_hex_jigsaw(seed = 42, rings = 2, diameter = 200)
h_path <- hex_gen_dh()
v_path <- hex_gen_dv()
b_path <- hex_gen_db()

cat("BORDER PATH (should be closed hexagon):\n")
cat(substr(b_path, 1, 200), "...\n\n")

cat("HORIZONTAL PATH:\n")
cat(substr(h_path, 1, 300), "...\n\n")

cat("VERTICAL PATH:\n")
cat(substr(v_path, 1, 300), "...\n\n")

# Parse border to understand structure
cat("PARSING BORDER PATH:\n")
b_parsed <- parse_svg_path(b_path)
cat(sprintf("Border has %d commands\n", length(b_parsed)))
if (length(b_parsed) > 0) {
  cat("First few commands:\n")
  for (i in 1:min(10, length(b_parsed))) {
    cmd <- b_parsed[[i]]
    cat(sprintf("  %d: %s (%.1f, %.1f)\n", i, cmd$type, cmd$x, cmd$y))
  }
}
cat("\n")

# The border is probably a single closed path - count M commands
m_count_border <- length(gregexpr("M", b_path)[[1]])
m_count_horiz <- length(gregexpr("M", h_path)[[1]])
m_count_vert <- length(gregexpr("M", v_path)[[1]])

cat(sprintf("M command counts:\n"))
cat(sprintf("  Border: %d\n", m_count_border))
cat(sprintf("  Horizontal: %d\n", m_count_horiz))
cat(sprintf("  Vertical: %d\n", m_count_vert))
cat("\n")

# The key insight: horizontal and vertical paths are INDIVIDUAL EDGE SEGMENTS
# They don't connect to each other directly - they form a mesh that creates cells
# The cells are the puzzle pieces!
