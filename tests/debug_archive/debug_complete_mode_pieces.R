# Debug: Analyze complete mode to understand expected piece sizes

cat("=" , rep("=", 70), "\n", sep = "")
cat("Debug: Complete Mode Piece Analysis\n")
cat("=" , rep("=", 70), "\n\n", sep = "")

source("R/hexagonal_puzzle.R")

rings <- 3
diameter <- 240
seed <- 1234

cat(sprintf("Parameters: rings=%d, diameter=%d, seed=%d\n\n", rings, diameter, seed))

# Generate complete mode puzzle with warp+trunc
result <- generate_hex_jigsaw_svg(
  seed = seed,
  rings = rings,
  diameter = diameter,
  tabsize = 20,
  jitter = 4,
  do_warp = TRUE,
  do_trunc = TRUE
)

# The complete mode generates the puzzle as a single SVG
# Let's look at key values from the internal state
cat("Complete mode internal values:\n")
cat(sprintf("  Diameter: %d\n", diameter))
cat(sprintf("  Rings: %d\n", rings))

# Parse some coordinates from the SVG to understand piece sizes
# The SVG is centered at (diameter/2 + margin, diameter/2 + margin)
# where margin = diameter * 0.1

svg_content <- result$svg

# Extract all M (moveto) commands which mark piece starts
m_matches <- gregexpr("M\\s+([0-9.]+)\\s+([0-9.]+)", svg_content)
m_coords <- regmatches(svg_content, m_matches)[[1]]

cat(sprintf("\nFound %d M commands (piece starting points)\n", length(m_coords)))

# Parse the first few
cat("\nFirst few M coordinates (piece starts):\n")
for (i in 1:min(5, length(m_coords))) {
  parts <- unlist(strsplit(m_coords[i], "\\s+"))
  x <- as.numeric(parts[2])
  y <- as.numeric(parts[3])

  # Convert to center-origin coordinates
  # Complete mode uses center at (diameter/2 + margin, diameter/2 + margin)
  margin <- diameter * 0.1
  center_offset <- diameter / 2 + margin
  cx <- x - center_offset
  cy <- y - center_offset

  dist <- sqrt(cx^2 + cy^2)
  cat(sprintf("  M %s: svg=(%.2f, %.2f), centered=(%.2f, %.2f), dist=%.2f\n",
              i, x, y, cx, cy, dist))
}

# Let's also extract arc commands from border edges
arc_matches <- gregexpr("A\\s+([0-9.]+)\\s+([0-9.]+)\\s+[0-9]+\\s+[0-9]+\\s+[0-9]+\\s+([0-9.]+)\\s+([0-9.]+)", svg_content)
arc_coords <- regmatches(svg_content, arc_matches)[[1]]

cat(sprintf("\nFound %d A (arc) commands\n", length(arc_coords)))

if (length(arc_coords) > 0) {
  cat("\nFirst few arc commands:\n")
  for (i in 1:min(5, length(arc_coords))) {
    parts <- unlist(strsplit(arc_coords[i], "\\s+"))
    rx <- as.numeric(parts[2])
    ry <- as.numeric(parts[3])
    cat(sprintf("  Arc %d: rx=%.2f, ry=%.2f\n", i, rx, ry))
  }
}

cat("\n")
cat("=" , rep("=", 70), "\n", sep = "")
cat("Key insight: What arc radius does complete mode use?\n")
cat("=" , rep("=", 70), "\n")
