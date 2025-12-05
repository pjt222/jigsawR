# Test concentric edge generation

source("R/logging.R")
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  if (!grepl("scripts_archive", f)) source(f)
}

cat("=== Testing Concentric Edge Generation ===\n\n")

rings <- 4
diameter <- 200
seed <- 1234

# Test with hexagon center
cat("--- Hexagon center ---\n")
pieces_hex <- generate_concentric_pieces(
  rings = rings,
  seed = seed,
  diameter = diameter,
  tabsize = 27,
  jitter = 5,
  center_shape = "hexagon"
)

cat(sprintf("Generated %d pieces\n\n", length(pieces_hex)))

# Show sample paths
cat("Sample paths:\n")
cat(sprintf("Piece 1 (center): %s...\n", substr(pieces_hex[[1]]$path, 1, 100)))
cat(sprintf("Piece 2 (ring 1): %s...\n", substr(pieces_hex[[2]]$path, 1, 100)))
cat(sprintf("Piece 8 (ring 2): %s...\n", substr(pieces_hex[[8]]$path, 1, 100)))

# Generate SVG
svg_paths <- sapply(pieces_hex, function(p) {
  sprintf('<path d="%s" fill="none" stroke="black" stroke-width="0.5"/>', p$path)
})

svg_content <- sprintf(
  '<svg xmlns="http://www.w3.org/2000/svg" viewBox="-110 -110 220 220" width="600" height="600">
  <rect x="-110" y="-110" width="220" height="220" fill="white"/>
  %s
</svg>',
  paste(svg_paths, collapse = "\n  ")
)

writeLines(svg_content, "output/test_concentric_hexcenter.svg")
cat("\nSVG saved to output/test_concentric_hexcenter.svg\n")

# Test with circle center
cat("\n--- Circle center ---\n")
pieces_circle <- generate_concentric_pieces(
  rings = rings,
  seed = seed,
  diameter = diameter,
  tabsize = 27,
  jitter = 5,
  center_shape = "circle"
)

cat(sprintf("Generated %d pieces\n", length(pieces_circle)))
cat(sprintf("Piece 1 (center circle): %s\n", pieces_circle[[1]]$path))

# Generate SVG
svg_paths <- sapply(pieces_circle, function(p) {
  sprintf('<path d="%s" fill="none" stroke="black" stroke-width="0.5"/>', p$path)
})

svg_content <- sprintf(
  '<svg xmlns="http://www.w3.org/2000/svg" viewBox="-110 -110 220 220" width="600" height="600">
  <rect x="-110" y="-110" width="220" height="220" fill="white"/>
  %s
</svg>',
  paste(svg_paths, collapse = "\n  ")
)

writeLines(svg_content, "output/test_concentric_circlecenter.svg")
cat("SVG saved to output/test_concentric_circlecenter.svg\n")
