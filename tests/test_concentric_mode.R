# Test script for concentric ring mode
# Run with: Rscript tests/test_concentric_mode.R

cat("=== Testing Concentric Ring Mode ===\n\n")

# Source required files
source("R/logging.R")
source("R/rectangular_puzzle.R")
source("R/hexagonal_puzzle.R")
source("R/bezier_utils.R")
source("R/hexagonal_bezier_generation.R")
source("R/concentric_geometry.R")
source("R/concentric_edge_generation.R")
source("R/unified_piece_generation.R")
source("R/piece_positioning.R")
source("R/config_utils.R")
source("R/unified_renderer.R")
source("R/jigsawR_clean.R")

# Create output directory
if (!dir.exists("output")) dir.create("output")

# Test 1: Basic concentric geometry
cat("Test 1: Basic concentric geometry calculations\n")
cat(strrep("-", 50), "\n")

rings <- 3
diameter <- 240
piece_count <- get_concentric_piece_count(rings)
piece_height <- get_concentric_piece_height(diameter, rings)

cat(sprintf("  Rings: %d\n", rings))
cat(sprintf("  Diameter: %d mm\n", diameter))
cat(sprintf("  Piece count: %d\n", piece_count))
cat(sprintf("  Piece height: %.2f mm\n", piece_height))
cat("\n")

# Test 2: Vertex calculation for center piece (hexagon)
cat("Test 2: Center piece vertices (hexagon)\n")
cat(strrep("-", 50), "\n")

center_verts <- calculate_concentric_vertices(1, rings, diameter, center_shape = "hexagon")
cat(sprintf("  Type: %s\n", center_verts$type))
cat(sprintf("  Vertices: %d (as list)\n", length(center_verts$vertices)))
# Extract x and y from list of vertices
xs <- sapply(center_verts$vertices, function(v) v[1])
ys <- sapply(center_verts$vertices, function(v) v[2])
cat(sprintf("  Vertex range X: [%.2f, %.2f]\n", min(xs), max(xs)))
cat(sprintf("  Vertex range Y: [%.2f, %.2f]\n", min(ys), max(ys)))
cat("\n")

# Test 3: Vertex calculation for center piece (circle)
cat("Test 3: Center piece vertices (circle)\n")
cat(strrep("-", 50), "\n")

center_circle <- calculate_concentric_vertices(1, rings, diameter, center_shape = "circle")
cat(sprintf("  Type: %s\n", center_circle$type))
cat(sprintf("  Center: (%.2f, %.2f)\n", center_circle$center[1], center_circle$center[2]))
cat(sprintf("  Radius: %.2f mm\n", center_circle$radius))
cat("\n")

# Test 4: Vertex calculation for ring 1 trapezoid
cat("Test 4: Ring 1 trapezoid vertices\n")
cat(strrep("-", 50), "\n")

trap_verts <- calculate_concentric_vertices(2, rings, diameter, center_shape = "hexagon")
cat(sprintf("  Type: %s\n", trap_verts$type))
cat(sprintf("  Vertices: %d (as list)\n", length(trap_verts$vertices)))
cat("  Vertex positions:\n")
for (i in 1:length(trap_verts$vertices)) {
  v <- trap_verts$vertices[[i]]
  cat(sprintf("    V%d: (%.2f, %.2f)\n", i, v[1], v[2]))
}
cat("\n")

# Test 5: Neighbor detection
cat("Test 5: Neighbor detection\n")
cat(strrep("-", 50), "\n")

# Ring 1, piece 2 neighbors
for (edge in 1:4) {
  neighbor <- get_concentric_neighbor(2, edge, rings)
  edge_name <- c("INNER", "RIGHT", "OUTER", "LEFT")[edge]
  if (neighbor$is_boundary) {
    cat(sprintf("  Piece 2, %s edge: boundary\n", edge_name))
  } else {
    cat(sprintf("  Piece 2, %s edge: piece %d\n", edge_name, neighbor$neighbor_id))
  }
}
cat("\n")

# Test 6: Generate concentric pieces
cat("Test 6: Generate concentric pieces\n")
cat(strrep("-", 50), "\n")

pieces_result <- generate_concentric_pieces(
  rings = 3,
  seed = 42,
  diameter = 240,
  tabsize = 20,
  jitter = 4,
  center_shape = "hexagon"
)

cat(sprintf("  Generated %d pieces\n", length(pieces_result$pieces)))
cat(sprintf("  Piece height: %.2f mm\n", pieces_result$piece_height))

# Show first few pieces
for (i in 1:min(3, length(pieces_result$pieces))) {
  p <- pieces_result$pieces[[i]]
  path_preview <- substr(p$path, 1, 60)
  cat(sprintf("  Piece %d: path starts with '%s...'\n", p$id, path_preview))
}
cat("\n")

# Test 7: Full pipeline - generate_puzzle with concentric mode
cat("Test 7: Full pipeline with generate_puzzle()\n")
cat(strrep("-", 50), "\n")

result <- generate_puzzle(
  type = "hexagonal",
  grid = c(3),
  size = c(240),
  seed = 42,
  tabsize = 20,
  jitter = 4,
  offset = 0,
  concentric_mode = TRUE,
  center_shape = "hexagon",
  palette = "viridis",
  stroke_width = 1.5,
  save_files = TRUE,
  output_dir = "output",
  filename_prefix = "test_concentric_hexcenter"
)

cat(sprintf("  SVG saved to: %s\n", result$files$svg))
cat(sprintf("  Canvas size: %.0f x %.0f mm\n",
            result$canvas_size[1], result$canvas_size[2]))
cat(sprintf("  Pieces: %d\n", length(result$pieces)))
cat("\n")

# Test 8: Concentric with circle center
cat("Test 8: Concentric with circle center\n")
cat(strrep("-", 50), "\n")

result_circle <- generate_puzzle(
  type = "hexagonal",
  grid = c(3),
  size = c(240),
  seed = 42,
  tabsize = 20,
  jitter = 4,
  offset = 0,
  concentric_mode = TRUE,
  center_shape = "circle",
  palette = "magma",
  stroke_width = 1.5,
  save_files = TRUE,
  output_dir = "output",
  filename_prefix = "test_concentric_circlecenter"
)

cat(sprintf("  SVG saved to: %s\n", result_circle$files$svg))
cat("\n")

# Test 9: Concentric with separation
cat("Test 9: Concentric with separation (offset=10)\n")
cat(strrep("-", 50), "\n")

result_sep <- generate_puzzle(
  type = "hexagonal",
  grid = c(3),
  size = c(240),
  seed = 42,
  tabsize = 20,
  jitter = 4,
  offset = 10,
  concentric_mode = TRUE,
  center_shape = "hexagon",
  palette = "plasma",
  stroke_width = 1.5,
  save_files = TRUE,
  output_dir = "output",
  filename_prefix = "test_concentric_separated"
)

cat(sprintf("  SVG saved to: %s\n", result_sep$files$svg))
cat(sprintf("  Canvas size (expanded): %.0f x %.0f mm\n",
            result_sep$canvas_size[1], result_sep$canvas_size[2]))
cat("\n")

cat("=== All Tests Complete ===\n")
cat("\nGenerated files:\n")
svgs <- list.files("output", pattern = "test_concentric.*\\.svg$", full.names = TRUE)
for (svg in svgs) {
  cat(sprintf("  %s\n", svg))
}
