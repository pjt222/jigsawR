# Test 7-ring concentric puzzle generation
# Verify that multi-piece edge connections work correctly

cat("=== Testing 7-Ring Concentric Puzzle ===\n\n")

source("R/logging.R")
source("R/concentric_geometry.R")
source("R/hexagonal_bezier_generation.R")
source("R/concentric_edge_generation.R")

rings <- 7
diameter <- 500
seed <- 42
tabsize <- diameter * 0.10  # 10%

num_pieces <- get_concentric_piece_count(rings)
piece_height <- get_concentric_piece_height(diameter, rings)

cat(sprintf("Rings: %d\n", rings))
cat(sprintf("Diameter: %d mm\n", diameter))
cat(sprintf("Piece height: %.2f mm\n", piece_height))
cat(sprintf("Tab size: %.2f mm (10%%)\n", tabsize))
cat(sprintf("Total pieces: %d\n\n", num_pieces))

# Generate edge map
cat("Generating edge map...\n")
edge_data <- generate_concentric_edge_map(
  rings = rings,
  seed = seed,
  diameter = diameter,
  tabsize = tabsize,
  jitter = 4,
  center_shape = "hexagon"
)
cat(sprintf("Generated %d unique edges\n\n", edge_data$num_edges))

# Verify key pieces have correct edges
test_pieces <- c(4, 12, 13, 26, 27, 28)

cat("=== Verifying Problematic Pieces ===\n\n")

errors_found <- 0

for (piece_id in test_pieces) {
  piece_info <- edge_data$piece_vertices[[piece_id]]
  piece_edges <- edge_data$piece_edges[[piece_id]]

  cat(sprintf("--- Piece %d (ring %d, pos %d) ---\n",
              piece_id, piece_info$ring, piece_info$position))

  # Count edge types
  edge_types <- sapply(piece_edges, function(e) e$type)
  cat(sprintf("  Edge types: %s\n", paste(edge_types, collapse=", ")))

  # Check inner segments
  inner_segs <- piece_edges[sapply(piece_edges, function(e) e$type %in% c("inner", "inner_segment"))]
  cat(sprintf("  Inner segments: %d\n", length(inner_segs)))

  # Check outer segments
  outer_segs <- piece_edges[sapply(piece_edges, function(e) e$type == "outer_segment")]
  cat(sprintf("  Outer segments: %d\n", length(outer_segs)))

  # Build path and check for errors
  path <- tryCatch({
    build_concentric_piece_path(piece_id, edge_data)
  }, error = function(e) {
    cat(sprintf("  ERROR building path: %s\n", e$message))
    errors_found <<- errors_found + 1
    NULL
  })

  if (!is.null(path)) {
    # Check path starts with M and ends with Z
    if (!grepl("^M ", path)) {
      cat("  ERROR: Path doesn't start with M\n")
      errors_found <- errors_found + 1
    }
    if (!grepl("Z\\s*$", path)) {
      cat("  ERROR: Path doesn't end with Z\n")
      errors_found <- errors_found + 1
    }

    # Count bezier commands
    c_count <- length(gregexpr("C ", path)[[1]])
    if (c_count > 0) {
      cat(sprintf("  Bezier curves (C): %d\n", c_count))
    }

    cat("  Path OK\n")
  }
  cat("\n")
}

# Test full puzzle generation
cat("=== Generating Full Puzzle ===\n\n")

pieces_data <- generate_concentric_pieces(
  rings = rings,
  seed = seed,
  diameter = diameter,
  tabsize = tabsize,
  jitter = 4,
  center_shape = "hexagon"
)

cat(sprintf("Generated %d pieces\n", length(pieces_data$pieces)))

# Check all paths are valid
invalid_paths <- 0
for (i in 1:length(pieces_data$pieces)) {
  path <- pieces_data$pieces[[i]]$path
  if (!grepl("^M ", path) || !grepl("Z\\s*$", path)) {
    invalid_paths <- invalid_paths + 1
  }
}

if (invalid_paths > 0) {
  cat(sprintf("ERROR: %d pieces have invalid paths\n", invalid_paths))
  errors_found <- errors_found + invalid_paths
} else {
  cat("All piece paths are valid\n")
}

# Summary
cat("\n=== Summary ===\n")
if (errors_found == 0) {
  cat("All tests passed!\n")
} else {
  cat(sprintf("FAILED: %d errors found\n", errors_found))
}
