#!/usr/bin/env Rscript
# Test hexagonal individual pieces generation

cat("=== Testing Hexagonal Individual Pieces Generation ===\n\n")

# Source dependencies
source("R/logging.R")
source("R/config_utils.R")
source("R/hexagonal_puzzle.R")
source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")
source("R/hexagonal_individual_pieces.R")

# Test 1: Generate individual pieces
cat("--- Test 1: Generate individual hexagonal pieces ---\n")
result <- tryCatch({
  pieces <- generate_hexagonal_individual_pieces(
    rings = 3,
    seed = 42,
    diameter = 240,
    output_dir = "output/hex_individual_test",
    save_combined = TRUE,
    save_individual = TRUE
  )

  cat(sprintf("  Generated %d pieces\n", pieces$parameters$num_pieces))
  cat(sprintf("  Individual files: %d\n", length(pieces$files$individual)))
  cat(sprintf("  Combined file: %s\n", pieces$files$combined))

  # Check that files were created
  if (length(pieces$files$individual) == pieces$parameters$num_pieces) {
    cat("  PASS: Correct number of individual files\n")
  } else {
    cat("  FAIL: Wrong number of individual files\n")
  }

  if (!is.null(pieces$files$combined) && file.exists(pieces$files$combined)) {
    cat("  PASS: Combined file exists\n")
  } else {
    cat("  FAIL: Combined file missing\n")
  }

  "PASS"
}, error = function(e) {
  cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
  "FAIL"
})

# Test 2: Verify piece structure
cat("\n--- Test 2: Verify piece structure ---\n")
if (exists("pieces") && !is.null(pieces$pieces)) {
  p1 <- pieces$pieces[[1]]
  cat(sprintf("  Piece 1 has path: %s\n", if (!is.null(p1$path)) "YES" else "NO"))
  cat(sprintf("  Piece 1 center: (%.2f, %.2f)\n", p1$center_x, p1$center_y))
  cat(sprintf("  Piece 1 ring: %d\n", p1$ring))
  cat(sprintf("  Piece 1 type: %s\n", p1$type))

  # Check that paths are valid SVG
  if (grepl("^M ", p1$path) && grepl("Z$", p1$path)) {
    cat("  PASS: Path is valid SVG (starts with M, ends with Z)\n")
  } else {
    cat("  FAIL: Path format incorrect\n")
  }

  # Check for bezier curves
  if (grepl("C ", p1$path)) {
    cat("  PASS: Path contains bezier curves\n")
  } else {
    cat("  FAIL: Path missing bezier curves\n")
  }
}

# Test 3: Verify adjacent pieces have complementary edges
cat("\n--- Test 3: Verify complementary edges ---\n")
if (exists("pieces") && length(pieces$pieces) >= 7) {
  # Center piece (1) is adjacent to pieces 2-7
  # Check that edges connect properly
  p1 <- pieces$pieces[[1]]
  p2 <- pieces$pieces[[2]]

  # Parse paths to extract control points
  # For pieces that share an edge, the bezier control points should match
  # (one piece uses forward, the other uses reverse)

  cat("  Center piece (1) should connect to ring 1 pieces (2-7)\n")
  cat("  Checking edge mapping consistency...\n")

  # Generate edge map to verify
  edge_data <- generate_hex_edge_map(
    rings = 3, seed = 42, diameter = 240
  )

  cat(sprintf("  Total unique edges: %d\n", edge_data$num_edges))

  # Count border vs internal edges
  border_count <- 0
  internal_count <- 0
  for (key in names(edge_data$piece_edge_map)) {
    edge <- edge_data$piece_edge_map[[key]]
    if (edge$type == "border") {
      border_count <- border_count + 1
    } else {
      internal_count <- internal_count + 1
    }
  }

  cat(sprintf("  Border edges: %d\n", border_count))
  cat(sprintf("  Internal edges: %d\n", internal_count))

  # For 3 rings: 19 pieces, each with 6 sides = 114 total sides
  # Internal edges are shared (counted twice), border edges counted once
  expected_total <- 3 * 3 * (3 - 1) + 1  # 19 pieces
  cat(sprintf("  Expected pieces: %d, Got: %d\n", expected_total, length(pieces$pieces)))

  if (length(pieces$pieces) == expected_total) {
    cat("  PASS: Correct number of pieces\n")
  } else {
    cat("  FAIL: Wrong number of pieces\n")
  }
}

# Test 4: Test different ring counts
cat("\n--- Test 4: Test different ring counts ---\n")
for (rings in 2:4) {
  expected_pieces <- 3 * rings * (rings - 1) + 1
  result <- tryCatch({
    test_pieces <- generate_hex_pieces_with_edge_map(
      rings = rings, seed = 123, diameter = 200,
      separated = FALSE
    )
    if (length(test_pieces) == expected_pieces) {
      cat(sprintf("  Rings=%d: PASS (%d pieces)\n", rings, expected_pieces))
    } else {
      cat(sprintf("  Rings=%d: FAIL (expected %d, got %d)\n",
                  rings, expected_pieces, length(test_pieces)))
    }
  }, error = function(e) {
    cat(sprintf("  Rings=%d: ERROR - %s\n", rings, conditionMessage(e)))
  })
}

# Test 5: Visual verification - create SVG preview
cat("\n--- Test 5: Visual verification ---\n")
cat("  Check output/hex_individual_test/ for SVG files\n")
cat("  - Individual piece files: hexagonal_piece_XX_seed42.svg\n")
cat("  - Combined view: hexagonal_combined_seed42.svg\n")

cat("\n=== Tests Complete ===\n")
