#!/usr/bin/env Rscript
# Test piece positioning engine (Issue #34)

cat("=== Testing Piece Positioning Engine ===\n\n")

# Source dependencies
source("R/logging.R")
source("R/config_utils.R")
source("R/rectangular_puzzle.R")
source("R/puzzle_core_clean.R")
source("R/hexagonal_puzzle.R")
source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")
source("R/unified_piece_generation.R")
source("R/piece_positioning.R")

# Test 1: Rectangular with offset=0
cat("--- Test 1: Rectangular with offset=0 ---\n")
result1 <- tryCatch({
  pieces <- generate_pieces_internal(
    type = "rectangular",
    seed = 42,
    grid = c(2, 3),
    size = c(150, 100)
  )

  positioned <- apply_piece_positioning(pieces, offset = 0)

  # Check that pieces are unchanged
  original_center <- pieces$pieces[[1]]$center
  positioned_center <- positioned$pieces[[1]]$center

  centers_match <- all(abs(original_center - positioned_center) < 0.01)
  canvas_unchanged <- all(abs(pieces$canvas_size - positioned$canvas_size) < 0.01)

  cat(sprintf("  Original center: (%.2f, %.2f)\n", original_center[1], original_center[2]))
  cat(sprintf("  Positioned center: (%.2f, %.2f)\n", positioned_center[1], positioned_center[2]))
  cat(sprintf("  Canvas size: %.1f x %.1f\n", positioned$canvas_size[1], positioned$canvas_size[2]))

  if (centers_match && canvas_unchanged) {
    cat("  PASS: offset=0 returns unchanged pieces\n")
    "PASS"
  } else {
    cat("  FAIL: Pieces changed unexpectedly\n")
    "FAIL"
  }
}, error = function(e) {
  cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
  "FAIL"
})

# Test 2: Rectangular with offset>0
cat("\n--- Test 2: Rectangular with offset>0 ---\n")
result2 <- tryCatch({
  pieces <- generate_pieces_internal(
    type = "rectangular",
    seed = 42,
    grid = c(2, 3),  # 2 rows, 3 cols
    size = c(150, 100)
  )

  offset <- 10
  positioned <- apply_piece_positioning(pieces, offset = offset)

  # Check that piece at (1, 1) moved by (1*offset, 1*offset)
  p_orig <- pieces$pieces[[5]]  # piece at (1, 1) is 5th (0,0), (1,0), (2,0), (0,1), (1,1)
  p_new <- positioned$pieces[[5]]

  expected_dx <- 1 * offset  # xi=1
  expected_dy <- 1 * offset  # yi=1

  actual_dx <- p_new$center[1] - p_orig$center[1]
  actual_dy <- p_new$center[2] - p_orig$center[2]

  cat(sprintf("  Piece (1,1) original center: (%.2f, %.2f)\n", p_orig$center[1], p_orig$center[2]))
  cat(sprintf("  Piece (1,1) new center: (%.2f, %.2f)\n", p_new$center[1], p_new$center[2]))
  cat(sprintf("  Expected offset: (%.2f, %.2f)\n", expected_dx, expected_dy))
  cat(sprintf("  Actual offset: (%.2f, %.2f)\n", actual_dx, actual_dy))

  # Check canvas grew
  orig_canvas <- pieces$canvas_size
  new_canvas <- positioned$canvas_size
  cat(sprintf("  Original canvas: %.1f x %.1f\n", orig_canvas[1], orig_canvas[2]))
  cat(sprintf("  New canvas: %.1f x %.1f\n", new_canvas[1], new_canvas[2]))

  # Canvas should grow by (cols-1)*offset in width and (rows-1)*offset in height + padding
  if (abs(actual_dx - expected_dx) < 0.01 && abs(actual_dy - expected_dy) < 0.01) {
    cat("  PASS: Rectangular pieces correctly separated\n")
    "PASS"
  } else {
    cat("  FAIL: Incorrect separation\n")
    "FAIL"
  }
}, error = function(e) {
  cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
  "FAIL"
})

# Test 3: Hexagonal with offset=0
cat("\n--- Test 3: Hexagonal with offset=0 ---\n")
result3 <- tryCatch({
  pieces <- generate_pieces_internal(
    type = "hexagonal",
    seed = 42,
    grid = c(3),
    size = c(200)
  )

  positioned <- apply_piece_positioning(pieces, offset = 0)

  # Check center piece stays at origin
  center_piece <- positioned$pieces[[1]]

  cat(sprintf("  Center piece position: (%.2f, %.2f)\n",
              center_piece$center[1], center_piece$center[2]))

  if (abs(center_piece$center[1]) < 0.01 && abs(center_piece$center[2]) < 0.01) {
    cat("  PASS: Hexagonal offset=0 keeps center at origin\n")
    "PASS"
  } else {
    cat("  FAIL: Center piece moved unexpectedly\n")
    "FAIL"
  }
}, error = function(e) {
  cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
  "FAIL"
})

# Test 4: Hexagonal with offset>0
cat("\n--- Test 4: Hexagonal with offset>0 ---\n")
result4 <- tryCatch({
  pieces <- generate_pieces_internal(
    type = "hexagonal",
    seed = 42,
    grid = c(3),
    size = c(200)
  )

  offset <- 10
  positioned <- apply_piece_positioning(pieces, offset = offset)

  # Center piece should still be at origin (it doesn't move)
  center_piece <- positioned$pieces[[1]]
  cat(sprintf("  Center piece: (%.2f, %.2f)\n",
              center_piece$center[1], center_piece$center[2]))

  # Ring 1 pieces should be further from center
  ring1_orig <- pieces$pieces[[2]]
  ring1_new <- positioned$pieces[[2]]

  orig_dist <- sqrt(ring1_orig$center[1]^2 + ring1_orig$center[2]^2)
  new_dist <- sqrt(ring1_new$center[1]^2 + ring1_new$center[2]^2)

  cat(sprintf("  Ring 1 piece original distance: %.2f\n", orig_dist))
  cat(sprintf("  Ring 1 piece new distance: %.2f\n", new_dist))
  cat(sprintf("  Canvas size: %.1f x %.1f\n", positioned$canvas_size[1], positioned$canvas_size[2]))

  if (new_dist > orig_dist) {
    cat("  PASS: Hexagonal pieces separated (ring pieces moved outward)\n")
    "PASS"
  } else {
    cat("  FAIL: Pieces not separated correctly\n")
    "FAIL"
  }
}, error = function(e) {
  cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
  "FAIL"
})

# Test 5: translate_svg_path function
cat("\n--- Test 5: translate_svg_path function ---\n")
result5 <- tryCatch({
  # Simple path
  path <- "M 10 20 L 30 40 Z"
  translated <- translate_svg_path(path, 5, 10)
  cat(sprintf("  Original: %s\n", path))
  cat(sprintf("  Translated: %s\n", translated))

  # Check M coordinates
  has_m_15_30 <- grepl("M 15.00 30.00", translated)

  # Bezier path
  path2 <- "M 0 0 C 10 20 30 40 50 60 Z"
  translated2 <- translate_svg_path(path2, 100, 200)
  cat(sprintf("  Bezier original: %s\n", path2))
  cat(sprintf("  Bezier translated: %s\n", translated2))

  # Arc path
  path3 <- "M 0 0 A 10 10 0 0 1 20 20 Z"
  translated3 <- translate_svg_path(path3, 5, 5)
  cat(sprintf("  Arc original: %s\n", path3))
  cat(sprintf("  Arc translated: %s\n", translated3))

  # Check arc - only endpoint should move
  has_arc_endpoint <- grepl("A 10 10 0 0 1 25.00 25.00", translated3)

  if (has_m_15_30 && has_arc_endpoint) {
    cat("  PASS: translate_svg_path works correctly\n")
    "PASS"
  } else {
    cat("  FAIL: Translation incorrect\n")
    "FAIL"
  }
}, error = function(e) {
  cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
  "FAIL"
})

# Test 6: Path preservation through transformation
cat("\n--- Test 6: Path preservation ---\n")
result6 <- tryCatch({
  pieces <- generate_pieces_internal(
    type = "rectangular",
    seed = 42,
    grid = c(2, 2),
    size = c(100, 100)
  )

  positioned <- apply_piece_positioning(pieces, offset = 15)

  # Verify all pieces still have valid closed paths
  all_valid <- TRUE
  for (i in seq_along(positioned$pieces)) {
    p <- positioned$pieces[[i]]
    starts_m <- grepl("^M ", p$path)
    ends_z <- grepl("Z$", trimws(p$path))
    has_curves <- grepl("C ", p$path)

    if (!starts_m || !ends_z || !has_curves) {
      cat(sprintf("  Piece %d invalid after translation\n", i))
      all_valid <- FALSE
    }
  }

  if (all_valid) {
    cat("  PASS: All paths preserved after translation\n")
    "PASS"
  } else {
    "FAIL"
  }
}, error = function(e) {
  cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
  "FAIL"
})

# Summary
cat("\n=== Test Summary ===\n")
cat(sprintf("Test 1 (Rect offset=0): %s\n", result1))
cat(sprintf("Test 2 (Rect offset>0): %s\n", result2))
cat(sprintf("Test 3 (Hex offset=0): %s\n", result3))
cat(sprintf("Test 4 (Hex offset>0): %s\n", result4))
cat(sprintf("Test 5 (translate_svg_path): %s\n", result5))
cat(sprintf("Test 6 (Path preservation): %s\n", result6))

cat("\n=== Tests Complete ===\n")
