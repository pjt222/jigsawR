#!/usr/bin/env Rscript
# Test unified piece generation module (Issue #33)

cat("=== Testing Unified Piece Generation ===\n\n")

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

# Test 1: Rectangular puzzle pieces
cat("--- Test 1: Rectangular puzzle pieces ---\n")
result_rect <- tryCatch({
  pieces <- generate_pieces_internal(
    type = "rectangular",
    seed = 42,
    grid = c(3, 4),  # 3 rows, 4 cols = 12 pieces
    size = c(200, 150),
    tabsize = 20,
    jitter = 4
  )

  cat(sprintf("  Type: %s\n", pieces$type))
  cat(sprintf("  Piece count: %d\n", length(pieces$pieces)))
  cat(sprintf("  Canvas size: %.1f x %.1f\n", pieces$canvas_size[1], pieces$canvas_size[2]))

  # Check first piece structure
  p1 <- pieces$pieces[[1]]
  cat(sprintf("  First piece ID: %s\n", p1$id))
  cat(sprintf("  First piece center: (%.2f, %.2f)\n", p1$center[1], p1$center[2]))
  cat(sprintf("  First piece grid_pos: (%d, %d)\n", p1$grid_pos["xi"], p1$grid_pos["yi"]))

  # Validate all pieces
  valid <- validate_pieces(pieces)
  if (valid && length(pieces$pieces) == 12) {
    cat("  PASS: 12 rectangular pieces generated and validated\n")
    "PASS"
  } else {
    cat("  FAIL: Validation failed\n")
    "FAIL"
  }
}, error = function(e) {
  cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
  "FAIL"
})

# Test 2: Hexagonal puzzle pieces
cat("\n--- Test 2: Hexagonal puzzle pieces ---\n")
result_hex <- tryCatch({
  pieces <- generate_pieces_internal(
    type = "hexagonal",
    seed = 42,
    grid = c(3),  # 3 rings = 19 pieces
    size = c(240),  # diameter
    tabsize = 27,
    jitter = 5
  )

  cat(sprintf("  Type: %s\n", pieces$type))
  cat(sprintf("  Piece count: %d\n", length(pieces$pieces)))
  cat(sprintf("  Canvas size: %.1f x %.1f\n", pieces$canvas_size[1], pieces$canvas_size[2]))

  # Check center piece (first piece)
  p1 <- pieces$pieces[[1]]
  cat(sprintf("  Center piece ID: %s\n", p1$id))
  cat(sprintf("  Center piece center: (%.2f, %.2f)\n", p1$center[1], p1$center[2]))
  cat(sprintf("  Center piece ring: %d\n", p1$ring_pos$ring))

  # Validate all pieces
  valid <- validate_pieces(pieces)
  if (valid && length(pieces$pieces) == 19) {
    cat("  PASS: 19 hexagonal pieces generated and validated\n")
    "PASS"
  } else {
    cat("  FAIL: Validation failed\n")
    "FAIL"
  }
}, error = function(e) {
  cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
  "FAIL"
})

# Test 3: Hexagonal with warp and trunc
cat("\n--- Test 3: Hexagonal with warp and trunc ---\n")
result_warp <- tryCatch({
  pieces <- generate_pieces_internal(
    type = "hexagonal",
    seed = 123,
    grid = c(3),
    size = c(200),
    do_warp = TRUE,
    do_trunc = TRUE
  )

  # Check that boundary pieces have arc commands
  outer_piece <- pieces$pieces[[length(pieces$pieces)]]  # Last piece is on outer ring
  has_arcs <- grepl(" A ", outer_piece$path)

  cat(sprintf("  Piece count: %d\n", length(pieces$pieces)))
  cat(sprintf("  Outer piece has arcs: %s\n", if (has_arcs) "YES" else "NO"))

  if (has_arcs) {
    cat("  PASS: Warp/trunc applied (arcs present)\n")
    "PASS"
  } else {
    cat("  FAIL: Expected arc commands not found\n")
    "FAIL"
  }
}, error = function(e) {
  cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
  "FAIL"
})

# Test 4: Different rectangular sizes
cat("\n--- Test 4: Different rectangular sizes ---\n")
for (grid_spec in list(c(2, 2), c(3, 3), c(4, 5), c(1, 6))) {
  expected <- grid_spec[1] * grid_spec[2]
  result <- tryCatch({
    pieces <- generate_pieces_internal(
      type = "rectangular",
      seed = 999,
      grid = grid_spec,
      size = c(100, 100)
    )
    if (length(pieces$pieces) == expected) "PASS" else "FAIL"
  }, error = function(e) "ERROR")

  cat(sprintf("  Grid %dx%d (%d pieces): %s\n",
              grid_spec[1], grid_spec[2], expected, result))
}

# Test 5: Different hexagonal rings
cat("\n--- Test 5: Different hexagonal ring counts ---\n")
for (rings in 2:5) {
  expected <- 3 * rings * (rings - 1) + 1
  result <- tryCatch({
    pieces <- generate_pieces_internal(
      type = "hexagonal",
      seed = 999,
      grid = c(rings),
      size = c(200)
    )
    if (length(pieces$pieces) == expected) "PASS" else "FAIL"
  }, error = function(e) "ERROR")

  cat(sprintf("  Rings=%d (%d pieces): %s\n", rings, expected, result))
}

# Test 6: Piece path structure
cat("\n--- Test 6: Piece path structure validation ---\n")
pieces <- generate_pieces_internal(type = "rectangular", seed = 42, grid = c(2, 2))

all_valid <- TRUE
for (i in seq_along(pieces$pieces)) {
  p <- pieces$pieces[[i]]
  starts_m <- grepl("^M ", p$path)
  ends_z <- grepl("Z$", trimws(p$path))
  has_curves <- grepl("C ", p$path)

  if (!starts_m || !ends_z || !has_curves) {
    cat(sprintf("  Piece %d: FAIL (M=%s, Z=%s, C=%s)\n",
                i, starts_m, ends_z, has_curves))
    all_valid <- FALSE
  }
}
if (all_valid) {
  cat("  PASS: All pieces have valid path structure (M...C...Z)\n")
}

# Test 7: get_piece_count utility
cat("\n--- Test 7: get_piece_count utility ---\n")
tests <- list(
  list(type = "rectangular", grid = c(2, 3), expected = 6),
  list(type = "rectangular", grid = c(4, 4), expected = 16),
  list(type = "hexagonal", grid = c(2), expected = 7),
  list(type = "hexagonal", grid = c(3), expected = 19),
  list(type = "hexagonal", grid = c(4), expected = 37)
)

for (test in tests) {
  count <- get_piece_count(test$type, test$grid)
  status <- if (count == test$expected) "PASS" else "FAIL"
  cat(sprintf("  %s grid=%s: expected=%d, got=%d - %s\n",
              test$type, paste(test$grid, collapse = ","),
              test$expected, count, status))
}

# Summary
cat("\n=== Test Summary ===\n")
cat(sprintf("Test 1 (Rectangular): %s\n", result_rect))
cat(sprintf("Test 2 (Hexagonal): %s\n", result_hex))
cat(sprintf("Test 3 (Warp/Trunc): %s\n", result_warp))
cat("Test 4-7: See individual results above\n")

cat("\n=== Tests Complete ===\n")
