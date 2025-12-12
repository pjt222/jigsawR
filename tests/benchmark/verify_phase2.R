#!/usr/bin/env Rscript
# Quick verification of Phase 2 parsed_segments caching

devtools::load_all(quiet = TRUE)

cat("\n=== Phase 2.1 Verification ===\n\n")

# Test 1: Rectangular
cat("Rectangular puzzle:\n")
rect <- generate_puzzle(type = "rectangular", seed = 42, grid = c(2,2), size = c(200,200))
cat("  parsed_segments exists:", !is.null(rect$pieces[[1]]$parsed_segments), "\n")
cat("  segment count:", length(rect$pieces[[1]]$parsed_segments), "\n")

# Test 2: Hexagonal
cat("\nHexagonal puzzle:\n")
hex <- generate_puzzle(type = "hexagonal", seed = 42, grid = c(2), size = c(200))
cat("  parsed_segments exists:", !is.null(hex$pieces[[1]]$parsed_segments), "\n")
cat("  segment count:", length(hex$pieces[[1]]$parsed_segments), "\n")

# Test 3: Concentric
cat("\nConcentric puzzle:\n")
conc <- generate_puzzle(type = "concentric", seed = 42, grid = c(2), size = c(200))
cat("  parsed_segments exists:", !is.null(conc$pieces[[1]]$parsed_segments), "\n")
cat("  segment count:", length(conc$pieces[[1]]$parsed_segments), "\n")

# Summary
all_ok <- !is.null(rect$pieces[[1]]$parsed_segments) &&
          !is.null(hex$pieces[[1]]$parsed_segments) &&
          !is.null(conc$pieces[[1]]$parsed_segments)

cat("\n=== Result ===\n")
if (all_ok) {
  cat("✓ All puzzle types have parsed_segments cached\n")
} else {
  cat("✗ Some puzzle types missing parsed_segments\n")
}
