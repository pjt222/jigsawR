#!/usr/bin/env Rscript
# Quick baseline benchmark for jigsawR

devtools::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(bench))

cat("\n=== jigsawR Performance Baseline ===\n\n")

# Rectangular 5x5
cat("Rectangular 5x5 puzzle (25 pieces):\n")
result_rect <- bench::mark(
  generate_puzzle(type = "rectangular", seed = 42, grid = c(5,5), size = c(500,500)),
  iterations = 5,
  check = FALSE
)
cat(sprintf("  Median: %s, Mean: %s\n", format(result_rect$median), format(result_rect$mean)))

# Hexagonal 3 rings
cat("\nHexagonal 3 rings puzzle (19 pieces):\n")
result_hex <- bench::mark(
  generate_puzzle(type = "hexagonal", seed = 42, grid = c(3), size = c(300)),
  iterations = 5,
  check = FALSE
)
cat(sprintf("  Median: %s, Mean: %s\n", format(result_hex$median), format(result_hex$mean)))

# Concentric 3 rings
cat("\nConcentric 3 rings puzzle (19 pieces):\n")
result_conc <- bench::mark(
  generate_puzzle(type = "concentric", seed = 42, grid = c(3), size = c(300)),
  iterations = 5,
  check = FALSE
)
cat(sprintf("  Median: %s, Mean: %s\n", format(result_conc$median), format(result_conc$mean)))

# Larger hexagonal
cat("\nHexagonal 5 rings puzzle (61 pieces):\n")
result_hex_large <- bench::mark(
  generate_puzzle(type = "hexagonal", seed = 42, grid = c(5), size = c(500)),
  iterations = 3,
  check = FALSE
)
cat(sprintf("  Median: %s, Mean: %s\n", format(result_hex_large$median), format(result_hex_large$mean)))

# Rectangular 8x8
cat("\nRectangular 8x8 puzzle (64 pieces):\n")
result_rect_large <- bench::mark(
  generate_puzzle(type = "rectangular", seed = 42, grid = c(8,8), size = c(800,800)),
  iterations = 3,
  check = FALSE
)
cat(sprintf("  Median: %s, Mean: %s\n", format(result_rect_large$median), format(result_rect_large$mean)))

cat("\n=== Baseline Complete ===\n")
