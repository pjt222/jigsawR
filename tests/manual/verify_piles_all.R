#!/usr/bin/env Rscript
# Verify PILES 'ALL' keyword creates single meta piece
# Issue #55

devtools::load_all(quiet = TRUE)

cat("\n=== Verifying PILES 'ALL' Keyword (Issue #55) ===\n\n")

# Helper to check fusion groups
check_all_keyword <- function(puzzle_type, grid, size) {
  cat(sprintf("Testing %s puzzle:\n", puzzle_type))

  # Generate puzzle with ALL fusion
  result <- suppressMessages(generate_puzzle(
    type = puzzle_type,
    seed = 42,
    grid = grid,
    size = size,
    fusion_groups = "ALL",
    save_files = FALSE
  ))

  n_pieces <- length(result$pieces)
  cat(sprintf("  Total pieces: %d\n", n_pieces))

  # Check 1: Verify parse_piles returns all pieces
  parsed <- parse_piles("all", result)
  cat(sprintf("  parse_piles('all') returns: %d pieces\n", length(parsed[[1]])))

  if (length(parsed[[1]]) != n_pieces) {
    cat("  ERROR: parse_piles did not return all pieces!\n")
    return(FALSE)
  }

  if (!all(parsed[[1]] == seq_len(n_pieces))) {
    cat("  ERROR: parse_piles returned wrong piece IDs!\n")
    return(FALSE)
  }

  # Check 2: Verify all pieces are in the same fusion group
  fusion_groups <- unique(sapply(result$pieces, function(p) p$fusion_group))
  fusion_groups <- fusion_groups[!is.na(fusion_groups)]
  cat(sprintf("  Unique fusion groups: %d\n", length(fusion_groups)))

  if (length(fusion_groups) != 1) {
    cat("  ERROR: Expected 1 fusion group, got %d!\n", length(fusion_groups))
    return(FALSE)
  }

  # Check 3: Count fused edges
  fused_edge_count <- 0
  for (piece in result$pieces) {
    fused_edges <- piece$fused_edges
    if (!is.null(fused_edges)) {
      fused_edge_count <- fused_edge_count + sum(unlist(fused_edges))
    }
  }
  cat(sprintf("  Total fused edges: %d\n", fused_edge_count))

  # Check 4: Verify SVG contains fused styling
  if (!is.null(result$svg_content)) {
    has_dashed <- grepl("stroke-dasharray", result$svg_content)
    cat(sprintf("  SVG has dashed lines: %s\n", has_dashed))
  }

  cat("  PASSED!\n\n")
  return(TRUE)
}

# Test all puzzle types
results <- list()

# Rectangular 3x3 = 9 pieces
results$rectangular <- check_all_keyword("rectangular", c(3, 3), c(300, 300))

# Hexagonal 2 rings = 7 pieces
results$hexagonal <- check_all_keyword("hexagonal", c(2), c(200))

# Concentric 2 rings = 7 pieces
results$concentric <- check_all_keyword("concentric", c(2), c(200))

# Summary
cat("=== Summary ===\n")
all_passed <- all(unlist(results))
for (type in names(results)) {
  status <- if (results[[type]]) "PASS" else "FAIL"
  cat(sprintf("  %s: %s\n", type, status))
}

if (all_passed) {
  cat("\nAll tests PASSED!\n")
} else {
  cat("\nSome tests FAILED!\n")
  quit(status = 1)
}
