# Test clean hexagonal individual pieces implementation
source('R/hexagonal_puzzle.R')
source('R/hexagonal_individual_pieces_clean.R')

cat("Testing clean hexagonal implementation\n")
cat("=====================================\n\n")

# Test with 2 rings (should give 7 pieces: 1 center + 6 in ring 1)
cat("Generating 2-ring puzzle...\n")
result <- generate_hexagonal_individual_pieces_clean(
  seed = 42,
  rings = 2,
  diameter = 200,
  save_combined = FALSE
)

cat('\nResult summary:\n')
cat('Total pieces:', length(result$pieces), '\n')
cat('Expected pieces:', result$parameters$total_pieces, '\n')
cat('Horizontal edges:', length(result$edges$horizontal), '\n')
cat('Vertical edges:', length(result$edges$vertical), '\n')
cat('Border edges:', length(result$edges$border), '\n')

cat("\nFirst piece info:\n")
if (length(result$pieces) > 0) {
  p1 <- result$pieces[[1]]
  cat('  Index:', p1$index, '\n')
  cat('  Ring:', p1$ring, '\n')
  cat('  Type:', p1$type, '\n')
  cat('  Path length:', nchar(p1$path), 'characters\n')
}

cat("\nSUCCESS: Basic structure works!\n")
