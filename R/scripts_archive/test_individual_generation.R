# Test the individual piece generation functions

# Load the package functions
devtools::load_all()

# Generate 2x2 individual pieces
cat("Generating 2x2 individual pieces...\n")
result <- generate_individual_pieces(
  seed = 42,
  xn = 2,
  yn = 2, 
  width = 200,
  height = 200,
  output_dir = "output"
)

cat("\nGenerated files:\n")
cat("Individual pieces:\n")
for (f in result$files$individual) {
  cat("  -", f, "\n")
}
cat("Combined view:", result$files$combined, "\n")

cat("\nPiece generation complete!\n")
cat("Check the output directory for the generated SVG files.\n")