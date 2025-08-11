# Run individual pieces generation with the package functions

# Clear output directory first
if (dir.exists("output/generated")) {
  unlink("output/generated", recursive = TRUE)
}
dir.create("output/generated", recursive = TRUE)

# Load all package functions
cat("Loading package functions...\n")
source("R/rectangular_puzzle.R")
source("R/bezier_utils.R") 
source("R/individual_pieces_correct.R")
source("R/generate_individual_pieces_proper.R")

# Test 1: Use the hardcoded correct version
cat("\n=== Test 1: Hardcoded correct 2x2 puzzle ===\n")
result1 <- generate_2x2_individual_pieces(
  seed = 42,
  width = 200,
  height = 200,
  output_dir = "output/generated"
)

cat("Generated files:\n")
for (f in result1$files$individual) {
  cat("  -", f, "\n")
}
cat("  -", result1$files$combined, "\n")

# Test 2: Try the general function
cat("\n=== Test 2: General function for 2x2 ===\n")
tryCatch({
  result2 <- generate_puzzle_pieces(
    seed = 42,
    xn = 2,
    yn = 2,
    width = 200, 
    height = 200,
    output_dir = "output/generated"
  )
  cat("Successfully generated", length(result2$pieces), "pieces\n")
}, error = function(e) {
  cat("Error:", e$message, "\n")
})

cat("\n=== Generation complete ===\n")
cat("Check output/generated/ directory for results\n")

# Also create a reference script that others can use
reference_script <- '# Generate individual puzzle pieces using jigsawR

# Install and load the package
# devtools::install_github("username/jigsawR")  # Once published
library(jigsawR)

# Generate a 2x2 puzzle with individual pieces
result <- generate_individual_pieces(
  seed = 42,        # Random seed for reproducibility
  xn = 2,           # Number of columns
  yn = 2,           # Number of rows  
  width = 200,      # Puzzle width in mm
  height = 200,     # Puzzle height in mm
  output_dir = "my_puzzle_pieces"
)

# The function creates:
# - Individual SVG files for each piece
# - A combined view showing all pieces with colors
# - Proper complementary edges between adjacent pieces

cat("Generated", result$parameters$total_pieces, "puzzle pieces\\n")
'

writeLines(reference_script, "output/generated/example_usage.R")
cat("\nCreated example_usage.R for reference\n")