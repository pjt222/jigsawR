# Update package documentation and exports

# Load devtools
library(devtools)

# Document the package to update NAMESPACE
cat("Updating package documentation...\n")
devtools::document()

# Check the package
cat("\nChecking package...\n")
check_results <- devtools::check()

cat("\nPackage update complete!\n")
cat("New exported functions:\n")
cat("- generate_individual_pieces()\n")
cat("- generate_2x2_individual_pieces()\n")
cat("- generate_puzzle_pieces()\n")
cat("- parse_svg_path()\n")
cat("- reverse_path_segments()\n")
cat("- flip_path_segments()\n")
cat("- create_complementary_edge()\n")