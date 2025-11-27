#!/usr/bin/env Rscript
# Trace the exact flow that Shiny app would use

# Simulate Shiny app environment
setwd("inst/shiny-app")
cat("Working directory:", getwd(), "\n")
cat("Files here:", paste(list.files(), collapse=", "), "\n\n")

# Source all R files like the Shiny app does
cat("Sourcing R files...\n")
r_path <- "../../R"
if (file.exists(r_path)) {
  files <- list.files(r_path, pattern = "\\.R$", full.names = TRUE)
  for (file in files) {
    if (!grepl("scripts_archive|examples", file)) {
      cat("  Sourcing:", basename(file), "\n")
      source(file)
    }
  }
} else {
  cat("ERROR: R directory not found!\n")
}

cat("\n")
cat("Checking function availability...\n")
cat("  generate_hex_pieces_with_edge_map exists?", exists("generate_hex_pieces_with_edge_map"), "\n")
cat("  generate_hex_edge_map exists?", exists("generate_hex_edge_map"), "\n")
cat("  generate_separated_hexagonal_svg exists?", exists("generate_separated_hexagonal_svg"), "\n")

cat("\n")
cat("Testing puzzle generation...\n")
svg <- generate_separated_hexagonal_svg(
  rings = 2,
  seed = 42,
  diameter = 240,
  offset = 10,
  arrangement = "hexagonal",
  use_bezier = TRUE,
  tabsize = 27,
  jitter = 5
)

cat("\nSVG generated, length:", nchar(svg), "characters\n")

# Check for any edge scrambling indicators
if (grepl("Creating edge mapping", svg)) {
  cat("✓ Using new edge mapping system\n")
} else {
  cat("✗ NOT using new edge mapping system\n")
}

# Save output
output_file <- "../../output/trace_shiny_flow.svg"
writeLines(svg, output_file)
cat("Saved to:", output_file, "\n")
