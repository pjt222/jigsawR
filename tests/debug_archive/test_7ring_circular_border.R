# Test 7-ring with circular border (arc commands)

setwd("D:/dev/p/jigsawR")

library(devtools)
load_all()

cat("Testing 7-ring puzzle with do_circular_border=TRUE\n")
cat("===================================================\n\n")

# Generate 7-ring puzzle with warp+trunc+circular_border
cat("Generating 7-ring puzzle...\n")
result <- generate_puzzle(
  type = "hexagonal",
  grid = c(7),
  size = c(400),
  seed = 42,
  offset = 0,
  do_warp = TRUE,
  do_trunc = TRUE,
  do_circular_border = TRUE  # This enables perfect circular arcs
)

cat("Generated:\n")
cat("  Canvas size:", result$canvas_size[1], "x", result$canvas_size[2], "\n")
cat("  Number of pieces:", length(result$pieces), "\n\n")

# Check for arc commands
arc_count <- length(gregexpr("A [0-9]", result$svg_content)[[1]])
if (arc_count > 0) {
  cat("Arc commands found:", arc_count, "\n")

  # Extract a sample arc
  arcs <- regmatches(result$svg_content, gregexpr("A [0-9.]+ [0-9.]+ [0-9]+ [0-9]+ [0-9]+ [0-9.-]+ [0-9.-]+", result$svg_content))
  sample_arcs <- unique(unlist(arcs))

  if (length(sample_arcs) > 0) {
    cat("\nSample arc commands (first 5):\n")
    for (i in 1:min(5, length(sample_arcs))) {
      cat("  ", sample_arcs[i], "\n")

      # Parse to extract radius
      parts <- strsplit(sample_arcs[i], " ")[[1]]
      radius <- as.numeric(parts[2])
      cat("    -> radius:", radius, "\n")
    }
    cat("\nExpected radius:", 400/2, "(diameter/2)\n")
  }
} else {
  cat("WARNING: No arc commands found!\n")
}

# Save
output_file <- "output/test_7ring_circular_border.svg"
writeLines(result$svg_content, output_file)
cat("\nSaved to:", output_file, "\n")

# Also test 3-ring for comparison
cat("\n\nGenerating 3-ring puzzle for comparison...\n")
result3 <- generate_puzzle(
  type = "hexagonal",
  grid = c(3),
  size = c(240),
  seed = 42,
  offset = 0,
  do_warp = TRUE,
  do_trunc = TRUE,
  do_circular_border = TRUE
)

arcs3 <- regmatches(result3$svg_content, gregexpr("A [0-9.]+ [0-9.]+ [0-9]+ [0-9]+ [0-9]+ [0-9.-]+ [0-9.-]+", result3$svg_content))
sample_arcs3 <- unique(unlist(arcs3))

if (length(sample_arcs3) > 0) {
  cat("3-ring sample arc:\n")
  parts <- strsplit(sample_arcs3[1], " ")[[1]]
  radius <- as.numeric(parts[2])
  cat("  radius:", radius, "\n")
  cat("  expected:", 240/2, "(diameter/2)\n")
}

output_file3 <- "output/test_3ring_circular_border.svg"
writeLines(result3$svg_content, output_file3)
cat("Saved to:", output_file3, "\n")
