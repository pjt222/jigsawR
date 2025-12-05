# Test the fix for 7-ring hexagonal puzzle

setwd("D:/dev/p/jigsawR")

library(devtools)
load_all()

cat("Testing fixed 7-ring puzzle generation\n")
cat("======================================\n\n")

# Generate 7-ring puzzle with warp+trunc
cat("Generating 7-ring puzzle with do_warp=TRUE, do_trunc=TRUE...\n")
result_7ring <- generate_puzzle(
  type = "hexagonal",
  grid = c(7),
  size = c(400),
  seed = 42,
  offset = 0,
  do_warp = TRUE,
  do_trunc = TRUE
)

cat("7-ring puzzle generated:\n")
cat("  Canvas size:", result_7ring$canvas_size[1], "x", result_7ring$canvas_size[2], "\n")
cat("  Number of pieces:", length(result_7ring$pieces), "\n\n")

# Save it
output_file_7 <- "output/test_7ring_warp_trunc_fixed.svg"
writeLines(result_7ring$svg_content, output_file_7)
cat("Saved to:", output_file_7, "\n\n")

# Generate 3-ring puzzle for comparison
cat("Generating 3-ring puzzle with do_warp=TRUE, do_trunc=TRUE...\n")
result_3ring <- generate_puzzle(
  type = "hexagonal",
  grid = c(3),
  size = c(240),
  seed = 42,
  offset = 0,
  do_warp = TRUE,
  do_trunc = TRUE
)

cat("3-ring puzzle generated:\n")
cat("  Canvas size:", result_3ring$canvas_size[1], "x", result_3ring$canvas_size[2], "\n")
cat("  Number of pieces:", length(result_3ring$pieces), "\n\n")

output_file_3 <- "output/test_3ring_warp_trunc_fixed.svg"
writeLines(result_3ring$svg_content, output_file_3)
cat("Saved to:", output_file_3, "\n\n")

# Also test separated mode
cat("Generating 7-ring SEPARATED puzzle...\n")
result_7ring_sep <- generate_puzzle(
  type = "hexagonal",
  grid = c(7),
  size = c(400),
  seed = 42,
  offset = 20,  # 20mm separation
  do_warp = TRUE,
  do_trunc = TRUE
)

cat("7-ring separated puzzle generated:\n")
cat("  Canvas size:", result_7ring_sep$canvas_size[1], "x", result_7ring_sep$canvas_size[2], "\n")
cat("  Number of pieces:", length(result_7ring_sep$pieces), "\n\n")

output_file_7_sep <- "output/test_7ring_warp_trunc_separated.svg"
writeLines(result_7ring_sep$svg_content, output_file_7_sep)
cat("Saved to:", output_file_7_sep, "\n\n")

cat("Done! Check the output files visually.\n")
