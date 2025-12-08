# Test fusion styling parameters flow
library(jigsawR)

# Test dashed style
result <- generate_puzzle(
  type = "rectangular",
  grid = c(2, 2),
  size = c(200, 200),
  seed = 42,
  offset = 0,
  fusion_groups = list(c(1, 2)),
  fusion_style = "dashed",
  fusion_opacity = 0.5
)

cat("fusion_style:", result$parameters$fusion_style, "\n")
cat("fusion_opacity:", result$parameters$fusion_opacity, "\n")
cat("Has stroke-dasharray:", grepl("stroke-dasharray", result$svg_content), "\n")
cat("Has opacity 0.50:", grepl('opacity="0.50"', result$svg_content), "\n")
