library(jigsawR)

# Test hexagonal
hex_result <- generate_puzzle(type = "hexagonal", seed = 42, grid = c(2), size = c(200), offset = 0)

cat("Hexagonal piece 1 fused_edges:\n")
print(names(hex_result$pieces[[1]]$fused_edges))

# Test concentric
conc_result <- generate_puzzle(type = "concentric", seed = 42, grid = c(2), size = c(200), offset = 0)

cat("\nConcentric piece 1 (center) fused_edges:\n")
print(names(conc_result$pieces[[1]]$fused_edges))

cat("\nConcentric piece 2 (trapezoid) fused_edges:\n")
print(names(conc_result$pieces[[2]]$fused_edges))
