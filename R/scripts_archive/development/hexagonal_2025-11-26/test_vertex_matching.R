#!/usr/bin/env Rscript

# Test the vertex matching logic
v1_p2 <- c(51.96, -10.00)  # Piece 2 V5
v2_p2 <- c(51.96, 10.00)   # Piece 2 V0 (wraps around)

v1_p8 <- c(51.96, 10.00)   # Piece 8 V2
v2_p8 <- c(51.96, -10.00)  # Piece 8 V3

tol <- 0.01

cat("Piece 2 side 5: (", v1_p2[1], ",", v1_p2[2], ") -> (", v2_p2[1], ",", v2_p2[2], ")\n")
cat("Piece 8 side 2: (", v1_p8[1], ",", v1_p8[2], ") -> (", v2_p8[1], ",", v2_p8[2], ")\n\n")

# Check if they match in reverse
match1 <- all(abs(v1_p2 - v2_p8) < tol) && all(abs(v2_p2 - v1_p8) < tol)
cat("Reverse match (v1_p2 == v2_p8 AND v2_p2 == v1_p8):", match1, "\n")

# Check if they match forward  
match2 <- all(abs(v1_p2 - v1_p8) < tol) && all(abs(v2_p2 - v2_p8) < tol)
cat("Forward match (v1_p2 == v1_p8 AND v2_p2 == v2_p8):", match2, "\n")

cat("\nThe condition in code:\n")
cat("((v1 - test_v2 < tol) && (v2 - test_v1 < tol)) ||\n")
cat("((v1 - test_v1 < tol) && (v2 - test_v2 < tol))\n\n")

cat("Should be TRUE for reverse match!\n")
