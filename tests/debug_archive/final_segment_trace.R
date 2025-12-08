library(devtools)
load_all()

result <- generate_puzzle(
  type = "concentric",
  seed = 42,
  grid = c(2),
  size = c(200),
  offset = 10,
  save_files = FALSE
)

piece <- result$pieces[[2]]

# Manual parse to be 100% sure
path_str <- piece$path
cat("Raw path:\n", path_str, "\n\n")

# Count C commands
c_count <- length(gregexpr("\\bC\\b", path_str)[[1]])
cat("Number of C commands:", c_count, "\n\n")

cat("=== STRUCTURE ===\n")
cat("M 62.99 7.50          <- Start (v1)\n")
cat("C ... C ... C ...     <- Bezier tab 1 (segs 2-4) = INNER\n")
cat("C ... C ... C ...     <- Bezier tab 2 (segs 5-7) = RIGHT\n")
cat("L 112.99 7.50         <- Straight line (seg 8)\n")
cat("C ... C ... C ...     <- Bezier tab 3 (segs 9-11) = OUTER connection\n")
cat("C ...                 <- Single C??? (seg 12)\n")
cat("Z                     <- Close (seg 13)\n\n")

cat("Wait, parser says there are only 12 segments total.\n")
cat("Let me check if the last C is actually multiple C's:\n\n")

# Extract all C commands
library(stringr)
c_positions <- str_locate_all(path_str, "C")[[1]]
cat("C command positions in string:\n")
print(c_positions)
cat("\nTotal C commands:", nrow(c_positions), "\n")

