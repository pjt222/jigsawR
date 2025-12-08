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
source("R/bezier_utils.R")
segments <- parse_svg_path(piece$path)

cat("Path: ", piece$path, "\n\n")

cat("Segment-by-segment:\n")
for (i in seq_along(segments)) {
  seg <- segments[[i]]
  cat(sprintf("%2d: %s", i, seg$type))
  if (seg$type %in% c("M", "L")) {
    cat(sprintf(" (%.2f, %.2f)", seg$x, seg$y))
  }
  cat("\n")
}

cat("\n=== CORRECTED COUNT ===\n")
cat("M at seg 1\n")
cat("C C C at seg 2-4 (bezier tab 1 = INNER edge)\n")
cat("C C C at seg 5-7 (bezier tab 2 = RIGHT edge)\n")
cat("L at seg 8 (straight line)\n")
cat("C C C at seg 9-11 (bezier tab 3)\n")
cat("Z at seg 12\n\n")

cat("Wait - that's only 3 bezier tabs total!\n")
cat("Where's the 4th edge (LEFT)?\n\n")

cat("Looking at the path string more carefully:\n")
path_str <- piece$path
# Split by command letters
parts <- strsplit(path_str, " ")[[1]]
cat("Path parts:\n")
print(parts)

