# Check the actual puzzle structure

source("R/rectangular_puzzle.R")

# Generate a 2x2 puzzle
puzzle <- generate_jigsaw_svg(seed = 42, xn = 2, yn = 2, width = 200, height = 200)

cat("Checking puzzle structure...\n")
cat("Names in puzzle object:\n")
print(names(puzzle))

# Check the SVG content
cat("\nFirst 500 characters of SVG:\n")
cat(substr(puzzle$svg, 1, 500), "...\n\n")

# Look for the actual paths in the SVG
svg_lines <- strsplit(puzzle$svg, "\n")[[1]]
path_lines <- svg_lines[grepl("<path", svg_lines)]

cat("Found", length(path_lines), "path elements\n\n")

# Extract and show the paths
for (i in seq_along(path_lines)) {
  cat("Path", i, ":\n")
  # Extract the d attribute
  d_match <- regmatches(path_lines[i], regexpr('d="([^"]+)"', path_lines[i]))
  if (length(d_match) > 0) {
    d_content <- gsub('d="([^"]+)"', "\\1", d_match)
    cat(substr(d_content, 1, 200), "...\n\n")
  }
}

# Let's also check the raw path data
cat("Checking puzzle paths directly:\n")
if (!is.null(puzzle$horizontal)) {
  cat("Horizontal path:\n", puzzle$horizontal, "\n\n")
}
if (!is.null(puzzle$vertical)) {
  cat("Vertical path:\n", puzzle$vertical, "\n\n")
}
if (!is.null(puzzle$border)) {
  cat("Border path:\n", substr(puzzle$border, 1, 100), "...\n\n")
}