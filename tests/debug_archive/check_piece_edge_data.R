# Check what edge information is available in the piece object

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

cat("=== PIECE OBJECT STRUCTURE ===\n")
cat("Available fields:\n")
print(names(piece))
cat("\n")

for (field in names(piece)) {
  cat(sprintf("$%s:\n", field))
  if (field == "path") {
    cat("  [SVG path string - omitted]\n")
  } else {
    print(piece[[field]])
  }
  cat("\n")
}

cat("=== CHECKING FOR EDGE METADATA ===\n")
cat("Does piece have edge_list? ", !is.null(piece$edge_list), "\n")
cat("Does piece have edges? ", !is.null(piece$edges), "\n")
cat("Does piece have piece_edges? ", !is.null(piece$piece_edges), "\n")

