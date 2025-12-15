# Minimal test for Random Shape puzzles
cat("Loading jigsawR...\n")
suppressPackageStartupMessages(library(jigsawR))

cat("Checking RCDT...\n")
has_rcdt <- requireNamespace("RCDT", quietly = TRUE)
cat("RCDT available:", has_rcdt, "\n")

if (!has_rcdt) {
  cat("Installing RCDT...\n")
  install.packages("RCDT", repos = "https://cloud.r-project.org")
}

cat("\nTesting random puzzle generation...\n")
tryCatch({
  result <- generate_puzzle(
    type = "random",
    seed = 42,
    grid = c(5),
    size = c(100, 100),
    offset = 0,
    save_files = FALSE
  )
  cat("SUCCESS: Generated", length(result$pieces), "pieces\n")
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
  cat("Call:", paste(deparse(e$call), collapse = "\n"), "\n")
})

cat("\nDone.\n")
