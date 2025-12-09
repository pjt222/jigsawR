# Baseline performance benchmark for jigsawR
# Run this to establish current performance metrics

# Load package from source
devtools::load_all(quiet = TRUE)

cat("=== jigsawR Performance Baseline ===\n\n")

# Helper to time operations
time_operation <- function(name, expr, times = 5) {
  cat(sprintf("Benchmarking: %s (%d iterations)\n", name, times))

  # Warm-up run
  suppressMessages(eval(expr))

  # Timed runs
  timings <- numeric(times)
  for (i in seq_len(times)) {
    start <- Sys.time()
    suppressMessages(eval(expr))
    end <- Sys.time()
    timings[i] <- as.numeric(difftime(end, start, units = "secs"))
  }

  result <- list(
    name = name,
    mean = mean(timings),
    sd = sd(timings),
    min = min(timings),
    max = max(timings)
  )

  cat(sprintf("  Mean: %.3fs (SD: %.3fs, Range: %.3f-%.3fs)\n\n",
              result$mean, result$sd, result$min, result$max))

  invisible(result)
}

results <- list()

# === RECTANGULAR PUZZLES ===
cat("--- Rectangular Puzzles ---\n")

results$rect_2x2 <- time_operation("Rectangular 2x2", quote({
  generate_puzzle(type = "rectangular", grid = c(2, 2), seed = 42, save_files = FALSE)
}))

results$rect_4x4 <- time_operation("Rectangular 4x4", quote({
  generate_puzzle(type = "rectangular", grid = c(4, 4), seed = 42, save_files = FALSE)
}))

results$rect_6x6 <- time_operation("Rectangular 6x6", quote({
  generate_puzzle(type = "rectangular", grid = c(6, 6), seed = 42, save_files = FALSE)
}))

results$rect_8x8 <- time_operation("Rectangular 8x8", quote({
  generate_puzzle(type = "rectangular", grid = c(8, 8), seed = 42, save_files = FALSE)
}))

# === HEXAGONAL PUZZLES ===
cat("--- Hexagonal Puzzles ---\n")

results$hex_2 <- time_operation("Hexagonal 2 rings (7 pieces)", quote({
  generate_puzzle(type = "hexagonal", grid = c(2), size = c(200), seed = 42, save_files = FALSE)
}))

results$hex_3 <- time_operation("Hexagonal 3 rings (19 pieces)", quote({
  generate_puzzle(type = "hexagonal", grid = c(3), size = c(200), seed = 42, save_files = FALSE)
}))

results$hex_4 <- time_operation("Hexagonal 4 rings (37 pieces)", quote({
  generate_puzzle(type = "hexagonal", grid = c(4), size = c(200), seed = 42, save_files = FALSE)
}))

results$hex_5 <- time_operation("Hexagonal 5 rings (61 pieces)", quote({
  generate_puzzle(type = "hexagonal", grid = c(5), size = c(200), seed = 42, save_files = FALSE)
}))

# === CONCENTRIC PUZZLES ===
cat("--- Concentric Puzzles ---\n")

results$conc_2 <- time_operation("Concentric 2 rings (7 pieces)", quote({
  generate_puzzle(type = "concentric", grid = c(2), size = c(200), seed = 42, save_files = FALSE)
}))

results$conc_3 <- time_operation("Concentric 3 rings (19 pieces)", quote({
  generate_puzzle(type = "concentric", grid = c(3), size = c(200), seed = 42, save_files = FALSE)
}))

results$conc_4 <- time_operation("Concentric 4 rings (37 pieces)", quote({
  generate_puzzle(type = "concentric", grid = c(4), size = c(200), seed = 42, save_files = FALSE)
}))

# === FUSION OPERATIONS ===
cat("--- Fusion Operations ---\n")

results$rect_fusion <- time_operation("Rectangular 4x4 with fusion", quote({
  generate_puzzle(type = "rectangular", grid = c(4, 4), seed = 42,
                  fusion_groups = "(1,2,5,6),(11,12,15,16)", save_files = FALSE)
}))

results$hex_fusion <- time_operation("Hexagonal 3 rings with fusion", quote({
  generate_puzzle(type = "hexagonal", grid = c(3), size = c(200), seed = 42,
                  fusion_groups = "(1,2,3),(7,8,9)", save_files = FALSE)
}))

# === SUMMARY ===
cat("\n=== SUMMARY ===\n")
cat(sprintf("%-35s %10s %10s\n", "Operation", "Mean (s)", "Pieces"))
cat(paste(rep("-", 57), collapse = ""), "\n")

summary_data <- list(
  c("Rectangular 2x2", results$rect_2x2$mean, 4),
  c("Rectangular 4x4", results$rect_4x4$mean, 16),
  c("Rectangular 6x6", results$rect_6x6$mean, 36),
  c("Rectangular 8x8", results$rect_8x8$mean, 64),
  c("Hexagonal 2 rings", results$hex_2$mean, 7),
  c("Hexagonal 3 rings", results$hex_3$mean, 19),
  c("Hexagonal 4 rings", results$hex_4$mean, 37),
  c("Hexagonal 5 rings", results$hex_5$mean, 61),
  c("Concentric 2 rings", results$conc_2$mean, 7),
  c("Concentric 3 rings", results$conc_3$mean, 19),
  c("Concentric 4 rings", results$conc_4$mean, 37),
  c("Rect 4x4 + fusion", results$rect_fusion$mean, 16),
  c("Hex 3 + fusion", results$hex_fusion$mean, 19)
)

for (row in summary_data) {
  cat(sprintf("%-35s %10.3f %10s\n", row[1], as.numeric(row[2]), row[3]))
}

cat("\nBaseline benchmark complete.\n")
