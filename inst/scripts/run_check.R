# Run R CMD check for jigsawR
# Usage: Rscript inst/scripts/run_check.R
#
# Note: If .claude/agents and .claude/skills symlinks cause build failures
# on Windows, remove them before running this script and restore after:
#   rm .claude/agents .claude/skills
#   Rscript inst/scripts/run_check.R
#   ln -s /home/phtho/.claude/agents .claude/agents
#   ln -s /home/phtho/.claude/skills .claude/skills

cat("Running devtools::check()...\n")

results <- devtools::check(args = c("--no-manual", "--as-cran"))

cat("\n=== CHECK RESULTS ===\n")
cat("Errors:", length(results$errors), "\n")
cat("Warnings:", length(results$warnings), "\n")
cat("Notes:", length(results$notes), "\n")

if (length(results$errors) > 0) {
  cat("\n--- ERRORS ---\n")
  for (e in results$errors) cat(e, "\n\n")
}
if (length(results$warnings) > 0) {
  cat("\n--- WARNINGS ---\n")
  for (w in results$warnings) cat(w, "\n\n")
}
if (length(results$notes) > 0) {
  cat("\n--- NOTES ---\n")
  for (n in results$notes) cat(n, "\n\n")
}
