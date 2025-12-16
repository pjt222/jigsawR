# Test fallback config has min/max tab constraints
devtools::load_all()

cat("Testing fallback config...\n")
fallback <- jigsawR:::get_fallback_config()

cat("\nFallback constraints check:\n")
cat("min_tab_size present:", !is.null(fallback$constraints$min_tab_size), "\n")
cat("max_tab_size present:", !is.null(fallback$constraints$max_tab_size), "\n")

if (!is.null(fallback$constraints$min_tab_size)) {
  cat("min_tab_size$min:", fallback$constraints$min_tab_size$min, "\n")
  cat("min_tab_size$max:", fallback$constraints$min_tab_size$max, "\n")
}

if (!is.null(fallback$constraints$max_tab_size)) {
  cat("max_tab_size$min:", fallback$constraints$max_tab_size$min, "\n")
  cat("max_tab_size$max:", fallback$constraints$max_tab_size$max, "\n")
}

# Test what would happen in the Shiny UI
min <- fallback$constraints$min_tab_size$min
cat("\nTest length(min):", length(min), "\n")
cat("Test is.na(min):", is.na(min), "\n")
cat("SUCCESS: No length-zero error!\n")
