# Test script for jigsawR Shiny Application
# This script verifies the Shiny app can be loaded and provides launch instructions

cat("=== Testing jigsawR Shiny Application ===\n\n")

# Check if running from package root
if (file.exists("inst/shiny-app/app.R")) {
  cat("✓ Shiny app directory found\n")
} else {
  cat("✗ Could not find Shiny app directory\n")
  cat("  Make sure you're running from the package root directory\n")
}

# Check dependencies
cat("\nChecking dependencies:\n")

check_package <- function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat("  ✓", pkg, "is installed\n")
    return(TRUE)
  } else {
    cat("  ✗", pkg, "is NOT installed\n")
    return(FALSE)
  }
}

shiny_ok <- check_package("shiny")
shinyjs_ok <- check_package("shinyjs")

# Check core package functions are available
cat("\nChecking core functions:\n")
if (file.exists("R/puzzle_core_clean.R")) {
  source("R/rectangular_puzzle.R")
  source("R/puzzle_core_clean.R")
  source("R/jigsawR_clean.R")
  source("R/puzzle_separation.R")
  cat("  ✓ Core puzzle functions loaded\n")
} else {
  cat("  ✗ Could not load core functions\n")
}

# Test basic puzzle generation
cat("\nTesting puzzle generation:\n")
tryCatch({
  test_puzzle <- generate_puzzle_core(
    seed = 1234,
    grid = c(2, 2),
    size = c(100, 100)
  )
  if (!is.null(test_puzzle)) {
    cat("  ✓ Basic puzzle generation works\n")
  }
}, error = function(e) {
  cat("  ✗ Error in puzzle generation:", e$message, "\n")
})

# Provide launch instructions
cat("\n=== Launch Instructions ===\n")

if (shiny_ok && shinyjs_ok) {
  cat("\nAll dependencies are installed. You can launch the app with:\n\n")
  cat("  # Option 1: Using the launch function\n")
  cat("  source('R/launch_app.R')\n")
  cat("  launch_jigsaw_app()\n\n")
  
  cat("  # Option 2: Direct launch\n")
  cat("  shiny::runApp('inst/shiny-app')\n\n")
  
  cat("  # Option 3: After installing the package\n")
  cat("  library(jigsawR)\n")
  cat("  launch_jigsaw_app()\n\n")
  
  cat("The app will open in your default web browser.\n")
  cat("Use Ctrl+C in the R console to stop the app.\n")
  
} else {
  cat("\n✗ Missing dependencies. Install them with:\n\n")
  missing <- c()
  if (!shiny_ok) missing <- c(missing, "shiny")
  if (!shinyjs_ok) missing <- c(missing, "shinyjs")
  cat("  install.packages(c(", paste0('"', missing, '"', collapse = ", "), "))\n\n")
}

cat("\n=== App Features ===\n")
cat("• Interactive parameter controls\n")
cat("• Real-time puzzle preview\n")
cat("• Multiple output modes (complete/individual/separated)\n")
cat("• SVG download for laser cutting\n")
cat("• Color schemes and styling options\n")
cat("• Responsive design with custom CSS\n")

cat("\n=== Testing Complete ===\n")