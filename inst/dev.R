# =============================================================================
# jigsawR Development Entry Point
# =============================================================================
#
# Quick entry script to start the Shiny app with a clean workspace.
# Source this file to reload the package and launch the app.
#
# Usage (from RStudio or R console):
#   source("inst/dev.R")
#
# IMPORTANT: Do not delete this file - it's used for daily development workflow.
# =============================================================================

# Clear workspace for clean testing
rm(list = ls(all.names = TRUE))

# Reload the package with latest changes
devtools::load_all()

# Launch the Shiny app
shiny::runApp("inst/shiny-app")
