# Quarto setup script for jigsawR documentation
# This script loads the package and sets up the environment

# Detect CI environment (GitHub Actions, etc.)
is_ci <- nzchar(Sys.getenv("CI")) || nzchar(Sys.getenv("GITHUB_ACTIONS"))

# Find project root (directory containing DESCRIPTION)
# This handles both:
#   - Quarto rendering from project root: source("quarto/_setup.R")
#   - RStudio interactive mode from quarto/: source("_setup.R")
find_project_root <- function() {
  # Check current directory first
  if (file.exists("DESCRIPTION")) {
    return(normalizePath("."))
  }
  # Check parent directory (when running from quarto/)
  if (file.exists("../DESCRIPTION")) {
    return(normalizePath(".."))
  }
  # Check two levels up (edge case)
  if (file.exists("../../DESCRIPTION")) {
    return(normalizePath("../.."))
  }
  # Not found - return NULL
  return(NULL)
}

project_root <- find_project_root()

# Loading strategy:
# - CI: Use installed package (workflow installs via devtools::install_local)
# - Local dev: Use devtools::load_all() for rapid iteration with latest source
if (is_ci) {
  # CI mode - use the installed package (avoids renv/compile conflicts)
  if (requireNamespace("jigsawR", quietly = TRUE)) {
    suppressPackageStartupMessages(library(jigsawR))
  } else {
    stop("jigsawR package not installed in CI. Check workflow installation step.")
  }
} else if (!is.null(project_root) && requireNamespace("devtools", quietly = TRUE)) {
  # Local development mode - load from source to get latest changes
  suppressMessages(devtools::load_all(path = project_root, quiet = TRUE))
} else if (requireNamespace("jigsawR", quietly = TRUE)) {
  # Fallback - use installed package
  suppressPackageStartupMessages(library(jigsawR))
} else {
  stop("jigsawR package not found. Please install with: devtools::install_github('pjt222/jigsawR')")
}

# Load required packages
suppressPackageStartupMessages({
  library(ggplot2)
  library(viridis)
  library(patchwork)
})

# Set knitr options
knitr::opts_chunk$set(
  fig.align = "center",
  comment = "#>",
  collapse = TRUE,
  dpi = 150
)

# Helper function to render API puzzle result as image
# Uses rsvg to convert SVG to PNG for display in Quarto
render_puzzle_preview <- function(result, width = 400) {
  if (!requireNamespace("rsvg", quietly = TRUE)) {
    message("Package 'rsvg' not available. Install with: install.packages('rsvg')")
    return(invisible(NULL))
  }

  # Create temp file for PNG

  tmp_png <- tempfile(fileext = ".png")

  # Convert SVG to PNG
  rsvg::rsvg_png(
    svg = charToRaw(result$svg_content),
    file = tmp_png,
    width = width
  )

  # Display using knitr
  knitr::include_graphics(tmp_png)
}
