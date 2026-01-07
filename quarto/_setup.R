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

# Helper function to render API puzzle result as inline SVG
# Embeds SVG directly in HTML for best quality
render_puzzle_preview <- function(result, width = "100%", max_width = "400px", ...) {
  # Note: ... captures unused arguments for compatibility

  # Get the SVG content
  svg_content <- result$svg_content

  # Wrap in a centered div with size constraints
  html_output <- sprintf(
    '<div style="text-align: center; margin: 1em 0;"><div style="display: inline-block; width: %s; max-width: %s;">%s</div></div>',
    width, max_width, svg_content
  )

  # Use htmltools for proper HTML handling in Quarto
  if (requireNamespace("htmltools", quietly = TRUE)) {
    htmltools::browsable(htmltools::HTML(html_output))
  } else {
    knitr::asis_output(html_output)
  }
}

# Helper function to render multiple puzzles in a grid layout
# Uses htmltools to avoid breaking Quarto's markdown parsing context
# (cat() with results: asis breaks ::: div markers)
render_puzzle_grid <- function(items, ncol = 3, labels = NULL) {
  # items: list of puzzle results (each with $svg_content)
  # labels: character vector of labels for each item (optional)
  # ncol: number of columns in grid

  if (is.null(labels) && !is.null(names(items))) {
    labels <- names(items)
  }

  # Build grid items
  grid_items <- vapply(seq_along(items), function(i) {
    svg_content <- items[[i]]$svg_content
    label <- if (!is.null(labels) && length(labels) >= i) {
      sprintf('<div style="font-weight: bold; margin-bottom: 0.5em;">%s</div>', labels[i])
    } else {
      ""
    }
    sprintf('<div style="text-align: center;">%s%s</div>', label, svg_content)
  }, character(1))

  # Build complete grid HTML (single line to avoid Quarto parsing issues)
  html_output <- sprintf(
    '<div style="display: grid; grid-template-columns: repeat(%d, 1fr); gap: 1em; margin: 1em 0;">%s</div>',
    ncol,
    paste(grid_items, collapse = "")
  )

  # Use htmltools for proper HTML handling in Quarto
  if (requireNamespace("htmltools", quietly = TRUE)) {
    htmltools::browsable(htmltools::HTML(html_output))
  } else {
    knitr::asis_output(html_output)
  }
}
