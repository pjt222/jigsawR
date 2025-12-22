# Quarto setup script for jigsawR documentation
# This script loads the package and sets up the environment

# Detect CI environment (GitHub Actions, etc.)
is_ci <- nzchar(Sys.getenv("CI")) || nzchar(Sys.getenv("GITHUB_ACTIONS"))

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
} else if (file.exists("DESCRIPTION") && requireNamespace("devtools", quietly = TRUE)) {
  # Local development mode - load from source to get latest changes

  suppressMessages(devtools::load_all(path = ".", quiet = TRUE))
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
