# Quarto setup script for jigsawR documentation
# This script loads the package and sets up the environment

# Try to load installed package first, fall back to devtools::load_all()
if (requireNamespace("jigsawR", quietly = TRUE)) {
  suppressPackageStartupMessages(library(jigsawR))
} else if (requireNamespace("devtools", quietly = TRUE)) {
  # Development mode - load from source
  suppressMessages(devtools::load_all(path = ".", quiet = TRUE))
} else {
  # CI/production mode - package should be installed
  stop("jigsawR package not found. Please install with: devtools::install_github('pjt222/jigsawR')")
}

# Load required packages
suppressPackageStartupMessages({
  library(ggplot2)
  library(viridis)
})

# Set knitr options
knitr::opts_chunk$set(
  fig.align = "center",
  comment = "#>",
  collapse = TRUE,
  dpi = 150
)
