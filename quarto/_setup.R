# Quarto setup script for jigsawR documentation
# This script loads the package and sets up the environment

# In development mode (DESCRIPTION file exists), always use devtools::load_all()
# to ensure latest source code is loaded including new functions like theme_puzzle()
if (file.exists("DESCRIPTION") && requireNamespace("devtools", quietly = TRUE)) {
  # Development mode - load from source to get latest changes
  suppressMessages(devtools::load_all(path = ".", quiet = TRUE))
} else if (requireNamespace("jigsawR", quietly = TRUE)) {
  # Production mode - use installed package
  suppressPackageStartupMessages(library(jigsawR))
} else {
  # CI/production mode - package should be installed
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
