#!/usr/bin/env Rscript
# Deployment script for jigsawR Shiny app to shinyapps.io
# This script is called by GitHub Actions workflow
#
# Strategy: Deploy jigsawR as an installed package so Rcpp C++ code is compiled
# on shinyapps.io, providing ~27x faster RNG performance.

# Load cli for logging
if (!requireNamespace("cli", quietly = TRUE)) {
  install.packages("cli")
}
library(cli)

log_header <- function(msg) cli::cli_h1(msg)
log_subheader <- function(msg) cli::cli_h2(msg)
log_info <- function(msg) cli::cli_alert_info(msg)
log_success <- function(msg) cli::cli_alert_success(msg)
log_warn <- function(msg) cli::cli_alert_warning(msg)
log_error <- function(msg) cli::cli_alert_danger(msg)

log_header("jigsawR Deployment")

# Get credentials from environment variables
account <- Sys.getenv("SHINYAPPS_ACCOUNT")
token <- Sys.getenv("SHINYAPPS_TOKEN")
secret <- Sys.getenv("SHINYAPPS_SECRET")

# Validate credentials
if (account == "" || token == "" || secret == "") {
  log_error("Missing required environment variables: SHINYAPPS_ACCOUNT, SHINYAPPS_TOKEN, or SHINYAPPS_SECRET")
  stop("Missing required environment variables")
}

log_success("Environment variables loaded successfully")

# Load rsconnect package
if (!requireNamespace("rsconnect", quietly = TRUE)) {
  log_info("Installing rsconnect package...")
  install.packages("rsconnect")
}
library(rsconnect)
log_info("rsconnect package loaded")

# Set account info
rsconnect::setAccountInfo(
  name = account,
  token = token,
  secret = secret
)

# Get app directory
app_dir <- file.path("inst", "shiny-app")
app_name <- "jigsawR"

# =============================================================================
# Create DESCRIPTION file for shinyapps.io to install jigsawR from GitHub
# This ensures the Rcpp C++ code gets compiled on the server
# =============================================================================
log_subheader("Creating package dependencies")

# Create a minimal DESCRIPTION that lists jigsawR as a GitHub dependency
# This tells shinyapps.io to install jigsawR from GitHub (with compiled C++)
app_desc <- c(
  "Title: jigsawR Shiny App",
  "Version: 0.1.0",
  "Imports:",
  "    shiny,",
  "    bslib,",
  "    shinyjs,",
  "    waiter,",
  "    cli,",
  "    colourpicker,",
  "    bsicons,",
  "    jigsawR",
  "Remotes: pjt222/jigsawR"
)

writeLines(app_desc, file.path(app_dir, "DESCRIPTION"))
log_success("Created DESCRIPTION with jigsawR as GitHub dependency")

# Also copy R files as fallback (in case package installation fails)
log_subheader("Copying R source files (fallback)")
r_source_dir <- "R"
r_dest_dir <- file.path(app_dir, "R")

if (dir.exists(r_dest_dir)) {
  unlink(r_dest_dir, recursive = TRUE)
}

dir.create(r_dest_dir, recursive = TRUE)
r_files <- list.files(r_source_dir, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
for (r_file in r_files) {
  if (!grepl("scripts_archive", r_file)) {
    rel_path <- sub(paste0("^", r_source_dir, "/"), "", r_file)
    dest_file <- file.path(r_dest_dir, rel_path)
    dest_dir_path <- dirname(dest_file)
    if (!dir.exists(dest_dir_path)) {
      dir.create(dest_dir_path, recursive = TRUE)
    }
    file.copy(r_file, dest_file)
  }
}
log_info("Copied R files as fallback")

# Deploy the application
log_subheader("Deploying application")
log_info("App directory: {.path {app_dir}}")
log_info("App name: {.strong {app_name}}")
log_info("Account: {.strong {account}}")
log_info("Note: shinyapps.io will install jigsawR from GitHub and compile C++ code")

rsconnect::deployApp(
  appDir = app_dir,
  appName = app_name,
  account = account,
  appFiles = c("app.R", "www/", "R/", "DESCRIPTION"),
  forceUpdate = TRUE,
  launch.browser = FALSE,
  lint = FALSE,
  metadata = list(
    appMode = "shiny",
    contentCategory = "application"
  )
)

log_success("Deployment successful!")
log_info("App URL: {.url https://{account}.shinyapps.io/{app_name}/}")
log_info("Check logs for: 'Rcpp C++ optimizations: ENABLED'")
