#!/usr/bin/env Rscript
# Deployment script for jigsawR Shiny app to shinyapps.io
# This script is called by GitHub Actions workflow

# Load cli for logging
if (!requireNamespace("cli", quietly = TRUE)) {
  install.packages("cli")
}

# Source logging utilities
source(file.path("R", "logging.R"))

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

# Set CRAN snapshot repository to yesterday to avoid package sync issues
# This prevents failures when packages released today haven't synced to shinyapps.io mirror
cran_snapshot_date <- Sys.getenv("CRAN_SNAPSHOT_DATE")
if (cran_snapshot_date != "") {
  cran_snapshot_url <- sprintf("https://packagemanager.posit.co/cran/%s", cran_snapshot_date)
  log_info("Using CRAN snapshot from: {.field {cran_snapshot_date}}")
  log_info("CRAN URL: {.url {cran_snapshot_url}}")
  options(repos = c(CRAN = cran_snapshot_url))
} else {
  log_warn("CRAN_SNAPSHOT_DATE not set, using default CRAN mirror")
}

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
# When called from GitHub Actions, use the inst/shiny-app directory
app_dir <- file.path("inst", "shiny-app")

# App name (you can change this if you want a different name on shinyapps.io)
app_name <- "jigsawR"

# Prepare files to deploy
# We need to copy R files to the app directory so they get deployed
r_source_dir <- "R"
r_dest_dir <- file.path(app_dir, "R")

log_subheader("Copying R source files")
log_info("From: {.path {r_source_dir}}")
log_info("To: {.path {r_dest_dir}}")

# Remove old R directory if it exists
if (dir.exists(r_dest_dir)) {
  unlink(r_dest_dir, recursive = TRUE)
}

# Copy R directory to app directory
dir.create(r_dest_dir, recursive = TRUE)
r_files <- list.files(r_source_dir, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
for (r_file in r_files) {
  # Skip archive files
  if (!grepl("scripts_archive", r_file)) {
    rel_path <- sub(paste0("^", r_source_dir, "/"), "", r_file)
    dest_file <- file.path(r_dest_dir, rel_path)
    dest_dir_path <- dirname(dest_file)
    if (!dir.exists(dest_dir_path)) {
      dir.create(dest_dir_path, recursive = TRUE)
    }
    file.copy(r_file, dest_file)
    log_info("Copied: {.file {rel_path}}")
  }
}

# Deploy the application
log_subheader("Deploying application")
log_info("App directory: {.path {app_dir}}")
log_info("App name: {.strong {app_name}}")
log_info("Account: {.strong {account}}")

# Ensure rsconnect uses the CRAN snapshot for dependency resolution
# This is critical for packages that were installed from source in GitHub Actions
if (cran_snapshot_date != "") {
  log_info("Configuring rsconnect to use CRAN snapshot: {.field {cran_snapshot_date}}")
  # Set environment variable that rsconnect will use
  Sys.setenv(R_REPOS = sprintf("https://packagemanager.posit.co/cran/%s", cran_snapshot_date))
}

rsconnect::deployApp(
  appDir = app_dir,
  appName = app_name,
  account = account,
  appFiles = c("app.R", "www/", "R/"),
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
