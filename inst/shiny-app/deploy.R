#!/usr/bin/env Rscript
# Deployment script for jigsawR Shiny app to shinyapps.io
# This script is called by GitHub Actions workflow

# Get credentials from environment variables
account <- Sys.getenv("SHINYAPPS_ACCOUNT")
token <- Sys.getenv("SHINYAPPS_TOKEN")
secret <- Sys.getenv("SHINYAPPS_SECRET")

# Validate credentials
if (account == "" || token == "" || secret == "") {
  stop("Missing required environment variables: SHINYAPPS_ACCOUNT, SHINYAPPS_TOKEN, or SHINYAPPS_SECRET")
}

# Load rsconnect package
if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect")
}
library(rsconnect)

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

cat("Copying R source files...\n")
cat("From:", r_source_dir, "\n")
cat("To:", r_dest_dir, "\n")

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
    cat("  Copied:", rel_path, "\n")
  }
}

# Deploy the application
cat("\nDeploying app from:", app_dir, "\n")
cat("App name:", app_name, "\n")
cat("Account:", account, "\n")

rsconnect::deployApp(
  appDir = app_dir,
  appName = app_name,
  account = account,
  forceUpdate = TRUE,
  launch.browser = FALSE
)

cat("\nDeployment successful!\n")
cat("App URL: https://", account, ".shinyapps.io/", app_name, "/\n", sep = "")
