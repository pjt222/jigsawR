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

# Get app directory (this script is in inst/shiny-app/)
app_dir <- dirname(normalizePath(sys.frame(1)$ofile))

# App name (you can change this if you want a different name on shinyapps.io)
app_name <- "jigsawR"

# Deploy the application
cat("Deploying app from:", app_dir, "\n")
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
