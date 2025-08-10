# Activate renv for reproducible package development
source("renv/activate.R")

# Load development tools and provide helpful messages
if (requireNamespace("devtools", quietly = TRUE)) {
  # Automatically provide helpful development tips
  if (interactive() && file.exists("DESCRIPTION")) {
    cat("📦 jigsawR package development environment\n")
    cat("💡 Use devtools::load_all() to load package functions\n")
    cat("🔧 Use devtools::check() to check package\n")
    cat("📋 Use devtools::test() to run tests\n")
    cat("📖 Use devtools::document() to update documentation\n\n")
  }
}

# Set options for package development
options(
  repos = c(CRAN = "https://cloud.r-project.org/"),
  renv.config.auto.snapshot = TRUE,
  devtools.name = "Philipp Thoss",
  devtools.desc.author = 'person("Philipp", "Thoss", email = "ph.thoss@gmx.de", role = c("aut", "cre"), comment = c(ORCID = "0000-0002-4672-2792"))',
  devtools.desc.license = "CC0"
)
