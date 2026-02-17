#' Launch jigsawR Shiny Application
#' 
#' Opens an interactive Shiny application for generating jigsaw puzzles
#' with real-time preview and download capabilities.
#' 
#' @param port The port to run the app on (optional, defaults to a random port)
#' @param launch.browser Whether to launch the app in a browser (default TRUE)
#' @param host The host address (default "127.0.0.1")
#' @param ... Additional arguments passed to shiny::runApp()
#' 
#' @return Runs the Shiny application
#' 
#' @examples
#' \dontrun{
#' # Launch the app
#' launch_jigsaw_app()
#' 
#' # Launch on a specific port
#' launch_jigsaw_app(port = 3838)
#' 
#' # Launch without opening browser
#' launch_jigsaw_app(launch.browser = FALSE)
#' }
#' 
#' @export
launch_jigsaw_app <- function(port = NULL, 
                             launch.browser = TRUE,
                             host = "127.0.0.1",
                             ...) {
  
  # Check required Shiny app packages (in Suggests, not Imports)
  shiny_pkgs <- c("shiny", "bslib", "bsicons", "shinyjs", "colourpicker", "waiter")
  missing_pkgs <- shiny_pkgs[!vapply(shiny_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    cli::cli_abort(c(
      "The Shiny app requires additional packages that are not installed.",
      "i" = "Missing: {.pkg {missing_pkgs}}",
      "i" = 'Install with: {.code install.packages(c({paste0(\'"\', missing_pkgs, \'"\', collapse = ", ")}))}'
    ))
  }
  
  # Find the app directory
  app_dir <- system.file("shiny-app", package = "jigsawR")
  
  if (app_dir == "") {
    # Try development mode path
    if (file.exists("inst/shiny-app/app.R")) {
      app_dir <- "inst/shiny-app"
    } else if (file.exists("shiny-app/app.R")) {
      app_dir <- "shiny-app"
    } else {
      stop("Could not find the Shiny app directory. Make sure the jigsawR package is properly installed.")
    }
  }
  
  # Inform user
  cli::cli_inform("Launching jigsawR Shiny Application...")
  cli::cli_inform("App directory: {.path {app_dir}}")

  if (is.null(port)) {
    cli::cli_inform("Running on a random port...")
  } else {
    cli::cli_inform("Running on port: {port}")
  }
  
  # Run the app
  shiny::runApp(
    appDir = app_dir,
    port = port,
    launch.browser = launch.browser,
    host = host,
    ...
  )
}

#' Launch jigsawR Shiny App (alias)
#' 
#' Alias for launch_jigsaw_app() for convenience
#' 
#' @param ... Arguments passed to launch_jigsaw_app()
#' @export
jigsawR_app <- function(...) {
  launch_jigsaw_app(...)
}

#' Check Shiny App Dependencies
#' 
#' Checks if all required packages for the Shiny app are installed
#' 
#' @return Logical indicating if all dependencies are met
#'
#' @examples
#' check_app_dependencies()
#'
#' @export
check_app_dependencies <- function() {
  required_packages <- c("shiny", "bslib", "bsicons", "shinyjs", "colourpicker", "waiter")
  optional_packages <- c("zip", "rsvg", "magick")  # For downloads and image processing

  required_installed <- sapply(required_packages, requireNamespace, quietly = TRUE)
  optional_installed <- sapply(optional_packages, requireNamespace, quietly = TRUE)

  if (all(required_installed)) {
    cli::cli_alert_success("All required Shiny app dependencies are installed")

    if (!all(optional_installed)) {
      missing_optional <- optional_packages[!optional_installed]
      cli::cli_alert_warning("Optional packages for enhanced features: {.pkg {missing_optional}}")
      install_cmd <- paste0('install.packages(c(', paste0('"', missing_optional, '"', collapse = ", "), '))')
      cli::cli_inform("Install with: {.code {install_cmd}}")
      cli::cli_inform("Note: Required for downloading individual pieces as ZIP files")
    }

    return(invisible(TRUE))
  } else {
    missing <- required_packages[!required_installed]
    cli::cli_alert_danger("Missing required packages: {.pkg {missing}}")
    install_cmd <- paste0('install.packages(c(', paste0('"', missing, '"', collapse = ", "), '))')
    cli::cli_inform("Install with: {.code {install_cmd}}")
    return(invisible(FALSE))
  }
}