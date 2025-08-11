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
  
  # Check if shiny is installed
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required to run the app. Please install it with: install.packages('shiny')")
  }
  
  if (!requireNamespace("shinyjs", quietly = TRUE)) {
    stop("Package 'shinyjs' is required to run the app. Please install it with: install.packages('shinyjs')")
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
  message("Launching jigsawR Shiny Application...")
  message("App directory: ", app_dir)
  
  if (is.null(port)) {
    message("Running on a random port...")
  } else {
    message("Running on port: ", port)
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
#' @export
check_app_dependencies <- function() {
  required_packages <- c("shiny", "shinyjs")
  installed <- sapply(required_packages, requireNamespace, quietly = TRUE)
  
  if (all(installed)) {
    message("✓ All Shiny app dependencies are installed")
    return(invisible(TRUE))
  } else {
    missing <- required_packages[!installed]
    message("✗ Missing packages: ", paste(missing, collapse = ", "))
    message("Install with: install.packages(c(", 
           paste0('"', missing, '"', collapse = ", "), "))")
    return(invisible(FALSE))
  }
}