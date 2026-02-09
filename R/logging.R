#' Logging Utilities
#'
#' Wrapper functions for consistent console logging using the cli package.
#' These functions provide colored, formatted output for better readability
#' during package operations and debugging.
#'
#' @name logging
#' @keywords internal
NULL

#' Log an informational message
#'
#' Uses cli for formatted output with proper variable interpolation.
#' Variables in the message are automatically captured from the calling environment.
#'
#' @param msg Message to log (can use cli inline markup like \code{\{.path\}}, \code{\{.file\}}, etc.)
#' @param .envir Environment for variable lookup (defaults to parent frame)
#' @keywords internal
#' @examples
#' \dontrun{
#' path <- "/home/user"
#' log_info("Working directory: {.path {path}}")
#' }
log_info <- function(msg, .envir = parent.frame()) {
  cli::cli_alert_info(msg, .envir = .envir)
}

#' Log a success message
#'
#' Uses cli for formatted output with proper variable interpolation.
#' Variables in the message are automatically captured from the calling environment.
#'
#' @param msg Message to log (can use cli inline markup)
#' @param .envir Environment for variable lookup (defaults to parent frame)
#' @keywords internal
log_success <- function(msg, .envir = parent.frame()) {
  cli::cli_alert_success(msg, .envir = .envir)
}

#' Log a warning message
#'
#' Uses cli for formatted output with proper variable interpolation.
#' Variables in the message are automatically captured from the calling environment.
#'
#' @param msg Message to log (can use cli inline markup)
#' @param .envir Environment for variable lookup (defaults to parent frame)
#' @keywords internal
log_warn <- function(msg, .envir = parent.frame()) {
  cli::cli_alert_warning(msg, .envir = .envir)
}

#' Log an error message
#'
#' Uses cli for formatted output with proper variable interpolation.
#' Variables in the message are automatically captured from the calling environment.
#'
#' @param msg Message to log (can use cli inline markup)
#' @param .envir Environment for variable lookup (defaults to parent frame)
#' @keywords internal
log_error <- function(msg, .envir = parent.frame()) {
  cli::cli_alert_danger(msg, .envir = .envir)
}

#' Start a progress bar
#'
#' @param total Total number of steps
#' @param format Progress bar format string
#' @param ... Additional arguments passed to cli::cli_progress_bar
#' @return Progress bar ID
#' @keywords internal
log_progress_start <- function(total, format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total}", ...) {
  cli::cli_progress_bar(total = total, format = format, ...)
}

#' Update progress bar
#'
#' @param id Progress bar ID (optional, uses current if not specified)
#' @param ... Additional arguments passed to cli::cli_progress_update
#' @keywords internal
log_progress_update <- function(id = NULL, ...) {
  cli::cli_progress_update(id = id, ...)
}

#' Complete and close progress bar
#'
#' @param id Progress bar ID (optional, uses current if not specified)
#' @param ... Additional arguments passed to cli::cli_progress_done
#' @keywords internal
log_progress_done <- function(id = NULL, ...) {
  cli::cli_progress_done(id = id, ...)
}

#' Log a formatted message with key-value pairs
#'
#' Useful for displaying parameters or configuration values
#'
#' @param title Section title
#' @param items Named list or vector of items to display
#' @keywords internal
log_params <- function(title, items) {
  cli::cli_h2(title)
  for (name in names(items)) {
    cli::cli_li("{.field {name}}: {.val {items[[name]]}}")
  }
}

#' Log a section header
#'
#' @param text Header text
#' @param .envir Environment for variable lookup (defaults to parent frame)
#' @keywords internal
log_header <- function(text, .envir = parent.frame()) {
  cli::cli_h1(text, .envir = .envir)
}

#' Log a subsection header
#'
#' @param text Subheader text
#' @param .envir Environment for variable lookup (defaults to parent frame)
#' @keywords internal
log_subheader <- function(text, .envir = parent.frame()) {
  cli::cli_h2(text, .envir = .envir)
}
