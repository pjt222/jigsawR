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
#' @param msg Message to log
#' @param ... Additional arguments passed to cli::cli_alert_info
#' @keywords internal
log_info <- function(msg, ...) {
  cli::cli_alert_info(msg, ...)
}

#' Log a success message
#'
#' @param msg Message to log
#' @param ... Additional arguments passed to cli::cli_alert_success
#' @keywords internal
log_success <- function(msg, ...) {
  cli::cli_alert_success(msg, ...)
}

#' Log a warning message
#'
#' @param msg Message to log
#' @param ... Additional arguments passed to cli::cli_alert_warning
#' @keywords internal
log_warn <- function(msg, ...) {
  cli::cli_alert_warning(msg, ...)
}

#' Log an error message
#'
#' @param msg Message to log
#' @param ... Additional arguments passed to cli::cli_alert_danger
#' @keywords internal
log_error <- function(msg, ...) {
  cli::cli_alert_danger(msg, ...)
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
#' @keywords internal
log_header <- function(text) {
  cli::cli_h1(text)
}

#' Log a subsection header
#'
#' @param text Subheader text
#' @keywords internal
log_subheader <- function(text) {
  cli::cli_h2(text)
}
