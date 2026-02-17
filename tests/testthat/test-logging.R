# Tests for logging utility functions (R/logging.R)

# cli writes to the "message" connection (stderr), not stdout.
# Use capture.output(type = "message") to capture cli output.

# Helper: capture cli output from the message connection
capture_cli <- function(expr) {
  capture.output(expr, type = "message")
}

# =============================================================================
# 1. Basic output tests for log_info, log_success, log_warn, log_error
# =============================================================================

test_that("log_info produces output", {
  out <- capture_cli(log_info("test message"))
  expect_true(any(grepl("test message", out)))
})

test_that("log_success produces output", {
  out <- capture_cli(log_success("operation done"))
  expect_true(any(grepl("operation done", out)))
})

test_that("log_warn produces output", {
  out <- capture_cli(log_warn("careful here"))
  expect_true(any(grepl("careful here", out)))
})

test_that("log_error produces output", {
  out <- capture_cli(log_error("something failed"))
  expect_true(any(grepl("something failed", out)))
})

# =============================================================================
# 2. CLI glue interpolation
# =============================================================================

test_that("log_info interpolates variables via cli glue syntax", {
  n <- 42
  out <- capture_cli(log_info("Found {n} items"))
  expect_true(any(grepl("42", out)))
})

test_that("log_success interpolates variables", {
  filename <- "output.svg"
  out <- capture_cli(log_success("Saved {filename}"))
  expect_true(any(grepl("output.svg", out)))
})

test_that("log_warn interpolates variables", {
  param_name <- "tabsize"
  out <- capture_cli(log_warn("Missing: {param_name}"))
  expect_true(any(grepl("tabsize", out)))
})

test_that("log_error interpolates variables", {
  err_code <- 404
  out <- capture_cli(log_error("Error code {err_code}"))
  expect_true(any(grepl("404", out)))
})

# =============================================================================
# 3. Functions don't error on various input types
# =============================================================================

test_that("log_info handles plain strings without error", {
  expect_no_error(capture_cli(log_info("simple string")))
})

test_that("log_success handles plain strings without error", {
  expect_no_error(capture_cli(log_success("simple string")))
})

test_that("log_warn handles plain strings without error", {
  expect_no_error(capture_cli(log_warn("simple string")))
})

test_that("log_error handles plain strings without error", {
  expect_no_error(capture_cli(log_error("simple string")))
})

test_that("log functions handle empty strings without error", {
  expect_no_error(capture_cli(log_info("")))
  expect_no_error(capture_cli(log_success("")))
  expect_no_error(capture_cli(log_warn("")))
  expect_no_error(capture_cli(log_error("")))
})

# =============================================================================
# 4. log_header and log_subheader
# =============================================================================

test_that("log_header produces output", {
  out <- capture_cli(log_header("Section Title"))
  expect_true(any(grepl("Section Title", out)))
})

test_that("log_subheader produces output", {
  out <- capture_cli(log_subheader("Subsection"))
  expect_true(any(grepl("Subsection", out)))
})

# =============================================================================
# 5. log_params
# =============================================================================

test_that("log_params displays key-value pairs", {
  items <- list(seed = 42, rings = 3)
  out <- capture_cli(log_params("Parameters", items))
  expect_true(any(grepl("seed", out)))
  expect_true(any(grepl("42", out)))
})

# =============================================================================
# 6. Progress bar functions don't error
# =============================================================================

test_that("log_progress_start returns a progress bar ID", {
  # cli_progress_bar returns a character ID in all modes
  id <- log_progress_start(total = 5)
  expect_type(id, "character")
  expect_true(nzchar(id))
})

test_that("log_progress_update and log_progress_done work in interactive mode", {
  # cli progress bars can only be updated/completed in interactive sessions;
  # in non-interactive (batch) mode the bar is auto-closed and update fails.
  skip_if_not(interactive(), "cli progress update/done requires interactive session")
  id <- log_progress_start(total = 5)
  expect_no_error(log_progress_update(id = id))
  expect_no_error(log_progress_done(id = id))
})
