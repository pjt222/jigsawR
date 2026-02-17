# Tests for logging utility functions (R/logging.R)

# =============================================================================
# 1. Basic output tests for log_info, log_success, log_warn, log_error
# =============================================================================

test_that("log_info produces output", {
  expect_output(log_info("test message"), "test message")
})

test_that("log_success produces output", {
  expect_output(log_success("operation done"), "operation done")
})

test_that("log_warn produces output", {
  expect_output(log_warn("careful here"), "careful here")
})

test_that("log_error produces output", {
  expect_output(log_error("something failed"), "something failed")
})

# =============================================================================
# 2. CLI glue interpolation
# =============================================================================

test_that("log_info interpolates variables via cli glue syntax", {
  n <- 42
  expect_output(log_info("Found {n} items"), "42")
})

test_that("log_success interpolates variables", {
  filename <- "output.svg"
  expect_output(log_success("Saved {filename}"), "output.svg")
})

test_that("log_warn interpolates variables", {
  param_name <- "tabsize"
  expect_output(log_warn("Missing: {param_name}"), "tabsize")
})

test_that("log_error interpolates variables", {
  err_code <- 404
  expect_output(log_error("Error code {err_code}"), "404")
})

# =============================================================================
# 3. Functions don't error on various input types
# =============================================================================

test_that("log_info handles plain strings without error", {
  expect_no_error(capture.output(log_info("simple string")))
})
test_that("log_success handles plain strings without error", {

  expect_no_error(capture.output(log_success("simple string")))
})

test_that("log_warn handles plain strings without error", {
  expect_no_error(capture.output(log_warn("simple string")))
})

test_that("log_error handles plain strings without error", {
  expect_no_error(capture.output(log_error("simple string")))
})

test_that("log functions handle empty strings without error", {
  expect_no_error(capture.output(log_info("")))
  expect_no_error(capture.output(log_success("")))
  expect_no_error(capture.output(log_warn("")))
  expect_no_error(capture.output(log_error("")))
})

# =============================================================================
# 4. log_header and log_subheader
# =============================================================================

test_that("log_header produces output", {
  expect_output(log_header("Section Title"), "Section Title")
})

test_that("log_subheader produces output", {
  expect_output(log_subheader("Subsection"), "Subsection")
})

# =============================================================================
# 5. log_params
# =============================================================================

test_that("log_params displays key-value pairs", {
  items <- list(seed = 42, rings = 3)
  expect_output(log_params("Parameters", items), "seed")
  expect_output(log_params("Parameters", items), "42")
})

# =============================================================================
# 6. Progress bar functions don't error
# =============================================================================

test_that("log_progress functions execute without error", {
  expect_no_error({
    id <- log_progress_start(total = 5)
    log_progress_update(id = id)
    log_progress_done(id = id)
  })
})
