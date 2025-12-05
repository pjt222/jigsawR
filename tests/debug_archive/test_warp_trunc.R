# Test do_warp and do_trunc parameters in separated hexagonal mode
# Run with: Rscript tests/test_warp_trunc.R

cat("=== Testing do_warp and do_trunc in Separated Mode ===\n\n")

# Source all required files
source("R/logging.R")
source("R/config_utils.R")
source("R/hexagonal_puzzle.R")
source("R/hexagonal_separation.R")

# Test all combinations
test_cases <- list(
  list(do_warp = FALSE, do_trunc = FALSE, name = "Neither"),
  list(do_warp = TRUE, do_trunc = FALSE, name = "Warp only"),
  list(do_warp = FALSE, do_trunc = TRUE, name = "Trunc only"),
  list(do_warp = TRUE, do_trunc = TRUE, name = "Both")
)

for (tc in test_cases) {
  cat(sprintf("\n--- %s (do_warp=%s, do_trunc=%s) ---\n",
              tc$name, tc$do_warp, tc$do_trunc))

  tryCatch({
    svg <- generate_separated_hexagonal_svg(
      rings = 3,
      seed = 42,
      diameter = 240,
      offset = 10,
      do_warp = tc$do_warp,
      do_trunc = tc$do_trunc,
      use_bezier = TRUE  # Use real bezier curves
    )

    # Check for arc commands (indicates warp)
    has_arcs <- grepl(" A ", svg)

    # Check SVG size (viewBox)
    vb_match <- regmatches(svg, regexpr('viewBox="[^"]+"', svg))

    cat(sprintf("  Has arc commands: %s\n", has_arcs))
    cat(sprintf("  ViewBox: %s\n", vb_match))
    cat(sprintf("  SVG length: %d characters\n", nchar(svg)))

    # Save for visual inspection
    output_file <- sprintf("output/test_warp_trunc_%s.svg", gsub(" ", "_", tolower(tc$name)))
    dir.create("output", showWarnings = FALSE)
    writeLines(svg, output_file)
    cat(sprintf("  Saved: %s\n", output_file))

  }, error = function(e) {
    cat(sprintf("  ERROR: %s\n", e$message))
  })
}

cat("\n\n=== Testing Complete Mode for Comparison ===\n")

for (tc in test_cases) {
  cat(sprintf("\n--- Complete: %s (do_warp=%s, do_trunc=%s) ---\n",
              tc$name, tc$do_warp, tc$do_trunc))

  tryCatch({
    result <- generate_hex_jigsaw_svg(
      rings = 3,
      seed = 42,
      diameter = 240,
      do_warp = tc$do_warp,
      do_trunc = tc$do_trunc
    )
    svg <- result$svg

    # Check for arc commands (indicates warp)
    has_arcs <- grepl(" a ", svg, ignore.case = TRUE)

    cat(sprintf("  Has arc commands: %s\n", has_arcs))
    cat(sprintf("  SVG length: %d characters\n", nchar(svg)))

    # Save for visual inspection
    output_file <- sprintf("output/test_warp_trunc_complete_%s.svg", gsub(" ", "_", tolower(tc$name)))
    writeLines(svg, output_file)
    cat(sprintf("  Saved: %s\n", output_file))

  }, error = function(e) {
    cat(sprintf("  ERROR: %s\n", e$message))
  })
}

cat("\n\nDone. Check output/ folder for visual comparison.\n")
