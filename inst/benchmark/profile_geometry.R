# Profile geometry processing to identify specific hotspots
# Usage: Rscript inst/benchmark/profile_geometry.R

suppressMessages(devtools::load_all(quiet = TRUE))
library(cli)

cli_h1("Geometry Processing Profiler")

# =============================================================================
# Manual timing of key functions
# =============================================================================

cli_h2("Function-level timing (5-ring hex puzzle, 61 pieces)")

# Wrap functions to track timing
parse_svg_path_original <- parse_svg_path
parse_count <- 0
parse_time <- 0

parse_svg_path_timed <- function(...) {
  parse_count <<- parse_count + 1
  t <- system.time(result <- parse_svg_path_original(...))
  parse_time <<- parse_time + t[["elapsed"]]
  result
}

# Temporarily replace
assignInNamespace("parse_svg_path", parse_svg_path_timed, "jigsawR")

# Run puzzle generation
cli_alert_info("Generating 5-ring hexagonal puzzle...")
t_total <- system.time({
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(5),
    size = c(200),
    seed = 42,
    save_files = FALSE
  )
})

# Restore original
assignInNamespace("parse_svg_path", parse_svg_path_original, "jigsawR")

cli_alert_success("Total time: {sprintf('%.3f', t_total[['elapsed']])} seconds")
cli_alert_info("parse_svg_path() called {parse_count} times")
cli_alert_info("parse_svg_path() total time: {sprintf('%.3f', parse_time)} seconds")
cli_alert_info("parse_svg_path() avg per call: {sprintf('%.3f', parse_time/parse_count * 1000)} ms")

# =============================================================================
# Isolate geometry processing phase
# =============================================================================

cli_h2("Isolated phase timing")

# Time just the piece generation (includes geometry processing)
cli_alert_info("Timing generate_pieces_internal()...")

# Get the puzzle structure first
hex_result <- generate_hex_jigsaw_svg(seed = 42, rings = 5)

t_pieces <- system.time({
  pieces <- generate_pieces_internal(
    type = "hexagonal",
    puzzle_result = hex_result,
    params = list(rings = 5, diameter = 200, piece_radius = 200 / (4 * 5 - 2)),
    fusion_groups = NULL,
    fused_edge_data = NULL
  )
})

cli_alert_info("generate_pieces_internal(): {sprintf('%.3f', t_pieces[['elapsed']])} seconds")

# =============================================================================
# Time calculate_pieces_bounds separately
# =============================================================================

cli_h2("Bounds calculation timing")

t_bounds <- system.time({
  for (i in 1:10) {
    bounds <- calculate_pieces_bounds(pieces$pieces, type = "hexagonal")
  }
})

cli_alert_info("calculate_pieces_bounds() x10: {sprintf('%.3f', t_bounds[['elapsed']])} seconds")
cli_alert_info("Per call: {sprintf('%.3f', t_bounds[['elapsed']] / 10 * 1000)} ms")

# =============================================================================
# Time extract_path_coords
# =============================================================================

cli_h2("Path coordinate extraction timing")

t_extract <- system.time({
  for (i in 1:10) {
    coords <- lapply(pieces$pieces, function(p) {
      extract_path_coords(p$path)
    })
  }
})

cli_alert_info("extract_path_coords() for all pieces x10: {sprintf('%.3f', t_extract[['elapsed']])} seconds")
cli_alert_info("Per full extraction: {sprintf('%.3f', t_extract[['elapsed']] / 10 * 1000)} ms")

# =============================================================================
# Summary
# =============================================================================

cli_h1("Summary")

cli_bullets(c(

  "*" = sprintf("parse_svg_path() called %d times (should be ~61 for 61 pieces)", parse_count),
  "*" = sprintf("parse_svg_path() accounts for %.1f%% of total time", (parse_time / t_total[["elapsed"]]) * 100),
  "!" = "Optimization opportunity: Cache parsed paths, reuse in bounds calculation"
))
