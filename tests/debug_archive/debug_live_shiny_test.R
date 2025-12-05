# This script mimics exactly what the Shiny app does when you select each option

source("R/logging.R")
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  if (!grepl("scripts_archive", f)) source(f)
}

# This is copied from the Shiny app
get_hex_boundary_params <- function(boundary_choice) {
  switch(boundary_choice,
    "zigzag"     = list(do_warp = FALSE, do_trunc = FALSE, do_circular_border = FALSE),
    "hexagon"    = list(do_warp = FALSE, do_trunc = TRUE,  do_circular_border = FALSE),
    "warped"     = list(do_warp = TRUE,  do_trunc = FALSE, do_circular_border = FALSE),
    "warped_hex" = list(do_warp = TRUE,  do_trunc = TRUE,  do_circular_border = FALSE),
    "circle"     = list(do_warp = TRUE,  do_trunc = TRUE,  do_circular_border = TRUE),
    # Default
    list(do_warp = FALSE, do_trunc = FALSE, do_circular_border = FALSE)
  )
}

cat("=== Testing exact Shiny app behavior ===\n\n")

# Simulate: User selects "Warped Hexagon" then "Perfect Circle"
# and checks if they look different

test_boundary_options <- c("warped_hex", "circle")
results <- list()

for (opt in test_boundary_options) {
  cat(sprintf("### Testing option: '%s' ###\n", opt))

  params <- get_hex_boundary_params(opt)
  cat(sprintf("  do_warp = %s\n", params$do_warp))
  cat(sprintf("  do_trunc = %s\n", params$do_trunc))
  cat(sprintf("  do_circular_border = %s\n", params$do_circular_border))

  # Generate exactly as Shiny does
  pieces_result <- generate_pieces_internal(
    type = "hexagonal",
    seed = 42,
    grid = c(3),
    size = c(200),
    tabsize = 27,
    jitter = 5,
    do_warp = params$do_warp,
    do_trunc = params$do_trunc,
    do_circular_border = params$do_circular_border
  )

  positioned <- apply_piece_positioning(pieces_result, offset = 0)

  svg_content <- render_puzzle_svg(
    positioned,
    fill = "none",
    stroke_width = 1.5,
    colors = NULL,
    palette = "magma",
    background = "white",
    opacity = 1.0
  )

  results[[opt]] <- list(
    svg = svg_content,
    pieces = pieces_result$pieces
  )

  # Save SVG
  filename <- sprintf("output/shiny_test_%s.svg", opt)
  writeLines(svg_content, filename)
  cat(sprintf("  Saved to: %s\n\n", filename))
}

# Compare piece paths
cat("=== Comparing outer piece paths ===\n")
cat("(Piece 8 is in outer ring for 3-ring puzzle)\n\n")

path1 <- results$warped_hex$pieces[[8]]$path
path2 <- results$circle$pieces[[8]]$path

cat("Warped Hexagon piece 8 (first 300 chars):\n")
cat(substr(path1, 1, 300), "...\n\n")

cat("Perfect Circle piece 8 (first 300 chars):\n")
cat(substr(path2, 1, 300), "...\n\n")

if (identical(path1, path2)) {
  cat("PROBLEM: Paths are IDENTICAL - this should not happen!\n")
} else {
  cat("OK: Paths are DIFFERENT as expected.\n")

  # Show the actual difference
  # Find where they differ
  for (i in 1:min(nchar(path1), nchar(path2))) {
    if (substr(path1, i, i) != substr(path2, i, i)) {
      cat(sprintf("\nFirst difference at character %d:\n", i))
      cat(sprintf("  Warped Hex: ...%s...\n", substr(path1, max(1, i-20), i+30)))
      cat(sprintf("  Circle:     ...%s...\n", substr(path2, max(1, i-20), i+30)))
      break
    }
  }
}
