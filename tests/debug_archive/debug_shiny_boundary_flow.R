# Debug: Simulate Shiny app boundary parameter flow

source("R/logging.R")
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  if (!grepl("scripts_archive", f)) source(f)
}

cat("=== Simulating Shiny App Boundary Parameter Flow ===\n\n")

# This function is defined in the Shiny app
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

# Test each option
options <- c("zigzag", "hexagon", "warped", "warped_hex", "circle")

cat("Parameter mappings:\n")
for (opt in options) {
  params <- get_hex_boundary_params(opt)
  cat(sprintf("  %s: do_warp=%s, do_trunc=%s, do_circular_border=%s\n",
              opt, params$do_warp, params$do_trunc, params$do_circular_border))
}

# Now simulate what the Shiny app does for "warped_hex" and "circle"
cat("\n=== Generating puzzles as Shiny app would ===\n\n")

rings <- 3
diameter <- 200
seed <- 1234

for (boundary_choice in c("warped_hex", "circle")) {
  cat(sprintf("--- Boundary choice: '%s' ---\n", boundary_choice))

  boundary_params <- get_hex_boundary_params(boundary_choice)
  cat(sprintf("Parameters: do_warp=%s, do_trunc=%s, do_circular_border=%s\n",
              boundary_params$do_warp, boundary_params$do_trunc,
              boundary_params$do_circular_border))

  # This is exactly what the Shiny app does
  pieces_result <- generate_pieces_internal(
    type = "hexagonal",
    seed = seed,
    grid = c(rings),
    size = c(diameter),
    tabsize = 27,
    jitter = 5,
    do_warp = boundary_params$do_warp,
    do_trunc = boundary_params$do_trunc,
    do_circular_border = boundary_params$do_circular_border
  )

  # Check outer piece path for arc commands
  outer_piece_path <- pieces_result$pieces[[8]]$path
  has_arc <- grepl(" A ", outer_piece_path)
  cat(sprintf("Outer piece has arc commands: %s\n\n", has_arc))
}

cat("=== Verification ===\n")
cat("If 'warped_hex' has NO arcs and 'circle' HAS arcs, the flow is correct.\n")
cat("If they both have the same arc status, there's a problem.\n")
