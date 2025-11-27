# Simplified Hexagonal Individual Piece Generation
# Direct approach: Generate each piece by identifying its 6 edges and generating them

#' Generate individual hexagonal puzzle pieces (simplified direct approach)
#'
#' Instead of parsing the complete puzzle, this generates each piece directly
#' by identifying which edges it needs and generating them using the core functions.
#'
#' @param seed Random seed
#' @param rings Number of rings
#' @param diameter Puzzle diameter in mm
#' @param tabsize Tab size percentage
#' @param jitter Jitter percentage
#' @param do_warp Apply circular warping
#' @param do_trunc Truncate edge pieces
#' @param output_dir Output directory
#' @param colors Color palette
#' @param stroke_width Stroke width
#' @return List with pieces and metadata
#' @export
generate_hex_individual_simple <- function(seed = 42, rings = 3,
                                          diameter = 240,
                                          tabsize = 27, jitter = 5,
                                          do_warp = FALSE, do_trunc = FALSE,
                                          output_dir = "output",
                                          colors = NULL,
                                          stroke_width = 1.5) {

  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Source required functions
  if (!exists("init_hex_jigsaw")) {
    if (file.exists("R/hexagonal_puzzle.R")) {
      source("R/hexagonal_puzzle.R")
    }
  }

  cat(sprintf("Generating %d-ring hexagonal puzzle (seed: %d)...\n", rings, seed))

  # Initialize hexagonal puzzle environment
  init_hex_jigsaw(seed = seed, tabsize = tabsize, jitter = jitter,
                  diameter = diameter, rings = rings,
                  do_warp = do_warp, do_trunc = do_trunc)

  # For now, generate the complete puzzle and save it
  # This is a placeholder - full individual piece extraction will be phase 2

  # Generate complete puzzle paths
  horizontal_paths <- hex_gen_dh()
  vertical_paths <- hex_gen_dv()
  border_paths <- hex_gen_db()

  # Calculate number of pieces
  num_pieces <- 3 * rings * (rings - 1) + 1

  cat(sprintf("Generated complete puzzle with %d pieces\n", num_pieces))
  cat(sprintf("  Horizontal paths: %d characters\n", nchar(horizontal_paths)))
  cat(sprintf("  Vertical paths: %d characters\n", nchar(vertical_paths)))
  cat(sprintf("  Border paths: %d characters\n", nchar(border_paths)))

  # For phase 1, return the complete puzzle
  # Phase 2 will implement actual piece extraction

  radius <- .hex_jigsaw_env$radius
  offset <- .hex_jigsaw_env$offset
  width <- 2.0 * (radius + offset)
  height <- 2.0 * (radius + offset)

  # Create combined SVG
  svg_lines <- c(
    sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="%.1fmm" height="%.1fmm" viewBox="0 0 %.1f %.1f">',
            width, height, width, height),
    sprintf('<path fill="none" stroke="black" stroke-width="%.1f" d="%s"/>',
            stroke_width, horizontal_paths),
    sprintf('<path fill="none" stroke="black" stroke-width="%.1f" d="%s"/>',
            stroke_width, vertical_paths),
    sprintf('<path fill="none" stroke="black" stroke-width="%.1f" d="%s"/>',
            stroke_width * 1.2, border_paths),
    '</svg>'
  )

  svg_content <- paste(svg_lines, collapse = "\n")

  # Save combined SVG
  combined_file <- file.path(output_dir, sprintf("hex_complete_rings%d_seed%d.svg", rings, seed))
  writeLines(svg_content, combined_file)

  cat(sprintf("Saved complete puzzle to: %s\n", combined_file))
  cat("\nNOTE: Individual piece extraction coming in next phase\n")
  cat("      Current output shows the complete puzzle\n")

  return(list(
    type = "hexagonal_complete",
    svg_content = svg_content,
    num_pieces = num_pieces,
    parameters = list(
      seed = seed,
      rings = rings,
      diameter = diameter,
      tabsize = tabsize,
      jitter = jitter,
      do_warp = do_warp,
      do_trunc = do_trunc
    ),
    paths = list(
      horizontal = horizontal_paths,
      vertical = vertical_paths,
      border = border_paths
    ),
    files = list(
      combined = combined_file
    )
  ))
}
