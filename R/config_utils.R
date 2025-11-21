#' Configuration Utilities
#'
#' Helper functions for loading and accessing package configuration
#'
#' @name config_utils
#' @keywords internal
NULL

#' Get package configuration
#'
#' Loads configuration from inst/config.yml
#'
#' @param config_name Configuration environment to load (default, development, production)
#' @return List containing configuration values
#' @export
get_puzzle_config <- function(config_name = "default") {
  config_file <- system.file("config.yml", package = "jigsawR")

  if (config_file == "") {
    # Fallback to hardcoded defaults if config file not found
    log_warn("Config file not found, using hardcoded defaults")
    return(get_fallback_config())
  }

  config::get(config = config_name, file = config_file)
}

#' Get viridis color palette
#'
#' Returns colors from specified viridis palette
#'
#' @param n Number of colors to generate
#' @param palette Palette name (viridis, magma, plasma, inferno, cividis, mako, rocket, turbo)
#' @return Character vector of hex colors
#' @export
get_puzzle_colors <- function(n, palette = NULL) {
  if (is.null(palette)) {
    cfg <- get_puzzle_config()
    palette <- cfg$colors$default_palette
  }

  # Validate palette name
  valid_palettes <- c("viridis", "magma", "plasma", "inferno", "cividis",
                      "mako", "rocket", "turbo")

  if (!palette %in% valid_palettes) {
    log_warn("Invalid palette '{palette}', using 'magma' instead")
    palette <- "magma"
  }

  # Generate colors using viridis
  viridis::viridis_pal(option = palette)(n)
}

#' Get fallback configuration
#'
#' Returns hardcoded defaults if config file is unavailable
#'
#' @return List containing default configuration values
#' @keywords internal
get_fallback_config <- function() {
  list(
    rectangular = list(
      tabsize = 20,
      jitter = 4,
      width = 300,
      height = 200,
      corner_radius = 2.0,
      stroke_width = 1.5
    ),
    hexagonal = list(
      tabsize = 27,
      jitter = 5,
      diameter = 240,
      rings = 6,
      stroke_width = 1.5
    ),
    colors = list(
      default_palette = "magma",
      available_palettes = c("viridis", "magma", "plasma", "inferno",
                             "cividis", "mako", "rocket", "turbo")
    ),
    gradient = list(
      dpi = 300,
      coord_limit = 1.3,
      range_expansion = 1.20,
      limit_contraction = 0.90,
      palette_size = 256,
      circle_resolution = 361,
      default_size_px = 2000
    ),
    ui = list(
      preview_height = "600px",
      container_height = "500px",
      download_delay_ms = 500,
      icon_size = "48px",
      info_padding = "10px"
    ),
    constants = list(
      dpi_to_mm = 25.4,
      coordinate_precision = 2
    )
  )
}
