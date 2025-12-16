#' Configuration Utilities
#'
#' Helper functions for loading and accessing package configuration
#'
#' @name config_utils
#' @keywords internal
NULL

#' Get package configuration
#'
#' Loads configuration from inst/config.yml. This function searches for the
#' config file in multiple locations to work both in package context and
#' when running from the Shiny app directory.
#'
#' @param config_name Configuration environment to load (default, development, production)
#' @return List containing configuration values
#' @export
get_puzzle_config <- function(config_name = "default") {
  # Try multiple locations for config file
  possible_paths <- c(
    system.file("config.yml", package = "jigsawR"),
    "inst/config.yml",
    "../config.yml",
    "../../inst/config.yml",
    file.path(getwd(), "inst", "config.yml")
  )

  config_file <- ""
  for (path in possible_paths) {
    if (file.exists(path)) {
      config_file <- path
      break
    }
  }

  if (config_file == "") {
    # Fallback to hardcoded defaults if config file not found
    log_warn("Config file not found in any location, using hardcoded defaults")
    return(get_fallback_config())
  }

  tryCatch({
    config::get(config = config_name, file = config_file)
  }, error = function(e) {
    log_warn("Error loading config file: {e$message}, using fallback")
    get_fallback_config()
  })
}

#' Get viridis color palette
#'
#' Returns colors from specified viridis palette
#'
#' @param n Number of colors to generate
#' @param palette Palette name (black, viridis, magma, plasma, inferno, cividis, mako, rocket, turbo)
#' @param invert Logical, if TRUE reverses the palette direction (default: FALSE)
#' @return Character vector of hex colors
#' @export
get_puzzle_colors <- function(n, palette = NULL, invert = FALSE) {
  if (is.null(palette)) {
    cfg <- get_puzzle_config()
    palette <- cfg$colors$default_palette
  }

  # Handle black palette as special case
  if (palette == "black") {
    return(rep("#000000", n))
  }

  # Validate palette name
  # Note: 'black' is handled as a special case above and is intentionally excluded from this list.
  valid_palettes <- c("viridis", "magma", "plasma", "inferno", "cividis",
                      "mako", "rocket", "turbo")

  if (!palette %in% valid_palettes) {
    log_warn("Invalid palette '{palette}', using 'magma' instead")
    palette <- "magma"
  }

  # Generate colors using viridis
  colors <- viridis::viridis_pal(option = palette)(n)

  # Reverse palette if requested
  if (isTRUE(invert)) {
    colors <- rev(colors)
  }

  colors
}

#' Get fallback configuration
#'
#' Returns hardcoded defaults if config file is unavailable.
#' These values should match inst/config.yml exactly.
#'
#' @return List containing default configuration values
#' @keywords internal
get_fallback_config <- function() {
  list(
    rectangular = list(
      rows = 2,
      cols = 2,
      width = 200,
      height = 200
    ),
    hexagonal = list(
      rings = 3,
      diameter = 240,
      boundary = "zigzag"
    ),
    concentric = list(
      rings = 3,
      diameter = 240,
      center_shape = "hexagon"
    ),
    styling = list(
      tabsize = 20,
      jitter = 4,
      offset = 0,
      stroke_width = 1.5,
      opacity = 100,
      fill_type = "none",
      fill_color = "#ffffff"
    ),
    seed = 1234,
    colors = list(
      default_palette = "magma",
      available_palettes = c("black", "viridis", "magma", "plasma", "inferno",
                             "cividis", "mako", "rocket", "turbo")
    ),
    labels = list(
      show = FALSE,
      color = "#000000",
      size = 0
    ),
    background = list(
      type = "none",
      solid_color = "#ffffff",
      gradient = list(
        center = "#e3f2fd",
        middle = "#bbdefb",
        edge = "#90caf9"
      )
    ),
    gradient_generation = list(
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
      info_padding = "10px",
      default_accordion_open = "settings"
    ),
    constraints = list(
      rows = list(min = 1, max = 10),
      cols = list(min = 1, max = 10),
      width = list(min = 50, max = 500),
      height = list(min = 50, max = 500),
      rings = list(min = 2, max = 6),
      diameter = list(min = 100, max = 500),
      tabsize = list(min = 0, max = 100),
      jitter = list(min = 0, max = 100),
      offset = list(min = 0, max = 100),
      repel_margin = list(min = 0, max = 20),
      repel_max_iter = list(min = 10, max = 500),
      stroke_width = list(min = 0.5, max = 10),
      opacity = list(min = 0, max = 100),
      seed = list(min = 1, max = 99999),
      label_size = list(min = 0, max = 30),  # 0 = auto-size
      min_tab_size = list(min = 0, max = 50),  # 0 = no constraint
      max_tab_size = list(min = 5, max = 100)
    ),
    constants = list(
      dpi_to_mm = 25.4,
      coordinate_precision = 2
    )
  )
}
