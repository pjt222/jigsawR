# =============================================================================
# Download Module and Server Helpers for jigsawR Shiny App
# Extracted from app.R (#108, #117)
# =============================================================================

# Helper: Map common errors to user-friendly messages (#117)
friendly_error_message <- function(msg) {
  if (grepl("subscript out of bounds", msg, fixed = TRUE)) {
    return("A piece index was out of range. Check your fusion group or grid settings.")
  }
  if (grepl("cannot open the connection", msg, fixed = TRUE)) {
    return("Could not read the file. Check the file path exists.")
  }
  if (grepl("non-numeric argument", msg, fixed = TRUE)) {
    return("A parameter expected a number but got text. Check your inputs.")
  }
  if (grepl("not found", msg, fixed = TRUE)) {
    return("A required function or package is missing. Try restarting the app.")
  }
  # Default: truncate long messages
  if (nchar(msg) > 120) {
    return(paste0(substr(msg, 1, 120), "..."))
  }
  msg
}

# Resolve fill specification from input widgets.
# Returns list(fill_color, fill_colors) where fill_color is a scalar spec
# and fill_colors is a per-piece vector (for palette mode) or NULL.
resolve_fill_spec <- function(input, n_pieces) {
  fill_color <- "none"
  fill_colors <- NULL

  if (input$fill_type == "none") {
    fill_color <- "none"
  } else if (input$fill_type == "solid") {
    fill_color <- input$fill_color
  } else if (input$fill_type == "palette") {
    fill_palette_val <- if (is.null(input$fill_palette)) "magma" else input$fill_palette
    fill_colors <- get_puzzle_colors(n_pieces, fill_palette_val,
                                      invert = isTRUE(input$fill_palette_invert))
  } else if (input$fill_type == "gradient") {
    fill_color <- list(
      type = "gradient",
      center = input$piece_gradient_center,
      middle = input$piece_gradient_middle,
      edge = input$piece_gradient_edge
    )
  } else if (input$fill_type == "noise") {
    if (noise_available) {
      fill_color <- noise_fill_spec(
        noise_type = if (is.null(input$fill_noise_type)) "simplex" else input$fill_noise_type,
        frequency = if (is.null(input$fill_noise_frequency)) 0.03 else input$fill_noise_frequency,
        color_low = if (is.null(input$fill_noise_color_low)) "#2d2d44" else input$fill_noise_color_low,
        color_high = if (is.null(input$fill_noise_color_high)) "#8888aa" else input$fill_noise_color_high,
        seed = if (is.null(input$fill_noise_seed)) 123 else input$fill_noise_seed
      )
    } else {
      log_warn("Noise fill requested but packages not available - using solid gray")
      fill_color <- "#a1a1a1"
    }
  }

  list(fill_color = fill_color, fill_colors = fill_colors)
}

# Resolve background specification from input widgets.
resolve_background_spec <- function(input) {
  if (input$background_type == "none") {
    "none"
  } else if (input$background_type == "solid") {
    input$background_color
  } else if (input$background_type == "gradient") {
    list(
      type = "gradient",
      center = input$gradient_center,
      middle = input$gradient_middle,
      edge = input$gradient_edge
    )
  } else if (input$background_type == "noise" && noise_available) {
    noise_fill_spec(
      noise_type = if (is.null(input$bg_noise_type)) "perlin" else input$bg_noise_type,
      frequency = if (is.null(input$bg_noise_frequency)) 0.02 else input$bg_noise_frequency,
      color_low = if (is.null(input$bg_noise_color_low)) "#1a1a2e" else input$bg_noise_color_low,
      color_high = if (is.null(input$bg_noise_color_high)) "#4a4a6e" else input$bg_noise_color_high,
      seed = if (is.null(input$bg_noise_seed)) 42 else input$bg_noise_seed
    )
  } else {
    "none"
  }
}

# Resolve stroke specification from input widgets.
# Returns list(stroke_width, stroke_colors, stroke_palette, stroke_palette_invert)
resolve_stroke_spec <- function(input, n_pieces) {
  stroke_color_type_val <- if (is.null(input$stroke_color_type)) "solid" else input$stroke_color_type
  stroke_width <- input$stroke_width
  stroke_colors <- NULL
  stroke_palette <- if (is.null(input$stroke_palette)) "viridis" else input$stroke_palette
  stroke_palette_invert <- isTRUE(input$stroke_palette_invert)

  if (stroke_color_type_val == "none") {
    stroke_width <- 0
  } else if (stroke_color_type_val == "solid") {
    stroke_colors <- rep(input$stroke_color, n_pieces)
    stroke_palette <- NULL
  }
  # "palette" uses default behavior (colors = NULL, palette used)

  list(stroke_width = stroke_width, stroke_colors = stroke_colors,
       stroke_palette = stroke_palette, stroke_palette_invert = stroke_palette_invert)
}

# Build filename prefix from puzzle data
build_filename_prefix <- function(data) {
  if (is.null(data)) return("puzzle")
  if (data$type == "hexagonal") {
    sprintf("hexagonal_%drings_seed%d", data$rings, data$seed)
  } else if (data$type == "concentric") {
    sprintf("concentric_%drings_seed%d", data$rings, data$seed)
  } else if (data$type == "voronoi") {
    sprintf("voronoi_%dcells_seed%d", data$n_cells, data$seed)
  } else if (data$type == "random") {
    sprintf("random_%dpieces_seed%d", data$total_pieces, data$seed)
  } else {
    sprintf("puzzle_%dx%d_seed%d", data$rows, data$cols, data$seed)
  }
}

# Register all download handlers for the Shiny app
# @param input Shiny input object
# @param output Shiny output object
# @param session Shiny session object
# @param rv List of reactive values: puzzle_data, svg_content, positioned_result
# @param build_puzzle_params_fn Function to build puzzle parameters from input
register_download_handlers <- function(input, output, session, rv, build_puzzle_params_fn) {

  # Download handler for Complete puzzle (offset=0)
  output$download_complete <- downloadHandler(
    filename = function() {
      paste0(build_filename_prefix(rv$puzzle_data()), "_complete.svg")
    },
    content = function(file) {
      data <- rv$puzzle_data()
      if (is.null(data)) return()

      # Build parameters using shared helper (#107)
      params <- build_puzzle_params_fn(input, data$type)

      # Resolve fill, background, stroke from input widgets
      fill_spec_dl <- resolve_fill_spec(input, data$total_pieces)
      background_value <- resolve_background_spec(input)
      stroke_spec_dl <- resolve_stroke_spec(input, data$total_pieces)

      result <- generate_puzzle(
        type = data$type,
        grid = params$grid,
        size = params$size,
        seed = data$seed,
        tabsize = input$tabsize,
        jitter = input$jitter,
        offset = 0,  # Always complete for this download
        fill_color = fill_spec_dl$fill_color,
        fills = fill_spec_dl$fill_colors,
        stroke_width = stroke_spec_dl$stroke_width,
        colors = stroke_spec_dl$stroke_colors,
        palette = stroke_spec_dl$stroke_palette,
        palette_invert = stroke_spec_dl$stroke_palette_invert,
        fill_direction = params$fill_direction,
        background = background_value,
        opacity = input$opacity / 100,
        save_files = FALSE,
        do_warp = if (data$type == "hexagonal") params$boundary_params$do_warp else FALSE,
        do_trunc = if (data$type == "hexagonal") params$boundary_params$do_trunc else FALSE,
        do_circular_border = params$do_circular_border,
        center_shape = params$center_shape,
        boundary_facing = params$boundary_facing,
        fusion_groups = if (params$has_fusion) params$fusion_groups_str else NULL,
        fusion_style = if (is.null(input$fusion_style)) "none" else input$fusion_style,
        fusion_opacity = if (is.null(input$fusion_opacity)) 0.3 else input$fusion_opacity / 100,
        point_distribution = if (data$type == "voronoi") params$point_distribution else "fermat",
        n_corner = if (data$type == "random") params$n_corner else 4,
        min_tab_size = params$min_tab_size,
        max_tab_size = params$max_tab_size,
        image_path = params$image_path,
        compactness = params$compactness,
        seed_type = params$seed_type
      )

      writeLines(result$svg_content, file)
    },
    contentType = "image/svg+xml"
  )

  # Download handler for WYSIWYG (current view)
  output$download_wysiwyg <- downloadHandler(
    filename = function() {
      data <- rv$puzzle_data()
      if (is.null(data)) return("puzzle.svg")
      sep_suffix <- if (data$offset > 0) "_separated" else "_complete"
      paste0(build_filename_prefix(rv$puzzle_data()), sep_suffix, ".svg")
    },
    content = function(file) {
      if (!is.null(rv$svg_content())) {
        # Add XML declaration for standalone SVG file (removed for inline HTML)
        svg_standalone <- paste0('<?xml version="1.0" encoding="UTF-8"?>\n', rv$svg_content())
        writeLines(svg_standalone, file)
      }
    },
    contentType = "image/svg+xml"
  )

  # Download handler for PNG (current view converted to PNG)
  output$download_png <- downloadHandler(
    filename = function() {
      data <- rv$puzzle_data()
      if (is.null(data)) return("puzzle.png")
      sep_suffix <- if (data$offset > 0) "_separated" else "_complete"
      paste0(build_filename_prefix(rv$puzzle_data()), sep_suffix, ".png")
    },
    content = function(file) {
      svg <- rv$svg_content()
      if (is.null(svg)) {
        showNotification("No puzzle to download.", type = "warning")
        return()
      }

      # Show progress notification
      progress_id <- showNotification(
        "Converting SVG to PNG...",
        type = "message",
        duration = NULL  # Don't auto-close
      )

      tryCatch({
        # Get canvas size for aspect ratio
        pos <- rv$positioned_result()
        canvas <- if (!is.null(pos)) pos$canvas_size else c(2000, 2000)

        # Calculate dimensions maintaining aspect ratio (max 2000px)
        max_dim <- 2000
        aspect_ratio <- canvas[1] / canvas[2]
        if (aspect_ratio >= 1) {
          width_px <- max_dim
          height_px <- round(max_dim / aspect_ratio)
        } else {
          height_px <- max_dim
          width_px <- round(max_dim * aspect_ratio)
        }

        # Create temp file for PNG output
        temp_png <- tempfile(fileext = ".png")

        # Convert SVG to PNG
        temp_svg <- tempfile(fileext = ".svg")
        writeLines(svg, temp_svg)

        # Try rsvg first (most reliable)
        success <- FALSE
        if (requireNamespace("rsvg", quietly = TRUE)) {
          tryCatch({
            png_data <- rsvg::rsvg_png(temp_svg, width = width_px, height = height_px)
            writeBin(png_data, temp_png)
            success <- TRUE
          }, error = function(e) {
            log_warn("rsvg conversion failed: {e$message}")
          })
        }

        # Fall back to magick if rsvg failed
        if (!success && requireNamespace("magick", quietly = TRUE)) {
          tryCatch({
            img <- magick::image_read_svg(temp_svg, width = width_px, height = height_px)
            magick::image_write(img, temp_png, format = "png")
            success <- TRUE
          }, error = function(e) {
            log_warn("magick conversion failed: {e$message}")
          })
        }

        # Clean up temp SVG
        unlink(temp_svg)

        if (success && file.exists(temp_png)) {
          # Copy to download file
          file.copy(temp_png, file, overwrite = TRUE)
          unlink(temp_png)
          removeNotification(progress_id)
          showNotification("PNG download ready!", type = "message", duration = 3)
        } else {
          removeNotification(progress_id)
          showNotification(
            "PNG conversion failed. Please install 'rsvg' or 'magick' package.",
            type = "error",
            duration = 10
          )
        }
      }, error = function(e) {
        removeNotification(progress_id)
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
      })
    },
    contentType = "image/png"
  )

  # Handle individual pieces download as ZIP - UNIFIED using render_single_piece_svg()
  output$download_pieces <- downloadHandler(
    filename = function() {
      data <- rv$puzzle_data()
      if (is.null(data)) return("pieces.zip")
      sprintf("%s_pieces_seed%d.zip", data$type, data$seed)
    },
    content = function(file) {
      pos <- rv$positioned_result()
      data <- rv$puzzle_data()

      if (is.null(pos) || is.null(data)) {
        # Create empty zip as fallback
        writeLines("No puzzle generated", file)
        return()
      }

      # Create temp directory for pieces
      pieces_dir <- tempfile("pieces_")
      dir.create(pieces_dir, recursive = TRUE)
      on.exit(unlink(pieces_dir, recursive = TRUE), add = TRUE)

      # Resolve fill and background from input widgets
      n_pieces <- length(pos$pieces)
      fill_spec_pieces <- resolve_fill_spec(input, n_pieces)
      fill_value <- fill_spec_pieces$fill_color
      fill_colors <- fill_spec_pieces$fill_colors

      background_value <- resolve_background_spec(input)

      # Handle stroke color for per-piece rendering
      stroke_color_type_val <- if (is.null(input$stroke_color_type)) "solid" else input$stroke_color_type
      stroke_width_val <- input$stroke_width
      stroke_palette_val <- if (is.null(input$stroke_palette)) "viridis" else input$stroke_palette

      stroke_colors <- if (stroke_color_type_val == "none") {
        rep("none", n_pieces)
      } else if (stroke_color_type_val == "solid") {
        rep(input$stroke_color, n_pieces)
      } else {
        get_puzzle_colors(n_pieces, stroke_palette_val, invert = isTRUE(input$stroke_palette_invert))
      }

      if (stroke_color_type_val == "none") {
        stroke_width_val <- 0
      }

      # Generate filename prefix based on type
      file_prefix <- switch(data$type,
        "hexagonal" = "hex_piece",
        "concentric" = "concentric_piece",
        "voronoi" = "voronoi_piece",
        "random" = "random_piece",
        "piece"  # rectangular default
      )

      # Generate individual piece SVGs using render_single_piece_svg()
      piece_files <- character(0)
      for (i in seq_along(pos$pieces)) {
        piece <- pos$pieces[[i]]

        # Determine fill for this piece
        piece_fill <- if (!is.null(fill_colors)) {
          fill_colors[i]
        } else {
          fill_value
        }

        # Render individual piece SVG
        piece_svg <- render_single_piece_svg(
          piece = piece,
          fill = piece_fill,
          stroke_color = stroke_colors[i],
          stroke_width = stroke_width_val,
          opacity = input$opacity / 100,
          background = background_value,
          padding = 0.15,
          show_label = isTRUE(input$show_labels),
          label_color = if (is.null(input$label_color)) "#000000" else input$label_color,
          label_size = if (is.null(input$label_size) || input$label_size == 0) NULL else input$label_size
        )

        # Generate filename and save
        filename <- sprintf("%s_%02d_seed%d.svg", file_prefix, i, data$seed)
        filepath <- file.path(pieces_dir, filename)
        writeLines(piece_svg, filepath)
        piece_files <- c(piece_files, filepath)
      }

      # Create ZIP file
      zip::zipr(file, piece_files, mode = "cherry-pick")
    },
    contentType = "application/zip"
  )
}
