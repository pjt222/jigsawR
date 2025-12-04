# jigsawR Shiny Application
# Interactive puzzle generator with real-time preview and download

library(shiny)
library(bslib)
library(shinyjs)
library(cli)

# Source logging utilities first
possible_logging_paths <- c("R/logging.R", "./R/logging.R", "../../R/logging.R", "../R/logging.R")
for (path in possible_logging_paths) {
  if (file.exists(path)) {
    source(path)
    break
  }
}

# Source the required functions from the package
# In production, these would be loaded via library(jigsawR)
source_dir <- function(path) {
  log_info("Attempting to source from: {.path {path}}")
  if (file.exists(path)) {
    log_success("Directory exists!")
    files <- list.files(path, pattern = "\\.R$", full.names = TRUE)
    n_files <- length(files)
    log_info("Found {n_files} R files")
    for (file in files) {
      # Skip archive and example files
      if (!grepl("scripts_archive|examples", file)) {
        file_name <- basename(file)
        log_info("Sourcing: {.file {file_name}}")
        source(file)
      }
    }
  } else {
    log_warn("Directory does NOT exist")
  }
}

# Debug: Show current working directory and files
log_header("App Initialization")
wd <- getwd()
log_info("Working directory: {.path {wd}}")
current_files <- paste(list.files(), collapse=', ')
log_info("Files in current dir: {current_files}")
parent_files <- paste(list.files('..'), collapse=', ')
log_info("Files in parent dir: {parent_files}")
parent_parent_files <- paste(list.files('../..'), collapse=', ')
log_info("Files in parent/parent dir: {parent_parent_files}")

# Try to load functions (adjust path based on where app is run from)
# On shinyapps.io, R files will be in ./R (same directory as app.R)
# During local development, they're in ../../R
possible_paths <- c("R", "./R", "../../R", "../R")
loaded <- FALSE

for (path in possible_paths) {
  log_info("Trying path: {.path {path}}")
  if (file.exists(path)) {
    log_success("Found R directory at {.path {path}}")
    source_dir(path)
    loaded <- TRUE
    break
  }
}

if (!loaded) {
  log_error("Could not find R directory in any expected location!")
  files_list <- paste(list.files(), collapse=', ')
  log_info("Current files: {files_list}")
}

# Define UI with enhanced bslib theme
ui <- page_fluid(
  theme = bs_theme(
    bootswatch = "darkly",
    # Enhanced theme customization for better component integration
    primary = "#375a7f",
    secondary = "#444",
    success = "#00bc8c",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c"
  ),
  useShinyjs(),

  # Add minimal custom CSS and JavaScript for sequential downloads
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('downloadFiles', function(files) {
        // Download files sequentially with 500ms delay between each
        files.forEach(function(file, index) {
          setTimeout(function() {
            var link = document.createElement('a');
            link.href = file.url;
            link.download = file.name;
            document.body.appendChild(link);
            link.click();
            document.body.removeChild(link);
          }, index * 500);
        });
      });
    "))
  ),

  # Application title
  titlePanel(
    div(
      h2("jigsawR Puzzle Generator"),
      p("Create customizable jigsaw puzzles for laser cutting and more",
        class = "text-muted fs-6")
    )
  ),

  # Modern bslib layout with sidebar
  layout_sidebar(
    sidebar = sidebar(
      width = 300,  # Approximately same as width = 4 in old layout (300px)

      # Parameter Accordion
      accordion(
        id = "params_accordion",
        open = c("basic", "advanced"),  # Keep basic and advanced open by default

        # Basic Settings Section
        accordion_panel(
          title = "Basic Settings",
          value = "basic",

        # Puzzle Type Selection
        radioButtons("puzzle_type", "Puzzle Type:",
                    choices = list("Rectangular" = "rectangular",
                                  "Hexagonal" = "hexagonal"),
                    selected = "rectangular",
                    inline = TRUE),

        # Conditional UI for puzzle parameters
        conditionalPanel(
          condition = "input.puzzle_type == 'rectangular'",
          # Grid dimensions for rectangular
          fluidRow(
            column(6,
              numericInput("rows", "Rows:",
                          value = 2, min = 1, max = 10, step = 1)
            ),
            column(6,
              numericInput("cols", "Columns:",
                          value = 2, min = 1, max = 10, step = 1)
            )
          ),

          # Size for rectangular
          fluidRow(
            column(6,
              numericInput("width", "Width (mm):",
                          value = 200, min = 50, max = 500, step = 10)
            ),
            column(6,
              numericInput("height", "Height (mm):",
                          value = 200, min = 50, max = 500, step = 10)
            )
          )
        ),

        conditionalPanel(
          condition = "input.puzzle_type == 'hexagonal'",
          # Rings for hexagonal
          numericInput("rings", "Rings:", value = 3, min = 2, max = 6),

          # Diameter for hexagonal
          numericInput("diameter", "Diameter (mm):",
                      value = 240, min = 100, max = 500, step = 10),

          # Hexagonal boundary shape options
          # Five mutually exclusive outcomes
          radioButtons("hex_boundary", "Boundary Shape:",
                      choices = list(
                        "Zigzag (Original)" = "zigzag",
                        "Clean Hexagon" = "hexagon",
                        "Warped Zigzag" = "warped",
                        "Warped Hexagon" = "warped_hex",
                        "Perfect Circle" = "circle"
                      ),
                      selected = "zigzag")
        ),

        # Seed
        fluidRow(
          column(8,
            numericInput("seed", "Random Seed:",
                        value = 1234, min = 1, max = 99999, step = 1)
          ),
          column(4,
            br(),
            actionButton("randomize", "Random",
                        icon = icon("dice"),
                        class = "mt-4")
          )
        )
        ),

        # Advanced Settings Section
        accordion_panel(
          title = "Advanced Settings",
          value = "advanced",

        tooltip(
          sliderInput("tabsize", "Tab Size:",
                     min = 0, max = 100, value = 20, step = 1,
                     ticks = TRUE,
                     post = "%",
                     sep = ""),
          "Controls the size of interlocking tabs. Higher values create larger, more prominent tabs. Recommended: 15-25%"
        ),

        tooltip(
          sliderInput("jitter", "Jitter:",
                     min = 0, max = 100, value = 4, step = 1,
                     ticks = TRUE,
                     post = "%",
                     sep = ""),
          "Adds randomness to piece shapes for more organic variation. Higher values create more irregular pieces. Recommended: 2-6%"
        ),

        # Unified offset slider (replaces output mode dropdowns - Epic #32)
        tooltip(
          sliderInput("offset", "Piece Separation:",
                     min = 0, max = 50, value = 0, step = 1,
                     ticks = TRUE,
                     post = " mm",
                     sep = ""),
          "Gap between pieces. 0mm = complete puzzle (pieces touching), >0mm = separated pieces for laser cutting"
        ),

        # Fill color for separated mode (visible when offset > 0)
        conditionalPanel(
          condition = "input.offset > 0",
          radioButtons("fill_type", "Piece Fill:",
                      choices = list(
                        "None (Outline only)" = "none",
                        "Solid Color" = "solid"
                      ),
                      selected = "none",
                      inline = TRUE),
          conditionalPanel(
            condition = "input.fill_type == 'solid'",
            colourpicker::colourInput(
              "fill_color",
              "Fill Color:",
              value = "#ffffff",
              showColour = "background"
            )
          )
        )
        ),

        # Styling Options Section
        accordion_panel(
          title = "Styling Options",
          value = "styling",

        selectInput("color_palette", "Color Palette:",
                   choices = list(
                     "Black (Solid)" = "black",
                     "Magma (Purple-Yellow)" = "magma",
                     "Viridis (Blue-Green-Yellow)" = "viridis",
                     "Plasma (Purple-Red-Yellow)" = "plasma",
                     "Inferno (Black-Purple-Yellow)" = "inferno",
                     "Cividis (Colorblind Friendly)" = "cividis",
                     "Mako (Blue-Green)" = "mako",
                     "Rocket (Black-Red-Yellow)" = "rocket",
                     "Turbo (Rainbow)" = "turbo"
                   ),
                   selected = "magma"),

        tooltip(
          sliderInput("stroke_width", "Line Width:",
                     min = 0.5, max = 10, value = 1.5, step = 0.5,
                     ticks = TRUE,
                     round = 1,
                     post = " mm",
                     sep = ""),
          "Thickness of puzzle piece outlines. For laser cutting, use 0.5mm. For printing or display, use 1.5-2.5mm."
        ),

        tooltip(
          sliderInput("opacity", "Opacity:",
                     min = 0, max = 100, value = 100, step = 5,
                     ticks = TRUE,
                     post = "%",
                     sep = ""),
          "Transparency of puzzle pieces. 100% = fully opaque, 0% = fully transparent. Lower values create a watermark effect."
        ),

        # Piece labels switch
        tooltip(
          input_switch(
            id = "show_labels",
            label = "Show Piece Labels",
            value = FALSE
          ),
          "Display piece ID numbers at the center of each piece. Useful for assembly instructions."
        ),

        # Conditional label options (shown when labels are enabled)
        conditionalPanel(
          condition = "input.show_labels == true",
          colourpicker::colourInput(
            "label_color",
            "Label Color:",
            value = "#000000",
            showColour = "background"
          ),
          tooltip(
            sliderInput("label_size", "Label Font Size:",
                       min = 4, max = 30, value = 0, step = 1,
                       ticks = TRUE,
                       post = " mm",
                       sep = ""),
            "Font size for piece labels. Set to 0 for automatic sizing based on piece dimensions."
          )
        ),

        # Background type selector
        radioButtons("background_type", "Background:",
                    choices = list(
                      "None" = "none",
                      "Solid Color" = "solid",
                      "Gradient" = "gradient"
                    ),
                    selected = "none",
                    inline = TRUE),

        # Solid color picker (shown when background_type == "solid")
        conditionalPanel(
          condition = "input.background_type == 'solid'",
          colourpicker::colourInput(
            "background_color",
            "Background Color:",
            value = "#ffffff",
            showColour = "background"
          )
        ),

        # Gradient color pickers (shown when background_type == "gradient")
        conditionalPanel(
          condition = "input.background_type == 'gradient'",
          colourpicker::colourInput(
            "gradient_center",
            "Center Color (0%):",
            value = "#e3f2fd",
            showColour = "background"
          ),
          colourpicker::colourInput(
            "gradient_middle",
            "Middle Color (50%):",
            value = "#bbdefb",
            showColour = "background"
          ),
          colourpicker::colourInput(
            "gradient_edge",
            "Edge Color (100%):",
            value = "#90caf9",
            showColour = "background"
          )
        )
        )
      ),  # Close accordion

      # Action Buttons
      div(class = "mt-3",
        actionButton("generate", "Generate Puzzle",
                    icon = icon("puzzle-piece"),
                    class = "btn-primary btn-lg btn-block mb-2"),

        # Reset button
        actionButton("reset", "Reset to Defaults",
                    icon = icon("undo"),
                    class = "btn-secondary btn-block mb-3"),

        # Download buttons section (Epic #32)
        h6("Download Options:", class = "text-muted mb-2"),

        # Complete puzzle (offset=0)
        tooltip(
          downloadButton("download_complete", "Complete Puzzle",
                        icon = icon("puzzle-piece"),
                        class = "btn-success btn-block mb-2"),
          "Download puzzle with all pieces in original positions (offset=0)"
        ),

        # Current view (WYSIWYG)
        tooltip(
          downloadButton("download_wysiwyg", "Current View",
                        icon = icon("eye"),
                        class = "btn-info btn-block mb-2"),
          "Download exactly what you see (WYSIWYG)"
        ),

        # Individual pieces (rectangular only for now)
        conditionalPanel(
          condition = "input.puzzle_type == 'rectangular'",
          tooltip(
            actionButton("download_pieces", "Individual Pieces",
                        icon = icon("download"),
                        class = "btn-warning btn-block mb-2"),
            "Download each piece as a separate SVG file"
          )
        )
      ),

      # Info text
      div(class = "alert alert-warning mt-3 mb-0",
        p(class = "small mb-0",
          icon("lightbulb"), " ",
          strong("Tip:"), " Click 'Generate Puzzle' to create your puzzle.
          The preview will appear on the right.")
      )
    ),  # End of sidebar

    # Main content area (no wrapper needed with layout_sidebar)
    # Tabs for different views
    tabsetPanel(
        id = "main_tabs",

        # Preview Tab
        tabPanel("Preview",
          br(),
          card(
            full_screen = TRUE,
            height = "600px",
            card_header(
              "Puzzle Preview",
              class = "d-flex align-items-center"
            ),
            card_body(
              class = "d-flex align-items-center justify-content-center p-3",
              style = "height: 500px; overflow: auto;",
              uiOutput("puzzle_display")
            )
          ),
          br(),
          # Puzzle Information Value Boxes
          uiOutput("puzzle_value_boxes")
        ),

        # Help Tab
        tabPanel("Help",
          br(),
          div(class = "p-4",
            h4("How to Use"),
            p("1. Adjust the settings in the left panel to customize your puzzle"),
            p("2. Click 'Generate Puzzle' to create a new puzzle"),
            p("3. Use the download buttons to save your puzzle"),
            br(),
            h4("Parameter Guide"),
            tags$ul(
              tags$li(strong("Grid:"), " Number of rows and columns determines piece count"),
              tags$li(strong("Size:"), " Physical dimensions in millimeters"),
              tags$li(strong("Tab Size:"), " Controls the size of interlocking tabs (15-25%)"),
              tags$li(strong("Jitter:"), " Adds randomness to piece shapes (2-6%)"),
              tags$li(strong("Piece Separation:"), " Gap between pieces (0 = complete puzzle, >0 = separated)")
            ),
            br(),
            h4("Download Options"),
            tags$ul(
              tags$li(strong("Complete Puzzle:"), " All pieces at original positions (offset=0)"),
              tags$li(strong("Current View:"), " Exactly what you see (WYSIWYG)"),
              tags$li(strong("Individual Pieces:"), " Each piece as a separate SVG file")
            ),
            br(),
            h4("Tips for Laser Cutting"),
            p("• Use separation of 3-5mm for rectangular, 5-10mm for hexagonal"),
            p("• Black color with 0.5mm line width works best"),
            p("• Consider material thickness when setting tab size"),
            p("• Test with small puzzles first")
          )
        )
      )  # End of tabsetPanel
  )  # End of layout_sidebar
)

# Helper function to map boundary shape selection to internal parameters
get_hex_boundary_params <- function(boundary_choice) {
  switch(boundary_choice,
    "zigzag"     = list(do_warp = FALSE, do_trunc = FALSE, do_circular_border = FALSE),
    "hexagon"    = list(do_warp = FALSE, do_trunc = TRUE,  do_circular_border = FALSE),
    "warped"     = list(do_warp = TRUE,  do_trunc = FALSE, do_circular_border = FALSE),
    "warped_hex" = list(do_warp = TRUE,  do_trunc = TRUE,  do_circular_border = FALSE),
    "circle"     = list(do_warp = TRUE,  do_trunc = TRUE,  do_circular_border = TRUE),
    # Default fallback
    list(do_warp = FALSE, do_trunc = FALSE, do_circular_border = FALSE)
  )
}

# Define server logic
server <- function(input, output, session) {

  # Reactive values to store puzzle data
  puzzle_data <- reactiveVal(NULL)
  svg_content <- reactiveVal(NULL)

  # Store positioned result for re-rendering (Epic #32 enhancement)
  # This allows styling changes without regenerating pieces

  positioned_result <- reactiveVal(NULL)

  # Randomize seed
  observeEvent(input$randomize, {
    updateNumericInput(session, "seed",
                      value = sample(1:99999, 1))
  })

  # Reset to defaults
  observeEvent(input$reset, {
    updateNumericInput(session, "rows", value = 2)
    updateNumericInput(session, "cols", value = 2)
    updateNumericInput(session, "width", value = 200)
    updateNumericInput(session, "height", value = 200)
    updateNumericInput(session, "seed", value = 1234)
    updateSliderInput(session, "tabsize", value = 20)
    updateSliderInput(session, "jitter", value = 4)
    updateSliderInput(session, "offset", value = 0)
    updateSelectInput(session, "color_palette", selected = "magma")
    updateSliderInput(session, "stroke_width", value = 1.5)
    updateSliderInput(session, "opacity", value = 100)
    # Reset label settings
    update_switch(id = "show_labels", value = FALSE, session = session)
    colourpicker::updateColourInput(session, "label_color", value = "#000000")
    updateSliderInput(session, "label_size", value = 0)
    # Reset fill options
    updateRadioButtons(session, "fill_type", selected = "none")
    # Reset background settings
    updateRadioButtons(session, "background_type", selected = "none")
    colourpicker::updateColourInput(session, "background_color", value = "#ffffff")
    colourpicker::updateColourInput(session, "gradient_center", value = "#e3f2fd")
    colourpicker::updateColourInput(session, "gradient_middle", value = "#bbdefb")
    colourpicker::updateColourInput(session, "gradient_edge", value = "#90caf9")
    # Reset hexagonal options
    updateNumericInput(session, "rings", value = 3)
    updateNumericInput(session, "diameter", value = 240)
    updateRadioButtons(session, "hex_boundary", selected = "zigzag")
  })

  # Generate puzzle using unified pipeline (Epic #32)
  # Only basic settings trigger regeneration; styling is reactive
  observeEvent(input$generate, {

    log_header("Generate button clicked")
    log_info("Puzzle type: {.strong {input$puzzle_type}}")
    log_info("Offset: {input$offset}mm")

    tryCatch({
      # Show progress
      withProgress(message = 'Generating puzzle...', value = 0, {

        incProgress(0.3, detail = "Creating puzzle structure")

        # === UNIFIED PIPELINE (Epic #32) ===
        puzzle_type <- input$puzzle_type

        # Build parameters based on puzzle type
        if (puzzle_type == "hexagonal") {
          grid_param <- c(input$rings)
          size_param <- c(input$diameter)
        } else {
          grid_param <- c(input$rows, input$cols)
          size_param <- c(input$width, input$height)
        }

        incProgress(0.5, detail = "Generating pieces")

        # Step 1: Generate pieces internally (basic settings only)
        # Get boundary parameters from radio button selection
        boundary_params <- get_hex_boundary_params(input$hex_boundary)
        pieces_result <- generate_pieces_internal(
          type = puzzle_type,
          seed = input$seed,
          grid = grid_param,
          size = size_param,
          tabsize = input$tabsize,
          jitter = input$jitter,
          do_warp = boundary_params$do_warp,
          do_trunc = boundary_params$do_trunc,
          do_circular_border = boundary_params$do_circular_border
        )

        incProgress(0.7, detail = "Applying positioning")

        # Step 2: Apply positioning (offset is a basic setting)
        positioned <- apply_piece_positioning(pieces_result, offset = input$offset)

        incProgress(1, detail = "Complete!")

        # Store positioned result for reactive rendering
        positioned_result(positioned)

        # Store puzzle data based on type
        if (puzzle_type == "hexagonal") {
          num_pieces <- 3 * input$rings * (input$rings - 1) + 1
          puzzle_data(list(
            type = "hexagonal",
            rings = input$rings,
            diameter = input$diameter,
            seed = input$seed,
            total_pieces = num_pieces,
            offset = input$offset
          ))
        } else {
          puzzle_data(list(
            type = "rectangular",
            rows = input$rows,
            cols = input$cols,
            width = input$width,
            height = input$height,
            seed = input$seed,
            total_pieces = input$rows * input$cols,
            offset = input$offset
          ))
        }
        log_success("Puzzle generation complete!")
      })
    }, error = function(e) {
      log_error("ERROR in puzzle generation")
      log_error("Message: {e$message}")
      log_error("Call: {deparse(e$call)}")
      log_info("Traceback:")
      print(traceback())
      showNotification(paste("Error:", e$message), type = "error", duration = 10)
    })
  })

  # Reactive SVG rendering - updates when styling options change

  # This re-renders without regenerating pieces
  rendered_svg <- reactive({
    # Depend on positioned result (from generate button)
    pos <- positioned_result()
    if (is.null(pos)) return(NULL)

    # Styling options (these trigger re-render, not regeneration)
    fill_color_value <- if (is.null(input$fill_type) || input$fill_type == "none") {
      "none"
    } else {
      input$fill_color
    }

    background_value <- switch(input$background_type,
      "none" = "none",
      "solid" = input$background_color,
      "gradient" = list(
        type = "gradient",
        center = input$gradient_center,
        middle = input$gradient_middle,
        edge = input$gradient_edge
      )
    )

    # Get label settings
    show_labels_value <- if (is.null(input$show_labels)) FALSE else input$show_labels
    label_color_value <- if (is.null(input$label_color)) "#000000" else input$label_color
    # 0 means auto-size, convert to NULL for render function
    label_size_value <- if (is.null(input$label_size) || input$label_size == 0) NULL else input$label_size

    # Render SVG with current styling
    svg <- render_puzzle_svg(
      pos,
      fill = fill_color_value,
      stroke_width = input$stroke_width,
      colors = NULL,
      palette = input$color_palette,
      background = background_value,
      opacity = input$opacity / 100,
      show_labels = show_labels_value,
      label_color = label_color_value,
      label_size = label_size_value
    )

    # Also update the svg_content reactive for downloads
    svg_content(svg)

    return(svg)
  })

  # Display puzzle - uses rendered_svg() for reactive styling updates
  output$puzzle_display <- renderUI({
    svg <- rendered_svg()
    if (is.null(svg)) {
      div(class = "text-center p-5 text-muted",
        icon("puzzle-piece", class = "fa-3x mb-3"),
        h4("No puzzle generated yet"),
        p("Click 'Generate Puzzle' to create your first puzzle")
      )
    } else {
      # Display SVG directly in HTML
      HTML(svg)
    }
  })

  # Display puzzle information
  output$puzzle_value_boxes <- renderUI({
    if (!is.null(puzzle_data())) {
      data <- puzzle_data()

      if (data$type == "hexagonal") {
        # Hexagonal puzzle value boxes
        layout_column_wrap(
          width = 1/4,
          value_box(
            title = "Type",
            value = "Hexagonal",
            showcase = bsicons::bs_icon("hexagon"),
            theme = "primary"
          ),
          value_box(
            title = "Rings",
            value = data$rings,
            showcase = bsicons::bs_icon("layers"),
            theme = "info"
          ),
          value_box(
            title = "Pieces",
            value = data$total_pieces,
            showcase = bsicons::bs_icon("puzzle"),
            theme = "success"
          ),
          value_box(
            title = "Diameter",
            value = sprintf("%.0f mm", data$diameter),
            showcase = bsicons::bs_icon("arrows-expand"),
            theme = "warning"
          )
        )
      } else {
        # Rectangular puzzle value boxes
        effective_width <- input$width
        effective_height <- input$height
        area_increase <- 0

        if (input$offset > 0) {
          effective_width <- input$width + (input$cols - 1) * input$offset
          effective_height <- input$height + (input$rows - 1) * input$offset
          area_increase <- ((effective_width * effective_height) -
                           (input$width * input$height)) /
                           (input$width * input$height) * 100
        }

        layout_column_wrap(
          width = 1/4,
          value_box(
            title = "Grid",
            value = sprintf("%d × %d", data$rows, data$cols),
            showcase = bsicons::bs_icon("grid-3x3"),
            theme = "primary"
          ),
          value_box(
            title = "Pieces",
            value = data$total_pieces,
            showcase = bsicons::bs_icon("puzzle"),
            theme = "success"
          ),
          value_box(
            title = "Output Size",
            value = sprintf("%.0f × %.0f mm", effective_width, effective_height),
            showcase = bsicons::bs_icon("arrows-expand"),
            theme = "info",
            p = if (input$offset > 0 && area_increase > 0) {
              sprintf("Area: +%.1f%%", area_increase)
            } else {
              NULL
            }
          ),
          value_box(
            title = "Seed",
            value = data$seed,
            showcase = bsicons::bs_icon("dice-3"),
            theme = "secondary"
          )
        )
      }
    }
  })

  # Helper function to build filename prefix
  build_filename_prefix <- function() {
    data <- puzzle_data()
    if (is.null(data)) return("puzzle")
    if (data$type == "hexagonal") {
      sprintf("hexagonal_%drings_seed%d", data$rings, data$seed)
    } else {
      sprintf("puzzle_%dx%d_seed%d", data$rows, data$cols, data$seed)
    }
  }

  # Download handler for Complete puzzle (offset=0)
  output$download_complete <- downloadHandler(
    filename = function() {
      paste0(build_filename_prefix(), "_complete.svg")
    },
    content = function(file) {
      data <- puzzle_data()
      if (is.null(data)) return()

      # Determine fill color value
      fill_color_value <- if (is.null(input$fill_type) || input$fill_type == "none") {
        "none"
      } else {
        input$fill_color
      }

      # Determine background value
      background_value <- switch(input$background_type,
        "none" = "none",
        "solid" = input$background_color,
        "gradient" = list(
          type = "gradient",
          center_color = input$gradient_center,
          edge_color = input$gradient_edge
        )
      )

      # Build parameters based on puzzle type
      if (data$type == "hexagonal") {
        grid_param <- c(input$rings)
        size_param <- c(input$diameter)
      } else {
        grid_param <- c(input$rows, input$cols)
        size_param <- c(input$width, input$height)
      }

      # Generate complete puzzle (offset=0)
      # Get boundary parameters from radio button selection
      boundary_params <- get_hex_boundary_params(input$hex_boundary)
      result <- generate_puzzle(
        type = data$type,
        grid = grid_param,
        size = size_param,
        seed = data$seed,
        tabsize = input$tabsize,
        jitter = input$jitter,
        offset = 0,  # Always complete for this download
        fill_color = fill_color_value,
        stroke_width = input$stroke_width,
        palette = input$color_palette,
        background = background_value,
        opacity = input$opacity / 100,
        save_files = FALSE,
        do_warp = boundary_params$do_warp,
        do_trunc = boundary_params$do_trunc,
        do_circular_border = boundary_params$do_circular_border
      )

      writeLines(result$svg_content, file)
    },
    contentType = "image/svg+xml"
  )

  # Download handler for WYSIWYG (current view)
  output$download_wysiwyg <- downloadHandler(
    filename = function() {
      data <- puzzle_data()
      if (is.null(data)) return("puzzle.svg")
      sep_suffix <- if (data$offset > 0) "_separated" else "_complete"
      paste0(build_filename_prefix(), sep_suffix, ".svg")
    },
    content = function(file) {
      if (!is.null(svg_content())) {
        writeLines(svg_content(), file)
      }
    },
    contentType = "image/svg+xml"
  )

  # Handle individual pieces download with sequential browser downloads
  observeEvent(input$download_pieces, {
    if (!is.null(puzzle_data())) {
      data <- puzzle_data()

      if (data$type == "rectangular") {
        # Create www/pieces directory if it doesn't exist
        pieces_dir <- file.path("www", "pieces")
        if (!dir.exists(pieces_dir)) {
          dir.create(pieces_dir, recursive = TRUE)
        }

        # Clean up old files
        old_files <- list.files(pieces_dir, pattern = "piece_.*\\.svg$", full.names = TRUE)
        if (length(old_files) > 0) {
          file.remove(old_files)
        }

        # Test write permissions
        test_file <- file.path(pieces_dir, "test.txt")
        tryCatch({
          writeLines("test", test_file)
          if (file.exists(test_file)) file.remove(test_file)
        }, error = function(e) {
          showNotification(
            paste("Cannot write to directory:", e$message),
            type = "error",
            duration = 10
          )
          return(NULL)
        })

        # Determine background value based on type
        background_value <- switch(input$background_type,
          "none" = "none",
          "solid" = input$background_color,
          "gradient" = list(
            type = "gradient",
            center = input$gradient_center,
            middle = input$gradient_middle,
            edge = input$gradient_edge
          )
        )

        # Generate individual pieces (suppress console output)
        result <- tryCatch({
          capture.output({
            generate_individual_pieces(
              seed = data$seed,
              xn = data$cols,
              yn = data$rows,
              width = data$width,
              height = data$height,
              tabsize = input$tabsize,
              jitter = input$jitter,
              output_dir = pieces_dir,
              save_combined = FALSE,
              palette = input$color_palette,
              stroke_width = input$stroke_width,
              background = background_value
            )
          }, type = "message")
        }, error = function(e) {
          showNotification(
            paste("Error generating pieces:", e$message),
            type = "error",
            duration = 10
          )
          return(NULL)
        })

        # Verify files were created and send to browser
        piece_files <- list.files(pieces_dir, pattern = "piece_.*\\.svg$", full.names = TRUE)

        if (length(piece_files) > 0) {
          # Check that files have content
          files_ok <- all(sapply(piece_files, file.size) > 0)

          if (files_ok) {
            # Create file list for JavaScript
            files_list <- lapply(basename(piece_files), function(filename) {
              list(
                url = paste0("pieces/", filename),
                name = filename
              )
            })

            # Send to JavaScript for sequential download
            session$sendCustomMessage("downloadFiles", files_list)

            showNotification(
              sprintf("Downloading %d piece files...", length(piece_files)),
              type = "message",
              duration = 3
            )
          } else {
            showNotification(
              "Error: Generated files are empty. Please try again.",
              type = "error",
              duration = 5
            )
          }
        } else {
          showNotification(
            "Error: No piece files were generated. Please try again.",
            type = "error",
            duration = 5
          )
        }

      } else if (data$type == "hexagonal") {
        # TODO: Implement hexagonal individual pieces (Issue #10)
        showNotification(
          "Individual hexagonal pieces coming soon! See GitHub Issue #10",
          type = "warning",
          duration = 5
        )
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
