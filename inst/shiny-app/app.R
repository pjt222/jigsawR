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

          # Hexagonal-specific options
          checkboxInput("do_warp", "Circular Warp", value = FALSE),
          checkboxInput("do_trunc", "Truncate Edges", value = FALSE)
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
                     min = 0, max = 50, value = 20, step = 1,
                     ticks = TRUE,
                     post = "%",
                     sep = ""),
          "Controls the size of interlocking tabs. Higher values create larger, more prominent tabs. Recommended: 15-25%"
        ),

        tooltip(
          sliderInput("jitter", "Jitter:",
                     min = 0, max = 25, value = 4, step = 1,
                     ticks = TRUE,
                     post = "%",
                     sep = ""),
          "Adds randomness to piece shapes for more organic variation. Higher values create more irregular pieces. Recommended: 2-6%"
        ),

        conditionalPanel(
          condition = "input.puzzle_type == 'rectangular'",
          tooltip(
            radioButtons("output_mode", "Output Mode:",
                        choices = list(
                          "Complete Puzzle" = "complete",
                          "Individual Pieces" = "individual",
                          "Separated Pieces" = "separated"
                        ),
                        selected = "complete"),
            "Complete: All pieces connected | Individual: Colored pieces | Separated: Pieces with gaps for laser cutting"
          )
        ),

        conditionalPanel(
          condition = "input.puzzle_type == 'hexagonal'",
          radioButtons("output_mode_hex", "Output Mode:",
                      choices = list(
                        "Complete Puzzle" = "complete"
                        # "Individual Pieces" = "individual",  # TODO: Issue #10 - Not yet implemented
                        # "Separated Pieces" = "separated"     # TODO: Issue #7 - Not yet implemented
                      ),
                      selected = "complete")
        ),

        # Conditional separation offset
        conditionalPanel(
          condition = "input.output_mode == 'separated' || input.output_mode_hex == 'separated'",
          tooltip(
            sliderInput("offset", "Separation:",
                       min = 0, max = 50, value = 10, step = 1,
                       ticks = TRUE,
                       post = " mm",
                       sep = ""),
            "Gap between pieces in separated mode. For laser cutting, use 3-5mm for rectangular or 5-10mm for hexagonal puzzles."
          ),
          conditionalPanel(
            condition = "input.puzzle_type == 'hexagonal'",
            radioButtons("arrangement", "Arrangement:",
                        choices = list(
                          "Hexagonal Grid" = "hexagonal",
                          "Rectangular Packing" = "rectangular"
                        ),
                        selected = "hexagonal",
                        inline = TRUE)
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

        selectInput("background", "Background:",
                   choices = list(
                     "None" = "none",
                     "White" = "white",
                     "Gradient" = "gradient",
                     "Light Blue" = "#e3f2fd"
                   ),
                   selected = "white")

        # Transparent background feature - waiting for PNG download implementation
        # Uncomment when Issue #25 (PNG download capability) is implemented:
        # checkboxInput("transparent_background",
        #              "Transparent Background (PNG only)",
        #              value = FALSE),
        # helpText(
        #   class = "text-muted small",
        #   icon("info-circle"), " ",
        #   "Makes areas outside puzzle circle transparent. Only works with PNG downloads."
        # )
        )
      ),  # Close accordion

      # Action Buttons
      div(class = "mt-3",
        actionButton("generate", "Generate Puzzle",
                    icon = icon("puzzle-piece"),
                    class = "btn-primary btn-lg btn-block mb-2"),

        fluidRow(
          column(6,
            actionButton("reset", "Reset",
                        icon = icon("undo"),
                        class = "btn-secondary btn-block")
          ),
          column(6,
            # Conditional download button based on output mode
            conditionalPanel(
              condition = "input.output_mode != 'individual' && input.output_mode_hex != 'individual'",
              downloadButton("download", "Download SVG",
                            class = "btn-success btn-block")
            ),
            conditionalPanel(
              condition = "input.output_mode == 'individual' || input.output_mode_hex == 'individual'",
              actionButton("download_pieces", "Download Pieces",
                          icon = icon("download"),
                          class = "btn-success btn-block")
            )
          )
        ),

        # Help text for individual pieces download
        conditionalPanel(
          condition = "input.output_mode == 'individual' || input.output_mode_hex == 'individual'",
          div(class = "alert alert-info mt-2 p-2",
            p(class = "small mb-0",
              icon("info-circle"),
              strong(" Individual Pieces:"),
              conditionalPanel(
                condition = "input.puzzle_type == 'rectangular'",
                br(),
                "Click 'Download Pieces' to download all pieces as separate SVG files.",
                br(),
                em("Note: Your browser will download files sequentially (one every 0.5 seconds).")
              ),
              conditionalPanel(
                condition = "input.puzzle_type == 'hexagonal'",
                br(),
                "Individual hexagonal pieces coming soon (see GitHub Issue #10).",
                br(),
                "Currently downloads combined SVG with all pieces."
              )
            )
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
            p("3. Use 'Download SVG' to save the puzzle for laser cutting or printing"),
            br(),
            h4("Parameter Guide"),
            tags$ul(
              tags$li(strong("Grid:"), " Number of rows and columns determines piece count"),
              tags$li(strong("Size:"), " Physical dimensions in millimeters"),
              tags$li(strong("Tab Size:"), " Controls the size of interlocking tabs (10-30%)"),
              tags$li(strong("Jitter:"), " Adds randomness to piece shapes (0-10%)"),
              tags$li(strong("Output Mode:"), " Choose how pieces are arranged"),
              tags$ul(
                tags$li("Complete: All pieces connected"),
                tags$li("Individual: Separate colored pieces"),
                tags$li("Separated: Pieces with gaps for laser cutting")
              ),
              tags$li(strong("Separation:"), " Gap between pieces in separated mode")
            ),
            br(),
            h4("Tips for Laser Cutting"),
            p("• Use 'Separated Pieces' mode with 3-5mm offset"),
            p("• Black color with 0.5mm line width works best"),
            p("• Consider material thickness when setting tab size"),
            p("• Test with small puzzles first"),
            br(),
            h4("Hexagonal Separation"),
            p("• Hexagonal Grid: Maintains natural hex pattern with gaps"),
            p("• Rectangular Packing: Efficient space usage for cutting"),
            p("• Higher offsets recommended for hex puzzles (5-10mm)")
          )
        )
      )  # End of tabsetPanel
  )  # End of layout_sidebar
)

# Define server logic
server <- function(input, output, session) {

  # Reactive values to store puzzle data
  puzzle_data <- reactiveVal(NULL)
  svg_content <- reactiveVal(NULL)

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
    updateRadioButtons(session, "output_mode", selected = "complete")
    updateSliderInput(session, "offset", value = 10)
    updateSelectInput(session, "color_scheme", selected = "black")
    updateSliderInput(session, "stroke_width", value = 1.5)
    updateSelectInput(session, "background", selected = "white")
  })

  # Generate puzzle
  observeEvent(input$generate, {

    log_header("Generate button clicked")
    log_info("Puzzle type: {.strong {input$puzzle_type}}")
    log_info("Working directory: {.path {getwd()}}")
    log_info("Files in current dir: {paste(list.files(), collapse=', ')}")

    tryCatch({
      # Show progress
      withProgress(message = 'Generating puzzle...', value = 0, {

        incProgress(0.3, detail = "Creating puzzle structure")
        log_info("Progress: Creating puzzle structure")

      # Use viridis palette (colors will be NULL, palette name will be passed)
      colors <- NULL
      palette <- input$color_palette

      incProgress(0.5, detail = "Generating SVG")

      # Determine puzzle type and output mode
      puzzle_type <- input$puzzle_type
      output_mode <- ifelse(puzzle_type == "rectangular",
                           input$output_mode,
                           input$output_mode_hex)

      # Generate puzzle based on type
      if (puzzle_type == "hexagonal") {
        # if (output_mode == "separated") {
        #   # TODO: Issue #7 - Hexagonal separation not yet fully implemented
        #   # Use hexagonal separation function
        #   svg <- generate_separated_hexagonal_svg(
        #     rings = input$rings,
        #     seed = input$seed,
        #     diameter = input$diameter,
        #     offset = input$offset,
        #     arrangement = ifelse(is.null(input$arrangement), "hexagonal", input$arrangement),
        #     tabsize = input$tabsize,
        #     jitter = input$jitter,
        #     do_warp = input$do_warp,
        #     do_trunc = input$do_trunc,
        #     colors = colors,
        #     stroke_width = input$stroke_width,
        #     background = input$background
        #   )
        # } else {
          # Generate standard hexagonal puzzle
          puzzle_result <- generate_puzzle(
            type = "hexagonal",
            grid = c(input$rings, input$rings),  # Use rings for grid
            size = c(input$diameter, input$diameter),  # Use diameter for size
            seed = input$seed,
            tabsize = input$tabsize,
            jitter = input$jitter,
            output = ifelse(output_mode == "complete", "complete", "individual"),
            colors = colors,
            background = input$background,
            save_files = FALSE,
            do_warp = input$do_warp,
            do_trunc = input$do_trunc,
            stroke_width = input$stroke_width,
            palette = palette
          )

          # Use proper conditional instead of ifelse for character vectors
          if (output_mode == "complete") {
            svg <- puzzle_result$svg_complete
          } else {
            svg <- puzzle_result$svg_individual
          }
        # }  # End commented out separated mode check

      } else if (output_mode == "separated") {
        # Use separated puzzle generation for rectangular
        puzzle_struct <- generate_puzzle_core(
          seed = input$seed,
          grid = c(input$rows, input$cols),
          size = c(input$width, input$height),
          tabsize = input$tabsize,
          jitter = input$jitter
        )

        svg <- generate_separated_puzzle_svg(
          puzzle_structure = puzzle_struct,
          offset = input$offset,
          colors = colors,
          stroke_width = input$stroke_width,
          background = input$background,
          palette = palette
        )

      } else {
        # Use standard generation for rectangular
        puzzle_result <- generate_puzzle(
          type = "rectangular",
          grid = c(input$rows, input$cols),
          size = c(input$width, input$height),
          seed = input$seed,
          tabsize = input$tabsize,
          jitter = input$jitter,
          output = ifelse(output_mode == "complete", "complete", "individual"),
          colors = colors,
          background = input$background,
          save_files = FALSE,
          palette = palette
        )

        # Use proper conditional instead of ifelse for character vectors
        if (output_mode == "complete") {
          svg <- puzzle_result$svg_complete
        } else {
          svg <- puzzle_result$svg_individual
        }
      }

      incProgress(1, detail = "Complete!")

      # Store the generated SVG
      log_info("Storing SVG content, length: {nchar(svg)}")
      svg_content(svg)

      # Store puzzle data based on type
      if (puzzle_type == "hexagonal") {
        num_pieces <- 3 * input$rings * (input$rings - 1) + 1
        puzzle_data(list(
          type = "hexagonal",
          rings = input$rings,
          diameter = input$diameter,
          seed = input$seed,
          total_pieces = num_pieces
        ))
      } else {
        puzzle_data(list(
          type = "rectangular",
          rows = input$rows,
          cols = input$cols,
          width = input$width,
          height = input$height,
          seed = input$seed,
          total_pieces = input$rows * input$cols
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

  # Display puzzle
  output$puzzle_display <- renderUI({
    if (is.null(svg_content())) {
      div(class = "text-center p-5 text-muted",
        icon("puzzle-piece", class = "fa-3x mb-3"),
        h4("No puzzle generated yet"),
        p("Click 'Generate Puzzle' to create your first puzzle")
      )
    } else {
      # Display SVG directly in HTML
      HTML(svg_content())
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

        if (input$output_mode == "separated") {
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
            p = if (input$output_mode == "separated" && area_increase > 0) {
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

  # Download handler for single SVG files (complete/separated modes)
  output$download <- downloadHandler(
    filename = function() {
      if (!is.null(puzzle_data())) {
        data <- puzzle_data()
        if (data$type == "hexagonal") {
          output_mode <- input$output_mode_hex
          sprintf("hexagonal_%drings_seed%d_%s.svg",
                 data$rings, data$seed, output_mode)
        } else {
          output_mode <- input$output_mode
          sprintf("puzzle_%dx%d_seed%d_%s.svg",
                 data$rows, data$cols, data$seed, output_mode)
        }
      } else {
        "puzzle.svg"
      }
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

        # Generate individual pieces (suppress console output)
        result <- tryCatch({
          capture.output({
            generate_individual_pieces(
              seed = data$seed,
              xn = data$cols,
              yn = data$rows,
              width = data$width,
              height = data$height,
              output_dir = pieces_dir,
              save_combined = FALSE,
              stroke_width = input$stroke_width
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
