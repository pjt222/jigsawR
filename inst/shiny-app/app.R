# jigsawR Shiny Application
# Interactive puzzle generator with real-time preview and download

library(shiny)
library(shinyjs)

# Source the required functions from the package
# In production, these would be loaded via library(jigsawR)
source_dir <- function(path) {
  if (file.exists(path)) {
    files <- list.files(path, pattern = "\\.R$", full.names = TRUE)
    for (file in files) {
      # Skip archive and example files
      if (!grepl("scripts_archive|examples", file)) {
        source(file)
      }
    }
  }
}

# Try to load functions (adjust path based on where app is run from)
if (file.exists("../../R")) {
  source_dir("../../R")
} else if (file.exists("R")) {
  source_dir("R")
}

# Define UI
ui <- fluidPage(
  useShinyjs(),

  # Add custom CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$style(HTML("
      .svg-container {
        width: 100%;
        height: 600px;
        border: 1px solid #ddd;
        border-radius: 4px;
        padding: 10px;
        background: white;
        overflow: auto;
      }
      .svg-display {
        width: 100%;
        height: 100%;
      }
      .sidebar-section {
        margin-bottom: 20px;
        padding: 10px;
        background: #f8f9fa;
        border-radius: 4px;
      }
      .section-title {
        font-weight: bold;
        margin-bottom: 10px;
        color: #495057;
      }
    "))
  ),

  # Application title
  titlePanel(
    div(
      h2("jigsawR Puzzle Generator", style = "color: #2c3e50;"),
      p("Create customizable jigsaw puzzles for laser cutting and more",
        style = "color: #7f8c8d; font-size: 14px;")
    )
  ),

  # Sidebar with input controls
  sidebarLayout(
    sidebarPanel(
      width = 4,

      # Basic Settings Section
      div(class = "sidebar-section",
        h4("Basic Settings", class = "section-title"),

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
                        style = "margin-top: 2px;")
          )
        )
      ),

      # Advanced Settings Section
      div(class = "sidebar-section",
        h4("Advanced Settings", class = "section-title"),

        sliderInput("tabsize", "Tab Size (%):",
                   min = 10, max = 30, value = 20, step = 1),

        sliderInput("jitter", "Jitter (%):",
                   min = 0, max = 10, value = 4, step = 1),

        conditionalPanel(
          condition = "input.puzzle_type == 'rectangular'",
          radioButtons("output_mode", "Output Mode:",
                      choices = list(
                        "Complete Puzzle" = "complete",
                        "Individual Pieces" = "individual",
                        "Separated Pieces" = "separated"
                      ),
                      selected = "complete")
        ),

        conditionalPanel(
          condition = "input.puzzle_type == 'hexagonal'",
          radioButtons("output_mode_hex", "Output Mode:",
                      choices = list(
                        "Complete Puzzle" = "complete",
                        "Individual Pieces" = "individual",
                        "Separated Pieces" = "separated"
                      ),
                      selected = "complete")
        ),

        # Conditional separation offset
        conditionalPanel(
          condition = "input.output_mode == 'separated' || input.output_mode_hex == 'separated'",
          sliderInput("offset", "Separation (mm):",
                     min = 0, max = 50, value = 10, step = 1),
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
      div(class = "sidebar-section",
        h4("Styling Options", class = "section-title"),

        selectInput("color_scheme", "Color Scheme:",
                   choices = list(
                     "Black" = "black",
                     "Rainbow" = "rainbow",
                     "Blues" = "blues",
                     "Warm" = "warm",
                     "Cool" = "cool"
                   ),
                   selected = "black"),

        sliderInput("stroke_width", "Line Width:",
                   min = 0.5, max = 3, value = 1.5, step = 0.1),

        selectInput("background", "Background:",
                   choices = list(
                     "None" = "none",
                     "White" = "white",
                     "Gradient" = "gradient",
                     "Light Blue" = "#e3f2fd"
                   ),
                   selected = "white")
      ),

      # Action Buttons
      div(style = "margin-top: 20px;",
        actionButton("generate", "Generate Puzzle",
                    icon = icon("puzzle-piece"),
                    class = "btn-primary btn-lg btn-block",
                    style = "margin-bottom: 10px;"),

        fluidRow(
          column(6,
            actionButton("reset", "Reset",
                        icon = icon("undo"),
                        class = "btn-default btn-block")
          ),
          column(6,
            downloadButton("download", "Download SVG",
                          class = "btn-success btn-block")
          )
        ),

        # Help text for individual pieces download
        conditionalPanel(
          condition = "input.output_mode == 'individual' || input.output_mode_hex == 'individual'",
          div(style = "margin-top: 10px; padding: 8px; background: #d1ecf1; border: 1px solid #bee5eb; border-radius: 4px;",
            p(style = "font-size: 12px; margin: 0;",
              icon("info-circle"),
              strong(" Individual Pieces Mode:"),
              br(),
              "Download creates a ZIP file with all pieces as separate SVG files.",
              conditionalPanel(
                condition = "input.puzzle_type == 'hexagonal'",
                br(),
                em("Note: Hexagonal individual pieces available as combined SVG (separate files coming in Issue #10)")
              )
            )
          )
        )
      ),

      # Info text
      div(style = "margin-top: 20px; padding: 10px; background: #fff3cd; border-radius: 4px;",
        p(strong("Tip:"), "Click 'Generate Puzzle' to create your puzzle.
          The preview will appear on the right.", style = "font-size: 12px; margin: 0;")
      )
    ),

    # Main panel with puzzle display
    mainPanel(
      width = 8,

      # Tabs for different views
      tabsetPanel(
        id = "main_tabs",

        # Preview Tab
        tabPanel("Preview",
          br(),
          div(class = "svg-container",
            uiOutput("puzzle_display")
          ),

          # Parameter summary
          div(class = "parameter-summary",
            h5("Puzzle Information"),
            uiOutput("puzzle_info")
          )
        ),

        # Help Tab
        tabPanel("Help",
          br(),
          div(style = "padding: 20px;",
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
      )
    )
  )
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

    # Show progress
    withProgress(message = 'Generating puzzle...', value = 0, {

      incProgress(0.3, detail = "Creating puzzle structure")

      # Define colors based on scheme
      colors <- switch(input$color_scheme,
        "black" = "black",
        "rainbow" = c("#FF6B6B", "#4ECDC4", "#45B7D1", "#FFA07A", "#98D8C8",
                     "#F7DC6F", "#BB8FCE", "#85C1E2", "#F8B739"),
        "blues" = c("#0066CC", "#3399FF", "#66B2FF", "#99CCFF"),
        "warm" = c("#FF6B6B", "#FFA07A", "#F7DC6F", "#F8B739"),
        "cool" = c("#4ECDC4", "#45B7D1", "#98D8C8", "#85C1E2"),
        "black"
      )

      incProgress(0.5, detail = "Generating SVG")

      # Determine puzzle type and output mode
      puzzle_type <- input$puzzle_type
      output_mode <- ifelse(puzzle_type == "rectangular",
                           input$output_mode,
                           input$output_mode_hex)

      # Generate puzzle based on type
      if (puzzle_type == "hexagonal") {
        if (output_mode == "separated") {
          # Use hexagonal separation function
          svg <- generate_separated_hexagonal_svg(
            rings = input$rings,
            seed = input$seed,
            diameter = input$diameter,
            offset = input$offset,
            arrangement = ifelse(is.null(input$arrangement), "hexagonal", input$arrangement),
            tabsize = input$tabsize,
            jitter = input$jitter,
            do_warp = input$do_warp,
            do_trunc = input$do_trunc,
            colors = colors,
            stroke_width = input$stroke_width,
            background = input$background
          )
        } else {
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
            stroke_width = input$stroke_width
          )

          # Use proper conditional instead of ifelse for character vectors
          if (output_mode == "complete") {
            svg <- puzzle_result$svg_complete
          } else {
            svg <- puzzle_result$svg_individual
          }
        }

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
          background = input$background
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
          save_files = FALSE
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
          seed = input$seed,
          total_pieces = input$rows * input$cols
        ))
      }
    })
  })

  # Display puzzle
  output$puzzle_display <- renderUI({
    if (is.null(svg_content())) {
      div(style = "text-align: center; padding: 50px; color: #999;",
        icon("puzzle-piece", style = "font-size: 48px;"),
        h4("No puzzle generated yet"),
        p("Click 'Generate Puzzle' to create your first puzzle")
      )
    } else {
      # Display SVG directly in HTML
      HTML(svg_content())
    }
  })

  # Display puzzle information
  output$puzzle_info <- renderUI({
    if (!is.null(puzzle_data())) {
      data <- puzzle_data()

      if (data$type == "hexagonal") {
        # Hexagonal puzzle info
        tags$div(
          tags$p(
            tags$strong("Type: "), "Hexagonal",
            tags$br(),
            tags$strong("Rings: "), data$rings,
            tags$br(),
            tags$strong("Total Pieces: "), data$total_pieces,
            tags$br(),
            tags$strong("Seed: "), data$seed,
            tags$br(),
            tags$strong("Diameter: "), sprintf("%.0f mm", data$diameter)
          )
        )
      } else {
        # Rectangular puzzle info
        effective_width <- input$width
        effective_height <- input$height

        if (input$output_mode == "separated") {
          effective_width <- input$width + (input$cols - 1) * input$offset
          effective_height <- input$height + (input$rows - 1) * input$offset
        }

        tags$div(
          tags$p(
            tags$strong("Grid: "), sprintf("%d × %d", data$rows, data$cols),
            tags$br(),
            tags$strong("Total Pieces: "), data$total_pieces,
            tags$br(),
            tags$strong("Seed: "), data$seed,
            tags$br(),
            tags$strong("Output Size: "), sprintf("%.0f × %.0f mm",
                                                 effective_width, effective_height),
            if (input$output_mode == "separated") {
              tags$span(
                tags$br(),
                tags$strong("Area Increase: "),
                sprintf("+%.1f%%",
                       ((effective_width * effective_height) -
                        (input$width * input$height)) /
                        (input$width * input$height) * 100)
              )
            }
          )
        )
      }
    }
  })

  # Download handler
  output$download <- downloadHandler(
    filename = function() {
      if (!is.null(puzzle_data())) {
        data <- puzzle_data()

        # Check if individual pieces mode is selected
        output_mode <- if (data$type == "hexagonal") input$output_mode_hex else input$output_mode

        if (output_mode == "individual") {
          # Return ZIP filename for individual pieces
          if (data$type == "hexagonal") {
            sprintf("hexagonal_%drings_seed%d_pieces.zip", data$rings, data$seed)
          } else {
            sprintf("puzzle_%dx%d_seed%d_pieces.zip", data$rows, data$cols, data$seed)
          }
        } else {
          # Return SVG filename for complete/separated puzzles
          if (data$type == "hexagonal") {
            sprintf("hexagonal_%drings_seed%d_%s.svg",
                   data$rings, data$seed, output_mode)
          } else {
            sprintf("puzzle_%dx%d_seed%d_%s.svg",
                   data$rows, data$cols, data$seed, output_mode)
          }
        }
      } else {
        "puzzle.svg"
      }
    },
    content = function(file) {
      if (!is.null(puzzle_data())) {
        data <- puzzle_data()
        output_mode <- if (data$type == "hexagonal") input$output_mode_hex else input$output_mode

        if (output_mode == "individual" && data$type == "rectangular") {
          # Generate individual pieces for rectangular puzzles
          temp_dir <- tempfile()
          dir.create(temp_dir, showWarnings = FALSE)

          # Generate individual pieces
          tryCatch({
            result <- generate_individual_pieces(
              seed = data$seed,
              xn = data$cols,
              yn = data$rows,
              width = data$width,
              height = data$height,
              tabsize = data$tabsize,
              jitter = data$jitter,
              output_dir = temp_dir,
              save_combined = TRUE
            )

            # Get list of generated files
            piece_files <- list.files(temp_dir, pattern = "\\.svg$", full.names = TRUE)

            if (length(piece_files) == 0) {
              stop("No piece files were generated")
            }

            # Create ZIP file using zip package
            zip::zip(zipfile = file,
                    files = basename(piece_files),
                    root = temp_dir)

            # Cleanup temp directory
            unlink(temp_dir, recursive = TRUE)

          }, error = function(e) {
            # Cleanup on error
            if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)
            stop(paste("Error generating individual pieces:", e$message))
          })

        } else if (output_mode == "individual" && data$type == "hexagonal") {
          # For hexagonal, currently download the combined SVG
          # TODO: Implement individual hexagonal pieces (Issue #10)
          if (!is.null(svg_content())) {
            writeLines(svg_content(), file)
          }

        } else {
          # For complete/separated modes, download single SVG
          if (!is.null(svg_content())) {
            writeLines(svg_content(), file)
          }
        }
      }
    },
    contentType = function() {
      if (!is.null(puzzle_data())) {
        data <- puzzle_data()
        output_mode <- if (data$type == "hexagonal") input$output_mode_hex else input$output_mode
        if (output_mode == "individual" && data$type == "rectangular") {
          return("application/zip")
        }
      }
      return("image/svg+xml")
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
