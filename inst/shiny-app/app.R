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

  # Add custom CSS and JavaScript for sequential downloads
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
    ")),
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
          div(style = "margin-top: 10px; padding: 8px; background: #d1ecf1; border: 1px solid #17a2b8; border-radius: 4px;",
            p(style = "font-size: 12px; margin: 0;",
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

        # Generate individual pieces (DON'T suppress output during debugging)
        cat("\n=== Starting piece generation ===\n")
        cat(sprintf("Seed: %d, Cols: %d, Rows: %d\n", data$seed, data$cols, data$rows))
        cat(sprintf("Width: %.0f, Height: %.0f\n", data$width, data$height))
        cat(sprintf("Output dir: %s\n", pieces_dir))

        result <- tryCatch({
          generate_individual_pieces(
            seed = data$seed,
            xn = data$cols,
            yn = data$rows,
            width = data$width,
            height = data$height,
            output_dir = pieces_dir,
            save_combined = FALSE
          )
        }, error = function(e) {
          showNotification(
            paste("Error generating pieces:", e$message),
            type = "error",
            duration = 10
          )
          cat("ERROR in generate_individual_pieces:\n")
          cat(e$message, "\n")
          print(e)
          return(NULL)
        })

        cat("=== Generation complete ===\n\n")

        # Verify files were created and have content
        piece_files <- list.files(pieces_dir, pattern = "piece_.*\\.svg$", full.names = TRUE)

        if (length(piece_files) > 0) {
          # Check that files have content
          file_sizes <- sapply(piece_files, file.size)
          files_ok <- all(file_sizes > 0)

          # Debug: show file info
          cat(sprintf("Generated %d files in %s\n", length(piece_files), pieces_dir))
          cat(sprintf("File sizes: %s\n", paste(file_sizes, collapse = ", ")))
          cat(sprintf("First file content (first 100 chars):\n%s\n",
                     substr(paste(readLines(piece_files[1], warn = FALSE), collapse = "\n"), 1, 100)))

          if (files_ok) {
            # Ensure files are flushed to disk
            Sys.sleep(0.5)

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
              sprintf("Downloading %d piece files... (sizes: %s bytes)",
                     length(piece_files),
                     paste(file_sizes, collapse = ", ")),
              type = "message",
              duration = 5
            )
          } else {
            showNotification(
              "Error: Generated files are empty",
              type = "error",
              duration = 5
            )
          }
        } else {
          showNotification(
            "Error: No piece files were generated",
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
