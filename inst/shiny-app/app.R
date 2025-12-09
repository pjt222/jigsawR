# jigsawR Shiny Application
# Interactive puzzle generator with real-time preview and download

library(shiny)
library(bslib)
library(shinyjs)
library(waiter)
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

# Load configuration - SINGLE SOURCE OF TRUTH for all defaults
cfg <- get_puzzle_config()
log_info("Loaded configuration from config.yml")

# Extract commonly used config values for cleaner UI code
cfg_rect <- cfg$rectangular
cfg_hex <- cfg$hexagonal
cfg_conc <- cfg$concentric
cfg_style <- cfg$styling
cfg_colors <- cfg$colors
cfg_labels <- cfg$labels
cfg_bg <- cfg$background
cfg_ui <- cfg$ui
cfg_const <- cfg$constraints

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
  useWaiter(),  # Enable waiter loading screens

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

      # Parameter Accordion - Reorganized into Settings, Styling, Download
      accordion(
        id = "params_accordion",
        open = cfg_ui$default_accordion_open,  # From config.yml

        # ===== SETTINGS PANEL =====
        # Combines Basic Settings + Advanced Settings + Generate/Reset buttons
        accordion_panel(
          title = "Settings",
          value = "settings",
          icon = bsicons::bs_icon("gear"),

          # Puzzle Type Selection
          radioButtons("puzzle_type", "Puzzle Type:",
                      choices = list("Rectangular" = "rectangular",
                                    "Hexagonal" = "hexagonal",
                                    "Concentric" = "concentric"),
                      selected = "rectangular",
                      inline = TRUE),

          # Conditional UI for puzzle parameters (values from config.yml)
          conditionalPanel(
            condition = "input.puzzle_type == 'rectangular'",
            # Grid dimensions for rectangular
            fluidRow(
              column(6,
                numericInput("rows", "Rows:",
                            value = cfg_rect$rows,
                            min = cfg_const$rows$min,
                            max = cfg_const$rows$max, step = 1)
              ),
              column(6,
                numericInput("cols", "Columns:",
                            value = cfg_rect$cols,
                            min = cfg_const$cols$min,
                            max = cfg_const$cols$max, step = 1)
              )
            ),

            # Size for rectangular
            fluidRow(
              column(6,
                numericInput("width", "Width (mm):",
                            value = cfg_rect$width,
                            min = cfg_const$width$min,
                            max = cfg_const$width$max, step = 10)
              ),
              column(6,
                numericInput("height", "Height (mm):",
                            value = cfg_rect$height,
                            min = cfg_const$height$min,
                            max = cfg_const$height$max, step = 10)
              )
            )
          ),

          conditionalPanel(
            condition = "input.puzzle_type == 'hexagonal'",
            # Rings for hexagonal
            numericInput("rings", "Rings:",
                        value = cfg_hex$rings,
                        min = cfg_const$rings$min,
                        max = cfg_const$rings$max),

            # Diameter for hexagonal
            numericInput("diameter", "Diameter (mm):",
                        value = cfg_hex$diameter,
                        min = cfg_const$diameter$min,
                        max = cfg_const$diameter$max, step = 10),

            # Hexagonal boundary shape options
            radioButtons("hex_boundary", "Boundary Shape:",
                        choices = list(
                          "Zigzag (Original)" = "zigzag",
                          "Clean Hexagon" = "hexagon",
                          "Warped Zigzag" = "warped",
                          "Warped Hexagon" = "warped_hex",
                          "Perfect Circle" = "circle"
                        ),
                        selected = cfg_hex$boundary)
          ),

          # Concentric puzzle type panel
          conditionalPanel(
            condition = "input.puzzle_type == 'concentric'",
            # Rings for concentric
            numericInput("rings_conc", "Rings:",
                        value = cfg_conc$rings,
                        min = cfg_const$rings$min,
                        max = cfg_const$rings$max),

            # Diameter for concentric
            numericInput("diameter_conc", "Diameter (mm):",
                        value = cfg_conc$diameter,
                        min = cfg_const$diameter$min,
                        max = cfg_const$diameter$max, step = 10),

            # Note: center_shape is hardcoded to "hexagon" for now
            # Future: may add UI option - see GitHub issue for refinement plans

            # Concentric boundary shape options
            radioButtons("conc_boundary", "Boundary Shape:",
                        choices = list(
                          "Straight" = "straight",
                          "Perfect Circle" = "circle"
                        ),
                        selected = if (!is.null(cfg_conc$boundary)) cfg_conc$boundary else "straight",
                        inline = TRUE),

            # Boundary facing direction (only shown when "Perfect Circle" is selected)
            conditionalPanel(
              condition = "input.conc_boundary == 'circle'",
              radioButtons("conc_boundary_facing", "Arc Direction:",
                          choices = list(
                            "Outward (convex)" = "outward",
                            "Inward (concave)" = "inward"
                          ),
                          selected = if (!is.null(cfg_conc$boundary_facing)) cfg_conc$boundary_facing else "outward",
                          inline = TRUE)
            )
          ),

          # Seed
          fluidRow(
            column(8,
              numericInput("seed", "Random Seed:",
                          value = cfg$seed,
                          min = cfg_const$seed$min,
                          max = cfg_const$seed$max, step = 1)
            ),
            column(4,
              br(),
              actionButton("randomize", "Random",
                          icon = icon("dice"),
                          class = "mt-4")
            )
          ),

          # Divider before fusion section
          tags$hr(class = "my-2"),
          tags$small(class = "text-muted", "Meta Pieces (Fusion)"),

          # Fusion groups input - ONLY applies when Generate is clicked
          # Supports both PILES notation (1-2-3,4-5) and legacy format (1,2),(3,4,5)
          tooltip(
            textInput("fusion_groups",
                     "Fuse Pieces:",
                     value = "",
                     placeholder = "1-2-3,4-5"),
            "Fuse adjacent pieces using PILES notation. Examples: '1-2' fuses pieces 1 and 2, '1-2-3,4-5' creates two groups. Use ':' for ranges (1:6), or keywords like 'R1' (row 1), 'ring1'. Changes apply when you click Generate."
          ),

          # Divider before action buttons
          tags$hr(class = "my-3"),

          # Generate and Reset buttons
          actionButton("generate", "Generate Puzzle",
                      icon = icon("puzzle-piece"),
                      class = "btn-primary btn-lg w-100 mb-2"),

          actionButton("reset", "Reset to Defaults",
                      icon = icon("undo"),
                      class = "btn-secondary w-100")
        ),

        # ===== STYLING PANEL =====
        # Visual options - changes update preview automatically (reactive)
        # All values from config.yml
        accordion_panel(
          title = "Styling",
          value = "styling",
          icon = bsicons::bs_icon("palette"),

          # Piece shape options (affect piece generation - reactive)
          tags$small(class = "text-muted", "Piece Shape"),

          tooltip(
            sliderInput("tabsize", "Tab Size:",
                       min = cfg_const$tabsize$min,
                       max = cfg_const$tabsize$max,
                       value = cfg_style$tabsize, step = 1,
                       ticks = TRUE,
                       post = "%",
                       sep = ""),
            "Controls the size of interlocking tabs. Higher values create larger, more prominent tabs. Recommended: 15-25%"
          ),

          tooltip(
            sliderInput("jitter", "Jitter:",
                       min = cfg_const$jitter$min,
                       max = cfg_const$jitter$max,
                       value = cfg_style$jitter, step = 1,
                       ticks = TRUE,
                       post = "%",
                       sep = ""),
            "Adds randomness to piece shapes for more organic variation. Higher values create more irregular pieces. Recommended: 2-6%"
          ),

          # Unified offset slider (replaces output mode dropdowns - Epic #32)
          tooltip(
            sliderInput("offset", "Piece Separation:",
                       min = cfg_const$offset$min,
                       max = cfg_const$offset$max,
                       value = cfg_style$offset, step = 1,
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
                          "Solid Color" = "solid",
                          "Gradient" = "gradient"
                        ),
                        selected = cfg_style$fill_type,
                        inline = TRUE),
            conditionalPanel(
              condition = "input.fill_type == 'solid'",
              colourpicker::colourInput(
                "fill_color",
                "Fill Color:",
                value = cfg_style$fill_color,
                showColour = "background"
              )
            ),
            # Gradient color pickers for piece fill
            conditionalPanel(
              condition = "input.fill_type == 'gradient'",
              colourpicker::colourInput(
                "piece_gradient_center",
                "Center Color:",
                value = "#ffffff",
                showColour = "background"
              ),
              colourpicker::colourInput(
                "piece_gradient_middle",
                "Middle Color:",
                value = "#e0e0e0",
                showColour = "background"
              ),
              colourpicker::colourInput(
                "piece_gradient_edge",
                "Edge Color:",
                value = "#808080",
                showColour = "background"
              )
            )
          ),

          tags$hr(class = "my-2"),
          tags$small(class = "text-muted", "Internal Edges (Fusion)"),

          # Fusion style - reactive styling for internal edges (fusion groups defined in Settings)
          # Only shown when fusion groups are specified in Settings panel
          conditionalPanel(
            condition = "input.fusion_groups != ''",
            radioButtons("fusion_style", "Internal Edge Style:",
                        choices = list(
                          "Hidden" = "none",
                          "Dashed" = "dashed",
                          "Solid" = "solid"
                        ),
                        selected = "none",
                        inline = TRUE),

            # Fusion opacity (only shown when style != none)
            conditionalPanel(
              condition = "input.fusion_style != 'none'",
              tooltip(
                sliderInput("fusion_opacity", "Internal Edge Opacity:",
                           min = 0,
                           max = 100,
                           value = 30, step = 5,
                           ticks = TRUE,
                           post = "%",
                           sep = ""),
                "Transparency of internal edges between fused pieces. 100% = fully visible, 0% = hidden."
              )
            ),
            tags$small(class = "text-muted fst-italic", "Define fusion groups in Settings panel")
          ),

          # Message when no fusion groups are defined
          conditionalPanel(
            condition = "input.fusion_groups == ''",
            tags$small(class = "text-muted fst-italic", "No fusion groups defined (set in Settings panel)")
          ),

          tags$hr(class = "my-2"),
          tags$small(class = "text-muted", "Appearance"),

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
                   selected = cfg_colors$default_palette),

          tooltip(
            input_switch(
              "palette_invert",
              "Invert Palette",
              value = FALSE
            ),
            "Reverse the color palette direction. Light colors become dark and vice versa."
          ),

        tooltip(
          sliderInput("stroke_width", "Line Width:",
                     min = cfg_const$stroke_width$min,
                     max = cfg_const$stroke_width$max,
                     value = cfg_style$stroke_width, step = 0.5,
                     ticks = TRUE,
                     round = 1,
                     post = " mm",
                     sep = ""),
          "Thickness of puzzle piece outlines. For laser cutting, use 0.5mm. For printing or display, use 1.5-2.5mm."
        ),

        tooltip(
          sliderInput("opacity", "Opacity:",
                     min = cfg_const$opacity$min,
                     max = cfg_const$opacity$max,
                     value = cfg_style$opacity, step = 5,
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
            value = cfg_labels$show
          ),
          "Display piece ID numbers at the center of each piece. Useful for assembly instructions."
        ),

        # Conditional label options (shown when labels are enabled)
        conditionalPanel(
          condition = "input.show_labels == true",
          colourpicker::colourInput(
            "label_color",
            "Label Color:",
            value = cfg_labels$color,
            showColour = "background"
          ),
          tooltip(
            sliderInput("label_size", "Label Font Size:",
                       min = cfg_const$label_size$min,
                       max = cfg_const$label_size$max,
                       value = cfg_labels$size, step = 1,
                       ticks = TRUE,
                       post = " mm",
                       sep = ""),
            "Font size for piece labels. Set to 0 for automatic sizing based on piece dimensions."
          )
        ),

        tags$hr(class = "my-2"),
        tags$small(class = "text-muted", "Background"),

        # Background type selector
        radioButtons("background_type", "Background:",
                    choices = list(
                      "None" = "none",
                      "Solid Color" = "solid",
                      "Gradient" = "gradient"
                    ),
                    selected = cfg_bg$type,
                    inline = TRUE),

        # Solid color picker (shown when background_type == "solid")
        conditionalPanel(
          condition = "input.background_type == 'solid'",
          colourpicker::colourInput(
            "background_color",
            "Background Color:",
            value = cfg_bg$solid_color,
            showColour = "background"
          )
        ),

        # Gradient color pickers (shown when background_type == "gradient")
        conditionalPanel(
          condition = "input.background_type == 'gradient'",
          colourpicker::colourInput(
            "gradient_center",
            "Center Color (0%):",
            value = cfg_bg$gradient$center,
            showColour = "background"
          ),
          colourpicker::colourInput(
            "gradient_middle",
            "Middle Color (50%):",
            value = cfg_bg$gradient$middle,
            showColour = "background"
          ),
          colourpicker::colourInput(
            "gradient_edge",
            "Edge Color (100%):",
            value = cfg_bg$gradient$edge,
            showColour = "background"
          )
        )
        ),

        # ===== DOWNLOAD PANEL =====
        # All download options - buttons disabled until puzzle is generated
        accordion_panel(
          title = "Download",
          value = "download",
          icon = bsicons::bs_icon("download"),

          # Status message - changes based on whether puzzle is generated
          uiOutput("download_status_message"),

          # Download Complete Puzzle (offset=0)
          tooltip(
            disabled(downloadButton("download_complete", "Complete Puzzle",
                          icon = icon("puzzle-piece"),
                          class = "btn-success w-100 mb-2")),
            "Download puzzle with all pieces in original positions (offset=0)"
          ),

          # Download Current View (WYSIWYG)
          tooltip(
            disabled(downloadButton("download_wysiwyg", "Current View",
                          icon = icon("eye"),
                          class = "btn-info w-100 mb-2")),
            "Download exactly what you see (WYSIWYG)"
          ),

          # Download Individual Pieces - available for ALL puzzle types
          tooltip(
            disabled(actionButton("download_pieces", "Individual Pieces",
                          icon = icon("download"),
                          class = "btn-warning w-100")),
            "Download each piece as a separate SVG file"
          )
        )
      )  # Close accordion
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
            p("1. Open the ", strong("Settings"), " panel to configure your puzzle parameters"),
            p("2. Click ", strong("Generate Puzzle"), " to create your puzzle"),
            p("3. Adjust ", strong("Styling"), " options - the preview updates automatically"),
            p("4. Open the ", strong("Download"), " panel to save your puzzle"),
            br(),
            h4("Settings Panel"),
            p("Changes here require clicking ", strong("Generate Puzzle"), " to apply."),
            tags$ul(
              tags$li(strong("Puzzle Type:"), " Choose Rectangular, Hexagonal, or Concentric"),
              tags$li(strong("Grid/Rings:"), " Controls piece count"),
              tags$li(strong("Size/Diameter:"), " Physical dimensions in millimeters"),
              tags$li(strong("Fuse Pieces:"), " Create meta-pieces using PILES notation (see below)")
            ),
            br(),
            h4("PILES Notation (Fusion Groups)"),
            p("PILES (Puzzle Input Line Entry System) is a concise notation for specifying piece fusions:"),
            tags$ul(
              tags$li(code("1-2"), " - Fuse pieces 1 and 2"),
              tags$li(code("1-2-3"), " - Fuse pieces 1, 2, and 3 in a chain"),
              tags$li(code("1-2,3-4"), " - Create two fusion groups: (1,2) and (3,4)"),
              tags$li(code("1:6"), " - Fuse consecutive pieces 1 through 6"),
              tags$li(code("R1"), " - Fuse entire row 1 (rectangular only)"),
              tags$li(code("C2"), " - Fuse entire column 2 (rectangular only)"),
              tags$li(code("ring1"), " - Fuse all pieces in ring 1 (hexagonal/concentric)"),
              tags$li(code("center"), " - Center piece (hexagonal/concentric)"),
              tags$li(code("boundary"), " - All edge pieces")
            ),
            p(class = "text-muted", "Legacy format (1,2),(3,4) is also supported."),
            br(),
            h4("Styling Panel"),
            p("Changes in this panel update the preview ", strong("automatically"), " - no need to regenerate!"),
            tags$ul(
              tags$li(strong("Tab Size:"), " Size of interlocking tabs (15-25% recommended)"),
              tags$li(strong("Jitter:"), " Randomness in piece shapes (2-6% recommended)"),
              tags$li(strong("Piece Separation:"), " Gap between pieces (0 = complete, >0 = separated)"),
              tags$li(strong("Internal Edges:"), " Style of edges between fused pieces (hidden/dashed/solid)"),
              tags$li(strong("Color Palette:"), " Choose from various color schemes"),
              tags$li(strong("Line Width:"), " For laser cutting use 0.5mm"),
              tags$li(strong("Opacity:"), " Transparency of puzzle pieces"),
              tags$li(strong("Labels:"), " Show piece ID numbers"),
              tags$li(strong("Background:"), " None, solid color, or gradient")
            ),
            br(),
            h4("Download Options"),
            tags$ul(
              tags$li(strong("Complete Puzzle:"), " All pieces at original positions (offset=0)"),
              tags$li(strong("Current View:"), " Exactly what you see (WYSIWYG)"),
              tags$li(strong("Individual Pieces:"), " Each piece as a separate SVG file (all types)")
            ),
            br(),
            h4("Tips for Laser Cutting"),
            tags$ul(
              tags$li("Use separation of 3-5mm for rectangular, 5-10mm for hexagonal/concentric"),
              tags$li("Black color with 0.5mm line width works best"),
              tags$li("Consider material thickness when setting tab size"),
              tags$li("Test with small puzzles first")
            )
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

# Helper function to map concentric boundary choice to parameters
get_conc_boundary_params <- function(boundary_choice, boundary_facing = "outward") {
  switch(boundary_choice,
    "circle" = list(do_circular_border = TRUE, boundary_facing = boundary_facing),
    # Default: straight (boundary_facing ignored for straight boundary)
    list(do_circular_border = FALSE, boundary_facing = "outward")
  )
}

# Define server logic
server <- function(input, output, session) {

  # Create a Waiter for the puzzle preview area

  # Shows spinner with message during puzzle generation, centered in the card
  w <- Waiter$new(
    id = "puzzle_display",
    html = div(
      style = "display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100%;",
      spin_fading_circles(),
      h4("Generating puzzle...", style = "color: white; margin-top: 10px;")
    ),
    color = "rgba(0, 0, 0, 0.7)"
  )

  # Reactive values to store puzzle data
  puzzle_data <- reactiveVal(NULL)
  svg_content <- reactiveVal(NULL)

  # Store base settings from Generate button (Epic #32 enhancement)
  # These are the "seed" settings that require clicking Generate:
  # - type, grid dimensions, size, seed, boundary mode, center shape
  # Styling options (tabsize, jitter, offset) are reactive and don't need Generate
  base_settings <- reactiveVal(NULL)

  # Positioned result is now computed reactively based on base_settings + styling
  positioned_result <- reactiveVal(NULL)

  # Randomize seed
  observeEvent(input$randomize, {
    updateNumericInput(session, "seed",
                      value = sample(1:99999, 1))
  })

  # Reset to defaults (from config.yml - SINGLE SOURCE OF TRUTH)
  observeEvent(input$reset, {
    # Rectangular options
    updateNumericInput(session, "rows", value = cfg_rect$rows)
    updateNumericInput(session, "cols", value = cfg_rect$cols)
    updateNumericInput(session, "width", value = cfg_rect$width)
    updateNumericInput(session, "height", value = cfg_rect$height)
    # Seed
    updateNumericInput(session, "seed", value = cfg$seed)
    # Styling options
    updateSliderInput(session, "tabsize", value = cfg_style$tabsize)
    updateSliderInput(session, "jitter", value = cfg_style$jitter)
    updateSliderInput(session, "offset", value = cfg_style$offset)
    updateSelectInput(session, "color_palette", selected = cfg_colors$default_palette)
    updateSliderInput(session, "stroke_width", value = cfg_style$stroke_width)
    updateSliderInput(session, "opacity", value = cfg_style$opacity)
    # Label settings
    update_switch(id = "show_labels", value = cfg_labels$show, session = session)
    colourpicker::updateColourInput(session, "label_color", value = cfg_labels$color)
    updateSliderInput(session, "label_size", value = cfg_labels$size)
    # Fill options
    updateRadioButtons(session, "fill_type", selected = cfg_style$fill_type)
    colourpicker::updateColourInput(session, "fill_color", value = cfg_style$fill_color)
    # Background settings
    updateRadioButtons(session, "background_type", selected = cfg_bg$type)
    colourpicker::updateColourInput(session, "background_color", value = cfg_bg$solid_color)
    colourpicker::updateColourInput(session, "gradient_center", value = cfg_bg$gradient$center)
    colourpicker::updateColourInput(session, "gradient_middle", value = cfg_bg$gradient$middle)
    colourpicker::updateColourInput(session, "gradient_edge", value = cfg_bg$gradient$edge)
    # Hexagonal options
    updateNumericInput(session, "rings", value = cfg_hex$rings)
    updateNumericInput(session, "diameter", value = cfg_hex$diameter)
    updateRadioButtons(session, "hex_boundary", selected = cfg_hex$boundary)
    # Concentric options
    updateNumericInput(session, "rings_conc", value = cfg_conc$rings)
    updateNumericInput(session, "diameter_conc", value = cfg_conc$diameter)
    # center_shape is hardcoded to "hexagon" - no UI update needed
    updateRadioButtons(session, "conc_boundary", selected = if (!is.null(cfg_conc$boundary)) cfg_conc$boundary else "straight")
    updateRadioButtons(session, "conc_boundary_facing", selected = if (!is.null(cfg_conc$boundary_facing)) cfg_conc$boundary_facing else "outward")
  })

  # Generate button stores base settings (Epic #32 enhancement)
  # Tabsize, jitter, offset are now reactive - they update preview without Generate
  observeEvent(input$generate, {

    log_header("Generate button clicked")
    log_info("Puzzle type: {.strong {input$puzzle_type}}")

    tryCatch({
      puzzle_type <- input$puzzle_type

      # Build parameters based on puzzle type
      if (puzzle_type == "hexagonal") {
        grid_param <- c(input$rings)
        size_param <- c(input$diameter)
      } else if (puzzle_type == "concentric") {
        grid_param <- c(input$rings_conc)
        size_param <- c(input$diameter_conc)
      } else {
        grid_param <- c(input$rows, input$cols)
        size_param <- c(input$width, input$height)
      }

      # Get boundary parameters from radio button selection (hexagonal only)
      boundary_params <- get_hex_boundary_params(input$hex_boundary)

      # Get concentric boundary parameters (with arc direction)
      conc_boundary_facing <- if (is.null(input$conc_boundary_facing)) "outward" else input$conc_boundary_facing
      conc_boundary_params <- get_conc_boundary_params(input$conc_boundary, conc_boundary_facing)

      # Center shape for concentric type (hardcoded to hexagon - see GitHub issue for future refinement)
      center_shape_value <- "hexagon"

      # Parse fusion groups from text input (only parsed on Generate click)
      # Uses parse_fusion() which supports both PILES notation and legacy format
      fusion_groups_val <- input$fusion_groups
      fusion_groups_parsed <- NULL
      if (!is.null(fusion_groups_val) && nchar(trimws(fusion_groups_val)) > 0) {
        log_info("Parsing fusion groups: '{fusion_groups_val}'")
        tryCatch({
          fusion_groups_parsed <- parse_fusion(fusion_groups_val)
          n_groups <- length(fusion_groups_parsed)
          log_success("Parsed {n_groups} fusion groups")
        }, error = function(e) {
          err_msg <- e$message
          log_warn("Invalid fusion input: {err_msg}")
          showNotification(paste("Invalid fusion groups:", err_msg), type = "warning", duration = 5)
          fusion_groups_parsed <- NULL
        })
      }

      # Store base settings - these trigger piece regeneration
      # fusion_groups is stored here and only updates on Generate click
      base_settings(list(
        type = puzzle_type,
        seed = input$seed,
        grid = grid_param,
        size = size_param,
        boundary_params = boundary_params,
        conc_boundary_params = conc_boundary_params,
        center_shape = center_shape_value,
        fusion_groups = fusion_groups_parsed  # Stored on Generate click only
      ))

      # Store puzzle metadata for display and downloads
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
      } else if (puzzle_type == "concentric") {
        num_pieces <- 3 * input$rings_conc * (input$rings_conc - 1) + 1
        puzzle_data(list(
          type = "concentric",
          rings = input$rings_conc,
          diameter = input$diameter_conc,
          center_shape = center_shape_value,
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

      log_success("Base settings stored - puzzle will regenerate reactively")

      # Enable download buttons after successful generation
      shinyjs::enable("download_complete")
      shinyjs::enable("download_wysiwyg")
      shinyjs::enable("download_pieces")

    }, error = function(e) {
      log_error("ERROR storing base settings")
      log_error("Message: {e$message}")
      showNotification(paste("Error:", e$message), type = "error", duration = 10)
    })
  })

  # Reactive piece generation - depends on base_settings + tabsize/jitter/offset
  # This allows styling options to update the puzzle without clicking Generate
  # IMPORTANT: fusion_groups is stored in base_settings (not reactive) to avoid slow re-parsing
  # Only fusion_style and fusion_opacity are reactive for quick styling changes
  observe({
    # IMPORTANT: Read ALL inputs FIRST to establish reactive dependencies
    # before the early return. Otherwise Shiny won't track changes to these inputs.
    tabsize_val <- input$tabsize
    jitter_val <- input$jitter
    offset_val <- input$offset
    # NOTE: fusion_groups input is NOT read here - it's stored in base_settings on Generate click
    # This prevents slow reactive re-computation when fusion text changes
    fusion_style_val <- if (is.null(input$fusion_style)) "none" else input$fusion_style
    fusion_opacity_val <- if (is.null(input$fusion_opacity)) 30 else input$fusion_opacity / 100

    # Now check base_settings - but dependencies are already established above
    settings <- base_settings()
    if (is.null(settings)) return()

    # Use fusion_groups from base_settings (stored on Generate click)
    fusion_groups_parsed <- settings$fusion_groups
    n_fusion <- if (!is.null(fusion_groups_parsed)) length(fusion_groups_parsed) else 0

    log_info("Regenerating pieces (tabsize={tabsize_val}, jitter={jitter_val}, offset={offset_val}mm)")
    log_info("Fusion: {n_fusion} groups from Generate click (style={fusion_style_val})")

    # Show loading indicator on preview area
    w$show()

    tryCatch({
      # Determine do_circular_border based on puzzle type
      do_circular_border_val <- if (settings$type == "hexagonal") {
        settings$boundary_params$do_circular_border
      } else if (settings$type == "concentric") {
        settings$conc_boundary_params$do_circular_border
      } else {
        FALSE
      }

      # Determine boundary_facing for concentric type
      boundary_facing_val <- if (settings$type == "concentric") {
        settings$conc_boundary_params$boundary_facing
      } else {
        "outward"
      }

      # Generate pieces with current styling values
      # fusion_groups comes from base_settings (Generate click)
      # fusion_style and fusion_opacity are reactive (Styling panel)
      pieces_result <- generate_pieces_internal(
        type = settings$type,
        seed = settings$seed,
        grid = settings$grid,
        size = settings$size,
        tabsize = tabsize_val,
        jitter = jitter_val,
        do_warp = if (settings$type == "hexagonal") settings$boundary_params$do_warp else FALSE,
        do_trunc = if (settings$type == "hexagonal") settings$boundary_params$do_trunc else FALSE,
        do_circular_border = do_circular_border_val,
        center_shape = settings$center_shape,
        boundary_facing = boundary_facing_val,
        fusion_groups = fusion_groups_parsed,
        fusion_style = fusion_style_val,
        fusion_opacity = fusion_opacity_val
      )

      # Log fusion result
      if (!is.null(pieces_result$fusion_data)) {
        n_fused <- length(pieces_result$fusion_data$fused_edges)
        log_success("Fusion applied: {n_fused} fused edges")
      }

      # Apply positioning with current offset
      positioned <- apply_piece_positioning(pieces_result, offset = offset_val)

      # Store for rendering
      positioned_result(positioned)
      log_info("Updated positioned_result")

      # Update puzzle_data with current offset (for downloads)
      current_data <- puzzle_data()
      if (!is.null(current_data)) {
        current_data$offset <- offset_val
        puzzle_data(current_data)
      }

      # Hide loading indicator after successful generation
      w$hide()

    }, error = function(e) {
      log_error("ERROR in reactive piece generation: {e$message}")
      w$hide()  # Also hide on error
    })
  })

  # Download status message - changes based on whether puzzle is generated
  output$download_status_message <- renderUI({
    if (is.null(puzzle_data())) {
      div(class = "alert alert-info mb-3",
        icon("info-circle"), " Generate a puzzle first to enable downloads."
      )
    } else {
      div(class = "alert alert-success mb-3",
        icon("check-circle"), " Puzzle ready! Choose a download format."
      )
    }
  })

  # Reactive SVG rendering - updates when styling options change

  # This re-renders without regenerating pieces
  rendered_svg <- reactive({
    # Depend on positioned result (from generate button)
    pos <- positioned_result()
    if (is.null(pos)) return(NULL)

    log_info("rendered_svg() triggered - re-rendering SVG")

    # Styling options (these trigger re-render, not regeneration)
    fill_color_value <- switch(input$fill_type,
      "none" = "none",
      "solid" = input$fill_color,
      "gradient" = list(
        type = "gradient",
        center = input$piece_gradient_center,
        middle = input$piece_gradient_middle,
        edge = input$piece_gradient_edge
      ),
      "none"  # default
    )

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
      palette_invert = isTRUE(input$palette_invert),
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
      } else if (data$type == "concentric") {
        # Concentric puzzle value boxes
        layout_column_wrap(
          width = 1/4,
          value_box(
            title = "Type",
            value = "Concentric",
            showcase = bsicons::bs_icon("bullseye"),
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
            title = "Center",
            value = "Hexagon",  # Hardcoded - see GitHub issue for future options
            showcase = bsicons::bs_icon("hexagon"),
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
    } else if (data$type == "concentric") {
      sprintf("concentric_%drings_seed%d", data$rings, data$seed)
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

      # Determine fill color value (supports none, solid, gradient)
      fill_color_value <- switch(input$fill_type,
        "none" = "none",
        "solid" = input$fill_color,
        "gradient" = list(
          type = "gradient",
          center = input$piece_gradient_center,
          middle = input$piece_gradient_middle,
          edge = input$piece_gradient_edge
        ),
        "none"  # default
      )

      # Determine background value (supports none, solid, gradient)
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

      # Build parameters based on puzzle type
      if (data$type == "hexagonal") {
        grid_param <- c(input$rings)
        size_param <- c(input$diameter)
      } else if (data$type == "concentric") {
        grid_param <- c(input$rings_conc)
        size_param <- c(input$diameter_conc)
      } else {
        grid_param <- c(input$rows, input$cols)
        size_param <- c(input$width, input$height)
      }

      # Generate complete puzzle (offset=0)
      # Get boundary parameters from radio button selection (hexagonal only)
      boundary_params <- get_hex_boundary_params(input$hex_boundary)

      # Get concentric boundary parameters (with arc direction)
      conc_boundary_facing <- if (is.null(input$conc_boundary_facing)) "outward" else input$conc_boundary_facing
      conc_boundary_params <- get_conc_boundary_params(input$conc_boundary, conc_boundary_facing)

      # Center shape for concentric type (hardcoded to hexagon - see GitHub issue for future refinement)
      center_shape_value <- "hexagon"

      # Determine do_circular_border based on puzzle type
      do_circular_border_val <- if (data$type == "hexagonal") {
        boundary_params$do_circular_border
      } else if (data$type == "concentric") {
        conc_boundary_params$do_circular_border
      } else {
        FALSE
      }

      # Determine boundary_facing for concentric type
      boundary_facing_val <- if (data$type == "concentric") {
        conc_boundary_params$boundary_facing
      } else {
        "outward"
      }

      # Parse fusion groups for download
      # Uses parse_fusion() which supports both PILES notation and legacy format
      fusion_groups_val <- input$fusion_groups
      fusion_groups_parsed <- NULL
      if (!is.null(fusion_groups_val) && nchar(trimws(fusion_groups_val)) > 0) {
        tryCatch({
          fusion_groups_parsed <- parse_fusion(fusion_groups_val)
        }, error = function(e) {
          fusion_groups_parsed <- NULL
        })
      }

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
        palette_invert = isTRUE(input$palette_invert),
        background = background_value,
        opacity = input$opacity / 100,
        save_files = FALSE,
        do_warp = if (data$type == "hexagonal") boundary_params$do_warp else FALSE,
        do_trunc = if (data$type == "hexagonal") boundary_params$do_trunc else FALSE,
        do_circular_border = do_circular_border_val,
        center_shape = center_shape_value,
        boundary_facing = boundary_facing_val,
        fusion_groups = fusion_groups_parsed,
        fusion_style = if (is.null(input$fusion_style)) "none" else input$fusion_style,
        fusion_opacity = if (is.null(input$fusion_opacity)) 0.3 else input$fusion_opacity / 100
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
        # Create www/pieces directory if it doesn't exist
        pieces_dir <- file.path("www", "pieces")
        if (!dir.exists(pieces_dir)) {
          dir.create(pieces_dir, recursive = TRUE)
        }

        # Clean up old files
        old_files <- list.files(pieces_dir, pattern = "hex_piece_.*\\.svg$", full.names = TRUE)
        if (length(old_files) > 0) {
          file.remove(old_files)
        }

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

        # Generate hexagonal individual pieces
        result <- tryCatch({
          capture.output({
            generate_hexagonal_individual_pieces(
              rings = data$rings,
              seed = data$seed,
              diameter = data$diameter,
              tabsize = input$tabsize,
              jitter = input$jitter,
              output_dir = pieces_dir,
              save_combined = FALSE,
              save_individual = TRUE,
              stroke_width = input$stroke_width,
              stroke_color = "black",
              background = background_value,
              opacity = input$opacity / 100
            )
          }, type = "message")
        }, error = function(e) {
          showNotification(
            paste("Error generating hexagonal pieces:", e$message),
            type = "error",
            duration = 10
          )
          return(NULL)
        })

        # Verify files were created and send to browser
        piece_files <- list.files(pieces_dir, pattern = "hex_piece_.*\\.svg$", full.names = TRUE)

        if (length(piece_files) > 0) {
          files_ok <- all(sapply(piece_files, file.size) > 0)

          if (files_ok) {
            files_list <- lapply(basename(piece_files), function(filename) {
              list(
                url = paste0("pieces/", filename),
                name = filename
              )
            })
            session$sendCustomMessage("downloadFiles", files_list)
            showNotification(
              sprintf("Downloading %d hexagonal piece files...", length(piece_files)),
              type = "message",
              duration = 3
            )
          } else {
            showNotification(
              "Error: Generated hexagonal files are empty. Please try again.",
              type = "error",
              duration = 5
            )
          }
        } else {
          showNotification(
            "Error: No hexagonal piece files were generated. Please try again.",
            type = "error",
            duration = 5
          )
        }

      } else if (data$type == "concentric") {
        # Create www/pieces directory if it doesn't exist
        pieces_dir <- file.path("www", "pieces")
        if (!dir.exists(pieces_dir)) {
          dir.create(pieces_dir, recursive = TRUE)
        }

        # Clean up old files
        old_files <- list.files(pieces_dir, pattern = "concentric_piece_.*\\.svg$", full.names = TRUE)
        if (length(old_files) > 0) {
          file.remove(old_files)
        }

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

        # Generate concentric individual pieces
        result <- tryCatch({
          capture.output({
            generate_concentric_individual_pieces(
              rings = data$rings,
              seed = data$seed,
              diameter = data$diameter,
              tabsize = input$tabsize,
              jitter = input$jitter,
              center_shape = data$center_shape,
              output_dir = pieces_dir,
              save_combined = FALSE,
              save_individual = TRUE,
              stroke_width = input$stroke_width,
              stroke_color = "black",
              background = background_value,
              opacity = input$opacity / 100
            )
          }, type = "message")
        }, error = function(e) {
          showNotification(
            paste("Error generating concentric pieces:", e$message),
            type = "error",
            duration = 10
          )
          return(NULL)
        })

        # Verify files were created and send to browser
        piece_files <- list.files(pieces_dir, pattern = "concentric_piece_.*\\.svg$", full.names = TRUE)

        if (length(piece_files) > 0) {
          files_ok <- all(sapply(piece_files, file.size) > 0)

          if (files_ok) {
            files_list <- lapply(basename(piece_files), function(filename) {
              list(
                url = paste0("pieces/", filename),
                name = filename
              )
            })
            session$sendCustomMessage("downloadFiles", files_list)
            showNotification(
              sprintf("Downloading %d concentric piece files...", length(piece_files)),
              type = "message",
              duration = 3
            )
          } else {
            showNotification(
              "Error: Generated concentric files are empty. Please try again.",
              type = "error",
              duration = 5
            )
          }
        } else {
          showNotification(
            "Error: No concentric piece files were generated. Please try again.",
            type = "error",
            duration = 5
          )
        }
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
