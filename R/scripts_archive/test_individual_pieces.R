# Test implementation of individual puzzle piece generation
# Step-by-step development approach

cat("=== Individual Piece Generation Test ===\n")

# Environment setup (minimal working version)
.jigsaw_env <- new.env()

init_jigsaw <- function(seed = NULL, tabsize = 20, jitter = 4,
                        width = 300, height = 200, radius = 2.0,
                        xn = 15, yn = 10) {
  if (is.null(seed)) {
    seed <- as.integer(runif(1) * 10000)
  }
  .jigsaw_env$seed_initial <- seed
  .jigsaw_env$seed <- seed
  .jigsaw_env$tabsize <- tabsize
  .jigsaw_env$jitter <- jitter
  .jigsaw_env$width <- width
  .jigsaw_env$height <- height
  .jigsaw_env$radius <- radius
  .jigsaw_env$xn <- xn
  .jigsaw_env$yn <- yn
  .jigsaw_env$offset <- 0.0
  .jigsaw_env$t <- tabsize / 200.0
  .jigsaw_env$j <- jitter / 100.0
}

# Random functions
random <- function() {
  x <- sin(.jigsaw_env$seed) * 10000
  .jigsaw_env$seed <- .jigsaw_env$seed + 1
  return(x - floor(x))
}

uniform <- function(min_val, max_val) {
  r <- random()
  return(min_val + r * (max_val - min_val))
}

rbool <- function() {
  return(random() > 0.5)
}

# Tab generation functions
first <- function() {
  .jigsaw_env$e <- uniform(-.jigsaw_env$j, .jigsaw_env$j)
  .jigsaw_env$flip <- NULL
  next_tab()
}

next_tab <- function() {
  flipold <- if (exists("flip", envir = .jigsaw_env)) .jigsaw_env$flip else NULL
  .jigsaw_env$flip <- rbool()
  .jigsaw_env$a <- if (is.null(flipold) || .jigsaw_env$flip == flipold) -.jigsaw_env$e else .jigsaw_env$e
  .jigsaw_env$b <- uniform(-.jigsaw_env$j, .jigsaw_env$j)
  .jigsaw_env$c <- uniform(-.jigsaw_env$j, .jigsaw_env$j)
  .jigsaw_env$d <- uniform(-.jigsaw_env$j, .jigsaw_env$j)
  .jigsaw_env$e <- uniform(-.jigsaw_env$j, .jigsaw_env$j)
}

# Extract all tab/blank data for reuse
extract_tab_data <- function(seed, xn, yn, tabsize = 20, jitter = 4) {
  
  init_jigsaw(seed = seed, tabsize = tabsize, jitter = jitter, 
              width = 300, height = 200, xn = xn, yn = yn)
  
  tab_data <- list(
    horizontal = list(),  # For horizontal dividers (between rows)
    vertical = list()     # For vertical dividers (between columns)
  )
  
  # Extract horizontal divider tab data (matches gen_dh logic)
  .jigsaw_env$vertical <- 0
  for (yi in 1:(yn - 1)) {
    .jigsaw_env$yi <- yi
    .jigsaw_env$xi <- 0
    
    line_tabs <- list()
    first()  # Initialize first tab for this line
    
    for (xi in 0:(xn - 1)) {
      .jigsaw_env$xi <- xi
      
      # Store current tab parameters
      line_tabs[[xi + 1]] <- list(
        flip = .jigsaw_env$flip,
        a = .jigsaw_env$a,
        b = .jigsaw_env$b, 
        c = .jigsaw_env$c,
        d = .jigsaw_env$d,
        e = .jigsaw_env$e
      )
      
      if (xi < xn - 1) {
        next_tab()  # Prepare for next segment
      }
    }
    
    tab_data$horizontal[[yi]] <- line_tabs
  }
  
  # Extract vertical divider tab data (matches gen_dv logic)
  .jigsaw_env$vertical <- 1
  for (xi in 1:(xn - 1)) {
    .jigsaw_env$xi <- xi
    .jigsaw_env$yi <- 0
    
    line_tabs <- list()
    first()  # Initialize first tab for this line
    
    for (yi in 0:(yn - 1)) {
      .jigsaw_env$yi <- yi
      
      # Store current tab parameters
      line_tabs[[yi + 1]] <- list(
        flip = .jigsaw_env$flip,
        a = .jigsaw_env$a,
        b = .jigsaw_env$b,
        c = .jigsaw_env$c, 
        d = .jigsaw_env$d,
        e = .jigsaw_env$e
      )
      
      if (yi < yn - 1) {
        next_tab()  # Prepare for next segment
      }
    }
    
    tab_data$vertical[[xi]] <- line_tabs
  }
  
  return(tab_data)
}

# Test with 2x2 puzzle
cat("Testing tab data extraction with 2x2 puzzle...\n")
seed <- 1234
tab_data <- extract_tab_data(seed, xn = 2, yn = 2)

# Display results
cat("Horizontal dividers (between rows):\n")
for (yi in seq_along(tab_data$horizontal)) {
  cat("  Row divider", yi, "(between rows", yi, "and", yi+1, "):\n")
  for (xi in seq_along(tab_data$horizontal[[yi]])) {
    tab <- tab_data$horizontal[[yi]][[xi]]
    cat("    Column", xi, "- flip:", tab$flip, "a:", round(tab$a, 3), 
        "b:", round(tab$b, 3), "\n")
  }
}

cat("Vertical dividers (between columns):\n") 
for (xi in seq_along(tab_data$vertical)) {
  cat("  Column divider", xi, "(between cols", xi, "and", xi+1, "):\n")
  for (yi in seq_along(tab_data$vertical[[xi]])) {
    tab <- tab_data$vertical[[xi]][[yi]]
    cat("    Row", yi, "- flip:", tab$flip, "a:", round(tab$a, 3), 
        "b:", round(tab$b, 3), "\n")
  }
}

cat("=== Tab Data Extraction Success ===\n")