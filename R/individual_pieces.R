# Individual Puzzle Piece Generation
# Implementation of individual piece extraction for jigsaw puzzles

#' Extract tab/blank data from puzzle generation
#' 
#' This function pre-generates all tab/blank information needed to create
#' individual piece boundaries consistently.
#' 
#' @param seed Random seed for puzzle generation
#' @param xn Number of columns  
#' @param yn Number of rows
#' @param tabsize Tab size percentage
#' @param jitter Jitter percentage
#' @return List containing tab/blank data for all dividers
extract_tab_data <- function(seed, xn, yn, tabsize = 20, jitter = 4) {
  
  # Initialize environment (reuse existing initialization)
  init_jigsaw(seed = seed, tabsize = tabsize, jitter = jitter, 
              width = 300, height = 200, xn = xn, yn = yn)
  
  # Storage for tab data
  tab_data <- list(
    horizontal = list(),  # For horizontal dividers (between rows)
    vertical = list()     # For vertical dividers (between columns)
  )
  
  # Extract horizontal divider tab data
  # This matches the logic in gen_dh()
  .jigsaw_env$vertical <- 0
  for (yi in 1:(yn - 1)) {
    .jigsaw_env$yi <- yi
    .jigsaw_env$xi <- 0
    
    # Store tab data for this horizontal line
    line_tabs <- list()
    first()  # Initialize first tab
    
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
  
  # Extract vertical divider tab data  
  # This matches the logic in gen_dv()
  .jigsaw_env$vertical <- 1
  for (xi in 1:(xn - 1)) {
    .jigsaw_env$xi <- xi
    .jigsaw_env$yi <- 0
    
    # Store tab data for this vertical line
    line_tabs <- list()
    first()  # Initialize first tab
    
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

#' Test function to validate tab data extraction
#' 
#' Creates a simple 2x2 puzzle and extracts tab data
test_tab_extraction <- function() {
  cat("Testing tab data extraction with 2x2 puzzle...\n")
  
  # Generate 2x2 puzzle (4 pieces)
  seed <- 1234
  tab_data <- extract_tab_data(seed, xn = 2, yn = 2)
  
  cat("Horizontal dividers (between rows):\n")
  for (yi in seq_along(tab_data$horizontal)) {
    cat("  Row divider", yi, ":\n")
    for (xi in seq_along(tab_data$horizontal[[yi]])) {
      tab <- tab_data$horizontal[[yi]][[xi]]
      cat("    Segment", xi, "- flip:", tab$flip, "a:", round(tab$a, 3), "\n")
    }
  }
  
  cat("Vertical dividers (between columns):\n") 
  for (xi in seq_along(tab_data$vertical)) {
    cat("  Column divider", xi, ":\n")
    for (yi in seq_along(tab_data$vertical[[xi]])) {
      tab <- tab_data$vertical[[xi]][[yi]]
      cat("    Segment", yi, "- flip:", tab$flip, "a:", round(tab$a, 3), "\n")
    }
  }
  
  return(tab_data)
}