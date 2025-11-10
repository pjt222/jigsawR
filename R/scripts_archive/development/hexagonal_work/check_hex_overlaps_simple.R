#!/usr/bin/env Rscript

# Simple Hexagonal Pieces Overlap Checker
cat("🔍 CHECKING HEXAGONAL PIECES FOR OVERLAPS\n")

# Read the SVG file directly to analyze path positions
svg_file <- "output/hex_individual_FINAL_FIXED.svg"

if (!file.exists(svg_file)) {
  stop("SVG file not found: ", svg_file)
}

svg_content <- readLines(svg_file)
cat("Loaded SVG with", length(svg_content), "lines\n")

# Extract path elements and their d attributes
path_lines <- grep('<path d=', svg_content, value = TRUE)
cat("Found", length(path_lines), "path elements\n\n")

# Simple function to extract first coordinate pair from path
extract_first_coords <- function(path_line) {
  # Extract the d attribute value
  d_match <- regmatches(path_line, regexpr('d="[^"]*"', path_line))
  if (length(d_match) == 0) return(c(NA, NA))
  
  # Remove d=" and trailing "
  path_data <- gsub('^d="', '', d_match)
  path_data <- gsub('".*$', '', path_data)
  
  # Extract first two numbers (ignoring M command)
  numbers <- regmatches(path_data, gregexpr("[-\\d\\.]+", path_data))[[1]]
  numbers <- as.numeric(numbers[!is.na(as.numeric(numbers))])
  
  if (length(numbers) >= 2) {
    return(c(numbers[1], numbers[2]))
  } else {
    return(c(NA, NA))
  }
}

# Extract piece information
pieces_info <- data.frame(
  piece_num = numeric(),
  start_x = numeric(),
  start_y = numeric(),
  path_snippet = character(),
  stringsAsFactors = FALSE
)

for (i in seq_along(path_lines)) {
  coords <- extract_first_coords(path_lines[i])
  
  # Extract a snippet of the path for identification
  snippet <- substr(path_lines[i], 1, 100)
  snippet <- gsub('.*d="', '', snippet)
  snippet <- substr(snippet, 1, 50)
  
  pieces_info <- rbind(pieces_info, data.frame(
    piece_num = i,
    start_x = coords[1],
    start_y = coords[2], 
    path_snippet = snippet,
    stringsAsFactors = FALSE
  ))
}

# Remove pieces with invalid coordinates
valid_pieces <- pieces_info[!is.na(pieces_info$start_x) & !is.na(pieces_info$start_y), ]

cat("=== PIECE START COORDINATES ===\n")
for (i in 1:nrow(valid_pieces)) {
  cat(sprintf("Piece %2d: Start(%.1f, %.1f) Path: %s...\n",
              valid_pieces[i, "piece_num"], 
              valid_pieces[i, "start_x"], 
              valid_pieces[i, "start_y"],
              substr(valid_pieces[i, "path_snippet"], 1, 30)))
}

# Check for duplicate start coordinates
cat("\n=== OVERLAP CHECK ===\n")
tolerance <- 1.0  # coordinate tolerance

overlaps_found <- 0
for (i in 1:(nrow(valid_pieces)-1)) {
  for (j in (i+1):nrow(valid_pieces)) {
    x_diff <- abs(valid_pieces[i, "start_x"] - valid_pieces[j, "start_x"])
    y_diff <- abs(valid_pieces[i, "start_y"] - valid_pieces[j, "start_y"])
    
    if (x_diff < tolerance && y_diff < tolerance) {
      overlaps_found <- overlaps_found + 1
      cat(sprintf("🔴 OVERLAP %d: Pieces %d & %d both start at (%.1f, %.1f)\n",
                  overlaps_found,
                  valid_pieces[i, "piece_num"], 
                  valid_pieces[j, "piece_num"],
                  valid_pieces[i, "start_x"], 
                  valid_pieces[i, "start_y"]))
    }
  }
}

# Group pieces by similar coordinates
cat("\n=== COORDINATE GROUPING ===\n")
coord_groups <- list()
group_tolerance <- 5.0

for (i in 1:nrow(valid_pieces)) {
  x <- valid_pieces[i, "start_x"]
  y <- valid_pieces[i, "start_y"]
  piece_num <- valid_pieces[i, "piece_num"]
  
  # Find existing group or create new one
  found_group <- FALSE
  for (g in seq_along(coord_groups)) {
    group_x <- coord_groups[[g]]$x[1]  # representative x
    group_y <- coord_groups[[g]]$y[1]  # representative y
    
    if (abs(x - group_x) < group_tolerance && abs(y - group_y) < group_tolerance) {
      coord_groups[[g]]$pieces <- c(coord_groups[[g]]$pieces, piece_num)
      coord_groups[[g]]$x <- c(coord_groups[[g]]$x, x)
      coord_groups[[g]]$y <- c(coord_groups[[g]]$y, y)
      found_group <- TRUE
      break
    }
  }
  
  if (!found_group) {
    coord_groups[[length(coord_groups) + 1]] <- list(
      pieces = piece_num,
      x = x,
      y = y
    )
  }
}

# Report coordinate groups
groups_with_multiple <- 0
for (g in seq_along(coord_groups)) {
  if (length(coord_groups[[g]]$pieces) > 1) {
    groups_with_multiple <- groups_with_multiple + 1
    pieces_in_group <- coord_groups[[g]]$pieces
    coords_in_group <- paste(sprintf("(%.1f,%.1f)", coord_groups[[g]]$x, coord_groups[[g]]$y), collapse = ", ")
    
    cat(sprintf("🟡 GROUP %d: %d pieces at similar positions\n", 
                groups_with_multiple, length(pieces_in_group)))
    cat(sprintf("     Pieces: %s\n", paste(pieces_in_group, collapse = ", ")))
    cat(sprintf("     Coords: %s\n", coords_in_group))
  }
}

# Summary
cat("\n=== SUMMARY ===\n")
cat("Total path elements:", length(path_lines), "\n")
cat("Valid coordinate pieces:", nrow(valid_pieces), "\n") 
cat("Exact overlaps (same start):", overlaps_found, "\n")
cat("Coordinate groups with multiple pieces:", groups_with_multiple, "\n")

if (overlaps_found > 0 || groups_with_multiple > 0) {
  cat("🔴 ISSUE: Overlapping or clustered pieces detected!\n")
} else {
  cat("✅ No overlapping pieces detected\n")
}