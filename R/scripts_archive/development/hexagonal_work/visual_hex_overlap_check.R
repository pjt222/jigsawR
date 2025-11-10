#!/usr/bin/env Rscript

# Visual Hexagonal Pieces Overlap Check
cat("🔍 VISUAL CHECK FOR HEXAGONAL PIECE OVERLAPS\n")

# Read the SVG and extract path coordinates manually
svg_file <- "output/hex_individual_FINAL_FIXED.svg"
svg_content <- readLines(svg_file)

# Extract path data lines
path_lines <- grep('<path d=', svg_content, value = TRUE)
cat("Found", length(path_lines), "path elements\n")

# Function to extract approximate center from path coordinates
get_path_center <- function(path_line) {
  # Extract d attribute
  d_attr <- regmatches(path_line, regexpr('d="[^"]*"', path_line))
  if (length(d_attr) == 0) return(c(NA, NA))
  
  # Clean up the path data
  path_data <- gsub('d="', '', d_attr)
  path_data <- gsub('".*', '', path_data)
  
  # Remove command letters and extract numbers
  path_clean <- gsub('[MLHVCSQTAZ]', ' ', path_data, ignore.case = TRUE)
  numbers <- as.numeric(unlist(strsplit(path_clean, "\\s+")))
  numbers <- numbers[!is.na(numbers)]
  
  if (length(numbers) >= 4) {
    # Take every other number as x, y pairs
    x_coords <- numbers[seq(1, length(numbers), by = 2)]
    y_coords <- numbers[seq(2, length(numbers), by = 2)]
    
    # Calculate approximate center
    center_x <- mean(x_coords, na.rm = TRUE)
    center_y <- mean(y_coords, na.rm = TRUE)
    
    return(c(center_x, center_y))
  }
  
  return(c(NA, NA))
}

# Extract centers for all pieces
piece_centers <- list()
cat("\n=== PIECE CENTER ANALYSIS ===\n")

for (i in seq_along(path_lines)) {
  center <- get_path_center(path_lines[i])
  piece_centers[[i]] <- center
  
  if (!is.na(center[1]) && !is.na(center[2])) {
    cat(sprintf("Piece %2d: Center(%.1f, %.1f)\n", i, center[1], center[2]))
  } else {
    cat(sprintf("Piece %2d: Center(INVALID)\n", i))
  }
}

# Check for overlapping centers
cat("\n=== OVERLAP DETECTION ===\n")
overlap_threshold <- 15  # pixels

overlapping_pairs <- 0
for (i in 1:(length(piece_centers)-1)) {
  center1 <- piece_centers[[i]]
  if (any(is.na(center1))) next
  
  for (j in (i+1):length(piece_centers)) {
    center2 <- piece_centers[[j]]
    if (any(is.na(center2))) next
    
    distance <- sqrt((center1[1] - center2[1])^2 + (center1[2] - center2[2])^2)
    
    if (distance < overlap_threshold) {
      overlapping_pairs <- overlapping_pairs + 1
      cat(sprintf("🔴 OVERLAP %d: Pieces %d & %d are %.1f pixels apart\n",
                  overlapping_pairs, i, j, distance))
      cat(sprintf("     Piece %d center: (%.1f, %.1f)\n", i, center1[1], center1[2]))
      cat(sprintf("     Piece %d center: (%.1f, %.1f)\n", j, center2[1], center2[2]))
      
      if (distance < 2) {
        cat("     ⚠️  NEARLY IDENTICAL - These pieces are overlapping!\n")
      }
      cat("\n")
    }
  }
}

# Create a simple visual grid to show piece distribution
cat("=== VISUAL DISTRIBUTION GRID ===\n")
valid_centers <- piece_centers[!sapply(piece_centers, function(x) any(is.na(x)))]

if (length(valid_centers) > 0) {
  # Find bounds
  all_x <- sapply(valid_centers, function(x) x[1])
  all_y <- sapply(valid_centers, function(x) x[2])
  
  min_x <- min(all_x)
  max_x <- max(all_x)
  min_y <- min(all_y) 
  max_y <- max(all_y)
  
  cat(sprintf("Coordinate bounds: X(%.1f to %.1f) Y(%.1f to %.1f)\n", 
              min_x, max_x, min_y, max_y))
  
  # Create simple grid representation
  grid_size <- 30  # pixels per grid cell
  grid_width <- ceiling((max_x - min_x) / grid_size) + 1
  grid_height <- ceiling((max_y - min_y) / grid_size) + 1
  
  cat(sprintf("Grid size: %dx%d (each cell = %dx%d pixels)\n", 
              grid_width, grid_height, grid_size, grid_size))
  
  # Count pieces per grid cell
  grid <- matrix(0, nrow = grid_height, ncol = grid_width)
  
  for (i in seq_along(valid_centers)) {
    center <- valid_centers[[i]]
    grid_x <- floor((center[1] - min_x) / grid_size) + 1
    grid_y <- floor((center[2] - min_y) / grid_size) + 1
    
    if (grid_x > 0 && grid_x <= grid_width && grid_y > 0 && grid_y <= grid_height) {
      grid[grid_y, grid_x] <- grid[grid_y, grid_x] + 1
    }
  }
  
  # Display grid
  cat("\nPiece distribution (numbers show count per region):\n")
  for (y in 1:grid_height) {
    row_str <- ""
    for (x in 1:grid_width) {
      if (grid[y, x] == 0) {
        row_str <- paste0(row_str, ".")
      } else if (grid[y, x] > 9) {
        row_str <- paste0(row_str, "+")
      } else {
        row_str <- paste0(row_str, grid[y, x])
      }
    }
    cat(row_str, "\n")
  }
  
  # Find cells with multiple pieces (potential overlaps)
  multi_cells <- which(grid > 1, arr.ind = TRUE)
  if (nrow(multi_cells) > 0) {
    cat("\n🔴 CLUSTERED REGIONS (>1 piece per region):\n")
    for (i in 1:nrow(multi_cells)) {
      y <- multi_cells[i, 1]
      x <- multi_cells[i, 2]
      cat(sprintf("Grid cell (%d,%d): %d pieces\n", x, y, grid[y, x]))
    }
  }
}

# Summary
cat("\n=== SUMMARY ===\n")
cat("Total pieces:", length(path_lines), "\n")
cat("Valid centers:", length(valid_centers), "\n")
cat("Overlapping pairs:", overlapping_pairs, "\n")

if (overlapping_pairs > 0) {
  cat("🔴 PROBLEM: Overlapping pieces detected!\n")
  cat("SOLUTION: Need to fix piece positioning/coordinate generation\n")
} else {
  cat("✅ No overlapping pieces detected\n")
}