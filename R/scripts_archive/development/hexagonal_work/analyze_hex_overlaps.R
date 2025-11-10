#!/usr/bin/env Rscript

# Analyze Hexagonal Individual Pieces for Overlapping Paths
cat("🔍 ANALYZING HEXAGONAL PIECES FOR OVERLAPS\n")

# Load the pieces data
if (file.exists("output/hex_individual_final_fixed.rds")) {
  data <- readRDS("output/hex_individual_final_fixed.rds")
  pieces <- data$pieces
  cat("Loaded data from hex_individual_final_fixed.rds\n")
} else if (file.exists("output/hex_individual_with_real_tabs.rds")) {
  data <- readRDS("output/hex_individual_with_real_tabs.rds")
  pieces <- data$pieces  
  cat("Loaded data from hex_individual_with_real_tabs.rds\n")
} else {
  stop("No piece data files found")
}

cat("Analyzing", length(pieces), "pieces for overlaps\n\n")

# Function to extract coordinates from path
extract_path_coordinates <- function(path_string) {
  # Extract all numbers from the path
  numbers <- as.numeric(regmatches(path_string, gregexpr("[-\\d\\.]+", path_string))[[1]])
  
  if (length(numbers) >= 2) {
    # Assume they alternate x, y coordinates
    x_coords <- numbers[seq(1, length(numbers), by = 2)]
    y_coords <- numbers[seq(2, length(numbers), by = 2)]
    
    return(data.frame(x = x_coords, y = y_coords))
  }
  
  return(data.frame(x = numeric(0), y = numeric(0)))
}

# Function to calculate bounding box
calculate_bounds <- function(coords) {
  if (nrow(coords) == 0) {
    return(list(min_x = 0, max_x = 0, min_y = 0, max_y = 0, 
               center_x = 0, center_y = 0))
  }
  
  min_x <- min(coords$x)
  max_x <- max(coords$x) 
  min_y <- min(coords$y)
  max_y <- max(coords$y)
  center_x <- (min_x + max_x) / 2
  center_y <- (min_y + max_y) / 2
  
  return(list(min_x = min_x, max_x = max_x, min_y = min_y, max_y = max_y,
             center_x = center_x, center_y = center_y))
}

# Function to check if two bounding boxes overlap
check_overlap <- function(bounds1, bounds2, tolerance = 5) {
  # Check if rectangles overlap with tolerance
  x_overlap <- (bounds1$max_x + tolerance >= bounds2$min_x) && 
               (bounds2$max_x + tolerance >= bounds1$min_x)
  y_overlap <- (bounds1$max_y + tolerance >= bounds2$min_y) && 
               (bounds2$max_y + tolerance >= bounds1$min_y)
  
  return(x_overlap && y_overlap)
}

# Analyze all pieces
piece_bounds <- list()
piece_coords <- list()

cat("=== PIECE ANALYSIS ===\n")
for (i in 1:length(pieces)) {
  piece_data <- pieces[[as.character(i)]]
  
  if (is.null(piece_data)) {
    piece_data <- pieces[[i]]
  }
  
  if (!is.null(piece_data)) {
    coords <- extract_path_coordinates(piece_data$path)
    bounds <- calculate_bounds(coords)
    
    piece_bounds[[i]] <- bounds
    piece_coords[[i]] <- coords
    
    cat(sprintf("Piece %2d: Center(%.1f, %.1f) Bounds[%.1f-%.1f, %.1f-%.1f] Points=%d %s\n",
                piece_data$piece_id, bounds$center_x, bounds$center_y,
                bounds$min_x, bounds$max_x, bounds$min_y, bounds$max_y,
                nrow(coords), if(piece_data$has_bezier) "✓tabs" else "○line"))
  }
}

# Check for overlaps
cat("\n=== OVERLAP ANALYSIS ===\n")
overlaps_found <- 0
tolerance <- 10  # pixels

for (i in 1:(length(pieces)-1)) {
  if (is.null(piece_bounds[[i]])) next
  
  for (j in (i+1):length(pieces)) {
    if (is.null(piece_bounds[[j]])) next
    
    bounds1 <- piece_bounds[[i]]
    bounds2 <- piece_bounds[[j]]
    
    if (check_overlap(bounds1, bounds2, tolerance)) {
      overlaps_found <- overlaps_found + 1
      
      # Calculate center distance
      center_dist <- sqrt((bounds1$center_x - bounds2$center_x)^2 + 
                         (bounds1$center_y - bounds2$center_y)^2)
      
      cat(sprintf("🔴 OVERLAP %d: Piece %d & %d - Centers %.1f apart\n",
                  overlaps_found, i, j, center_dist))
      cat(sprintf("     Piece %d: Center(%.1f, %.1f)\n", 
                  i, bounds1$center_x, bounds1$center_y))
      cat(sprintf("     Piece %d: Center(%.1f, %.1f)\n", 
                  j, bounds2$center_x, bounds2$center_y))
      
      # Check if they have identical centers (exact overlap)
      if (abs(bounds1$center_x - bounds2$center_x) < 1 && 
          abs(bounds1$center_y - bounds2$center_y) < 1) {
        cat("     ⚠️  IDENTICAL CENTERS - Pieces are overlapping exactly!\n")
      }
      cat("\n")
    }
  }
}

# Summary
cat("=== OVERLAP SUMMARY ===\n")
cat("Total pieces analyzed:", length(pieces), "\n")
cat("Overlapping pairs found:", overlaps_found, "\n")

if (overlaps_found > 0) {
  cat("🔴 ISSUE: Overlapping pieces detected!\n")
  cat("Recommendation: Check piece positioning and coordinate generation\n")
} else {
  cat("✅ No overlapping pieces detected\n")
}

# Find pieces with very similar centers (potential duplicates)
cat("\n=== DUPLICATE CENTER ANALYSIS ===\n")
centers <- data.frame(
  piece_id = numeric(),
  center_x = numeric(), 
  center_y = numeric()
)

for (i in 1:length(pieces)) {
  if (!is.null(piece_bounds[[i]])) {
    centers <- rbind(centers, data.frame(
      piece_id = i,
      center_x = piece_bounds[[i]]$center_x,
      center_y = piece_bounds[[i]]$center_y
    ))
  }
}

# Group by similar centers
duplicates_found <- 0
tolerance_center <- 2  # pixels for center similarity

for (i in 1:(nrow(centers)-1)) {
  for (j in (i+1):nrow(centers)) {
    dist <- sqrt((centers[i, "center_x"] - centers[j, "center_x"])^2 + 
                 (centers[i, "center_y"] - centers[j, "center_y"])^2)
    
    if (dist < tolerance_center) {
      duplicates_found <- duplicates_found + 1
      cat(sprintf("🟡 DUPLICATE CENTER %d: Pieces %d & %d (distance: %.2f)\n",
                  duplicates_found, centers[i, "piece_id"], centers[j, "piece_id"], dist))
    }
  }
}

if (duplicates_found == 0) {
  cat("✅ No duplicate centers found\n")
}

cat(sprintf("\n🎯 ANALYSIS COMPLETE: %d overlaps, %d duplicate centers\n", 
            overlaps_found, duplicates_found))