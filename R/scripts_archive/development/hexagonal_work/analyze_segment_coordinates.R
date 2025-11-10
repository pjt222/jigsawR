#!/usr/bin/env Rscript

# Analyze Segment Coordinates to Fix Positioning
# Step 1: Understand the current segment coordinate ranges

cat("🔍 ANALYZING SEGMENT COORDINATES FOR CENTERING\n")

# Load segments data
if (!file.exists("output/hex_segments_data.rds")) {
  stop("Please run analyze_hex_tabs_simple.R first")
}

segments_data <- readRDS("output/hex_segments_data.rds")
all_segments <- c(segments_data$horizontal_segments, 
                 segments_data$vertical_segments, 
                 segments_data$border_segments)

cat("Loaded", length(all_segments), "segments for analysis\n")

# Function to extract coordinates from a path
extract_coordinates_from_path <- function(path_string) {
  coords <- list()
  
  # Extract M coordinates (move to)
  m_pattern <- "M\\s+([-\\d\\.]+)\\s+([-\\d\\.]+)"
  m_matches <- gregmatches(path_string, gregexpr(m_pattern, path_string, perl = TRUE))[[1]]
  
  for (match in m_matches) {
    nums <- as.numeric(regmatches(match, gregexpr("[-\\d\\.]+", match))[[1]])
    if (length(nums) >= 2) {
      coords[[length(coords) + 1]] <- list(x = nums[1], y = nums[2], type = "M")
    }
  }
  
  # Extract C coordinates (cubic bezier - we want the end points)
  c_pattern <- "C\\s+([-\\d\\.\\s]+?)(?=\\s+[CMLz]|$)"
  c_matches <- gregmatches(path_string, gregexpr(c_pattern, path_string, perl = TRUE))[[1]]
  
  for (match in c_matches) {
    # Extract all numbers from the C command
    nums <- as.numeric(regmatches(match, gregexpr("[-\\d\\.]+", match))[[1]])
    if (length(nums) >= 6) {
      # The end point is the last two numbers (x3, y3)
      coords[[length(coords) + 1]] <- list(x = nums[5], y = nums[6], type = "C")
    }
  }
  
  # Extract L coordinates (line to)
  l_pattern <- "L\\s+([-\\d\\.]+)\\s+([-\\d\\.]+)"
  l_matches <- gregmatches(path_string, gregexpr(l_pattern, path_string, perl = TRUE))[[1]]
  
  for (match in l_matches) {
    nums <- as.numeric(regmatches(match, gregexpr("[-\\d\\.]+", match))[[1]])
    if (length(nums) >= 2) {
      coords[[length(coords) + 1]] <- list(x = nums[1], y = nums[2], type = "L")
    }
  }
  
  return(coords)
}

# Analyze each segment
cat("\n=== SEGMENT COORDINATE ANALYSIS ===\n")

segment_analysis <- list()

for (i in 1:length(all_segments)) {
  segment <- all_segments[[i]]
  
  coords <- extract_coordinates_from_path(segment$segment)
  
  if (length(coords) > 0) {
    x_coords <- sapply(coords, function(c) c$x)
    y_coords <- sapply(coords, function(c) c$y)
    
    # Calculate bounding box
    min_x <- min(x_coords)
    max_x <- max(x_coords)
    min_y <- min(y_coords)
    max_y <- max(y_coords)
    
    # Calculate center
    center_x <- (min_x + max_x) / 2
    center_y <- (min_y + max_y) / 2
    
    # Calculate size
    width <- max_x - min_x
    height <- max_y - min_y
    
    segment_analysis[[i]] <- list(
      id = segment$id,
      segment_text = segment$segment,
      coord_count = length(coords),
      bounds = list(
        min_x = min_x, max_x = max_x,
        min_y = min_y, max_y = max_y,
        center_x = center_x, center_y = center_y,
        width = width, height = height
      )
    )
    
    cat(sprintf("%s: center=(%.1f, %.1f), size=%.1fx%.1f, coords=%d\n",
                segment$id, center_x, center_y, width, height, length(coords)))
  }
}

# Show overall coordinate ranges
all_centers_x <- sapply(segment_analysis, function(s) s$bounds$center_x)
all_centers_y <- sapply(segment_analysis, function(s) s$bounds$center_y)

cat("\n=== OVERALL COORDINATE RANGES ===\n")
cat("Center X range:", round(min(all_centers_x), 1), "to", round(max(all_centers_x), 1), "\n")
cat("Center Y range:", round(min(all_centers_y), 1), "to", round(max(all_centers_y), 1), "\n")

# Show some sample segments in detail
cat("\n=== SAMPLE SEGMENT DETAILS ===\n")
for (i in 1:min(3, length(segment_analysis))) {
  seg <- segment_analysis[[i]]
  cat(sprintf("\nSegment %s:\n", seg$id))
  cat(sprintf("  Bounds: X(%.1f to %.1f) Y(%.1f to %.1f)\n", 
              seg$bounds$min_x, seg$bounds$max_x, seg$bounds$min_y, seg$bounds$max_y))
  cat(sprintf("  Center: (%.1f, %.1f)\n", seg$bounds$center_x, seg$bounds$center_y))
  cat(sprintf("  Original path: %s...\n", substr(seg$segment_text, 1, 80)))
}

# Save analysis for next step
analysis_data <- list(
  segment_analysis = segment_analysis,
  overall_bounds = list(
    center_x_range = c(min(all_centers_x), max(all_centers_x)),
    center_y_range = c(min(all_centers_y), max(all_centers_y))
  ),
  original_segments = all_segments
)

saveRDS(analysis_data, "output/segment_coordinate_analysis.rds")

cat("\n✅ COORDINATE ANALYSIS COMPLETE\n")
cat("Key findings:\n")
cat("- Segments are positioned in the lower right area of the original puzzle\n")
cat("- Each segment has its own center coordinates that need to be subtracted\n")
cat("- After centering, segments can be positioned using hexagonal grid transforms\n")
cat("Saved analysis to: output/segment_coordinate_analysis.rds\n")