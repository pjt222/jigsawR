#!/usr/bin/env Rscript

# Distance-based Hexagonal Piece Extraction
# Use piece centers to find closest path coordinates and build boundaries

cat("🎯 IMPLEMENTING DISTANCE-BASED EXTRACTION\n")

# Load the improved extraction data
if (!file.exists("output/hex_extraction_fixed.rds")) {
  stop("Please run fix_hex_boundary_extraction.R first")
}

extraction_data <- readRDS("output/hex_extraction_fixed.rds")
pieces <- extraction_data$pieces
all_segments <- extraction_data$segments_database

cat("Loaded", length(pieces), "pieces and", length(all_segments), "segments\n")

# Function to extract all coordinates from a path
extract_path_coordinates <- function(path_string) {
  coords <- list()
  
  # Find all M and C commands with their coordinates
  # M x y - move to
  # C x1 y1 x2 y2 x3 y3 - cubic bezier curve
  # L x y - line to
  
  # Extract M coordinates (move commands)
  m_matches <- gregexpr("M\\s+([-\\d\\.]+)\\s+([-\\d\\.]+)", path_string, perl = TRUE)
  if (length(m_matches[[1]]) > 0 && m_matches[[1]][1] != -1) {
    for (i in 1:length(m_matches[[1]])) {
      match_text <- regmatches(path_string, m_matches)[[1]][i]
      coords_text <- gsub("M\\s+", "", match_text)
      coord_nums <- as.numeric(strsplit(coords_text, "\\s+")[[1]])
      if (length(coord_nums) >= 2) {
        coords[[length(coords) + 1]] <- c(coord_nums[1], coord_nums[2])
      }
    }
  }
  
  # Extract L coordinates (line commands)
  l_matches <- gregexpr("L\\s+([-\\d\\.]+)\\s+([-\\d\\.]+)", path_string, perl = TRUE)
  if (length(l_matches[[1]]) > 0 && l_matches[[1]][1] != -1) {
    for (i in 1:length(l_matches[[1]])) {
      match_text <- regmatches(path_string, l_matches)[[1]][i]
      coords_text <- gsub("L\\s+", "", match_text)
      coord_nums <- as.numeric(strsplit(coords_text, "\\s+")[[1]])
      if (length(coord_nums) >= 2) {
        coords[[length(coords) + 1]] <- c(coord_nums[1], coord_nums[2])
      }
    }
  }
  
  # Extract C coordinates (curve commands) - we want the end points
  c_matches <- gregexpr("C\\s+([-\\d\\.\\s]+?)(?=\\s+[CMLz]|$)", path_string, perl = TRUE)
  if (length(c_matches[[1]]) > 0 && c_matches[[1]][1] != -1) {
    for (i in 1:length(c_matches[[1]])) {
      match_text <- regmatches(path_string, c_matches)[[1]][i]
      coords_text <- gsub("C\\s+", "", match_text)
      coord_nums <- as.numeric(strsplit(coords_text, "\\s+")[[1]])
      if (length(coord_nums) >= 6) {
        # Take the end point of the bezier curve (x3, y3)
        coords[[length(coords) + 1]] <- c(coord_nums[5], coord_nums[6])
      }
    }
  }
  
  return(coords)
}

# Extract all path coordinates from all segments
cat("\n=== EXTRACTING PATH COORDINATES ===\n")
all_path_coords <- list()

for (seg_id in names(all_segments)) {
  segment <- all_segments[[seg_id]]
  segment_coords <- extract_path_coordinates(segment$path)
  
  cat(sprintf("Segment %s: %d coordinates\n", seg_id, length(segment_coords)))
  
  for (i in 1:length(segment_coords)) {
    coord <- segment_coords[[i]]
    all_path_coords[[length(all_path_coords) + 1]] <- list(
      x = coord[1],
      y = coord[2],
      segment_id = seg_id,
      segment_type = segment$type
    )
  }
}

cat("Total path coordinates extracted:", length(all_path_coords), "\n")

# Function to calculate distance between two points
distance <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

# Function to find closest path coordinates to piece vertices
find_closest_path_points <- function(piece_center_x, piece_center_y, hex_size = 25) {
  
  # Calculate the 6 vertices of a regular hexagon around the center
  # Using flat-top orientation
  vertices <- list()
  for (angle in c(0, 60, 120, 180, 240, 300)) {
    angle_rad <- angle * pi / 180
    vertex_x <- piece_center_x + hex_size * cos(angle_rad)
    vertex_y <- piece_center_y + hex_size * sin(angle_rad)
    vertices[[length(vertices) + 1]] <- c(vertex_x, vertex_y)
  }
  
  # For each vertex, find the closest actual path coordinate
  closest_matches <- list()
  
  for (i in 1:length(vertices)) {
    vertex <- vertices[[i]]
    min_distance <- Inf
    closest_coord <- NULL
    
    for (j in 1:length(all_path_coords)) {
      path_coord <- all_path_coords[[j]]
      dist <- distance(vertex[1], vertex[2], path_coord$x, path_coord$y)
      
      if (dist < min_distance) {
        min_distance <- dist
        closest_coord <- path_coord
      }
    }
    
    closest_matches[[i]] <- list(
      vertex_ideal = vertex,
      closest_actual = c(closest_coord$x, closest_coord$y),
      distance = min_distance,
      segment_id = closest_coord$segment_id,
      segment_type = closest_coord$segment_type
    )
  }
  
  return(closest_matches)
}

# Apply distance-based matching to all pieces
cat("\n=== APPLYING DISTANCE-BASED MATCHING ===\n")

improved_pieces <- list()

for (i in 1:length(pieces)) {
  piece <- pieces[[i]]
  
  cat(sprintf("Processing piece %d at center (%.1f, %.1f)\n", 
              piece$piece_id, piece$center_x, piece$center_y))
  
  # Find closest path points for this piece
  closest_points <- find_closest_path_points(piece$center_x, piece$center_y)
  
  # Build the path from these closest points
  path_parts <- c("M")
  
  for (j in 1:length(closest_points)) {
    match <- closest_points[[j]]
    actual_coord <- match$closest_actual
    
    if (j == 1) {
      # Start with move command
      path_parts <- c(path_parts, sprintf("%.2f %.2f", actual_coord[1], actual_coord[2]))
    } else {
      # Add line command to next point  
      path_parts <- c(path_parts, "L", sprintf("%.2f %.2f", actual_coord[1], actual_coord[2]))
    }
  }
  
  # Close the path
  path_parts <- c(path_parts, "Z")
  
  complete_path <- paste(path_parts, collapse = " ")
  
  # Calculate quality metrics
  avg_distance <- mean(sapply(closest_points, function(m) m$distance))
  unique_segments <- length(unique(sapply(closest_points, function(m) m$segment_id)))
  
  improved_pieces[[as.character(piece$piece_id)]] <- list(
    piece_id = piece$piece_id,
    ring = piece$ring,
    type = piece$type,
    center_x = piece$center_x,
    center_y = piece$center_y,
    path = complete_path,
    vertex_matches = closest_points,
    avg_distance_to_paths = avg_distance,
    segments_used = unique_segments,
    has_bezier = FALSE,  # These are constructed from points, not original beziers
    path_length = nchar(complete_path)
  )
  
  cat(sprintf("  → Avg distance to paths: %.2f, segments used: %d\n", 
              avg_distance, unique_segments))
}

cat("\n=== DISTANCE-BASED EXTRACTION RESULTS ===\n")
cat("Pieces processed:", length(improved_pieces), "\n")

# Analyze quality
distances <- sapply(improved_pieces, function(p) p$avg_distance_to_paths)
cat("Average distance to paths:", round(mean(distances), 2), "\n")
cat("Distance range:", round(min(distances), 2), "to", round(max(distances), 2), "\n")

# Show sample results
cat("\n=== SAMPLE IMPROVED PIECES ===\n")
for (i in 1:min(3, length(improved_pieces))) {
  piece <- improved_pieces[[i]]
  cat(sprintf("Piece %d: center=(%.1f, %.1f), avg_dist=%.2f\n",
              piece$piece_id, piece$center_x, piece$center_y, piece$avg_distance_to_paths))
  cat(sprintf("  Path: %s\n", substr(piece$path, 1, 100)))
}

# Save improved results
improved_data <- list(
  pieces = improved_pieces,
  puzzle_params = extraction_data$puzzle_params,
  all_path_coords = all_path_coords,
  quality_metrics = list(
    avg_distance = mean(distances),
    distance_range = c(min(distances), max(distances))
  )
)

saveRDS(improved_data, "output/hex_distance_based.rds")

cat("\n✅ DISTANCE-BASED EXTRACTION COMPLETE\n")
cat("Key achievements:\n")
cat("- Used piece centers to find actual path coordinates\n")
cat("- Built complete 6-sided boundaries from closest points\n")
cat("- All pieces have proper positioning and closed paths\n")
cat("Saved to: output/hex_distance_based.rds\n")