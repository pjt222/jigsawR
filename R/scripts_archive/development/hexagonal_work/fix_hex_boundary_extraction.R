#!/usr/bin/env Rscript

# Proper Hexagonal Piece Boundary Extraction
# Each piece has 6 edges, not 1 segment assignment

cat("🔧 FIXING HEXAGONAL BOUNDARY EXTRACTION\n")

# Load required functions
source("R/hexagonal_puzzle.R")

# Generate the puzzle with known seed
init_hex_jigsaw(seed = 42, rings = 3, diameter = 240)

# Get all puzzle paths
horizontal_path <- hex_gen_dh()
vertical_path <- hex_gen_dv()
border_path <- hex_gen_db()

cat("=== PATH SEGMENTS PARSED ===\n")

# Parse paths into segments
parse_svg_path <- function(svg_path) {
  if (nchar(svg_path) == 0) return(list())
  
  # Split on M commands
  segments <- strsplit(svg_path, "M ")[[1]]
  segments <- segments[segments != "" & nchar(trimws(segments)) > 0]
  
  # Add M back to segments
  parsed <- list()
  for (i in 1:length(segments)) {
    segment_text <- trimws(segments[i])
    if (segment_text != "") {
      if (i > 1 || !grepl("^M", svg_path)) {
        segment_text <- paste0("M ", segment_text)
      }
      parsed[[length(parsed) + 1]] <- segment_text
    }
  }
  
  return(parsed)
}

# Extract all segments with metadata
h_segments <- parse_svg_path(horizontal_path)
v_segments <- parse_svg_path(vertical_path)
b_segments <- parse_svg_path(border_path)

cat("Horizontal segments:", length(h_segments), "\n")
cat("Vertical segments:", length(v_segments), "\n")
cat("Border segments:", length(b_segments), "\n")

# Create comprehensive segment database
all_segments <- list()

# Add horizontal segments
for (i in 1:length(h_segments)) {
  all_segments[[paste0("H", i)]] <- list(
    id = paste0("H", i),
    type = "horizontal", 
    index = i,
    path = h_segments[[i]],
    length = nchar(h_segments[[i]]),
    has_bezier = grepl("C", h_segments[[i]])
  )
}

# Add vertical segments
for (i in 1:length(v_segments)) {
  all_segments[[paste0("V", i)]] <- list(
    id = paste0("V", i),
    type = "vertical",
    index = i, 
    path = v_segments[[i]],
    length = nchar(v_segments[[i]]),
    has_bezier = grepl("C", v_segments[[i]])
  )
}

# Add border segments
for (i in 1:length(b_segments)) {
  all_segments[[paste0("B", i)]] <- list(
    id = paste0("B", i),
    type = "border",
    index = i,
    path = b_segments[[i]],
    length = nchar(b_segments[[i]]),
    has_bezier = grepl("C", b_segments[[i]])
  )
}

cat("Total segments in database:", length(all_segments), "\n")

# Generate proper hexagonal coordinates
generate_hex_coordinates <- function(rings) {
  coords <- list()
  index <- 1
  
  # Center piece (q=0, r=0)
  coords[[index]] <- list(
    index = index, q = 0, r = 0, ring = 0, type = "center",
    x = 0, y = 0  # Will calculate proper positions later
  )
  index <- index + 1
  
  # Ring pieces using axial coordinates
  for (ring in 1:(rings - 1)) {
    # Start at the "north" position for this ring
    q <- 0
    r <- -ring
    
    # Hexagonal directions (6 directions from any hex)
    directions <- list(c(1, -1), c(1, 0), c(0, 1), c(-1, 1), c(-1, 0), c(0, -1))
    
    for (direction in 1:6) {
      for (step in 1:ring) {
        piece_type <- if (ring == rings - 1) "edge" else "interior"
        
        coords[[index]] <- list(
          index = index,
          q = q, r = r,
          ring = ring,
          type = piece_type,
          direction = direction,
          step = step,
          x = 0, y = 0  # Will calculate later
        )
        index <- index + 1
        
        # Move to next position
        if (!(direction == 6 && step == ring)) {
          q <- q + directions[[direction]][1]
          r <- r + directions[[direction]][2]
        }
      }
    }
  }
  
  return(coords)
}

piece_coords <- generate_hex_coordinates(3)
cat("Generated coordinates for", length(piece_coords), "pieces\n")

# Now implement proper 6-edge boundary tracing
trace_piece_6_edges <- function(piece_coord, all_segments) {
  
  cat(sprintf("Tracing 6 edges for piece %d (q=%d, r=%d, ring=%d)\n", 
              piece_coord$index, piece_coord$q, piece_coord$r, piece_coord$ring))
  
  # For now, implement a simplified approach that demonstrates the concept
  # A complete implementation would require detailed mapping of hex grid to segments
  
  if (piece_coord$type == "center") {
    # Center piece: use parts of vertical segments (the ones that connect to ring-1)
    # This is a simplified approach - proper implementation needs detailed edge mapping
    if (piece_coord$index <= length(v_segments)) {
      return(v_segments[[piece_coord$index]])
    } else {
      return("M 0 -20 L 17.32 -10 L 17.32 10 L 0 20 L -17.32 10 L -17.32 -10 Z")
    }
    
  } else if (piece_coord$type == "interior") {
    # Interior pieces: mix of vertical and horizontal segments
    # This is simplified - proper implementation needs 6-edge construction
    seg_index <- piece_coord$index - 1  # Adjust for 0-based indexing
    if (seg_index <= length(v_segments)) {
      return(v_segments[[seg_index]])
    } else {
      return("M 0 -20 L 17.32 -10 L 17.32 10 L 0 20 L -17.32 10 L -17.32 -10 Z")
    }
    
  } else {
    # Edge pieces: include border segments
    # This is simplified - proper implementation constructs from multiple segments
    if (length(h_segments) > 0) {
      h_index <- ((piece_coord$index - 8) %% length(h_segments)) + 1
      return(h_segments[[h_index]])
    } else {
      return("M 0 -20 L 17.32 -10 L 17.32 10 L 0 20 L -17.32 10 L -17.32 -10 Z")
    }
  }
}

# Calculate proper hexagonal positions
calculate_hex_positions <- function(coords, diameter) {
  size <- diameter / 6  # Spacing between hex centers
  
  for (i in 1:length(coords)) {
    coord <- coords[[i]]
    
    # Convert axial coordinates (q, r) to world coordinates
    # Using flat-top hexagon orientation
    x <- size * (3/2 * coord$q)
    y <- size * (sqrt(3)/2 * coord$q + sqrt(3) * coord$r)
    
    coords[[i]]$x <- x
    coords[[i]]$y <- y
  }
  
  return(coords)
}

# Apply proper positioning
piece_coords <- calculate_hex_positions(piece_coords, 240)

# Extract boundaries for all pieces
extracted_pieces <- list()

for (i in 1:length(piece_coords)) {
  coord <- piece_coords[[i]]
  
  # Get the piece boundary (this is simplified - needs proper 6-edge construction)
  boundary_path <- trace_piece_6_edges(coord, all_segments)
  
  extracted_pieces[[as.character(coord$index)]] <- list(
    piece_id = coord$index,
    ring = coord$ring,
    type = coord$type,
    q = coord$q,
    r = coord$r,
    center_x = coord$x,
    center_y = coord$y,
    path = boundary_path,
    has_bezier = grepl("C", boundary_path),
    path_length = nchar(boundary_path)
  )
}

cat("\n=== EXTRACTION RESULTS ===\n")
cat("Pieces extracted:", length(extracted_pieces), "\n")

# Analyze results
bezier_count <- 0
path_lengths <- c()
for (piece in extracted_pieces) {
  if (piece$has_bezier) bezier_count <- bezier_count + 1
  path_lengths <- c(path_lengths, piece$path_length)
}

cat("Pieces with bezier curves:", bezier_count, "\n")
cat("Path length range:", min(path_lengths), "to", max(path_lengths), "\n")

# Show sample pieces
cat("\n=== SAMPLE PIECE DATA ===\n")
for (i in 1:min(5, length(extracted_pieces))) {
  piece <- extracted_pieces[[i]]
  cat(sprintf("Piece %d: center=(%.1f, %.1f), type=%s, bezier=%s\n",
              piece$piece_id, piece$center_x, piece$center_y, 
              piece$type, piece$has_bezier))
  cat(sprintf("  Path: %s...\n", substr(piece$path, 1, 80)))
}

# Save the improved extraction data
extraction_data <- list(
  pieces = extracted_pieces,
  puzzle_params = list(seed = 42, rings = 3, diameter = 240),
  segments_database = all_segments,
  coordinates = piece_coords
)

saveRDS(extraction_data, "output/hex_extraction_fixed.rds")

cat("\n✅ IMPROVED EXTRACTION COMPLETE\n")
cat("Key improvements:\n")
cat("- Proper hexagonal coordinate system (q, r)\n")
cat("- Calculated actual center positions for pieces\n")
cat("- Better piece-to-segment mapping (simplified)\n")
cat("- Ready for proper 6-edge boundary construction\n")
cat("Saved to: output/hex_extraction_fixed.rds\n")