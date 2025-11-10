#!/usr/bin/env Rscript

# Analysis of Hexagonal Puzzle Structure
# Understand how pieces are arranged and what edges they should have

cat("🔍 ANALYZING HEXAGONAL PUZZLE STRUCTURE\n")

# Source the hexagonal puzzle functions
source("R/hexagonal_puzzle.R")

# Generate a 3-ring hexagonal puzzle
cat("=== GENERATING 3-RING HEXAGONAL PUZZLE ===\n")
init_hex_jigsaw(seed = 42, rings = 3, diameter = 240)

# Get the puzzle paths
horizontal_path <- hex_gen_dh()
vertical_path <- hex_gen_dv()
border_path <- hex_gen_db()

cat("Horizontal path length:", nchar(horizontal_path), "chars\n")
cat("Vertical path length:", nchar(vertical_path), "chars\n") 
cat("Border path length:", nchar(border_path), "chars\n")

# Parse paths to understand segments
parse_svg_path <- function(svg_path) {
  # Split on M commands to get segments
  segments <- strsplit(svg_path, "M ")[[1]]
  segments <- segments[segments != ""]  # Remove empty
  
  # Add M back to segments (except first if it was split)
  for (i in 1:length(segments)) {
    if (i > 1 || !grepl("^M", svg_path)) {
      segments[i] <- paste0("M ", segments[i])
    }
  }
  
  return(segments)
}

# Analyze each path type
cat("\n=== PATH SEGMENT ANALYSIS ===\n")

h_segments <- parse_svg_path(horizontal_path)
v_segments <- parse_svg_path(vertical_path)
b_segments <- parse_svg_path(border_path)

cat("Horizontal segments:", length(h_segments), "\n")
cat("Vertical segments:", length(v_segments), "\n")
cat("Border segments:", length(b_segments), "\n")
cat("Total segments:", length(h_segments) + length(v_segments) + length(b_segments), "\n")

# Show sample segments
cat("\n=== SAMPLE SEGMENTS ===\n")
if (length(h_segments) > 0) {
  cat("Sample horizontal segment:\n")
  cat(substr(h_segments[1], 1, 100), "...\n")
}

if (length(v_segments) > 0) {
  cat("Sample vertical segment:\n") 
  cat(substr(v_segments[1], 1, 100), "...\n")
}

if (length(b_segments) > 0) {
  cat("Sample border segment:\n")
  cat(substr(b_segments[1], 1, 100), "...\n")
}

# Calculate expected pieces for 3 rings
expected_pieces <- 3 * 3 * (3 - 1) + 1  # Formula: 3 * rings * (rings - 1) + 1
cat("\n=== PIECE COUNT ANALYSIS ===\n")
cat("Expected pieces for 3 rings:", expected_pieces, "\n")
cat("Ring 0 (center): 1 piece\n")
cat("Ring 1: 6 pieces\n") 
cat("Ring 2: 12 pieces\n")
cat("Total: 1 + 6 + 12 =", 1 + 6 + 12, "pieces\n")

# Analyze hexagonal coordinate system
cat("\n=== HEXAGONAL COORDINATE ANALYSIS ===\n")
cat("Hexagonal puzzles use axial coordinates (q, r)\n")
cat("Each piece has 6 neighbors in different directions\n")

# Generate axial coordinates for all pieces
generate_hex_coordinates <- function(rings) {
  coords <- list()
  index <- 1
  
  # Center piece
  coords[[index]] <- list(index = index, q = 0, r = 0, ring = 0, type = "center")
  index <- index + 1
  
  # Ring pieces
  for (ring in 1:(rings - 1)) {
    # Start at the "north" position
    q <- 0
    r <- -ring
    
    # Directions to walk around hex (6 directions)
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
          step = step
        )
        index <- index + 1
        
        # Move in current direction
        if (!(direction == 6 && step == ring)) {
          q <- q + directions[[direction]][1] 
          r <- r + directions[[direction]][2]
        }
      }
    }
  }
  
  return(coords)
}

hex_coords <- generate_hex_coordinates(3)
cat("Generated", length(hex_coords), "hexagonal coordinates\n")

# Show sample coordinates
cat("\n=== SAMPLE COORDINATES ===\n")
for (i in 1:min(10, length(hex_coords))) {
  coord <- hex_coords[[i]]
  cat(sprintf("Piece %d: q=%d, r=%d, ring=%d, type=%s\n", 
              coord$index, coord$q, coord$r, coord$ring, coord$type))
}

# Analyze edge relationships
cat("\n=== EDGE RELATIONSHIP ANALYSIS ===\n")
cat("Each interior piece should have 6 edges:\n")
cat("- Some edges are shared with adjacent pieces\n") 
cat("- Edge pieces have some straight borders\n")
cat("- Center piece has 6 edges shared with ring-1 pieces\n")

# Calculate piece neighbors using axial coordinates
get_hex_neighbors <- function(q, r) {
  # 6 directions in axial coordinates
  directions <- list(c(1, -1), c(1, 0), c(0, 1), c(-1, 1), c(-1, 0), c(0, -1))
  neighbors <- list()
  
  for (i in 1:6) {
    dir <- directions[[i]]
    neighbor_q <- q + dir[1]
    neighbor_r <- r + dir[2]
    neighbors[[i]] <- c(neighbor_q, neighbor_r)
  }
  
  return(neighbors)
}

# Show neighbor analysis for center piece
center_neighbors <- get_hex_neighbors(0, 0)
cat("Center piece neighbors (q,r coordinates):\n")
for (i in 1:length(center_neighbors)) {
  neighbor <- center_neighbors[[i]]
  cat(sprintf("  Direction %d: (%d, %d)\n", i, neighbor[1], neighbor[2]))
}

cat("\n🎯 ANALYSIS COMPLETE\n")
cat("Key insights:\n")
cat("- 19 total pieces in 3-ring hexagonal puzzle\n")
cat("- Each piece needs 6 edges constructed from path segments\n")
cat("- Adjacent pieces share edges with complementary orientations\n")
cat("- Need proper axial coordinate to edge segment mapping\n")