#!/usr/bin/env Rscript

# Extract Individual Pieces from Circular Puzzle - REFINED APPROACH
# This approach recognizes that circular puzzles have continuous paths that need to be segmented
cat("🔄 REFINED EXTRACTION: Individual Pieces from Circular Puzzle\n")

# For circular/hexagonal puzzles, we need to understand the actual structure
# The puzzle has continuous paths, and individual pieces are segments of these paths
# Based on our analysis of hexagonal puzzle generation, we need to:
# 1. Identify piece boundaries within the continuous paths
# 2. Extract the relevant path segments for each piece
# 3. Create proper individual piece boundaries with tabs/blanks

cat("📋 CIRCULAR PUZZLE STRUCTURE ANALYSIS:\n")
cat("- Circular puzzles use continuous paths spanning multiple pieces\n")
cat("- Individual pieces are segments of these continuous paths\n")
cat("- Proper extraction requires understanding hexagonal coordinates\n")
cat("- Each piece needs boundary reconstruction from path segments\n")

# Read the original circular puzzle structure
source("R/hexagonal_puzzle.R")

# We need to regenerate the puzzle with individual piece tracking
cat("\n🔧 REGENERATING PUZZLE WITH PIECE TRACKING...\n")

# Use the same parameters as the original circular puzzle
rings <- 3  # This gives us 3*3*(3-1) + 1 = 19 pieces, but we see 18 in analysis
seed <- 12345
diameter <- 200

# Initialize the hexagonal puzzle environment  
init_hex_jigsaw(rings = rings, diameter = diameter, seed = seed)

# Get the puzzle structure with individual piece information
puzzle_structure <- generate_hex_jigsaw_svg(rings = rings, diameter = diameter, seed = seed)

cat("Generated puzzle structure analysis:\n")

# Extract individual pieces using the hexagonal coordinate system
extract_hexagonal_pieces <- function(rings, seed = 12345, diameter = 200) {
  # Initialize hex environment
  init_hex_jigsaw(rings = rings, diameter = diameter, seed = seed)
  
  # Generate coordinate system for hexagonal pieces
  piece_coords <- list()
  piece_id <- 1
  
  # Center piece (0,0)
  piece_coords[[piece_id]] <- list(q = 0, r = 0, piece_id = piece_id, ring = 0)
  piece_id <- piece_id + 1
  
  # Ring pieces
  for (ring in 1:rings) {
    # Six sides of hexagon for this ring
    for (side in 0:5) {
      for (step in 0:(ring-1)) {
        # Calculate axial coordinates for this position
        q <- ring * cos(side * pi/3) - step * cos((side + 2) * pi/3)
        r <- ring * sin(side * pi/3) - step * sin((side + 2) * pi/3)
        
        # Round to nearest integer coordinates
        q <- round(q)
        r <- round(r)
        
        piece_coords[[piece_id]] <- list(q = q, r = r, piece_id = piece_id, ring = ring)
        piece_id <- piece_id + 1
      }
    }
  }
  
  cat(sprintf("Generated %d piece coordinates\n", length(piece_coords)))
  
  # Convert axial coordinates to pixel coordinates
  for (i in seq_along(piece_coords)) {
    coord <- piece_coords[[i]]
    # Convert hex coordinates to pixel coordinates (using hex puzzle conversion)
    x <- coord$q * sqrt(3) * (diameter/6) + coord$r * sqrt(3)/2 * (diameter/6)
    y <- coord$r * 3/2 * (diameter/6)
    
    # Center in the 240x240 viewbox
    piece_coords[[i]]$x <- x + 120
    piece_coords[[i]]$y <- y + 120
  }
  
  return(piece_coords)
}

# Generate the hexagonal piece coordinates
piece_coordinates <- extract_hexagonal_pieces(rings = rings, seed = seed, diameter = diameter)

cat(sprintf("✅ Generated %d piece coordinates\n", length(piece_coordinates)))

# Create individual piece SVGs using proper hexagonal boundaries
create_hex_individual_pieces <- function(piece_coordinates) {
  # Create output directory
  if (!dir.exists("output/circular_individual_refined")) {
    dir.create("output/circular_individual_refined", recursive = TRUE)
  }
  
  # Colors for pieces
  colors <- c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12", "#9B59B6",
             "#1ABC9C", "#E67E22", "#34495E", "#F1C40F", "#E91E63",
             "#00BCD4", "#4CAF50", "#FF9800", "#607D8B", "#795548",
             "#FF5722", "#009688", "#8BC34A", "#FFC107", "#673AB7")
  
  pieces_data <- list()
  
  for (i in seq_along(piece_coordinates)) {
    coord <- piece_coordinates[[i]]
    piece_id <- coord$piece_id
    
    cat(sprintf("Creating piece %d (q=%d, r=%d, x=%.1f, y=%.1f)...\n", 
                piece_id, coord$q, coord$r, coord$x, coord$y))
    
    color_idx <- ((piece_id - 1) %% length(colors)) + 1
    color <- colors[color_idx]
    
    # Create individual piece boundary (simplified hexagonal shape for now)
    piece_size <- diameter / (rings * 2 + 1)
    hex_points <- generate_hex_boundary(coord$x, coord$y, piece_size / 2)
    
    # Create SVG for this piece
    margin <- piece_size
    min_x <- coord$x - margin
    max_x <- coord$x + margin  
    min_y <- coord$y - margin
    max_y <- coord$y + margin
    
    width <- max_x - min_x
    height <- max_y - min_y
    
    svg_lines <- c(
      '<?xml version="1.0" encoding="UTF-8"?>',
      sprintf('<svg xmlns="http://www.w3.org/2000/svg" version="1.0" width="%.1fmm" height="%.1fmm" viewBox="%.1f %.1f %.1f %.1f">', 
              width, height, min_x, min_y, width, height),
      sprintf('  <title>Circular Puzzle - Piece %d (Hex %d,%d)</title>', piece_id, coord$q, coord$r),
      '  <defs>',
      '    <filter id="shadow" x="-50%" y="-50%" width="200%" height="200%">',
      '      <feDropShadow dx="1" dy="1" stdDeviation="0.5" flood-opacity="0.3"/>',
      '    </filter>',
      '  </defs>',
      '  <rect width="100%" height="100%" fill="#fafafa"/>',
      sprintf('  <g id="piece-%d">', piece_id)
    )
    
    # Add hexagonal boundary
    path_data <- sprintf("M %s Z", paste(hex_points, collapse = " L "))
    svg_lines <- c(svg_lines,
      sprintf('    <path d="%s" fill="%s" fill-opacity="0.3" stroke="%s" stroke-width="2" filter="url(#shadow)"/>',
              path_data, color, color)
    )
    
    # Add piece center and ID
    svg_lines <- c(svg_lines,
      sprintf('    <circle cx="%.2f" cy="%.2f" r="3" fill="%s"/>',
              coord$x, coord$y, color),
      sprintf('    <text x="%.2f" y="%.2f" text-anchor="middle" dominant-baseline="central" font-family="Arial" font-size="10" font-weight="bold" fill="white">%d</text>',
              coord$x, coord$y, piece_id),
      sprintf('    <text x="%.2f" y="%.2f" text-anchor="middle" font-family="Arial" font-size="6" fill="%s">(%d,%d)</text>',
              coord$x, coord$y + 15, color, coord$q, coord$r)
    )
    
    # Close SVG
    svg_lines <- c(svg_lines, '  </g>', '</svg>')
    
    # Save individual piece
    piece_filename <- sprintf("output/circular_individual_refined/hex_piece_%02d.svg", piece_id)
    writeLines(paste(svg_lines, collapse = "\n"), piece_filename)
    
    pieces_data[[i]] <- list(
      piece_id = piece_id,
      hex_coords = c(coord$q, coord$r),
      pixel_coords = c(coord$x, coord$y),
      ring = coord$ring,
      filename = piece_filename,
      color = color
    )
  }
  
  return(pieces_data)
}

# Helper function to generate hexagonal boundary points
generate_hex_boundary <- function(center_x, center_y, radius) {
  points <- c()
  for (i in 0:5) {
    angle <- i * pi / 3
    x <- center_x + radius * cos(angle)
    y <- center_y + radius * sin(angle)
    points <- c(points, sprintf("%.2f,%.2f", x, y))
  }
  return(points)
}

# Generate individual pieces
pieces_data <- create_hex_individual_pieces(piece_coordinates)

# Create comprehensive index
create_hex_pieces_index <- function(pieces_data) {
  svg_lines <- c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<svg xmlns="http://www.w3.org/2000/svg" version="1.0" width="300.0mm" height="300.0mm" viewBox="0 0 300.0 300.0">',
    '  <title>Circular Puzzle - Hexagonal Pieces Index</title>',
    '  <rect width="100%" height="100%" fill="#f8f9fa"/>',
    '  <g id="hex-pieces-index">'
  )
  
  # Title
  svg_lines <- c(svg_lines,
    '    <text x="150" y="20" text-anchor="middle" font-family="Arial, sans-serif" font-size="16" font-weight="bold" fill="#2c3e50">Circular Puzzle - Hexagonal Individual Pieces</text>',
    sprintf('    <text x="150" y="35" text-anchor="middle" font-family="Arial, sans-serif" font-size="12" fill="#7f8c8d">%d pieces • Hexagonal coordinate system • Ring-based layout</text>', length(pieces_data))
  )
  
  # Draw pieces in their hexagonal positions (scaled for visibility)
  scale_factor <- 1.2
  center_x <- 150
  center_y <- 150
  
  for (piece in pieces_data) {
    # Scale coordinates for better visibility
    display_x <- center_x + (piece$pixel_coords[1] - 120) * scale_factor
    display_y <- center_y + (piece$pixel_coords[2] - 120) * scale_factor
    
    # Draw piece
    svg_lines <- c(svg_lines,
      sprintf('    <circle cx="%.2f" cy="%.2f" r="12" fill="%s" fill-opacity="0.7" stroke="white" stroke-width="2"/>',
              display_x, display_y, piece$color),
      sprintf('    <text x="%.2f" y="%.2f" text-anchor="middle" dominant-baseline="central" font-family="Arial" font-size="8" font-weight="bold" fill="white">%d</text>',
              display_x, display_y, piece$piece_id),
      sprintf('    <text x="%.2f" y="%.2f" text-anchor="middle" font-family="Arial" font-size="6" fill="%s">R%d</text>',
              display_x, display_y + 20, piece$color, piece$ring)
    )
  }
  
  # Legend
  svg_lines <- c(svg_lines,
    '    <g id="hex-legend">',
    '      <rect x="20" y="250" width="260" height="40" fill="white" fill-opacity="0.9" stroke="#bdc3c7" stroke-width="1"/>',
    '      <text x="30" y="265" font-family="Arial, sans-serif" font-size="12" font-weight="bold" fill="#2c3e50">Hexagonal Coordinate Extraction:</text>',
    sprintf('      <text x="30" y="275" font-family="Arial, sans-serif" font-size="10" fill="#27ae60">%d pieces generated using axial coordinates (q,r)</text>', length(pieces_data)),
    '      <text x="30" y="285" font-family="Arial, sans-serif" font-size="9" fill="#7f8c8d">Each piece represents a hexagon in the circular puzzle grid</text>',
    '    </g>'
  )
  
  # Close SVG
  svg_lines <- c(svg_lines, '  </g>', '</svg>')
  
  return(paste(svg_lines, collapse = "\n"))
}

# Save comprehensive index
index_svg <- create_hex_pieces_index(pieces_data)
writeLines(index_svg, "output/circular_individual_refined_index.svg")

# Save summary
summary_data <- list(
  approach = "hexagonal_coordinate_extraction",
  timestamp = Sys.time(),
  total_pieces = length(pieces_data),
  pieces_data = pieces_data,
  parameters = list(rings = rings, seed = seed, diameter = diameter),
  output_directory = "output/circular_individual_refined/",
  index_file = "output/circular_individual_refined_index.svg"
)

saveRDS(summary_data, "output/circular_individual_refined_summary.rds")

cat("\n✅ REFINED CIRCULAR PIECES EXTRACTION COMPLETED\n")
cat("📊 REFINED RESULTS:\n")
cat("- Approach: Hexagonal coordinate system\n")
cat("- Total pieces:", length(pieces_data), "\n")
cat("- Coordinate system: Axial (q,r) coordinates\n")
cat("- Layout: Ring-based hexagonal grid\n")
cat("- Output directory: output/circular_individual_refined/\n")
cat("- Index file: output/circular_individual_refined_index.svg\n")

cat("\n📂 REFINED FILES CREATED:\n")
for (piece in pieces_data) {
  cat(sprintf("- hex_piece_%02d.svg (q=%d, r=%d, ring=%d)\n", 
              piece$piece_id, piece$hex_coords[1], piece$hex_coords[2], piece$ring))
}

cat("\n🎯 REFINED APPROACH BENEFITS:\n")
cat("✅ Uses proper hexagonal coordinate system\n")
cat("✅ Ring-based piece organization\n") 
cat("✅ Axial coordinates (q,r) for each piece\n")
cat("✅ Simplified hexagonal boundaries for verification\n")
cat("✅ Foundation for complex tab/blank extraction\n")

cat("\n📝 NEXT REFINEMENT:\n")
cat("Replace simplified hexagonal boundaries with actual puzzle piece paths\n")
cat("Extract the real tabs and blanks from the continuous puzzle paths\n")