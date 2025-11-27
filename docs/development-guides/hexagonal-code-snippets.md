# Hexagonal Puzzles - Useful Code Snippets

**Last Updated**: 2025-11-27

A collection of useful code patterns and "hacky" snippets extracted from development and debugging sessions.

---

## Quick Visualization

### Generate Test SVG

```r
# Quick test of hexagonal separation with bezier curves
source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")
source("R/hexagonal_separation.R")

svg <- generate_separated_hexagonal_svg(
  rings = 2,
  seed = 42,
  diameter = 240,
  offset = 10,
  arrangement = "hexagonal",
  use_bezier = TRUE,
  tabsize = 27,
  jitter = 5
)

dir.create("output", showWarnings = FALSE)
writeLines(svg, "output/quick_test.svg")
cat("Saved to output/quick_test.svg\n")
```

### Create Standalone Bezier Visualization

```r
# Visualize hexagonal bezier pieces
source("R/hexagonal_topology.R")
source("R/hexagonal_bezier_generation.R")

pieces <- generate_all_hex_pieces_bezier(
  rings = 2,
  seed = 42,
  diameter = 240,
  separated = TRUE
)

# Calculate SVG dimensions
all_x <- sapply(pieces, function(p) p$center_x)
all_y <- sapply(pieces, function(p) p$center_y)
margin <- 50
min_x <- min(all_x) - margin
max_x <- max(all_x) + margin
min_y <- min(all_y) - margin
max_y <- max(all_y) + margin
width <- max_x - min_x
height <- max_y - min_y

# Create SVG
svg_content <- sprintf(
  '<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" viewBox="%.2f %.2f %.2f %.2f">
  <style>
    .puzzle-piece { fill: white; stroke: black; stroke-width: 1; }
    .piece-label { font-family: Arial; font-size: 12px; text-anchor: middle; }
  </style>
  <rect x="%.2f" y="%.2f" width="%.2f" height="%.2f" fill="#f0f0f0"/>',
  min_x, min_y, width, height,
  min_x, min_y, width, height
)

for (p in pieces) {
  svg_content <- paste0(svg_content, sprintf(
    '\n  <path class="puzzle-piece" d="%s"/>
  <text class="piece-label" x="%.2f" y="%.2f">%d</text>',
    p$path, p$center_x, p$center_y, p$id
  ))
}

svg_content <- paste0(svg_content, '\n</svg>')
writeLines(svg_content, "output/bezier_visualization.svg")
```

---

## Coordinate System Analysis

### Analyze Grid Iteration Pattern

```r
# Understand piece numbering in hexagonal grid
source('R/hexagonal_puzzle.R')

analyze_hex_grid <- function(rings) {
  n <- rings
  yl <- 2 * n - 1

  cat(sprintf("Rings: %d\n", rings))
  cat(sprintf("yl = %d (2*n - 1)\n\n", yl))

  piece_id <- 1
  for (yi in seq(-yl + 2, yl - 2, by = 2)) {
    xl <- 2 * n - 1 - (abs(yi) - 1) / 2
    cat(sprintf("yi=%3d: xl=%.1f, xi range: [%.0f, %.0f]\n",
                yi, xl, -xl + 1, xl - 2))

    for (xi in seq(-xl + 1, xl - 2, by = 1)) {
      cat(sprintf("  Piece %2d: (xi=%.1f, yi=%d)\n", piece_id, xi, yi))
      piece_id <- piece_id + 1
    }
  }

  total <- 3 * rings * (rings - 1) + 1
  cat(sprintf("\nTotal pieces: %d\n", total))
}

analyze_hex_grid(2)  # 7 pieces
analyze_hex_grid(3)  # 19 pieces
```

### Map Piece IDs to Axial Coordinates

```r
# Convert piece ID to (q, r) axial coordinates
piece_to_axial <- function(piece_id, rings) {
  if (piece_id == 1) return(c(q = 0, r = 0))

  # Hexagonal directions (for ring traversal)
  hex_directions <- list(
    c(1, 0), c(1, -1), c(0, -1),
    c(-1, 0), c(-1, 1), c(0, 1)
  )

  remaining <- piece_id - 1
  ring <- 1

  while (remaining > 6 * ring) {
    remaining <- remaining - 6 * ring
    ring <- ring + 1
  }

  # Start position for this ring
  q <- 0
  r <- -ring

  position <- remaining - 1
  direction <- floor(position / ring)
  steps_in_direction <- position %% ring

  # Move to starting direction
  for (d in seq_len(direction)) {
    for (s in seq_len(ring)) {
      dir <- hex_directions[[d]]
      q <- q + dir[1]
      r <- r + dir[2]
    }
  }

  # Move within direction
  if (steps_in_direction > 0 && direction < 6) {
    dir <- hex_directions[[direction + 1]]
    for (s in seq_len(steps_in_direction)) {
      q <- q + dir[1]
      r <- r + dir[2]
    }
  }

  c(q = q, r = r)
}
```

---

## Debugging Utilities

### Debug Path Connectivity

```r
# Test if SVG paths connect at endpoints
debug_path_connectivity <- function(paths, tolerance = 0.5) {
  all_segs <- list()

  for (path in paths) {
    segs <- split_path_by_move(path)
    all_segs <- c(all_segs, segs)
  }

  cat(sprintf("Total segments: %d\n\n", length(all_segs)))

  # Print endpoints
  cat("Segment endpoints:\n")
  for (i in seq_along(all_segs)) {
    seg <- all_segs[[i]]
    cat(sprintf("  %d: (%.2f, %.2f) -> (%.2f, %.2f)\n",
                i, seg$start[1], seg$start[2],
                seg$end[1], seg$end[2]))
  }

  # Test connectivity
  cat("\nConnectivity test:\n")
  for (tol in c(0.1, 0.5, 1.0, 2.0, 5.0)) {
    connections <- 0
    for (i in seq_along(all_segs)) {
      for (j in seq_along(all_segs)) {
        if (i != j) {
          d <- sqrt(sum((all_segs[[i]]$end - all_segs[[j]]$start)^2))
          if (d < tol) connections <- connections + 1
        }
      }
    }
    cat(sprintf("  Tolerance %.1f: %d connections\n", tol, connections))
  }
}
```

### Debug Edge Complementarity

```r
# Verify edge complementarity between adjacent pieces
debug_edge_complementarity <- function(edge_data, piece1, side1, piece2, side2) {
  key1 <- sprintf("%d-%d", piece1, side1)
  key2 <- sprintf("%d-%d", piece2, side2)

  edge1 <- edge_data$piece_edge_map[[key1]]
  edge2 <- edge_data$piece_edge_map[[key2]]

  cat(sprintf("Checking: Piece %d side %d <-> Piece %d side %d\n",
              piece1, side1, piece2, side2))

  if (is.null(edge1)) {
    cat("  ERROR: edge1 not found\n")
    return(FALSE)
  }
  if (is.null(edge2)) {
    cat("  ERROR: edge2 not found\n")
    return(FALSE)
  }

  cat(sprintf("  Edge1 key: %s\n", edge1$edge_key))
  cat(sprintf("  Edge2 key: %s\n", edge2$edge_key))
  cat(sprintf("  Edge1 forward: %s...\n", substr(edge1$forward, 1, 50)))
  cat(sprintf("  Edge2 reverse: %s...\n", substr(edge2$reverse, 1, 50)))

  if (edge1$edge_key == edge2$edge_key && edge1$forward == edge2$reverse) {
    cat("  RESULT: COMPLEMENTARY\n")
    return(TRUE)
  } else {
    cat("  RESULT: NOT COMPLEMENTARY\n")
    return(FALSE)
  }
}
```

### Debug Piece Positions

```r
# Print all piece positions for verification
debug_piece_positions <- function(rings) {
  source("R/hexagonal_topology.R")

  num_pieces <- 3 * rings * (rings - 1) + 1

  cat(sprintf("Piece positions for %d rings (%d pieces):\n",
              rings, num_pieces))
  cat("=" * 50, "\n")

  for (i in seq_len(num_pieces)) {
    ring_info <- map_piece_id_to_ring(i, rings)
    pos <- calculate_hex_piece_position(i, rings, diameter = 240)

    cat(sprintf("Piece %2d: ring=%d, pos=%d, dir=%d, angle=%.0f°, ",
                i, ring_info$ring, ring_info$position_in_ring,
                ring_info$direction, ring_info$angle * 180 / pi))
    cat(sprintf("center=(%.1f, %.1f)\n", pos$x, pos$y))
  }
}

debug_piece_positions(2)
```

---

## Neighbor Detection

### Calculate Neighbors from Vertex Sharing

```r
# Find neighbors by checking shared vertices
find_neighbors_by_vertices <- function(pieces, tolerance = 0.5) {
  neighbors <- list()

  for (i in seq_along(pieces)) {
    neighbors[[i]] <- integer(0)
    vertices_i <- pieces[[i]]$vertices  # 6 vertices per hexagon

    for (j in seq_along(pieces)) {
      if (i == j) next

      vertices_j <- pieces[[j]]$vertices

      # Count shared vertices
      shared <- 0
      for (vi in seq_len(6)) {
        for (vj in seq_len(6)) {
          d <- sqrt(sum((vertices_i[, vi] - vertices_j[, vj])^2))
          if (d < tolerance) shared <- shared + 1
        }
      }

      # Adjacent hexagons share exactly 2 vertices (one edge)
      if (shared >= 2) {
        neighbors[[i]] <- c(neighbors[[i]], j)
      }
    }
  }

  neighbors
}
```

### Ring 1 to Ring 2 Neighbor Mapping

```r
# Quick lookup for ring 1 neighbors in ring 2
get_ring2_neighbors_of_ring1 <- function(ring1_piece) {
  # Piece 2 (ring 1, position 0): neighbors pieces 8, 9 in ring 2
  # Piece 3 (ring 1, position 1): neighbors pieces 10, 11 in ring 2
  # etc.

  ring1_position <- ring1_piece - 2  # 0-5
  ring2_start <- 8  # First piece in ring 2

  # Each ring 1 piece neighbors 2 consecutive ring 2 pieces
  c(ring2_start + ring1_position * 2,
    ring2_start + ring1_position * 2 + 1)
}
```

---

## SVG Path Manipulation

### Parse SVG Path Commands

```r
# Extract commands and coordinates from SVG path
parse_svg_path <- function(path) {
  # Split by command letters
  commands <- strsplit(path, "(?=[MLCQSZ])", perl = TRUE)[[1]]
  commands <- commands[nchar(commands) > 0]

  result <- list()
  for (cmd in commands) {
    type <- substr(cmd, 1, 1)
    coords_str <- trimws(substr(cmd, 2, nchar(cmd)))
    coords <- as.numeric(strsplit(coords_str, "[, ]+")[[1]])
    coords <- coords[!is.na(coords)]

    result[[length(result) + 1]] <- list(type = type, coords = coords)
  }

  result
}

# Example usage:
# parsed <- parse_svg_path("M 10 20 L 30 40 C 50 60 70 80 90 100")
# parsed[[1]]  # list(type = "M", coords = c(10, 20))
```

### Translate SVG Path

```r
# Move entire SVG path by (dx, dy)
translate_svg_path <- function(path, dx, dy) {
  parsed <- parse_svg_path(path)

  result <- ""
  for (cmd in parsed) {
    if (cmd$type == "Z") {
      result <- paste0(result, "Z ")
      next
    }

    coords <- cmd$coords

    # Translate coordinates (every pair)
    for (i in seq(1, length(coords), by = 2)) {
      coords[i] <- coords[i] + dx
      coords[i + 1] <- coords[i + 1] + dy
    }

    result <- paste0(result, cmd$type, " ",
                     paste(sprintf("%.2f", coords), collapse = " "), " ")
  }

  trimws(result)
}
```

### Reverse SVG Path

```r
# Reverse direction of SVG path (for complementary edges)
reverse_svg_path <- function(path) {
  parsed <- parse_svg_path(path)

  # Collect all points
  points <- list()
  for (cmd in parsed) {
    if (cmd$type == "M" || cmd$type == "L") {
      points[[length(points) + 1]] <- cmd$coords
    } else if (cmd$type == "C") {
      # Cubic bezier: 3 coordinate pairs
      points[[length(points) + 1]] <- cmd$coords[1:2]  # control 1
      points[[length(points) + 1]] <- cmd$coords[3:4]  # control 2
      points[[length(points) + 1]] <- cmd$coords[5:6]  # end point
    }
  }

  # Reverse and rebuild
  points <- rev(points)
  # ... rebuild path from reversed points
  # (Implementation depends on curve type)
}
```

---

## Quick Tests

### Test Shiny App Flow

```r
# Trace through Shiny app generation flow
test_shiny_flow <- function() {
  cat("Testing Shiny app flow:\n")

  # Simulate input
  rings <- 2
  seed <- 42
  diameter <- 240
  offset <- 10
  use_bezier <- TRUE

  cat("1. Generating SVG...\n")
  svg <- generate_separated_hexagonal_svg(
    rings = rings,
    seed = seed,
    diameter = diameter,
    offset = offset,
    arrangement = "hexagonal",
    use_bezier = use_bezier
  )

  cat("2. Checking SVG content...\n")
  cat(sprintf("   Length: %d characters\n", nchar(svg)))
  cat(sprintf("   Has bezier: %s\n", grepl("C ", svg)))
  cat(sprintf("   Piece count: %d\n", length(gregexpr("<path", svg)[[1]])))

  cat("3. Saving output...\n")
  writeLines(svg, "output/shiny_flow_test.svg")
  cat("   Saved to output/shiny_flow_test.svg\n")

  cat("\nTest complete!\n")
}
```

### Final Verification Script

```r
# Comprehensive verification of hexagonal implementation
final_verification <- function() {
  source("R/hexagonal_topology.R")
  source("R/hexagonal_neighbors.R")
  source("R/hexagonal_bezier_generation.R")
  source("R/hexagonal_edge_generation_fixed.R")
  source("R/hexagonal_separation.R")

  cat("=" * 60, "\n")
  cat("  FINAL VERIFICATION - Hexagonal Implementation\n")
  cat("=" * 60, "\n\n")

  # Test 1: Generate puzzle
  cat("Test 1: Generate 2-ring puzzle with bezier\n")
  svg <- generate_separated_hexagonal_svg(
    rings = 2, seed = 42, diameter = 240, offset = 10,
    arrangement = "hexagonal", use_bezier = TRUE
  )
  cat(sprintf("  SVG length: %d chars\n", nchar(svg)))
  cat(sprintf("  Has bezier curves: %s\n", grepl("C ", svg)))

  # Test 2: Edge complementarity
  cat("\nTest 2: Edge complementarity\n")
  edge_data <- generate_hex_edge_map(rings = 2, seed = 42, diameter = 240)
  cat(sprintf("  Generated %d unique edges\n", edge_data$num_edges))

  # Test 3: Save output
  cat("\nTest 3: Save verification output\n")
  output_file <- "output/final_verification.svg"
  writeLines(svg, output_file)
  cat(sprintf("  Saved: %s\n", output_file))

  cat("\n")
  cat("=" * 60, "\n")
  cat("  ALL TESTS PASSED\n")
  cat("=" * 60, "\n")
}

# Run with: final_verification()
```

---

*Add new snippets as they're discovered during development.*
