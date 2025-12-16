# Bezier curve utilities for puzzle piece generation

#' Parse SVG path string into structured segments
#' 
#' @param path_string SVG path d attribute string
#' @return List of path segments
#' @export
parse_svg_path <- function(path_string) {
  # Remove extra whitespace and split by commands
  path_string <- gsub("\\s+", " ", trimws(path_string))
  
  # Extract commands and their parameters
  # Commands: M (move), L (line), C (cubic bezier), A (arc), Z (close)
  pattern <- "([MLCAZ])([^MLCAZ]*)"
  matches <- gregexpr(pattern, path_string, perl = TRUE)
  
  segments <- list()
  for (match in regmatches(path_string, matches)[[1]]) {
    cmd <- substr(match, 1, 1)
    params <- trimws(substr(match, 2, nchar(match)))
    
    if (cmd == "M" || cmd == "L") {
      coords <- as.numeric(strsplit(params, "[, ]+")[[1]])
      segments[[length(segments) + 1]] <- list(
        type = cmd,
        x = coords[1],
        y = coords[2]
      )
    } else if (cmd == "C") {
      coords <- as.numeric(strsplit(params, "[, ]+")[[1]])
      # Cubic bezier can have multiple sets of 6 coordinates
      for (i in seq(1, length(coords), 6)) {
        segments[[length(segments) + 1]] <- list(
          type = "C",
          cp1x = coords[i], cp1y = coords[i+1],
          cp2x = coords[i+2], cp2y = coords[i+3],
          x = coords[i+4], y = coords[i+5]
        )
      }
    } else if (cmd == "A") {
      coords <- as.numeric(strsplit(params, "[, ]+")[[1]])
      segments[[length(segments) + 1]] <- list(
        type = "A",
        rx = coords[1], ry = coords[2],
        rotation = coords[3],
        large_arc = coords[4],
        sweep = coords[5],
        x = coords[6], y = coords[7]
      )
    } else if (cmd == "Z") {
      segments[[length(segments) + 1]] <- list(type = "Z")
    }
  }
  
  return(segments)
}

#' Reverse a series of bezier curve segments
#' 
#' @param segments List of parsed path segments to reverse
#' @param start_point Starting point for the reversed path
#' @return Reversed SVG path string
#' @export
reverse_path_segments <- function(segments, start_point) {
  # Filter to just the bezier curves
  bezier_segments <- segments[sapply(segments, function(s) s$type == "C")]
  
  if (length(bezier_segments) == 0) {
    return("")
  }
  
  # Reverse the order and the control points
  reversed <- character(length(bezier_segments))
  
  for (i in seq_along(bezier_segments)) {
    # Take segments in reverse order
    seg <- bezier_segments[[length(bezier_segments) - i + 1]]
    
    # Determine the start point for this reversed segment
    if (i == 1) {
      prev_x <- start_point[1]
      prev_y <- start_point[2]
    } else {
      # Use the end point of the previous segment in original order
      prev_seg <- bezier_segments[[length(bezier_segments) - i + 2]]
      prev_x <- prev_seg$x
      prev_y <- prev_seg$y
    }
    
    # Reverse the bezier curve: swap control points and reverse direction
    reversed[i] <- sprintf("C %.2f %.2f %.2f %.2f %.2f %.2f",
                          seg$cp2x, seg$cp2y,  # Second control point becomes first
                          seg$cp1x, seg$cp1y,  # First control point becomes second
                          prev_x, prev_y)      # End at the previous point
  }
  
  return(paste(reversed, collapse = " "))
}

#' Transform coordinates for complementary puzzle edges
#' 
#' @param segments List of path segments
#' @param flip_axis "x" or "y" axis to flip around
#' @param center Center point for flipping (default 100)
#' @return Transformed SVG path string
#' @export
flip_path_segments <- function(segments, flip_axis = "x", center = 100) {
  flipped <- character()
  
  for (seg in segments) {
    if (seg$type == "C") {
      if (flip_axis == "x") {
        flipped <- c(flipped, sprintf("C %.2f %.2f %.2f %.2f %.2f %.2f",
                                     2*center - seg$cp1x, seg$cp1y,
                                     2*center - seg$cp2x, seg$cp2y,
                                     2*center - seg$x, seg$y))
      } else {
        flipped <- c(flipped, sprintf("C %.2f %.2f %.2f %.2f %.2f %.2f",
                                     seg$cp1x, 2*center - seg$cp1y,
                                     seg$cp2x, 2*center - seg$cp2y,
                                     seg$x, 2*center - seg$y))
      }
    }
  }
  
  return(paste(flipped, collapse = " "))
}

#' Extract curve segment between two points
#' 
#' @param full_path Full SVG path string
#' @param start_y Y-coordinate to start extraction
#' @param end_y Y-coordinate to end extraction
#' @return Extracted path segment
#' @export
extract_vertical_segment <- function(full_path, start_y, end_y) {
  segments <- parse_svg_path(full_path)
  
  # Find segments between the y-coordinates
  extracted <- list()
  in_range <- FALSE
  
  for (i in seq_along(segments)) {
    seg <- segments[[i]]
    
    if (seg$type == "M" || seg$type == "L" || seg$type == "C") {
      # Check if we're entering the range
      if (!in_range && abs(seg$y - start_y) < 1) {
        in_range <- TRUE
      }
      
      # Add segment if in range
      if (in_range && seg$type == "C") {
        extracted[[length(extracted) + 1]] <- seg
      }
      
      # Check if we're leaving the range
      if (in_range && abs(seg$y - end_y) < 1) {
        break
      }
    }
  }
  
  return(extracted)
}

#' Extract all coordinates from an SVG path string (optimized)
#'
#' Parses the SVG path and extracts X/Y coordinates from all segment types.
#' Handles M, L, C, A, and Z commands correctly.
#'
#' @param path SVG path d attribute string
#' @return List with x and y numeric vectors
#' @keywords internal
extract_path_coords <- function(path) {
  # Use parse_svg_path to correctly handle all SVG commands
  # (especially Arc which has 7 parameters, not 2)
  segments <- tryCatch(
    parse_svg_path(path),
    error = function(e) NULL
  )

  if (is.null(segments) || length(segments) == 0) {
    return(list(x = numeric(0), y = numeric(0)))
  }

  x_coords <- c()
  y_coords <- c()

  for (seg in segments) {
    if (seg$type %in% c("M", "L")) {
      x_coords <- c(x_coords, seg$x)
      y_coords <- c(y_coords, seg$y)
    } else if (seg$type == "C") {
      # Bezier: include control points for bounds accuracy
      x_coords <- c(x_coords, seg$cp1x, seg$cp2x, seg$x)
      y_coords <- c(y_coords, seg$cp1y, seg$cp2y, seg$y)
    } else if (seg$type == "A") {
      # Arc: only the endpoint is a coordinate
      # (rx, ry, rotation, flags are not coordinates)
      x_coords <- c(x_coords, seg$x)
      y_coords <- c(y_coords, seg$y)
    }
    # Z (close path) has no coordinates
  }

  list(x = x_coords, y = y_coords)
}


#' Extract all coordinates from multiple pieces efficiently (O(n) instead of O(n²))
#'
#' Uses the list + unlist pattern to avoid grow-on-append O(n²) behavior.
#'
#' @param pieces List of piece objects with $path fields
#' @return List with all_x and all_y numeric vectors
#' @keywords internal
extract_all_piece_coords <- function(pieces) {
  # Extract coords from each piece into a list (O(n))
  coords_list <- lapply(pieces, function(piece) extract_path_coords(piece$path))

  # Combine all coordinates with single unlist call (O(n))
  list(
    all_x = unlist(lapply(coords_list, `[[`, "x"), use.names = FALSE),
    all_y = unlist(lapply(coords_list, `[[`, "y"), use.names = FALSE)
  )
}


#' Calculate bounding box from pieces efficiently
#'
#' Extracts coordinates from all pieces and calculates min/max bounds.
#' Uses optimized O(n) extraction instead of grow-on-append.
#'
#' @param pieces List of piece objects with $path fields
#' @param fallback_fn Function to call if no coordinates found, returns list(min_x, max_x, min_y, max_y)
#' @return List with min_x, max_x, min_y, max_y
#' @keywords internal
calculate_pieces_bounds <- function(pieces, fallback_fn = NULL) {
  coords <- extract_all_piece_coords(pieces)

  if (length(coords$all_x) > 0 && length(coords$all_y) > 0) {
    list(
      min_x = min(coords$all_x),
      max_x = max(coords$all_x),
      min_y = min(coords$all_y),
      max_y = max(coords$all_y)
    )
  } else if (!is.null(fallback_fn)) {
    fallback_fn()
  } else {
    list(min_x = 0, max_x = 100, min_y = 0, max_y = 100)
  }
}


#' Create complementary edge for adjacent puzzle piece
#'
#' Takes an edge and creates its complement for the adjacent piece
#'
#' @param edge_segments Parsed edge segments
#' @param direction "horizontal" or "vertical"
#' @param reverse Whether to reverse the path direction
#' @return Complementary edge path string
#' @export
create_complementary_edge <- function(edge_segments, direction = "horizontal", reverse = TRUE) {
  if (length(edge_segments) == 0) {
    return("")
  }
  
  # Determine flip axis based on direction
  flip_axis <- ifelse(direction == "horizontal", "y", "x")
  center <- 100  # For 200x200 puzzle
  
  # First flip the coordinates
  flipped <- flip_path_segments(edge_segments, flip_axis, center)
  
  # Then reverse if needed
  if (reverse) {
    # Parse the flipped path to reverse it
    flipped_segments <- parse_svg_path(paste("M 0 0", flipped))[-1]  # Remove dummy M command
    
    # Determine start point for reversal
    last_seg <- edge_segments[[length(edge_segments)]]
    if (flip_axis == "x") {
      start_point <- c(2*center - last_seg$x, last_seg$y)
    } else {
      start_point <- c(last_seg$x, 2*center - last_seg$y)
    }
    
    return(reverse_path_segments(flipped_segments, start_point))
  }
  
  return(flipped)
}


# =============================================================================
# ggpuzzle Support Functions (for ggplot2 integration)
# =============================================================================

#' Convert cubic Bezier curve to line segments
#'
#' Uses De Casteljau's algorithm to approximate a cubic Bezier curve
#' as a series of line segments suitable for polygon rendering.
#'
#' @param p0 Start point as c(x, y)
#' @param cp1 First control point as c(x, y)
#' @param cp2 Second control point as c(x, y)
#' @param p1 End point as c(x, y)
#' @param n_points Number of points to generate (default 20)
#' @return Data frame with x and y columns
#' @export
#' @examples
#' # Simple S-curve
#' pts <- bezier_to_points(c(0, 0), c(0, 1), c(1, 0), c(1, 1))
#' plot(pts$x, pts$y, type = "l")
bezier_to_points <- function(p0, cp1, cp2, p1, n_points = 20) {
  t <- seq(0, 1, length.out = n_points)


  # Cubic Bezier formula: B(t) = (1-t)³P₀ + 3(1-t)²tCP₁ + 3(1-t)t²CP₂ + t³P₁
  one_minus_t <- 1 - t
  one_minus_t_sq <- one_minus_t^2
  one_minus_t_cu <- one_minus_t^3
  t_sq <- t^2
  t_cu <- t^3

  x <- one_minus_t_cu * p0[1] +
       3 * one_minus_t_sq * t * cp1[1] +
       3 * one_minus_t * t_sq * cp2[1] +
       t_cu * p1[1]

  y <- one_minus_t_cu * p0[2] +
       3 * one_minus_t_sq * t * cp1[2] +
       3 * one_minus_t * t_sq * cp2[2] +
       t_cu * p1[2]

  data.frame(x = x, y = y)
}


#' Convert SVG path to polygon coordinates
#'
#' Parses an SVG path string and converts it to x/y coordinates suitable
#' for rendering with ggplot2's geom_polygon or grid's polygonGrob.
#' Bezier curves are approximated as line segments.
#'
#' @param path SVG path string (d attribute)
#' @param bezier_resolution Number of points per Bezier curve (default 20)
#' @return Data frame with x and y columns
#' @export
#' @examples
#' # Simple rectangle
#' path <- "M 0 0 L 100 0 L 100 100 L 0 100 Z"
#' poly <- svg_path_to_polygon(path)
#' plot(poly$x, poly$y, type = "l")
svg_path_to_polygon <- function(path, bezier_resolution = 20) {
  segments <- parse_svg_path(path)

  if (length(segments) == 0) {
    return(data.frame(x = numeric(0), y = numeric(0)))
  }

  coords <- list()
  current_x <- 0
  current_y <- 0

  for (seg in segments) {
    if (seg$type == "M") {
      # Move to - start point
      coords[[length(coords) + 1]] <- data.frame(x = seg$x, y = seg$y)
      current_x <- seg$x
      current_y <- seg$y

    } else if (seg$type == "L") {
      # Line to
      coords[[length(coords) + 1]] <- data.frame(x = seg$x, y = seg$y)
      current_x <- seg$x
      current_y <- seg$y

    } else if (seg$type == "C") {
      # Cubic Bezier curve - approximate as line segments
      bez_pts <- bezier_to_points(
        c(current_x, current_y),
        c(seg$cp1x, seg$cp1y),
        c(seg$cp2x, seg$cp2y),
        c(seg$x, seg$y),
        bezier_resolution
      )
      # Skip first point (duplicate of current position)
      coords[[length(coords) + 1]] <- bez_pts[-1, , drop = FALSE]
      current_x <- seg$x
      current_y <- seg$y

    } else if (seg$type == "A") {
      # Arc - for simplicity, just add endpoint
      # (full arc approximation could be added later)
      coords[[length(coords) + 1]] <- data.frame(x = seg$x, y = seg$y)
      current_x <- seg$x
      current_y <- seg$y
    }
    # Z (close path) - no coordinates needed, polygon auto-closes
  }

  if (length(coords) == 0) {
    return(data.frame(x = numeric(0), y = numeric(0)))
  }

  do.call(rbind, coords)
}