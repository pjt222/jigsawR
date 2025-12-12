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
#' Uses regex to extract all numeric coordinates from an SVG path.
#' Returns a list with x and y coordinate vectors.
#'
#' @param path SVG path d attribute string
#' @return List with x and y numeric vectors
#' @keywords internal
extract_path_coords <- function(path) {
  numbers <- as.numeric(unlist(regmatches(path, gregexpr("-?[0-9]+\\.?[0-9]*", path))))
  numbers <- numbers[!is.na(numbers)]

  if (length(numbers) >= 2) {
    list(
      x = numbers[seq(1, length(numbers), by = 2)],
      y = numbers[seq(2, length(numbers), by = 2)]
    )
  } else {
    list(x = numeric(0), y = numeric(0))
  }
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