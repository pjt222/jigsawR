# Rotation Utilities for Hexagonal Puzzle Pieces
# Provides functions for rotating points, bezier curves, and SVG paths

#' Rotate a 2D point around origin
#'
#' Applies standard 2D rotation matrix transformation.
#'
#' @param x X coordinate
#' @param y Y coordinate
#' @param angle Rotation angle in radians (positive = counterclockwise)
#' @param center Center of rotation (default: origin c(0, 0))
#' @return Named list with rotated x and y coordinates
#'
#' @details
#' Uses standard rotation matrix:
#' x' = cos(θ) * x - sin(θ) * y
#' y' = sin(θ) * x + cos(θ) * y
#'
#' If center is not origin, translates point to origin, rotates, then translates back.
#'
#' @examples
#' # Rotate point (10, 0) by 60 degrees
#' rotated <- rotate_point(10, 0, pi/3)
#' # Returns: list(x = 5, y = 8.66)
#'
#' # Rotate around custom center
#' rotated <- rotate_point(10, 10, pi/2, center = c(5, 5))
#'
#' @export
rotate_point <- function(x, y, angle, center = c(0, 0)) {
  # Translate to origin if needed
  if (!isTRUE(all.equal(center, c(0, 0)))) {
    x <- x - center[1]
    y <- y - center[2]
  }

  # Apply rotation
  cos_a <- cos(angle)
  sin_a <- sin(angle)
  x_rot <- cos_a * x - sin_a * y
  y_rot <- sin_a * x + cos_a * y

  # Translate back if needed
  if (!isTRUE(all.equal(center, c(0, 0)))) {
    x_rot <- x_rot + center[1]
    y_rot <- y_rot + center[2]
  }

  return(list(x = x_rot, y = y_rot))
}

#' Rotate multiple points
#'
#' Convenience function to rotate a list of points.
#'
#' @param points List of c(x, y) coordinates
#' @param angle Rotation angle in radians
#' @param center Center of rotation (default: origin)
#' @return List of rotated points as c(x, y) vectors
#'
#' @examples
#' points <- list(c(0, 0), c(10, 0), c(10, 10))
#' rotated <- rotate_points(points, pi/4)
#'
#' @export
rotate_points <- function(points, angle, center = c(0, 0)) {
  lapply(points, function(pt) {
    result <- rotate_point(pt[1], pt[2], angle, center)
    c(result$x, result$y)
  })
}

#' Rotate bezier control points
#'
#' Rotates control points of a bezier curve while preserving curve properties.
#'
#' @param control_points List of c(x, y) control points
#' @param angle Rotation angle in radians
#' @param center Center of rotation (default: origin)
#' @return List of rotated control points
#'
#' @details
#' Bezier curves are defined by control points. Rotating all control points
#' by the same angle around the same center preserves the curve shape and
#' all its properties (continuity, smoothness, etc.).
#'
#' @examples
#' # Quadratic bezier curve
#' controls <- list(c(0, 0), c(5, 10), c(10, 0))
#' rotated <- rotate_bezier_curve(controls, pi/6)
#'
#' @export
rotate_bezier_curve <- function(control_points, angle, center = c(0, 0)) {
  rotate_points(control_points, angle, center)
}

#' Parse SVG path string to coordinate list
#'
#' Extracts numeric coordinates from SVG path commands.
#' Simplified version for basic M, L, C, Q commands.
#'
#' @param path_string SVG path string (e.g., "M 0 0 L 10 10 C 20 20 30 30 40 40")
#' @return List with commands and coordinates
#'
#' @details
#' Returns a list structure:
#' list(
#'   commands = c("M", "L", "C", ...),
#'   coords = list(c(x, y), c(x, y), ...)
#' )
#'
#' @keywords internal
parse_simple_svg_path <- function(path_string) {
  # Remove extra whitespace
  path_string <- gsub("\\s+", " ", trimws(path_string))

  # Split by commands (M, L, C, Q, Z)
  parts <- strsplit(path_string, "(?=[MLCQZ])", perl = TRUE)[[1]]
  parts <- parts[parts != ""]

  commands <- character()
  coords_list <- list()

  for (part in parts) {
    # Extract command letter
    cmd <- substr(part, 1, 1)
    commands <- c(commands, cmd)

    # Extract coordinates
    if (cmd != "Z") {
      coord_str <- trimws(substring(part, 2))
      if (coord_str != "") {
        coords <- as.numeric(strsplit(coord_str, "\\s+")[[1]])

        # Group coordinates into (x, y) pairs
        coord_pairs <- list()
        for (i in seq(1, length(coords), by = 2)) {
          if (i + 1 <= length(coords)) {
            coord_pairs[[length(coord_pairs) + 1]] <- c(coords[i], coords[i + 1])
          }
        }
        coords_list[[length(coords_list) + 1]] <- coord_pairs
      } else {
        coords_list[[length(coords_list) + 1]] <- list()
      }
    } else {
      coords_list[[length(coords_list) + 1]] <- list()
    }
  }

  return(list(commands = commands, coords = coords_list))
}

#' Rebuild SVG path from parsed structure
#'
#' @param parsed_path Result from parse_simple_svg_path
#' @return SVG path string
#'
#' @keywords internal
rebuild_svg_path <- function(parsed_path) {
  parts <- character()

  for (i in seq_along(parsed_path$commands)) {
    cmd <- parsed_path$commands[i]
    coords <- parsed_path$coords[[i]]

    if (cmd == "Z") {
      parts <- c(parts, "Z")
    } else {
      coord_str <- paste(sapply(coords, function(pt) {
        sprintf("%.2f %.2f", pt[1], pt[2])
      }), collapse = " ")
      parts <- c(parts, paste(cmd, coord_str))
    }
  }

  return(paste(parts, collapse = " "))
}

#' Rotate SVG path string
#'
#' Parses an SVG path, rotates all coordinates, and rebuilds the path.
#'
#' @param path_string SVG path string
#' @param angle Rotation angle in radians
#' @param center Center of rotation (default: origin)
#' @return Rotated SVG path string
#'
#' @details
#' This function:
#' 1. Parses the SVG path to extract coordinates
#' 2. Rotates all coordinate pairs
#' 3. Rebuilds the SVG path string with rotated coordinates
#'
#' Preserves path commands (M, L, C, Q, Z) and structure.
#'
#' @examples
#' path <- "M 0 0 L 10 0 L 10 10 Z"
#' rotated <- rotate_svg_path(path, pi/4)
#'
#' @export
rotate_svg_path <- function(path_string, angle, center = c(0, 0)) {
  # No rotation needed
  if (abs(angle) < 1e-10) {
    return(path_string)
  }

  # Parse path
  parsed <- parse_simple_svg_path(path_string)

  # Rotate all coordinates
  for (i in seq_along(parsed$coords)) {
    if (length(parsed$coords[[i]]) > 0) {
      parsed$coords[[i]] <- rotate_points(parsed$coords[[i]], angle, center)
    }
  }

  # Rebuild path
  return(rebuild_svg_path(parsed))
}

#' Create rotation transformation function
#'
#' Returns a function that applies rotation to points.
#' Compatible with hexagonal_puzzle.R's hex_rotate pattern.
#'
#' @param angle Rotation angle in radians
#' @return Function that takes c(x, y) and returns rotated c(x, y)
#'
#' @examples
#' rotate_fn <- create_rotation_fn(pi/3)
#' rotated_point <- rotate_fn(c(10, 0))
#'
#' @export
create_rotation_fn <- function(angle) {
  function(pos) {
    x <- pos[1]
    y <- pos[2]
    c(
      cos(angle) * x - sin(angle) * y,
      sin(angle) * x + cos(angle) * y
    )
  }
}

#' Test if two rotation angles are equivalent
#'
#' Compares rotation angles accounting for 2π periodicity.
#'
#' @param angle1 First angle in radians
#' @param angle2 Second angle in radians
#' @param tolerance Numeric tolerance (default: 1e-6)
#' @return Logical TRUE if angles are equivalent
#'
#' @examples
#' angles_equal(0, 2*pi)  # TRUE
#' angles_equal(pi/3, pi/3 + 2*pi)  # TRUE
#'
#' @export
angles_equal <- function(angle1, angle2, tolerance = 1e-6) {
  # Normalize to [0, 2π)
  norm1 <- angle1 %% (2 * pi)
  norm2 <- angle2 %% (2 * pi)

  abs(norm1 - norm2) < tolerance
}
