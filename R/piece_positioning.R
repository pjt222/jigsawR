# Piece Positioning Engine
# Part of Epic #32 - Unified Puzzle Generation Pipeline
# Applies separation transforms to piece positions based on offset parameter

#' Apply offset/separation to piece positions
#'
#' Transforms piece positions based on offset parameter.
#' offset=0 returns pieces unchanged, offset>0 separates them.
#'
#' @param piece_result Output from generate_pieces_internal()
#' @param offset Separation distance (0 = no change, >0 = separated)
#' @return List with:
#'   - pieces: Transformed piece objects
#'   - canvas_size: c(width, height) for the new layout
#'   - canvas_offset: c(x, y) viewBox offset
#'   - offset: The offset value used
#' @export
apply_piece_positioning <- function(piece_result, offset = 0) {

  if (offset == 0) {
    # No transformation needed - return as-is with consistent structure
    return(list(
      pieces = piece_result$pieces,
      canvas_size = piece_result$canvas_size,
      canvas_offset = if (!is.null(piece_result$canvas_offset)) {
        piece_result$canvas_offset
      } else {
        c(0, 0)
      },
      offset = 0,
      type = piece_result$type,
      parameters = piece_result$parameters
    ))
  }

  # Apply type-specific positioning
  if (piece_result$type == "concentric") {
    return(apply_concentric_positioning(piece_result, offset))
  } else if (piece_result$type == "hexagonal") {
    return(apply_hex_positioning(piece_result, offset))
  } else {
    return(apply_rect_positioning(piece_result, offset))
  }
}


#' Apply rectangular piece positioning with offset
#'
#' @param piece_result Output from generate_pieces_internal() for rectangular
#' @param offset Separation distance in mm
#' @return Positioned result
#' @keywords internal
apply_rect_positioning <- function(piece_result, offset) {

  params <- piece_result$parameters
  piece_width <- params$piece_width
  piece_height <- params$piece_height
  xn <- params$grid[2]  # columns
  yn <- params$grid[1]  # rows

  # Transform each piece
  transformed_pieces <- lapply(piece_result$pieces, function(piece) {
    xi <- piece$grid_pos["xi"]
    yi <- piece$grid_pos["yi"]

    # Calculate offset for this piece
    dx <- xi * offset
    dy <- yi * offset

    # Apply translation to path
    new_path <- translate_svg_path(piece$path, dx, dy)

    # Update center
    new_center <- piece$center + c(dx, dy)

    # Return transformed piece
    list(
      id = piece$id,
      path = new_path,
      center = new_center,
      grid_pos = piece$grid_pos,
      type = piece$type
    )
  })

  # Calculate new canvas size
  # Original size + (n-1) gaps
  original_width <- params$size[1]
  original_height <- params$size[2]

  new_width <- original_width + (xn - 1) * offset
  new_height <- original_height + (yn - 1) * offset

  # Add padding for visual clarity
  padding <- offset
  canvas_width <- new_width + 2 * padding
  canvas_height <- new_height + 2 * padding

  return(list(
    pieces = transformed_pieces,
    canvas_size = c(canvas_width, canvas_height),
    canvas_offset = c(-padding, -padding),
    offset = offset,
    type = "rectangular",
    parameters = params
  ))
}


#' Apply concentric piece positioning with offset
#'
#' Uses radial separation for concentric ring pieces.
#'
#' @param piece_result Output from generate_pieces_internal() for concentric
#' @param offset Separation distance (multiplier for base spacing)
#' @return Positioned result
#' @keywords internal
apply_concentric_positioning <- function(piece_result, offset) {

  params <- piece_result$parameters
  rings <- params$rings
  piece_size <- params$piece_height

  # For concentric, offset acts as a separation factor
  separation_factor <- 1.0 + (offset / piece_size)

  # Transform each piece
  transformed_pieces <- lapply(piece_result$pieces, function(piece) {
    current_center <- piece$center
    new_center <- current_center * separation_factor

    dx <- new_center[1] - current_center[1]
    dy <- new_center[2] - current_center[2]

    new_path <- translate_svg_path(piece$path, dx, dy)

    list(
      id = piece$id,
      path = new_path,
      center = new_center,
      ring_pos = piece$ring_pos,
      type = piece$type
    )
  })

  # Calculate canvas size from actual transformed piece paths
  all_path_x <- c()
  all_path_y <- c()

  for (piece in transformed_pieces) {
    path <- piece$path
    numbers <- as.numeric(unlist(regmatches(path, gregexpr("-?[0-9]+\\.?[0-9]*", path))))
    numbers <- numbers[!is.na(numbers)]

    if (length(numbers) >= 2) {
      x_coords <- numbers[seq(1, length(numbers), by = 2)]
      y_coords <- numbers[seq(2, length(numbers), by = 2)]
      all_path_x <- c(all_path_x, x_coords)
      all_path_y <- c(all_path_y, y_coords)
    }
  }

  if (length(all_path_x) > 0 && length(all_path_y) > 0) {
    path_min_x <- min(all_path_x)
    path_max_x <- max(all_path_x)
    path_min_y <- min(all_path_y)
    path_max_y <- max(all_path_y)
  } else {
    all_x <- sapply(transformed_pieces, function(p) p$center[1])
    all_y <- sapply(transformed_pieces, function(p) p$center[2])
    path_min_x <- min(all_x) - piece_size
    path_max_x <- max(all_x) + piece_size
    path_min_y <- min(all_y) - piece_size
    path_max_y <- max(all_y) + piece_size
  }

  stroke_margin <- piece_size * 0.15 + offset
  min_x <- path_min_x - stroke_margin
  max_x <- path_max_x + stroke_margin
  min_y <- path_min_y - stroke_margin
  max_y <- path_max_y + stroke_margin

  canvas_width <- max_x - min_x
  canvas_height <- max_y - min_y

  return(list(
    pieces = transformed_pieces,
    canvas_size = c(canvas_width, canvas_height),
    canvas_offset = c(min_x, min_y),
    offset = offset,
    separation_factor = separation_factor,
    type = "concentric",
    parameters = params
  ))
}


#' Apply hexagonal piece positioning with offset
#'
#' Uses topology-based separation for hexagonal pieces.
#'
#' @param piece_result Output from generate_pieces_internal() for hexagonal
#' @param offset Separation distance (multiplier for base spacing)
#' @return Positioned result
#' @keywords internal
apply_hex_positioning <- function(piece_result, offset) {

  params <- piece_result$parameters
  rings <- params$rings

  # Handle both regular hexagonal (piece_radius) and concentric (piece_height) modes
  if (!is.null(params$piece_radius)) {
    piece_size <- params$piece_radius
  } else if (!is.null(params$piece_height)) {
    piece_size <- params$piece_height
  } else {
    # Fallback: calculate from diameter and rings
    diameter <- params$diameter
    piece_size <- diameter / (4 * rings - 2)
  }

  # For hexagonal, offset acts as a separation factor
  # Base separation is determined by piece_size
  # separation_factor = 1.0 + offset/piece_size gives proportional separation
  separation_factor <- 1.0 + (offset / piece_size)

  # Transform each piece
  transformed_pieces <- lapply(piece_result$pieces, function(piece) {
    # Get the piece's current center (at compact position)
    current_center <- piece$center

    # Calculate new center with separation factor
    # The center is already relative to origin (0,0)
    # Just scale it by separation factor
    new_center <- current_center * separation_factor

    # Calculate translation needed
    dx <- new_center[1] - current_center[1]
    dy <- new_center[2] - current_center[2]

    # Apply translation to path
    new_path <- translate_svg_path(piece$path, dx, dy)

    # Return transformed piece
    list(
      id = piece$id,
      path = new_path,
      center = new_center,
      ring_pos = piece$ring_pos,
      type = piece$type
    )
  })

  # Calculate canvas size from actual transformed piece paths
  # This is critical when warp/trunc are enabled
  all_path_x <- c()
  all_path_y <- c()

  for (piece in transformed_pieces) {
    path <- piece$path
    numbers <- as.numeric(unlist(regmatches(path, gregexpr("-?[0-9]+\\.?[0-9]*", path))))
    numbers <- numbers[!is.na(numbers)]

    if (length(numbers) >= 2) {
      x_coords <- numbers[seq(1, length(numbers), by = 2)]
      y_coords <- numbers[seq(2, length(numbers), by = 2)]
      all_path_x <- c(all_path_x, x_coords)
      all_path_y <- c(all_path_y, y_coords)
    }
  }

  if (length(all_path_x) > 0 && length(all_path_y) > 0) {
    path_min_x <- min(all_path_x)
    path_max_x <- max(all_path_x)
    path_min_y <- min(all_path_y)
    path_max_y <- max(all_path_y)
  } else {
    # Fallback to center-based calculation
    all_x <- sapply(transformed_pieces, function(p) p$center[1])
    all_y <- sapply(transformed_pieces, function(p) p$center[2])
    path_min_x <- min(all_x) - piece_size
    path_max_x <- max(all_x) + piece_size
    path_min_y <- min(all_y) - piece_size
    path_max_y <- max(all_y) + piece_size
  }

  # Add margin for stroke width and offset
  stroke_margin <- piece_size * 0.15 + offset
  min_x <- path_min_x - stroke_margin
  max_x <- path_max_x + stroke_margin
  min_y <- path_min_y - stroke_margin
  max_y <- path_max_y + stroke_margin

  canvas_width <- max_x - min_x
  canvas_height <- max_y - min_y

  return(list(
    pieces = transformed_pieces,
    canvas_size = c(canvas_width, canvas_height),
    canvas_offset = c(min_x, min_y),
    offset = offset,
    separation_factor = separation_factor,
    type = "hexagonal",
    parameters = params
  ))
}


#' Translate SVG path coordinates
#'
#' Applies (dx, dy) translation to all coordinates in an SVG path.
#' Handles M, L, C, A, and Z commands.
#'
#' @param path_string SVG path d attribute string
#' @param dx X translation
#' @param dy Y translation
#' @return Translated SVG path string
#' @export
translate_svg_path <- function(path_string, dx, dy) {
  if (dx == 0 && dy == 0) {
    return(path_string)
  }

  # Split path into tokens, preserving command letters
  # Use a regex that matches commands and numbers
  tokens <- unlist(strsplit(path_string, "(?<=\\s)|(?=\\s)|(?<=[MLCAZ])|(?=[MLCAZ])",
                            perl = TRUE))
  tokens <- tokens[tokens != "" & tokens != " "]

  result <- character()
  i <- 1

  while (i <= length(tokens)) {
    token <- trimws(tokens[i])

    if (token == "M" || token == "L") {
      # Move or Line: x y
      result <- c(result, token)
      if (i + 2 <= length(tokens)) {
        x <- as.numeric(tokens[i + 1]) + dx
        y <- as.numeric(tokens[i + 2]) + dy
        result <- c(result, sprintf("%.2f", x), sprintf("%.2f", y))
        i <- i + 3
      } else {
        i <- i + 1
      }

    } else if (token == "C") {
      # Cubic Bezier: x1 y1 x2 y2 x3 y3
      result <- c(result, token)
      if (i + 6 <= length(tokens)) {
        for (j in 0:2) {
          x <- as.numeric(tokens[i + 1 + j*2]) + dx
          y <- as.numeric(tokens[i + 2 + j*2]) + dy
          result <- c(result, sprintf("%.2f", x), sprintf("%.2f", y))
        }
        i <- i + 7
      } else {
        i <- i + 1
      }

    } else if (token == "A") {
      # Arc: rx ry x-rotation large-arc sweep x y
      # Only x,y (last two) are coordinates
      result <- c(result, token)
      if (i + 7 <= length(tokens)) {
        # Copy rx, ry, rotation, large-arc, sweep unchanged
        result <- c(result,
                    tokens[i + 1],  # rx
                    tokens[i + 2],  # ry
                    tokens[i + 3],  # x-rotation
                    tokens[i + 4],  # large-arc
                    tokens[i + 5])  # sweep
        # Translate x, y
        x <- as.numeric(tokens[i + 6]) + dx
        y <- as.numeric(tokens[i + 7]) + dy
        result <- c(result, sprintf("%.2f", x), sprintf("%.2f", y))
        i <- i + 8
      } else {
        i <- i + 1
      }

    } else if (token == "Z") {
      # Close path: no coordinates
      result <- c(result, token)
      i <- i + 1

    } else {
      # Unknown token or continuation - keep as-is
      result <- c(result, token)
      i <- i + 1
    }
  }

  return(paste(result, collapse = " "))
}


#' Calculate canvas size for compact layout
#'
#' @param piece_result Output from generate_pieces_internal()
#' @return c(width, height)
#' @keywords internal
calculate_compact_canvas <- function(piece_result) {
  piece_result$canvas_size
}


#' Calculate canvas size for separated layout
#'
#' @param positioned Output from apply_piece_positioning()
#' @return c(width, height)
#' @keywords internal
calculate_separated_canvas <- function(positioned) {
  positioned$canvas_size
}
