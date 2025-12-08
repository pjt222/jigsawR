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

  # Build fusion group position mapping
  # Each piece gets an "effective" grid position for offset calculation
  # Fused pieces share the same effective position (min of group)
  effective_positions <- build_effective_positions_rect(
    piece_result$pieces,
    piece_result$fusion_data,
    params$grid
  )

  # Transform each piece
  transformed_pieces <- lapply(seq_along(piece_result$pieces), function(i) {
    piece <- piece_result$pieces[[i]]
    xi <- piece$grid_pos["xi"]
    yi <- piece$grid_pos["yi"]

    # Use effective position for offset calculation (handles fusion)
    eff_pos <- effective_positions[[i]]
    dx <- eff_pos$xi * offset
    dy <- eff_pos$yi * offset

    # Apply translation to path
    new_path <- translate_svg_path(piece$path, dx, dy)

    # Update center
    new_center <- piece$center + c(dx, dy)

    # Return transformed piece (preserve all metadata including fused_edges)
    list(
      id = piece$id,
      path = new_path,
      center = new_center,
      grid_pos = piece$grid_pos,
      type = piece$type,
      fusion_group = if (!is.null(piece$fusion_group)) piece$fusion_group else NA,
      fused_edges = piece$fused_edges  # Preserve fusion edge metadata for rendering
    )
  })

  # Calculate new canvas size
  # For fusion: count unique effective positions, not individual pieces
  # This gives correct gaps when meta-pieces span multiple grid cells
  unique_eff_xi <- unique(sapply(effective_positions, function(p) p$xi))
  unique_eff_yi <- unique(sapply(effective_positions, function(p) p$yi))
  n_gaps_x <- length(unique_eff_xi) - 1
  n_gaps_y <- length(unique_eff_yi) - 1

  original_width <- params$size[1]
  original_height <- params$size[2]

  new_width <- original_width + n_gaps_x * offset
  new_height <- original_height + n_gaps_y * offset

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
    parameters = params,
    fusion_data = piece_result$fusion_data
  ))
}

#' Build effective positions for offset calculation with fusion
#'
#' For non-fused pieces, returns their grid position.
#' For fused pieces, returns the min grid position of their group.
#'
#' @param pieces List of piece objects
#' @param fusion_data Fusion data from compute_fused_edges() or NULL
#' @param grid c(rows, cols)
#' @return List of effective positions (one per piece)
#' @keywords internal
build_effective_positions_rect <- function(pieces, fusion_data, grid) {

  n_pieces <- length(pieces)
  effective_positions <- vector("list", n_pieces)

  if (is.null(fusion_data) || is.null(fusion_data$piece_to_group)) {
    # No fusion - each piece uses its own position
    for (i in seq_len(n_pieces)) {
      xi <- pieces[[i]]$grid_pos["xi"]
      yi <- pieces[[i]]$grid_pos["yi"]
      effective_positions[[i]] <- list(xi = xi, yi = yi)
    }
    return(effective_positions)
  }

  # With fusion - compute min position for each group
  piece_to_group <- fusion_data$piece_to_group

  # First pass: collect all positions per group
  group_positions <- list()
  for (i in seq_len(n_pieces)) {
    group_id <- piece_to_group[[as.character(i)]]
    xi <- pieces[[i]]$grid_pos["xi"]
    yi <- pieces[[i]]$grid_pos["yi"]

    if (!is.null(group_id)) {
      key <- as.character(group_id)
      if (is.null(group_positions[[key]])) {
        group_positions[[key]] <- list(xis = c(), yis = c())
      }
      group_positions[[key]]$xis <- c(group_positions[[key]]$xis, xi)
      group_positions[[key]]$yis <- c(group_positions[[key]]$yis, yi)
    }
  }

  # Compute min position for each group
  group_min_pos <- list()
  for (key in names(group_positions)) {
    group_min_pos[[key]] <- list(
      xi = min(group_positions[[key]]$xis),
      yi = min(group_positions[[key]]$yis)
    )
  }

  # Second pass: assign effective positions
  for (i in seq_len(n_pieces)) {
    group_id <- piece_to_group[[as.character(i)]]
    xi <- pieces[[i]]$grid_pos["xi"]
    yi <- pieces[[i]]$grid_pos["yi"]

    if (!is.null(group_id)) {
      # Fused piece: use group's min position
      key <- as.character(group_id)
      effective_positions[[i]] <- group_min_pos[[key]]
    } else {
      # Non-fused piece: use own position
      effective_positions[[i]] <- list(xi = xi, yi = yi)
    }
  }

  return(effective_positions)
}


#' Apply concentric piece positioning with offset
#'
#' Uses radial separation for concentric ring pieces.
#' Handles fusion groups by keeping fused pieces together.
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

  # Build effective centers for fusion groups
  # Fused pieces share the same effective center (group centroid)
  effective_centers <- build_effective_centers_radial(
    piece_result$pieces,
    piece_result$fusion_data
  )

  # Transform each piece
  transformed_pieces <- lapply(seq_along(piece_result$pieces), function(i) {
    piece <- piece_result$pieces[[i]]
    current_center <- piece$center

    # Use effective center for offset calculation (handles fusion)
    eff_center <- effective_centers[[i]]

    # Calculate new position based on effective center
    new_eff_center <- eff_center * separation_factor

    # Translation is based on effective center movement
    dx <- new_eff_center[1] - eff_center[1]
    dy <- new_eff_center[2] - eff_center[2]

    new_path <- translate_svg_path(piece$path, dx, dy)

    # Update actual center with same translation
    new_center <- current_center + c(dx, dy)

    # Return transformed piece (preserve all metadata including fusion fields)
    list(
      id = piece$id,
      path = new_path,
      center = new_center,
      ring_pos = piece$ring_pos,
      type = piece$type,
      fusion_group = if (!is.null(piece$fusion_group)) piece$fusion_group else NA,
      fused_edges = piece$fused_edges  # Preserve fusion edge metadata for rendering
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
    parameters = params,
    fusion_data = piece_result$fusion_data
  ))
}


#' Build effective centers for radial piece positioning with fusion
#'
#' For non-fused pieces, returns their own center.
#' For fused pieces, returns the centroid of their group.
#' This ensures all pieces in a fusion group get the same translation.
#'
#' @param pieces List of piece objects with center coordinates
#' @param fusion_data Fusion data from compute_*_fused_edges() or NULL
#' @return List of effective centers (one per piece, each a c(x, y) vector)
#' @keywords internal
build_effective_centers_radial <- function(pieces, fusion_data) {

  n_pieces <- length(pieces)
  effective_centers <- vector("list", n_pieces)

  if (is.null(fusion_data) || is.null(fusion_data$piece_to_group)) {
    # No fusion - each piece uses its own center
    for (i in seq_len(n_pieces)) {
      effective_centers[[i]] <- pieces[[i]]$center
    }
    return(effective_centers)
  }

  # With fusion - compute centroid for each group
  piece_to_group <- fusion_data$piece_to_group

  # First pass: collect all centers per group
  group_centers <- list()
  for (i in seq_len(n_pieces)) {
    group_id <- piece_to_group[[as.character(i)]]
    center <- pieces[[i]]$center

    if (!is.null(group_id)) {
      key <- as.character(group_id)
      if (is.null(group_centers[[key]])) {
        group_centers[[key]] <- list(xs = c(), ys = c())
      }
      group_centers[[key]]$xs <- c(group_centers[[key]]$xs, center[1])
      group_centers[[key]]$ys <- c(group_centers[[key]]$ys, center[2])
    }
  }

  # Compute centroid for each group
  group_centroid <- list()
  for (key in names(group_centers)) {
    group_centroid[[key]] <- c(
      mean(group_centers[[key]]$xs),
      mean(group_centers[[key]]$ys)
    )
  }

  # Second pass: assign effective centers
  for (i in seq_len(n_pieces)) {
    group_id <- piece_to_group[[as.character(i)]]

    if (!is.null(group_id)) {
      # Fused piece: use group's centroid
      key <- as.character(group_id)
      effective_centers[[i]] <- group_centroid[[key]]
    } else {
      # Non-fused piece: use own center
      effective_centers[[i]] <- pieces[[i]]$center
    }
  }

  return(effective_centers)
}


#' Apply hexagonal piece positioning with offset
#'
#' Uses topology-based separation for hexagonal pieces.
#' Handles fusion groups by keeping fused pieces together.
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

  # Build effective centers for fusion groups
  # Fused pieces share the same effective center (group centroid)
  effective_centers <- build_effective_centers_radial(
    piece_result$pieces,
    piece_result$fusion_data
  )

  # Transform each piece
  transformed_pieces <- lapply(seq_along(piece_result$pieces), function(i) {
    piece <- piece_result$pieces[[i]]

    # Get the piece's current center (at compact position)
    current_center <- piece$center

    # Use effective center for offset calculation (handles fusion)
    eff_center <- effective_centers[[i]]

    # Calculate new position based on effective center
    # Effective center determines offset, but piece keeps its relative position
    new_eff_center <- eff_center * separation_factor

    # Translation is based on effective center movement
    dx <- new_eff_center[1] - eff_center[1]
    dy <- new_eff_center[2] - eff_center[2]

    # Apply translation to path
    new_path <- translate_svg_path(piece$path, dx, dy)

    # Update actual center with same translation
    new_center <- current_center + c(dx, dy)

    # Return transformed piece (preserve all metadata including fusion fields)
    list(
      id = piece$id,
      path = new_path,
      center = new_center,
      ring_pos = piece$ring_pos,
      type = piece$type,
      fusion_group = if (!is.null(piece$fusion_group)) piece$fusion_group else NA,
      fused_edges = piece$fused_edges  # Preserve fusion edge metadata for rendering
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
    parameters = params,
    fusion_data = piece_result$fusion_data
  ))
}


#' Translate SVG path coordinates
#'
#' Applies (dx, dy) translation to all coordinates in an SVG path.
#' Handles M, L, C, A, and Z commands.
#'
#' For arc commands (A), only the endpoint is translated while radii stay constant.
#' This correctly preserves arc shape because the SVG renderer calculates the
#' arc center from endpoints + radii - when endpoints move by (dx, dy), the
#' calculated center also moves by (dx, dy), producing an identical arc shape.
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
      #
      # Arc translation IS correct: translate endpoints, keep radii constant.
      # The SVG renderer calculates the arc center from endpoints + radii.
      # When both endpoints translate by (dx, dy) with constant radii, the
      # renderer calculates the center also moved by (dx, dy).
      #
      # Math proof: |P₁' - C₂| = |(P₁ + Δ) - (C₁ + Δ)| = |P₁ - C₁| = r
      # The arc shape is preserved - like moving a slice of pie outward.
      result <- c(result, token)
      if (i + 7 <= length(tokens)) {
        # Keep rx, ry, rotation, large-arc, sweep unchanged
        result <- c(result,
                    tokens[i + 1],  # rx
                    tokens[i + 2],  # ry
                    tokens[i + 3],  # x-rotation
                    tokens[i + 4],  # large-arc
                    tokens[i + 5])  # sweep
        # Translate only the endpoint (x, y)
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
