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
#' @param layout Layout algorithm: "grid" (default) or "repel".
#'   "grid" uses regular grid-based positioning.
#'   "repel" uses iterative collision resolution to prevent overlapping.
#' @param repel_margin Minimum gap between pieces for repel layout (default: 2)
#' @param repel_max_iter Maximum iterations for repel algorithm (default: 100)
#' @return List with:
#'   - pieces: Transformed piece objects
#'   - canvas_size: c(width, height) for the new layout
#'   - canvas_offset: c(x, y) viewBox offset
#'   - offset: The offset value used
#' @export
apply_piece_positioning <- function(piece_result, offset = 0, layout = "grid",
                                     repel_margin = 2, repel_max_iter = 100) {

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
    positioned <- apply_concentric_positioning(piece_result, offset)
  } else if (piece_result$type == "hexagonal") {
    positioned <- apply_hex_positioning(piece_result, offset)
  } else {
    positioned <- apply_rect_positioning(piece_result, offset)
  }

  # Apply repel layout if requested
  if (layout == "repel") {
    positioned <- apply_repel_layout(
      positioned,
      margin = repel_margin,
      max_iterations = repel_max_iter
    )
  }

  positioned
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
      fused_edges = piece$fused_edges,  # Preserve fusion edge metadata for rendering
      fused_neighbor_ids = piece$fused_neighbor_ids  # Preserve neighbor IDs for deduplication
    )
  })

  # Calculate canvas size from actual transformed piece paths
  # This ensures all bezier tabs and strokes are visible (fixes clipping issue)
  piece_width <- params$piece_width
  piece_height <- params$piece_height

  bounds <- calculate_pieces_bounds(transformed_pieces, fallback_fn = function() {
    # Fallback to theoretical calculation
    unique_eff_xi <- unique(sapply(effective_positions, function(p) p$xi))
    unique_eff_yi <- unique(sapply(effective_positions, function(p) p$yi))
    n_gaps_x <- length(unique_eff_xi) - 1
    n_gaps_y <- length(unique_eff_yi) - 1
    original_width <- params$size[1]
    original_height <- params$size[2]
    list(
      min_x = -offset,
      max_x = original_width + n_gaps_x * offset + offset,
      min_y = -offset,
      max_y = original_height + n_gaps_y * offset + offset
    )
  })

  # Add margin for stroke width and visual clarity
  # Use larger of piece dimensions to estimate tab protrusion
  stroke_margin <- max(piece_width, piece_height) * 0.1 + offset
  min_x <- bounds$min_x - stroke_margin
  max_x <- bounds$max_x + stroke_margin
  min_y <- bounds$min_y - stroke_margin
  max_y <- bounds$max_y + stroke_margin

  canvas_width <- max_x - min_x
  canvas_height <- max_y - min_y

  return(list(
    pieces = transformed_pieces,
    canvas_size = c(canvas_width, canvas_height),
    canvas_offset = c(min_x, min_y),
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

    # Translate segment-level edge paths if present
    translated_segments <- piece$fused_edge_segments
    if (!is.null(translated_segments) && !is.null(translated_segments$OUTER)) {
      for (seg_idx in seq_along(translated_segments$OUTER)) {
        seg <- translated_segments$OUTER[[seg_idx]]
        # Translate start_point and end_point
        if (!is.null(seg$start_point)) {
          translated_segments$OUTER[[seg_idx]]$start_point <- seg$start_point + c(dx, dy)
        }
        if (!is.null(seg$end_point)) {
          translated_segments$OUTER[[seg_idx]]$end_point <- seg$end_point + c(dx, dy)
        }
        # Translate the bezier path
        if (!is.null(seg$path)) {
          translated_segments$OUTER[[seg_idx]]$path <- translate_svg_path(seg$path, dx, dy)
        }
      }
    }

    # Return transformed piece (preserve all metadata including fusion fields)
    list(
      id = piece$id,
      path = new_path,
      center = new_center,
      ring_pos = piece$ring_pos,
      type = piece$type,
      fusion_group = if (!is.null(piece$fusion_group)) piece$fusion_group else NA,
      fused_edges = piece$fused_edges,  # Preserve fusion edge metadata for rendering
      fused_neighbor_ids = piece$fused_neighbor_ids,  # Preserve neighbor IDs for deduplication
      # Segment-level fusion data for many-to-one OUTER edges
      outer_segments_mixed = piece$outer_segments_mixed,
      fused_edge_segments = translated_segments,
      inner_radius = piece$inner_radius,
      outer_radius = piece$outer_radius
    )
  })

  # Calculate canvas size from actual transformed piece paths
  # Uses optimized O(n) extraction instead of grow-on-append O(n²)
  bounds <- calculate_pieces_bounds(transformed_pieces, fallback_fn = function() {
    all_x <- sapply(transformed_pieces, function(p) p$center[1])
    all_y <- sapply(transformed_pieces, function(p) p$center[2])
    list(
      min_x = min(all_x) - piece_size,
      max_x = max(all_x) + piece_size,
      min_y = min(all_y) - piece_size,
      max_y = max(all_y) + piece_size
    )
  })
  path_min_x <- bounds$min_x
  path_max_x <- bounds$max_x
  path_min_y <- bounds$min_y
  path_max_y <- bounds$max_y

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

    # Translate segment-level edge paths if present
    translated_segments <- piece$fused_edge_segments
    if (!is.null(translated_segments) && !is.null(translated_segments$OUTER)) {
      for (seg_idx in seq_along(translated_segments$OUTER)) {
        seg <- translated_segments$OUTER[[seg_idx]]
        # Translate start_point and end_point
        if (!is.null(seg$start_point)) {
          translated_segments$OUTER[[seg_idx]]$start_point <- seg$start_point + c(dx, dy)
        }
        if (!is.null(seg$end_point)) {
          translated_segments$OUTER[[seg_idx]]$end_point <- seg$end_point + c(dx, dy)
        }
        # Translate the bezier path
        if (!is.null(seg$path)) {
          translated_segments$OUTER[[seg_idx]]$path <- translate_svg_path(seg$path, dx, dy)
        }
      }
    }

    # Return transformed piece (preserve all metadata including fusion fields)
    list(
      id = piece$id,
      path = new_path,
      center = new_center,
      ring_pos = piece$ring_pos,
      type = piece$type,
      fusion_group = if (!is.null(piece$fusion_group)) piece$fusion_group else NA,
      fused_edges = piece$fused_edges,  # Preserve fusion edge metadata for rendering
      fused_neighbor_ids = piece$fused_neighbor_ids,  # Preserve neighbor IDs for deduplication
      # Segment-level fusion data (may be set for radial puzzles)
      outer_segments_mixed = piece$outer_segments_mixed,
      fused_edge_segments = translated_segments,
      inner_radius = piece$inner_radius,
      outer_radius = piece$outer_radius
    )
  })

  # Calculate canvas size from actual transformed piece paths
  # This is critical when warp/trunc are enabled
  # Uses optimized O(n) extraction instead of grow-on-append O(n²)
  bounds <- calculate_pieces_bounds(transformed_pieces, fallback_fn = function() {
    # Fallback to center-based calculation
    all_x <- sapply(transformed_pieces, function(p) p$center[1])
    all_y <- sapply(transformed_pieces, function(p) p$center[2])
    list(
      min_x = min(all_x) - piece_size,
      max_x = max(all_x) + piece_size,
      min_y = min(all_y) - piece_size,
      max_y = max(all_y) + piece_size
    )
  })
  path_min_x <- bounds$min_x
  path_max_x <- bounds$max_x
  path_min_y <- bounds$min_y
  path_max_y <- bounds$max_y

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


# =============================================================================
# Repel Layout Algorithm (Issue #53)
# =============================================================================

#' Apply repel layout to prevent piece overlapping
#'
#' Iteratively pushes overlapping pieces apart until no collisions remain.
#' Works with individual pieces and fused meta-pieces.
#'
#' @param positioned Output from apply_piece_positioning() with offset > 0
#' @param margin Minimum gap between pieces in mm (default: 2)
#' @param max_iterations Maximum iterations before giving up (default: 100)
#' @param step_size How far to push pieces apart per iteration (default: 1.0)
#' @param compact If TRUE, pull pieces toward center after repelling (default: FALSE
#' @return Updated positioned result with non-overlapping pieces
#' @export
apply_repel_layout <- function(positioned, margin = 2, max_iterations = 100,
                                step_size = 1.0, compact = FALSE) {

  pieces <- positioned$pieces
  n_pieces <- length(pieces)

  if (n_pieces <= 1) {
    return(positioned)
  }

  # Group pieces by fusion_group for collision detection
  # Each group is treated as a single unit
  groups <- build_collision_groups(pieces)

  # Get initial bounding boxes for each group
  group_bboxes <- lapply(groups, function(g) {
    compute_group_bbox(pieces[g$piece_indices])
  })

  # Iterative repulsion
 iteration <- 0
  has_overlap <- TRUE

  while (has_overlap && iteration < max_iterations) {
    has_overlap <- FALSE
    iteration <- iteration + 1

    # Check all pairs of groups for overlap
    for (i in seq_along(groups)) {
      for (j in seq_along(groups)) {
        if (i >= j) next  # Skip self and already-checked pairs

        bbox_i <- group_bboxes[[i]]
        bbox_j <- group_bboxes[[j]]

        overlap <- compute_bbox_overlap(bbox_i, bbox_j, margin)

        if (overlap$overlaps) {
          has_overlap <- TRUE

          # Compute repulsion direction (from i to j)
          repel_vec <- compute_repulsion_vector(bbox_i, bbox_j, overlap, step_size)

          # Apply half the repulsion to each group (push apart equally)
          # Update pieces in group i (move in negative direction)
          for (idx in groups[[i]]$piece_indices) {
            pieces[[idx]] <- translate_piece(pieces[[idx]], -repel_vec[1] / 2, -repel_vec[2] / 2)
          }
          # Update pieces in group j (move in positive direction)
          for (idx in groups[[j]]$piece_indices) {
            pieces[[idx]] <- translate_piece(pieces[[idx]], repel_vec[1] / 2, repel_vec[2] / 2)
          }

          # Update bounding boxes
          group_bboxes[[i]] <- compute_group_bbox(pieces[groups[[i]]$piece_indices])
          group_bboxes[[j]] <- compute_group_bbox(pieces[groups[[j]]$piece_indices])
        }
      }
    }
  }

  # Optional compaction: pull pieces toward center
  if (compact && iteration > 0) {
    pieces <- apply_compaction(pieces, groups, group_bboxes, margin)
  }

  # Recalculate canvas size based on new positions
  bounds <- calculate_pieces_bounds(pieces, fallback_fn = function() {
    all_x <- sapply(pieces, function(p) p$center[1])
    all_y <- sapply(pieces, function(p) p$center[2])
    list(min_x = min(all_x) - 50, max_x = max(all_x) + 50,
         min_y = min(all_y) - 50, max_y = max(all_y) + 50)
  })

  padding <- margin * 2
  canvas_width <- bounds$max_x - bounds$min_x + 2 * padding
  canvas_height <- bounds$max_y - bounds$min_y + 2 * padding
  canvas_offset <- c(bounds$min_x - padding, bounds$min_y - padding)

  # Return updated result
  list(
    pieces = pieces,
    canvas_size = c(canvas_width, canvas_height),
    canvas_offset = canvas_offset,
    offset = positioned$offset,
    type = positioned$type,
    parameters = positioned$parameters,
    fusion_data = positioned$fusion_data,
    repel_iterations = iteration
  )
}


#' Build collision groups from pieces
#'
#' Groups pieces by their fusion_group. Non-fused pieces are each their own group.
#'
#' @param pieces List of piece objects
#' @return List of groups, each with piece_indices
#' @keywords internal
build_collision_groups <- function(pieces) {
  groups <- list()
  group_map <- list()  # fusion_group_id -> group index

  for (i in seq_along(pieces)) {
    piece <- pieces[[i]]
    fg <- piece$fusion_group

    if (is.na(fg) || is.null(fg)) {
      # Non-fused piece: its own group
      groups[[length(groups) + 1]] <- list(
        fusion_group = NA,
        piece_indices = i
      )
    } else {
      # Fused piece: add to existing group or create new
      key <- as.character(fg)
      if (is.null(group_map[[key]])) {
        group_map[[key]] <- length(groups) + 1
        groups[[group_map[[key]]]] <- list(
          fusion_group = fg,
          piece_indices = c()
        )
      }
      idx <- group_map[[key]]
      groups[[idx]]$piece_indices <- c(groups[[idx]]$piece_indices, i)
    }
  }

  groups
}


#' Compute bounding box for a group of pieces
#'
#' @param pieces List of piece objects
#' @return List with min_x, max_x, min_y, max_y, center_x, center_y
#' @keywords internal
compute_group_bbox <- function(pieces) {
  if (length(pieces) == 0) {
    return(list(min_x = 0, max_x = 0, min_y = 0, max_y = 0, center_x = 0, center_y = 0))
  }

  # Use parsed_segments if available, otherwise use center estimate
  all_x <- c()
  all_y <- c()

  for (piece in pieces) {
    if (!is.null(piece$parsed_segments)) {
      coords <- extract_segment_coords(piece$parsed_segments)
      all_x <- c(all_x, coords$x)
      all_y <- c(all_y, coords$y)
    } else {
      # Fallback: use center with estimated size
      cx <- piece$center[1]
      cy <- piece$center[2]
      est_size <- 30  # Rough estimate
      all_x <- c(all_x, cx - est_size, cx + est_size)
      all_y <- c(all_y, cy - est_size, cy + est_size)
    }
  }

  list(
    min_x = min(all_x),
    max_x = max(all_x),
    min_y = min(all_y),
    max_y = max(all_y),
    center_x = (min(all_x) + max(all_x)) / 2,
    center_y = (min(all_y) + max(all_y)) / 2
  )
}


#' Extract coordinates from parsed segments
#'
#' @param segments List of parsed SVG segments
#' @return List with x and y coordinate vectors
#' @keywords internal
extract_segment_coords <- function(segments) {
  x_coords <- c()
  y_coords <- c()

  for (seg in segments) {
    if (seg$type %in% c("M", "L")) {
      x_coords <- c(x_coords, seg$x)
      y_coords <- c(y_coords, seg$y)
    } else if (seg$type == "C") {
      x_coords <- c(x_coords, seg$cp1x, seg$cp2x, seg$x)
      y_coords <- c(y_coords, seg$cp1y, seg$cp2y, seg$y)
    } else if (seg$type == "A") {
      x_coords <- c(x_coords, seg$x)
      y_coords <- c(y_coords, seg$y)
    }
  }

  list(x = x_coords, y = y_coords)
}


#' Compute overlap between two bounding boxes
#'
#' @param bbox1 First bounding box
#' @param bbox2 Second bounding box
#' @param margin Required gap between boxes
#' @return List with overlaps (boolean), overlap_x, overlap_y
#' @keywords internal
compute_bbox_overlap <- function(bbox1, bbox2, margin = 0) {
  # Expand boxes by margin
  b1_min_x <- bbox1$min_x - margin
  b1_max_x <- bbox1$max_x + margin
  b1_min_y <- bbox1$min_y - margin
  b1_max_y <- bbox1$max_y + margin

  b2_min_x <- bbox2$min_x - margin
  b2_max_x <- bbox2$max_x + margin
  b2_min_y <- bbox2$min_y - margin
  b2_max_y <- bbox2$max_y + margin

  # Check for overlap
  overlap_x <- min(b1_max_x, b2_max_x) - max(b1_min_x, b2_min_x)
  overlap_y <- min(b1_max_y, b2_max_y) - max(b1_min_y, b2_min_y)

  overlaps <- overlap_x > 0 && overlap_y > 0

  list(
    overlaps = overlaps,
    overlap_x = if (overlaps) overlap_x else 0,
    overlap_y = if (overlaps) overlap_y else 0
  )
}


#' Compute repulsion vector between two overlapping boxes
#'
#' @param bbox1 First bounding box
#' @param bbox2 Second bounding box
#' @param overlap Overlap info from compute_bbox_overlap
#' @param step_size Multiplier for repulsion distance
#' @return c(dx, dy) repulsion vector (from bbox1 to bbox2)
#' @keywords internal
compute_repulsion_vector <- function(bbox1, bbox2, overlap, step_size = 1.0) {
  # Direction from center of bbox1 to center of bbox2
  dx <- bbox2$center_x - bbox1$center_x
  dy <- bbox2$center_y - bbox1$center_y

  # Normalize direction
  dist <- sqrt(dx^2 + dy^2)
  if (dist < 0.001) {
    # Centers are the same - push in arbitrary direction
    dx <- 1
    dy <- 0
    dist <- 1
  }

  dx <- dx / dist
  dy <- dy / dist

  # Push apart by the minimum overlap distance (push along axis of least overlap)
  if (overlap$overlap_x < overlap$overlap_y) {
    # Push horizontally
    push_dist <- overlap$overlap_x * step_size
    c(sign(dx) * push_dist, 0)
  } else {
    # Push vertically
    push_dist <- overlap$overlap_y * step_size
    c(0, sign(dy) * push_dist)
  }
}


#' Translate a piece by dx, dy
#'
#' @param piece Piece object
#' @param dx X translation
#' @param dy Y translation
#' @return Translated piece
#' @keywords internal
translate_piece <- function(piece, dx, dy) {
  if (abs(dx) < 0.001 && abs(dy) < 0.001) {
    return(piece)
  }

  # Update path
  piece$path <- translate_svg_path(piece$path, dx, dy)

  # Update center
  piece$center <- piece$center + c(dx, dy)

  # Update parsed_segments if present
  if (!is.null(piece$parsed_segments)) {
    piece$parsed_segments <- translate_segments(piece$parsed_segments, dx, dy)
  }

  # Update fused_edge_segments if present (critical for concentric OUTER edges)
  if (!is.null(piece$fused_edge_segments)) {
    for (edge_name in names(piece$fused_edge_segments)) {
      segments <- piece$fused_edge_segments[[edge_name]]
      if (!is.null(segments)) {
        for (seg_idx in seq_along(segments)) {
          seg <- segments[[seg_idx]]
          # Translate start_point and end_point
          if (!is.null(seg$start_point)) {
            piece$fused_edge_segments[[edge_name]][[seg_idx]]$start_point <- seg$start_point + c(dx, dy)
          }
          if (!is.null(seg$end_point)) {
            piece$fused_edge_segments[[edge_name]][[seg_idx]]$end_point <- seg$end_point + c(dx, dy)
          }
          # Translate the bezier path
          if (!is.null(seg$path)) {
            piece$fused_edge_segments[[edge_name]][[seg_idx]]$path <- translate_svg_path(seg$path, dx, dy)
          }
        }
      }
    }
  }

  piece
}


#' Translate parsed segments by dx, dy
#'
#' @param segments List of parsed SVG segments
#' @param dx X translation
#' @param dy Y translation
#' @return Translated segments
#' @keywords internal
translate_segments <- function(segments, dx, dy) {
  lapply(segments, function(seg) {
    if (seg$type %in% c("M", "L")) {
      seg$x <- seg$x + dx
      seg$y <- seg$y + dy
    } else if (seg$type == "C") {
      seg$cp1x <- seg$cp1x + dx
      seg$cp1y <- seg$cp1y + dy
      seg$cp2x <- seg$cp2x + dx
      seg$cp2y <- seg$cp2y + dy
      seg$x <- seg$x + dx
      seg$y <- seg$y + dy
    } else if (seg$type == "A") {
      seg$x <- seg$x + dx
      seg$y <- seg$y + dy
    }
    seg
  })
}


#' Apply compaction to pull pieces toward center
#'
#' @param pieces List of piece objects
#' @param groups Collision groups
#' @param group_bboxes Bounding boxes for each group
#' @param margin Minimum margin to maintain
#' @return Updated pieces
#' @keywords internal
apply_compaction <- function(pieces, groups, group_bboxes, margin) {
  # Find overall center
  all_cx <- sapply(group_bboxes, function(b) b$center_x)
  all_cy <- sapply(group_bboxes, function(b) b$center_y)
  overall_cx <- mean(all_cx)
  overall_cy <- mean(all_cy)

  # Pull each group slightly toward center (10% of distance)
  attraction <- 0.1

  for (i in seq_along(groups)) {
    bbox <- group_bboxes[[i]]
    dx <- (overall_cx - bbox$center_x) * attraction
    dy <- (overall_cy - bbox$center_y) * attraction

    for (idx in groups[[i]]$piece_indices) {
      pieces[[idx]] <- translate_piece(pieces[[idx]], dx, dy)
    }
  }

  pieces
}
