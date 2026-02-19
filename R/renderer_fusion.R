# Fusion Rendering for SVG Output
# Split from unified_renderer.R for maintainability
# Contains multi-pass fusion styling for fused puzzle pieces

#' Generate SVG arc path for an angular segment
#'
#' Creates an SVG path string for an arc segment at a given radius between
#' two angles. Used for rendering segment-level fusion in concentric puzzles.
#'
#' @param radius Arc radius
#' @param start_angle Start angle in radians
#' @param end_angle End angle in radians
#' @param center Optional center point (default: c(0, 0))
#' @return SVG path string
#' @keywords internal
generate_arc_segment_path <- function(radius, start_angle, end_angle, center = c(0, 0)) {
  # Calculate start and end points
  x1 <- center[1] + radius * cos(start_angle)
  y1 <- center[2] + radius * sin(start_angle)
  x2 <- center[1] + radius * cos(end_angle)
  y2 <- center[2] + radius * sin(end_angle)

  # Determine arc flags
  # large-arc-flag: 1 if arc > 180 degrees, 0 otherwise
  arc_span <- end_angle - start_angle
  large_arc <- if (arc_span > pi) 1 else 0
  # sweep-flag: 1 for clockwise (which matches increasing angle in SVG coords)
  sweep <- 1

  sprintf("M%.2f,%.2f A%.2f,%.2f 0 %d,%d %.2f,%.2f",
          x1, y1, radius, radius, large_arc, sweep, x2, y2)
}


# put id:"fusion_render", label:"Fusion Renderer", input:"edge_paths,fusion_groups", output:"svg_elements"
#' Render pieces with styled fusion edges
#'
#' Three-pass rendering: fills, non-fused edges, fused edges with styling.
#' For "none" mode, fusion_opacity should be 0 to make fused edges invisible.
#' Supports rectangular, hexagonal, and concentric puzzle types.
#'
#' @param pieces List of positioned pieces
#' @param colors Vector of stroke colors
#' @param fill Fill value for pieces (single value or vector of per-piece fills)
#' @param stroke_width Stroke width
#' @param opacity Piece opacity
#' @param fusion_style "dashed" or "solid"
#' @param fusion_opacity Opacity for fused edges (0.0 to 1.0)
#' @return Vector of SVG elements
#' @keywords internal
render_pieces_with_fusion_styled <- function(pieces, colors, fill, stroke_width, opacity,
                                              fusion_style, fusion_opacity) {
  n_pieces <- length(pieces)
  element_list <- vector("list", n_pieces * 8L)  # pre-allocate generously
  element_idx <- 0L
  add_element <- function(el) {
    element_idx <<- element_idx + 1L
    if (element_idx > length(element_list)) {
      element_list[[element_idx * 2L]] <<- NULL  # grow if needed
    }
    element_list[[element_idx]] <<- el
  }

  # Build piece_id lookup hash map for O(1) neighbor lookup (instead of O(n) loops)
  piece_lookup <- new.env(hash = TRUE, parent = emptyenv())
  for (i in seq_along(pieces)) {
    p_id <- pieces[[i]]$id %||% i
    piece_lookup[[as.character(p_id)]] <- i
  }

  # Helper function for O(1) piece lookup by ID
  get_piece_by_id <- function(target_id) {
    idx <- piece_lookup[[as.character(target_id)]]
    if (!is.null(idx)) pieces[[idx]] else NULL
  }

  # Check if fill is per-piece (vector) or single value
  use_per_piece_fills <- length(fill) == n_pieces && n_pieces > 1

  # Pass 1: Draw fills only (no stroke)
  opacity_attr <- if (opacity < 1.0) sprintf(' opacity="%.2f"', opacity) else ""

  for (i in seq_along(pieces)) {
    piece <- pieces[[i]]
    piece_fill <- if (use_per_piece_fills) fill[i] else fill[1]
    fill_element <- sprintf(
      '<path d="%s" fill="%s" stroke="none"%s/>',
      piece$path, piece_fill, opacity_attr
    )
    add_element(fill_element)
  }

  # Pass 2: Draw non-fused edges with normal stroke
  for (i in seq_along(pieces)) {
    piece <- pieces[[i]]
    color <- colors[i]
    piece_type <- piece$type %||% "rectangular"

    # Use type-aware path splitting (for all types including voronoi/random)
    # For voronoi/random, edge_segments are keyed by neighbor_id
    edge_paths <- get_piece_edge_paths(piece)
    edge_names <- get_piece_edge_names(piece)

    # Fallback: if no edge paths available, draw full outline
    if (length(edge_paths) == 0 || length(edge_names) == 0) {
      edge_element <- sprintf(
        '<path d="%s" fill="none" stroke="%s" stroke-width="%.2f" stroke-linecap="round" stroke-linejoin="round"/>',
        piece$path, color, stroke_width
      )
      add_element(edge_element)
      next
    }

    for (edge_name in edge_names) {
      # Check for segment-level fusion (many-to-one OUTER edges)
      if (edge_name == "OUTER" && isTRUE(piece$outer_segments_mixed)) {
        # Mixed segments - handle in pass 3.5 (segment-level rendering)
        next
      }

      if (isTRUE(piece$fused_edges[[edge_name]])) {
        next  # Will handle in pass 3
      }

      edge_path <- edge_paths[[edge_name]]
      if (!is.null(edge_path) && nzchar(edge_path)) {
        edge_element <- sprintf(
          '<path d="%s" fill="none" stroke="%s" stroke-width="%.2f" stroke-linecap="round" stroke-linejoin="round"/>',
          edge_path, color, stroke_width
        )
        add_element(edge_element)
      }
    }
  }

  # Pass 3: Draw fused edges with special styling
  # Each fused edge is shared by two pieces - draw it only ONCE
  # Use piece ID + edge name for deduplication (works with offset > 0)
  # Key format: "min_id-edge|max_id-edge" where ids are sorted
  drawn_fused_edges <- new.env(hash = TRUE, parent = emptyenv())

  # Build style attributes for fused edges
  dash_attr <- ""
  if (fusion_style == "dashed") {
    dash_attr <- sprintf(' stroke-dasharray="%.1f %.1f"', stroke_width * 3, stroke_width * 2)
  }
  fusion_opacity_attr <- sprintf(' opacity="%.2f"', fusion_opacity)

  for (i in seq_along(pieces)) {
    piece <- pieces[[i]]
    color <- colors[i]
    piece_id <- piece$id %||% i
    piece_type <- piece$type %||% "rectangular"

    if (is.null(piece$fused_edges) || length(piece$fused_edges) == 0) {
      next
    }

    # Use type-aware path splitting (for all types including voronoi/random)
    # For voronoi/random, edge_segments are keyed by neighbor_id
    edge_paths <- get_piece_edge_paths(piece)
    edge_names <- get_piece_edge_names(piece)

    for (edge_name in edge_names) {
      # Skip mixed segment edges - handled in Pass 3.5
      if (edge_name == "OUTER" && isTRUE(piece$outer_segments_mixed)) {
        next
      }

      # Skip INNER edges if the neighbor has outer_segments_mixed = TRUE
      # Those edges are handled in Pass 3.5 from the neighbor's perspective
      if (edge_name == "INNER") {
        neighbor_id <- piece$fused_neighbor_ids[[edge_name]]
        if (!is.null(neighbor_id)) {
          # O(1) lookup via hash map (instead of O(n) loop)
          np <- get_piece_by_id(neighbor_id)
          if (!is.null(np) && isTRUE(np$outer_segments_mixed)) {
            next  # Skip - will be handled in Pass 3.5
          }
        }
      }

      if (!isTRUE(piece$fused_edges[[edge_name]])) {
        next  # Only draw fused edges here
      }

      edge_path <- edge_paths[[edge_name]]
      if (!is.null(edge_path) && nzchar(edge_path)) {
        # Create canonical edge key using piece ID and edge name
        # This works regardless of coordinate translation (offset > 0)
        # Format: "piece_id-edge_name" with canonical ordering
        this_edge_key <- sprintf("%d-%s", piece_id, edge_name)

        # For deduplication, we need a canonical key that's the same
        # whether we're processing piece A or piece B of a shared edge.
        # Use sorted piece IDs to create consistent keys.
        # We'll use the piece's own key and check if we've drawn the complementary edge.
        #
        # Since fused_edges marks BOTH pieces' edges as fused, we can simply
        # use the smaller piece ID's edge key as the canonical form.
        # If this piece has a smaller ID than any neighbor that shares this edge,
        # we draw it. Otherwise, we skip (the neighbor with smaller ID will draw it).

        # Get neighbor info from the fused_edges structure
        # For now, use a simple approach: track drawn edges by this_edge_key
        # and its complement (which will have format "neighbor_id-opposite_edge")
        if (exists(this_edge_key, envir = drawn_fused_edges, inherits = FALSE)) {
          next
        }

        # Mark this edge as drawn
        drawn_fused_edges[[this_edge_key]] <- TRUE

        # Also mark the complementary edge as drawn to prevent double-drawing
        # We find the neighbor piece and mark ALL of their fused edges that point to us
        # This handles asymmetric adjacency (like in hexagonal puzzles)
        #
        # For OUTER edges with many-to-one relationships (segment data), we need to
        # mark ALL neighbors' INNER edges as drawn, not just the one in fused_neighbor_ids
        neighbor_ids <- NULL

        # Check for segment data first (many-to-one relationships)
        if (edge_name == "OUTER" && !is.null(piece$fused_edge_segments) &&
            !is.null(piece$fused_edge_segments$OUTER)) {
          # Get all neighbor IDs from segment data
          neighbor_ids <- sapply(piece$fused_edge_segments$OUTER, function(seg) seg$neighbor_id)
          neighbor_ids <- unique(neighbor_ids[!is.na(neighbor_ids)])
        }

        # Fall back to single neighbor from fused_neighbor_ids
        if (is.null(neighbor_ids) || length(neighbor_ids) == 0) {
          neighbor_ids <- piece$fused_neighbor_ids[[edge_name]]
        }

        if (!is.null(neighbor_ids) && length(neighbor_ids) > 0) {
          for (neighbor_id in neighbor_ids) {
            # O(1) lookup via hash map (instead of O(n) loop)
            neighbor_piece <- get_piece_by_id(neighbor_id)

            if (!is.null(neighbor_piece) && !is.null(neighbor_piece$fused_neighbor_ids)) {
              # Mark all neighbor edges that point back to this piece as drawn
              for (n_edge in names(neighbor_piece$fused_neighbor_ids)) {
                if (neighbor_piece$fused_neighbor_ids[[n_edge]] == piece_id) {
                  comp_edge_key <- sprintf("%d-%s", neighbor_id, n_edge)
                  drawn_fused_edges[[comp_edge_key]] <- TRUE
                }
              }
            }
          }
        }

        edge_element <- sprintf(
          '<path d="%s" fill="none" stroke="%s" stroke-width="%.2f" stroke-linecap="round" stroke-linejoin="round"%s%s/>',
          edge_path, color, stroke_width, dash_attr, fusion_opacity_attr
        )
        add_element(edge_element)
      }
    }
  }

  # Pass 3.5: Draw segment-level fused OUTER edges for concentric puzzles
  # This handles many-to-one relationships where different segments of an
  # OUTER edge have different fusion status (some fused, some not)
  drawn_segment_keys <- new.env(hash = TRUE, parent = emptyenv())

  for (i in seq_along(pieces)) {
    piece <- pieces[[i]]
    color <- colors[i]
    piece_id <- piece$id %||% i

    # Only process pieces with mixed segment fusion
    if (!isTRUE(piece$outer_segments_mixed)) {
      next
    }

    # Get outer radius for arc generation
    outer_radius <- piece$outer_radius
    if (is.null(outer_radius)) {
      next  # Can't render without radius
    }

    # Get segment fusion data
    segments <- piece$fused_edge_segments[["OUTER"]]
    if (is.null(segments) || length(segments) == 0) {
      next
    }

    # Render each segment with appropriate styling
    for (seg in segments) {
      # Create segment key for deduplication: "innerPiece-outerPiece"
      # Use sorted IDs for canonical form
      neighbor_id <- seg$neighbor_id
      seg_key <- sprintf("%d-%d", min(piece_id, neighbor_id), max(piece_id, neighbor_id))

      if (exists(seg_key, envir = drawn_segment_keys, inherits = FALSE)) {
        next
      }
      drawn_segment_keys[[seg_key]] <- TRUE

      # IMPORTANT: Also mark the neighbor's complementary edge as drawn in
      # drawn_fused_edges so Pass 3 doesn't draw it again.
      # The neighbor's INNER edge points to this piece.
      neighbor_inner_key <- sprintf("%d-INNER", neighbor_id)
      drawn_fused_edges[[neighbor_inner_key]] <- TRUE

      # Use the actual edge path with bezier tabs if available,
      # otherwise fall back to arc generation
      if (!is.null(seg$path) && !is.null(seg$start_point)) {
        # Use the pre-computed bezier path
        # Path needs M (moveto) + the bezier curves
        edge_path <- sprintf("M %.2f %.2f %s",
                            seg$start_point[1], seg$start_point[2], seg$path)
      } else {
        # Fallback to arc generation
        edge_path <- generate_arc_segment_path(
          radius = outer_radius,
          start_angle = seg$start_angle,
          end_angle = seg$end_angle
        )
      }

      # Apply styling based on segment fusion status
      if (isTRUE(seg$fused)) {
        # Fused segment - dashed with fusion opacity
        edge_element <- sprintf(
          '<path d="%s" fill="none" stroke="%s" stroke-width="%.2f" stroke-linecap="round" stroke-linejoin="round"%s%s/>',
          edge_path, color, stroke_width, dash_attr, fusion_opacity_attr
        )
      } else {
        # Non-fused segment - solid with normal opacity
        edge_element <- sprintf(
          '<path d="%s" fill="none" stroke="%s" stroke-width="%.2f" stroke-linecap="round" stroke-linejoin="round"/>',
          edge_path, color, stroke_width
        )
      }
      add_element(edge_element)
    }
  }

  return(unlist(element_list[seq_len(element_idx)], use.names = FALSE))
}
