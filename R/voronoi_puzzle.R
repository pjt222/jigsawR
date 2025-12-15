# Voronoi Puzzle Implementation
#
# Generates puzzle pieces using Voronoi tessellation with various
# point distribution options (Fermat spiral, uniform, jittered grid).
#
# Implements Issue #42: Fermat spiral based cell generation

# ============================================================================
# Main Piece Generation
# ============================================================================

#' Generate Voronoi puzzle pieces
#'
#' Creates puzzle pieces using Voronoi tessellation. Points can be distributed
#' using Fermat spiral (golden ratio), uniform random, or jittered grid patterns.
#'
#' @param seed Random seed for reproducibility
#' @param grid Number of cells (single value) or grid dimensions c(cols, rows)
#' @param size Canvas dimensions c(width, height)
#' @param tabsize Tab size percentage (default: 20)
#' @param jitter Jitter percentage for tabs (default: 4)
#' @param boundary Boundary type: "rectangular" or "circular" (default: "rectangular
#' @param point_distribution Point distribution method: "fermat", "uniform", or "jittered"
#' @param fusion_groups Fusion group specification (PILES notation or list)
#' @param fusion_style Style for fused edges: "none", "dashed", "solid"
#' @param fusion_opacity Opacity for fusion regions
#' @return List with pieces, canvas_size, type, parameters, and fusion_data
#'
#' @details
#' The Voronoi tessellation creates organic, non-uniform puzzle shapes where
#' each piece corresponds to a Voronoi cell. The Fermat spiral distribution
#' provides visually pleasing results with naturally varying piece sizes
#' (smaller near center, larger at edges).
#'
#' @keywords internal
generate_voronoi_pieces_internal <- function(seed, grid, size, tabsize, jitter,
                                              boundary = "rectangular",
                                              point_distribution = "fermat",
                                              min_tab_size = NULL,
                                              max_tab_size = NULL,
                                              fusion_groups = NULL,
                                              fusion_style = "none",
                                              fusion_opacity = 1.0) {

  # Check for deldir package
  if (!has_deldir()) {
    cli::cli_abort(c(
      "Package {.pkg deldir} is required for Voronoi puzzles.",
      "i" = "Install with: {.code install.packages('deldir')}"
    ))
  }

  # Determine number of cells
  n_cells <- if (length(grid) == 1) grid[1] else grid[1] * grid[2]

  # Ensure size is c(width, height)
  if (length(size) == 1) {
    size <- c(size, size)
  }

  # Generate seed points based on distribution method
  points <- switch(point_distribution,
    fermat = generate_fermat_points(n_cells, size, seed = seed),
    uniform = generate_uniform_points(n_cells, size, seed),
    jittered = generate_jittered_grid_points(grid, size, seed),
    {
      cli::cli_abort("Unknown point distribution: {point_distribution}")
    }
  )

  # Compute Voronoi tessellation using deldir
  # rw = rectangular window for clipping
  rw <- c(0, size[1], 0, size[2])
  vor <- deldir::deldir(points$x, points$y, rw = rw)

  # Extract tiles as polygons
  tiles <- deldir::tile.list(vor)

  # Build adjacency from Delaunay triangulation
  adjacency <- extract_voronoi_adjacency(vor, tiles)

  # Generate edge map with tabs
  edge_map <- build_voronoi_edge_map(tiles, adjacency, vor, seed, tabsize, jitter,
                                      min_tab_size = min_tab_size,
                                      max_tab_size = max_tab_size)

  # Assemble pieces from tiles
  pieces <- assemble_voronoi_pieces(tiles, edge_map, adjacency, boundary, size)

  # Store parameters
  parameters <- list(
    seed = seed,
    grid = grid,
    size = size,
    tabsize = tabsize,
    jitter = jitter,
    n_cells = n_cells,
    boundary = boundary,
    point_distribution = point_distribution,
    min_tab_size = min_tab_size,
    max_tab_size = max_tab_size
  )

  # Handle fusion groups if specified
  fusion_data <- NULL
  if (!is.null(fusion_groups)) {
    # Parse and validate fusion groups
    puzzle_result <- list(
      pieces = pieces,
      type = "voronoi",
      parameters = parameters
    )
    fusion_data <- process_voronoi_fusion(puzzle_result, fusion_groups,
                                           fusion_style, fusion_opacity)
    pieces <- fusion_data$pieces
  }

  list(
    pieces = pieces,
    canvas_size = size,
    canvas_offset = c(0, 0),
    type = "voronoi",
    parameters = parameters,
    fusion_data = fusion_data,
    adjacency = adjacency,  # Store for later use
    edge_map = edge_map
  )
}

# ============================================================================
# Voronoi Adjacency Extraction
# ============================================================================

#' Extract adjacency from Voronoi tessellation
#'
#' Builds adjacency data from deldir output, identifying which cells
#' share edges and the coordinates of those edges.
#'
#' @param vor deldir output object
#' @param tiles tile.list output
#' @return Data frame with cell_a, cell_b, v1_x, v1_y, v2_x, v2_y columns
#'
#' @keywords internal
extract_voronoi_adjacency <- function(vor, tiles) {
  # The deldir output contains:
  # - vor$dirsgs: Delaunay/Voronoi dual segments
  # - vor$delsgs: Delaunay triangulation segments (connects point indices)

  # Use delsgs to get adjacency (which points are connected = which cells are adjacent)
  delsgs <- vor$delsgs

  # delsgs columns: x1, y1, x2, y2, ind1, ind2
  # ind1, ind2 are point indices (corresponding to cell indices)

  adjacency_list <- list()

  for (i in seq_len(nrow(delsgs))) {
    cell_a <- delsgs[i, "ind1"]
    cell_b <- delsgs[i, "ind2"]

    # Find the shared edge between these cells
    edge <- find_shared_voronoi_edge(tiles, cell_a, cell_b)

    if (!is.null(edge)) {
      adjacency_list[[length(adjacency_list) + 1]] <- data.frame(
        cell_a = cell_a,
        cell_b = cell_b,
        v1_x = edge$v1[1],
        v1_y = edge$v1[2],
        v2_x = edge$v2[1],
        v2_y = edge$v2[2],
        stringsAsFactors = FALSE
      )
    }
  }

  # Also add boundary edges (cells with edges on the boundary)
  for (cell_id in seq_along(tiles)) {
    tile <- tiles[[cell_id]]
    boundary_pts <- tile$bp  # Logical vector: TRUE = boundary vertex

    if (any(boundary_pts)) {
      # Find boundary edge segments
      n_verts <- length(tile$x)
      for (j in seq_len(n_verts)) {
        j_next <- if (j == n_verts) 1 else j + 1

        if (boundary_pts[j] && boundary_pts[j_next]) {
          # This is a boundary edge
          adjacency_list[[length(adjacency_list) + 1]] <- data.frame(
            cell_a = cell_id,
            cell_b = -1,  # -1 indicates boundary
            v1_x = tile$x[j],
            v1_y = tile$y[j],
            v2_x = tile$x[j_next],
            v2_y = tile$y[j_next],
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  if (length(adjacency_list) == 0) {
    # Return empty data frame with correct structure
    return(data.frame(
      cell_a = integer(),
      cell_b = integer(),
      v1_x = numeric(),
      v1_y = numeric(),
      v2_x = numeric(),
      v2_y = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, adjacency_list)
}

#' Find shared edge between two Voronoi cells
#'
#' @param tiles tile.list output
#' @param cell_a First cell index
#' @param cell_b Second cell index
#' @return List with v1 and v2 (edge endpoints) or NULL if not found
#'
#' @keywords internal
find_shared_voronoi_edge <- function(tiles, cell_a, cell_b) {
  tile_a <- tiles[[cell_a]]
  tile_b <- tiles[[cell_b]]

  verts_a <- cbind(tile_a$x, tile_a$y)
  verts_b <- cbind(tile_b$x, tile_b$y)

  # Find vertices that are shared (within tolerance)
  tolerance <- 1e-6
  shared_verts <- list()

  for (i in seq_len(nrow(verts_a))) {
    for (j in seq_len(nrow(verts_b))) {
      dist <- sqrt((verts_a[i, 1] - verts_b[j, 1])^2 +
                     (verts_a[i, 2] - verts_b[j, 2])^2)
      if (dist < tolerance) {
        shared_verts[[length(shared_verts) + 1]] <- verts_a[i, ]
      }
    }
  }

  if (length(shared_verts) < 2) {
    return(NULL)
  }

  # Return first two shared vertices as edge endpoints
  list(
    v1 = shared_verts[[1]],
    v2 = shared_verts[[2]]
  )
}

# ============================================================================
# Edge Map Construction
# ============================================================================

#' Build edge map for Voronoi puzzle
#'
#' Creates the edge map with bezier tabs for all adjacent cell pairs.
#'
#' @param tiles tile.list from deldir
#' @param adjacency Adjacency data frame
#' @param vor deldir output
#' @param seed Random seed
#' @param tabsize Tab size percentage
#' @param jitter Jitter percentage
#' @param min_tab_size Minimum absolute tab size (optional)
#' @param max_tab_size Maximum absolute tab size (optional)
#' @return Named list of edge paths
#'
#' @keywords internal
build_voronoi_edge_map <- function(tiles, adjacency, vor, seed, tabsize, jitter,
                                    min_tab_size = NULL, max_tab_size = NULL) {
  edge_map <- list()

  for (i in seq_len(nrow(adjacency))) {
    row <- adjacency[i, ]
    cell_a <- row$cell_a
    cell_b <- row$cell_b
    v1 <- c(row$v1_x, row$v1_y)
    v2 <- c(row$v2_x, row$v2_y)

    # Create canonical edge key
    if (cell_b < 0) {
      # Boundary edge
      edge_key <- sprintf("E%d-boundary-%d", cell_a, i)
    } else {
      min_id <- min(cell_a, cell_b)
      max_id <- max(cell_a, cell_b)
      edge_key <- sprintf("E%d-%d", min_id, max_id)
    }

    # Skip if already generated
    if (!is.null(edge_map[[edge_key]])) {
      next
    }

    # Determine edge type
    is_boundary <- cell_b < 0

    if (is_boundary) {
      edge_map[[edge_key]] <- generate_straight_edge(v1, v2)
      edge_map[[edge_key]]$cell_a <- cell_a
      edge_map[[edge_key]]$cell_b <- -1
    } else {
      # Generate edge ID for deterministic tab
      edge_id <- min(cell_a, cell_b) * 10000 + max(cell_a, cell_b)

      edge_map[[edge_key]] <- generate_tessellation_edge(
        v1, v2, seed, edge_id,
        tabsize = tabsize,
        jitter = jitter,
        tab_direction = 1,
        min_tab_size = min_tab_size,
        max_tab_size = max_tab_size
      )
      edge_map[[edge_key]]$cell_a <- min(cell_a, cell_b)
      edge_map[[edge_key]]$cell_b <- max(cell_a, cell_b)
    }
  }

  edge_map
}

# ============================================================================
# Piece Assembly
# ============================================================================

#' Assemble Voronoi pieces from tiles
#'
#' Converts tile.list output to standardized puzzle pieces with SVG paths.
#'
#' @param tiles tile.list from deldir
#' @param edge_map Edge map with bezier paths
#' @param adjacency Adjacency data frame
#' @param boundary Boundary type
#' @param size Canvas size
#' @return List of piece objects
#'
#' @keywords internal
assemble_voronoi_pieces <- function(tiles, edge_map, adjacency, boundary, size) {
  pieces <- list()

  for (cell_id in seq_along(tiles)) {
    tile <- tiles[[cell_id]]

    # Get tile vertices (already in anticlockwise order)
    n_verts <- length(tile$x)
    vertices <- cbind(tile$x, tile$y)

    # Calculate center (centroid)
    center <- c(mean(tile$x), mean(tile$y))

    # Build SVG path by traversing edges
    path <- sprintf("M %.4f %.4f ", vertices[1, 1], vertices[1, 2])

    for (j in seq_len(n_verts)) {
      j_next <- if (j == n_verts) 1 else j + 1
      v1 <- vertices[j, ]
      v2 <- vertices[j_next, ]

      # Find the edge path for this segment
      edge_path <- find_edge_for_segment(edge_map, cell_id, v1, v2, adjacency)

      if (!is.null(edge_path)) {
        path <- paste0(path, edge_path, " ")
      } else {
        # Fallback to straight line
        path <- paste0(path, sprintf("L %.4f %.4f ", v2[1], v2[2]))
      }
    }

    path <- paste0(path, "Z")

    # Parse segments for rendering
    parsed_segments <- tryCatch(
      parse_svg_path(path),
      error = function(e) NULL
    )

    # Identify boundary pieces
    is_boundary_piece <- any(tile$bp)

    pieces[[cell_id]] <- list(
      id = cell_id,
      path = path,
      parsed_segments = parsed_segments,
      center = center,
      voronoi_pos = list(
        seed_x = tile$pt[1],
        seed_y = tile$pt[2],
        n_vertices = n_verts
      ),
      type = "voronoi",
      is_boundary = is_boundary_piece,
      fusion_group = NA,
      fused_edges = list(),
      fused_neighbor_ids = list()
    )
  }

  pieces
}

#' Find edge path for a cell segment
#'
#' Looks up the appropriate edge path from the edge map for a specific
#' segment of a cell's boundary.
#'
#' @param edge_map Edge map
#' @param cell_id Cell ID
#' @param v1 Start vertex
#' @param v2 End vertex
#' @param adjacency Adjacency data frame
#' @return SVG path string or NULL
#'
#' @keywords internal
find_edge_for_segment <- function(edge_map, cell_id, v1, v2, adjacency) {
  # Find which edge in adjacency matches this segment
  tolerance <- 1e-4

  for (i in seq_len(nrow(adjacency))) {
    row <- adjacency[i, ]

    # Check if this adjacency row involves our cell
    if (row$cell_a != cell_id && row$cell_b != cell_id) {
      next
    }

    # Check if vertices match (in either direction)
    edge_v1 <- c(row$v1_x, row$v1_y)
    edge_v2 <- c(row$v2_x, row$v2_y)

    dist_forward <- sqrt(sum((v1 - edge_v1)^2)) + sqrt(sum((v2 - edge_v2)^2))
    dist_reverse <- sqrt(sum((v1 - edge_v2)^2)) + sqrt(sum((v2 - edge_v1)^2))

    if (dist_forward < tolerance || dist_reverse < tolerance) {
      # Found matching edge
      other_cell <- if (row$cell_a == cell_id) row$cell_b else row$cell_a

      # Get edge key
      if (other_cell < 0) {
        edge_key <- sprintf("E%d-boundary-%d", cell_id, i)
      } else {
        min_id <- min(cell_id, other_cell)
        max_id <- max(cell_id, other_cell)
        edge_key <- sprintf("E%d-%d", min_id, max_id)
      }

      edge <- edge_map[[edge_key]]
      if (is.null(edge)) {
        return(NULL)
      }

      # Determine if we need forward or reverse path based on traversal direction
      # The path direction is purely geometric - independent of cell ID
      if (dist_forward < tolerance) {
        # Traversing in same direction as stored edge (v1 → v2)
        return(edge$forward)
      } else {
        # Traversing in opposite direction (v2 → v1)
        return(edge$reverse)
      }
    }
  }

  NULL
}

# ============================================================================
# Positioning
# ============================================================================

#' Apply positioning to Voronoi puzzle
#'
#' Separates pieces based on offset parameter.
#'
#' @param piece_result Result from generate_voronoi_pieces_internal()
#' @param offset Separation amount (0 = compact, >0 = separated)
#' @return Positioned piece result
#'
#' @keywords internal
apply_voronoi_positioning <- function(piece_result, offset) {
  params <- piece_result$parameters
  pieces <- piece_result$pieces
  size <- params$size

  if (offset == 0) {
    # No separation needed
    return(piece_result)
  }

  # Calculate piece positions based on their seed points
  # Voronoi cells naturally radiate from seed points
  center <- size / 2

  # Separation factor based on distance from center
  transformed_pieces <- lapply(pieces, function(piece) {
    seed_pos <- c(piece$voronoi_pos$seed_x, piece$voronoi_pos$seed_y)

    # Calculate displacement direction from center
    dir <- seed_pos - center
    dist <- sqrt(sum(dir^2))

    if (dist < 0.001) {
      # Center piece stays in place
      dx <- 0
      dy <- 0
    } else {
      # Move outward proportional to distance from center
      dir_norm <- dir / dist
      # Scale offset by relative position
      scale_factor <- dist / max(size) * 2
      dx <- dir_norm[1] * offset * scale_factor
      dy <- dir_norm[2] * offset * scale_factor
    }

    # Translate path
    translated_path <- translate_voronoi_path(piece$path, dx, dy)

    list(
      id = piece$id,
      path = translated_path,
      parsed_segments = tryCatch(parse_svg_path(translated_path), error = function(e) NULL),
      center = piece$center + c(dx, dy),
      voronoi_pos = piece$voronoi_pos,
      type = piece$type,
      is_boundary = piece$is_boundary,
      fusion_group = piece$fusion_group,
      fused_edges = piece$fused_edges,
      fused_neighbor_ids = piece$fused_neighbor_ids
    )
  })

  # Calculate new canvas bounds
  bounds <- calculate_voronoi_bounds(transformed_pieces)

  # Add margin
  margin <- max(size) * 0.05 + offset

  list(
    pieces = transformed_pieces,
    canvas_size = c(
      bounds$max_x - bounds$min_x + 2 * margin,
      bounds$max_y - bounds$min_y + 2 * margin
    ),
    canvas_offset = c(bounds$min_x - margin, bounds$min_y - margin),
    offset = offset,
    type = "voronoi",
    parameters = params,
    fusion_data = piece_result$fusion_data,
    adjacency = piece_result$adjacency,
    edge_map = piece_result$edge_map
  )
}

#' Translate Voronoi path by offset
#'
#' Uses the existing translate_svg_path function from piece_positioning.R
#'
#' @param path SVG path string
#' @param dx X offset
#' @param dy Y offset
#' @return Translated path string
#'
#' @keywords internal
translate_voronoi_path <- function(path, dx, dy) {
  # Use the existing translate_svg_path function
  translate_svg_path(path, dx, dy)
}

#' Calculate bounds for Voronoi pieces
#'
#' @param pieces List of piece objects
#' @return List with min_x, max_x, min_y, max_y
#'
#' @keywords internal
calculate_voronoi_bounds <- function(pieces) {
  all_centers <- do.call(rbind, lapply(pieces, function(p) p$center))

  list(
    min_x = min(all_centers[, 1]) - 50,
    max_x = max(all_centers[, 1]) + 50,
    min_y = min(all_centers[, 2]) - 50,
    max_y = max(all_centers[, 2]) + 50
  )
}

# ============================================================================
# Fusion Support
# ============================================================================

#' Process fusion groups for Voronoi puzzle
#'
#' @param puzzle_result Puzzle result
#' @param fusion_groups Fusion specification
#' @param fusion_style Style for fused edges
#' @param fusion_opacity Opacity for fusion regions
#' @return Updated fusion data
#'
#' @keywords internal
process_voronoi_fusion <- function(puzzle_result, fusion_groups, fusion_style, fusion_opacity) {
  # Use existing fusion processing infrastructure
  # This would call parse_fusion() and apply_fusion_to_pieces()
  # For now, return NULL (fusion support to be added later)
  NULL
}

# ============================================================================
# Topology / Adjacency API
# ============================================================================

#' Get Voronoi cell neighbors
#'
#' Returns the neighbors of a Voronoi cell based on the stored adjacency data.
#'
#' @param piece_id Piece/cell ID
#' @param puzzle_result Puzzle result containing adjacency data
#' @param include_boundary Whether to include boundary edges
#' @return Data frame with direction, neighbor_id, is_boundary columns
#'
#' @keywords internal
get_voronoi_neighbors <- function(piece_id, puzzle_result, include_boundary = TRUE) {
  adjacency <- puzzle_result$adjacency

  if (is.null(adjacency)) {
    return(data.frame(
      direction = character(),
      neighbor_id = integer(),
      is_boundary = logical()
    ))
  }

  # Find all adjacencies involving this cell
  cell_adj <- adjacency[adjacency$cell_a == piece_id | adjacency$cell_b == piece_id, ]

  if (nrow(cell_adj) == 0) {
    return(data.frame(
      direction = character(),
      neighbor_id = integer(),
      is_boundary = logical()
    ))
  }

  neighbors <- lapply(seq_len(nrow(cell_adj)), function(i) {
    row <- cell_adj[i, ]
    neighbor_id <- if (row$cell_a == piece_id) row$cell_b else row$cell_a
    is_boundary <- neighbor_id < 0

    if (!include_boundary && is_boundary) {
      return(NULL)
    }

    # Calculate direction based on edge midpoint relative to piece center
    edge_mid <- c((row$v1_x + row$v2_x) / 2, (row$v1_y + row$v2_y) / 2)
    piece_center <- puzzle_result$pieces[[piece_id]]$center
    dir_vec <- edge_mid - piece_center

    # Convert to compass direction
    angle <- atan2(dir_vec[2], dir_vec[1]) * 180 / pi
    direction <- angle_to_direction(angle)

    data.frame(
      direction = direction,
      neighbor_id = if (is_boundary) NA_integer_ else neighbor_id,
      is_boundary = is_boundary,
      stringsAsFactors = FALSE
    )
  })

  neighbors <- neighbors[!sapply(neighbors, is.null)]

  if (length(neighbors) == 0) {
    return(data.frame(
      direction = character(),
      neighbor_id = integer(),
      is_boundary = logical()
    ))
  }

  do.call(rbind, neighbors)
}

#' Convert angle to compass direction
#'
#' @param angle Angle in degrees (-180 to 180)
#' @return Direction string (N, NE, E, SE, S, SW, W, NW)
#'
#' @keywords internal
angle_to_direction <- function(angle) {
  # Normalize to 0-360
  angle <- (angle + 360) %% 360

  if (angle < 22.5 || angle >= 337.5) return("E")
  if (angle < 67.5) return("NE")
  if (angle < 112.5) return("N")
  if (angle < 157.5) return("NW")
  if (angle < 202.5) return("W")
  if (angle < 247.5) return("SW")
  if (angle < 292.5) return("S")
  return("SE")
}
