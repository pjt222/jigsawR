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
#' @param size Canvas dimensions c(height, width) to match grid c(rows, cols)
#' @param tabsize Tab size as percentage (0-100). Default: 6.
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

  # Ensure size is c(height, width) to match grid c(rows, cols)
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
  # rw = rectangular window for clipping: c(xmin, xmax, ymin, ymax)
  # size = c(height, width), so width = size[2], height = size[1]
  rw <- c(0, size[2], 0, size[1])
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
    max_tab_size = max_tab_size,
    fusion_style = fusion_style,
    fusion_opacity = fusion_opacity
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
#' Delegates to the shared build_typed_edge_map() implementation.
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
  build_typed_edge_map(adjacency, seed, tabsize, jitter,
                       min_tab_size = min_tab_size,
                       max_tab_size = max_tab_size)
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
  assemble_tile_pieces(
    tiles = tiles,
    edge_map = edge_map,
    adjacency = adjacency,
    puzzle_type = "voronoi",
    get_center = function(tile) c(mean(tile$x), mean(tile$y)),
    get_type_pos = function(tile, n_verts) list(
      seed_x = tile$pt[1], seed_y = tile$pt[2], n_vertices = n_verts
    )
  )
}

#' Assemble tile-based puzzle pieces (voronoi/snic)
#'
#' Shared implementation for assembling pieces from tile vertices.
#' Each tile has $x, $y vertex coordinates and $bp boundary flags.
#'
#' @param tiles List of tiles (each with $x, $y, $bp)
#' @param edge_map Edge map with bezier paths
#' @param adjacency Adjacency data frame
#' @param puzzle_type Type string ("voronoi" or "snic")
#' @param get_center Function(tile) returning c(cx, cy)
#' @param get_type_pos Function(tile, n_verts) returning type-specific pos list
#' @return List of piece objects
#'
#' @keywords internal
assemble_tile_pieces <- function(tiles, edge_map, adjacency, puzzle_type,
                                 get_center, get_type_pos) {
  pieces <- list()
  type_pos_name <- paste0(puzzle_type, "_pos")

  for (cell_id in seq_along(tiles)) {
    tile <- tiles[[cell_id]]

    n_verts <- length(tile$x)
    vertices <- cbind(tile$x, tile$y)

    center <- get_center(tile)

    # Build SVG path by traversing edges
    path <- sprintf("M %.4f %.4f ", vertices[1, 1], vertices[1, 2])
    edge_segments <- list()

    for (j in seq_len(n_verts)) {
      j_next <- if (j == n_verts) 1 else j + 1
      v1 <- vertices[j, ]
      v2 <- vertices[j_next, ]

      edge_result <- find_edge_for_segment(edge_map, cell_id, v1, v2, adjacency)

      if (!is.null(edge_result)) {
        path <- paste0(path, edge_result$path, " ")
        if (edge_result$neighbor_id < 0) {
          neighbor_key <- sprintf("boundary_%d", j)
        } else {
          neighbor_key <- as.character(edge_result$neighbor_id)
        }
        edge_segments[[neighbor_key]] <- list(
          path = sprintf("M %.4f %.4f %s", v1[1], v1[2], edge_result$path),
          neighbor_id = edge_result$neighbor_id,
          is_boundary = edge_result$neighbor_id < 0
        )
      } else {
        fallback_path <- sprintf("L %.4f %.4f ", v2[1], v2[2])
        path <- paste0(path, fallback_path)
        boundary_key <- sprintf("boundary_%d", j)
        edge_segments[[boundary_key]] <- list(
          path = sprintf("M %.4f %.4f %s", v1[1], v1[2], fallback_path),
          neighbor_id = -1,
          is_boundary = TRUE
        )
      }
    }

    path <- paste0(path, "Z")

    parsed_segments <- tryCatch(
      parse_svg_path(path),
      error = function(e) NULL
    )

    is_boundary_piece <- any(tile$bp)

    piece <- list(
      id = cell_id,
      path = path,
      parsed_segments = parsed_segments,
      edge_segments = edge_segments,
      center = center,
      type = puzzle_type,
      is_boundary = is_boundary_piece,
      fusion_group = NA,
      fused_edges = list(),
      fused_neighbor_ids = list()
    )
    piece[[type_pos_name]] <- get_type_pos(tile, n_verts)

    pieces[[cell_id]] <- piece
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
  # Returns list(path, neighbor_id) for edge-level fusion support
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
      # Return BOTH path and neighbor_id for edge-level fusion support
      if (dist_forward < tolerance) {
        # Traversing in same direction as stored edge (v1 → v2)
        return(list(path = edge$forward, neighbor_id = other_cell))
      } else {
        # Traversing in opposite direction (v2 → v1)
        return(list(path = edge$reverse, neighbor_id = other_cell))
      }
    }
  }

  NULL
}

# Voronoi positioning is now handled by apply_tessellation_positioning() in piece_positioning.R

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

# Voronoi neighbors are now handled by get_tessellation_neighbors() in adjacency_api.R

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
