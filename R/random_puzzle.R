# Random Shape Puzzle Implementation
#
# Generates puzzle pieces using Delaunay triangulation within a base polygon.
# Each piece is a triangle formed by the triangulation.
#
# Implements Issue #41: Random Shape Puzzles

# ============================================================================
# Main Piece Generation
# ============================================================================

# put id:"rand_gen", label:"Random Generator", input:"rand_path", output:"pieces_raw"
#' Generate random shape puzzle pieces
#'
#' Creates puzzle pieces using constrained Delaunay triangulation within a
#' base polygon. The base polygon can have any number of corners (default 4
#' for rectangle). Interior points are generated randomly, and triangulation
#' creates triangular puzzle pieces.
#'
#' @param seed Random seed for reproducibility
#' @param grid Number of interior points (single value) that determines piece count
#' @param size Canvas dimensions c(width, height)
#' @param tabsize Tab size as percentage (0-100). Default: 6.
#' @param jitter Jitter percentage for tabs (default: 4)
#' @param n_corner Number of base polygon corners (default: 4 for rectangle)
#' @param min_piece_size Minimum piece dimension constraint (default: NULL for auto)
#' @param fusion_groups Fusion group specification (PILES notation or list)
#' @param fusion_style Style for fused edges: "none", "dashed", "solid"
#' @param fusion_opacity Opacity for fusion regions
#' @return List with pieces, canvas_size, type, parameters, and fusion_data
#'
#' @details
#' The triangulation creates triangular puzzle pieces where each triangle
#' vertex is either a polygon corner or an interior point. The number of
#' resulting pieces is approximately 2 * n_interior + n_corner - 2.
#'
#' Supported base polygon shapes:
#' - n_corner = 3: Triangle
#' - n_corner = 4: Rectangle (axis-aligned)
#' - n_corner = 5: Pentagon
#' - n_corner = 6+: Higher polygons
#'
#' @keywords internal
generate_random_pieces_internal <- function(seed, grid, size, tabsize, jitter,
                                             n_corner = 4, min_piece_size = NULL,
                                             min_tab_size = NULL,
                                             max_tab_size = NULL,
                                             fusion_groups = NULL,
                                             fusion_style = "none",
                                             fusion_opacity = 1.0) {

  # Check for RCDT package
  if (!has_rcdt()) {
    cli::cli_abort(c(
      "Package {.pkg RCDT} is required for random shape puzzles.",
      "i" = "RCDT is archived from CRAN. Install from GitHub with:",
      " " = "{.code remotes::install_github('stla/RCDT')}"
    ))
  }

  # Determine number of interior points
  n_interior <- if (length(grid) == 1) grid[1] else grid[1] * grid[2]

  # Ensure size is c(width, height)
  if (length(size) == 1) {
    size <- c(size, size)
  }

  # Generate base polygon
  boundary <- generate_base_polygon(n_corner, size)

  # Generate interior points
  interior_points <- generate_interior_points(n_interior, boundary, seed,
                                               min_distance = min_piece_size)

  # Combine boundary vertices and interior points
  all_vertices <- rbind(boundary$vertices, interior_points)

  # Run constrained Delaunay triangulation using RCDT
  triangulation <- run_constrained_triangulation(
    all_vertices,
    boundary$constraint_edges,
    n_corner
  )

  # Build adjacency from triangulation
  adjacency <- extract_random_adjacency(triangulation, all_vertices, n_corner)

  # Generate edge map with tabs
  edge_map <- build_random_edge_map(triangulation, adjacency, seed, tabsize, jitter,
                                     min_tab_size = min_tab_size,
                                     max_tab_size = max_tab_size)

  # Assemble pieces from triangles
  pieces <- assemble_random_pieces(triangulation, edge_map, adjacency,
                                    all_vertices, n_corner, size)

  # Store parameters
  parameters <- list(
    seed = seed,
    grid = grid,
    size = size,
    tabsize = tabsize,
    jitter = jitter,
    n_interior = n_interior,
    n_corner = n_corner,
    n_pieces = length(pieces),
    min_piece_size = min_piece_size,
    min_tab_size = min_tab_size,
    max_tab_size = max_tab_size,
    fusion_style = fusion_style,
    fusion_opacity = fusion_opacity
  )

  # Handle fusion groups if specified
  fusion_data <- NULL
  if (!is.null(fusion_groups)) {
    puzzle_result <- list(
      pieces = pieces,
      type = "random",
      parameters = parameters
    )
    fusion_data <- process_random_fusion(puzzle_result, fusion_groups,
                                          fusion_style, fusion_opacity)
    pieces <- fusion_data$pieces
  }

  list(
    pieces = pieces,
    canvas_size = size,
    canvas_offset = c(0, 0),
    type = "random",
    parameters = parameters,
    fusion_data = fusion_data,
    adjacency = adjacency,
    edge_map = edge_map,
    triangulation = triangulation
  )
}

# ============================================================================
# Triangulation
# ============================================================================

#' Run constrained Delaunay triangulation
#'
#' Uses RCDT to create a constrained Delaunay triangulation with the
#' boundary edges as constraints.
#'
#' @param vertices Matrix of all vertices (boundary + interior)
#' @param constraint_edges Matrix of constraint edge indices
#' @param n_corner Number of boundary vertices
#' @return RCDT triangulation result
#'
#' @keywords internal
run_constrained_triangulation <- function(vertices, constraint_edges, n_corner) {
  # RCDT expects 1-indexed constraint edges
  # Create triangulation with boundary constraints
  tri <- RCDT::delaunay(
    vertices,
    edges = constraint_edges
  )

  # RCDT returns: vertices, mesh (rgl mesh3d with $it for triangles), edges
  # Extract triangles from mesh for easier access
  # mesh$it is 3 x n_triangles, we need n_triangles x 3
  tri$triangles <- t(tri$mesh$it)

  tri
}

# ============================================================================
# Adjacency Extraction
# ============================================================================

#' Extract adjacency from triangulation
#'
#' Builds adjacency data from RCDT triangulation output.
#'
#' @param tri RCDT triangulation result
#' @param vertices All vertices matrix
#' @param n_corner Number of boundary vertices
#' @return Data frame with cell_a, cell_b, v1_x, v1_y, v2_x, v2_y columns
#'
#' @keywords internal
extract_random_adjacency <- function(tri, vertices, n_corner) {
  triangles <- tri$triangles
  n_triangles <- nrow(triangles)

  adjacency_list <- list()
  edge_to_triangles <- list()  # Map edge to triangles that share it

  # Build edge-to-triangle mapping
  for (tri_id in seq_len(n_triangles)) {
    tri_verts <- triangles[tri_id, ]

    # Three edges per triangle
    edges <- list(
      sort(c(tri_verts[1], tri_verts[2])),
      sort(c(tri_verts[2], tri_verts[3])),
      sort(c(tri_verts[3], tri_verts[1]))
    )

    for (edge in edges) {
      edge_key <- paste(edge, collapse = "-")

      if (is.null(edge_to_triangles[[edge_key]])) {
        edge_to_triangles[[edge_key]] <- list(
          edge = edge,
          triangles = c(tri_id)
        )
      } else {
        edge_to_triangles[[edge_key]]$triangles <- c(
          edge_to_triangles[[edge_key]]$triangles,
          tri_id
        )
      }
    }
  }

  # Convert to adjacency data frame
  for (edge_key in names(edge_to_triangles)) {
    entry <- edge_to_triangles[[edge_key]]
    edge_verts <- entry$edge
    tris <- entry$triangles

    v1 <- vertices[edge_verts[1], ]
    v2 <- vertices[edge_verts[2], ]

    # Determine if this is a boundary edge
    is_boundary_edge <- all(edge_verts <= n_corner) &&
                        abs(edge_verts[2] - edge_verts[1]) %in% c(1, n_corner - 1)

    if (length(tris) == 2) {
      # Internal edge - two adjacent triangles
      adjacency_list[[length(adjacency_list) + 1]] <- data.frame(
        cell_a = tris[1],
        cell_b = tris[2],
        v1_x = v1[1],
        v1_y = v1[2],
        v2_x = v2[1],
        v2_y = v2[2],
        is_constraint = is_boundary_edge,
        stringsAsFactors = FALSE
      )
    } else if (length(tris) == 1) {
      # Boundary edge
      adjacency_list[[length(adjacency_list) + 1]] <- data.frame(
        cell_a = tris[1],
        cell_b = -1,
        v1_x = v1[1],
        v1_y = v1[2],
        v2_x = v2[1],
        v2_y = v2[2],
        is_constraint = TRUE,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(adjacency_list) == 0) {
    return(data.frame(
      cell_a = integer(),
      cell_b = integer(),
      v1_x = numeric(),
      v1_y = numeric(),
      v2_x = numeric(),
      v2_y = numeric(),
      is_constraint = logical(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, adjacency_list)
}

# ============================================================================
# Edge Map Construction
# ============================================================================

#' Build edge map for random shape puzzle
#'
#' Creates the edge map with bezier tabs for all adjacent triangle pairs.
#' Delegates to the shared build_typed_edge_map() with a custom predicate
#' that treats constraint edges (on the polygon boundary) as straight.
#'
#' @param tri RCDT triangulation
#' @param adjacency Adjacency data frame
#' @param seed Random seed
#' @param tabsize Tab size percentage
#' @param jitter Jitter percentage
#' @param min_tab_size Minimum absolute tab size (optional)
#' @param max_tab_size Maximum absolute tab size (optional)
#' @return Named list of edge paths
#'
#' @keywords internal
build_random_edge_map <- function(tri, adjacency, seed, tabsize, jitter,
                                   min_tab_size = NULL, max_tab_size = NULL) {
  build_typed_edge_map(adjacency, seed, tabsize, jitter,
                       min_tab_size = min_tab_size,
                       max_tab_size = max_tab_size,
                       is_straight_fn = function(row) {
                         row$cell_b < 0 || isTRUE(row$is_constraint)
                       })
}

# ============================================================================
# Piece Assembly
# ============================================================================

#' Assemble random shape pieces from triangles
#'
#' Converts triangulation output to standardized puzzle pieces with SVG paths.
#'
#' @param tri RCDT triangulation
#' @param edge_map Edge map with bezier paths
#' @param adjacency Adjacency data frame
#' @param vertices All vertices matrix
#' @param n_corner Number of boundary vertices
#' @param size Canvas size
#' @return List of piece objects
#'
#' @keywords internal
assemble_random_pieces <- function(tri, edge_map, adjacency, vertices,
                                    n_corner, size) {
  triangles <- tri$triangles
  pieces <- list()

  for (tri_id in seq_len(nrow(triangles))) {
    tri_verts <- triangles[tri_id, ]

    # Get triangle vertices
    v1 <- vertices[tri_verts[1], ]
    v2 <- vertices[tri_verts[2], ]
    v3 <- vertices[tri_verts[3], ]

    # Calculate center (centroid)
    center <- c(mean(c(v1[1], v2[1], v3[1])),
                mean(c(v1[2], v2[2], v3[2])))

    # Build SVG path by traversing edges
    # Also track edge_segments for edge-level fusion rendering
    path <- sprintf("M %.4f %.4f ", v1[1], v1[2])
    edge_segments <- list()  # Map neighbor_id -> path segment

    # Edge 1: v1 -> v2
    edge_result_1 <- find_edge_for_segment(edge_map, tri_id, v1, v2, adjacency)
    if (!is.null(edge_result_1)) {
      path <- paste0(path, edge_result_1$path, " ")
      # IMPORTANT: For boundary edges (neighbor_id < 0), use unique key "boundary_1"
      # to avoid overwrites when a piece has multiple boundary edges
      if (edge_result_1$neighbor_id < 0) {
        neighbor_key <- "boundary_1"
      } else {
        neighbor_key <- as.character(edge_result_1$neighbor_id)
      }
      # Include start point to create valid SVG path with "M" command
      edge_segments[[neighbor_key]] <- list(
        path = sprintf("M %.4f %.4f %s", v1[1], v1[2], edge_result_1$path),
        neighbor_id = edge_result_1$neighbor_id,
        is_boundary = edge_result_1$neighbor_id < 0
      )
    } else {
      fallback_path <- sprintf("L %.4f %.4f", v2[1], v2[2])
      path <- paste0(path, fallback_path, " ")
      # Store fallback boundary edge
      edge_segments[["boundary_1"]] <- list(
        path = sprintf("M %.4f %.4f %s", v1[1], v1[2], fallback_path),
        neighbor_id = -1,
        is_boundary = TRUE
      )
    }

    # Edge 2: v2 -> v3
    edge_result_2 <- find_edge_for_segment(edge_map, tri_id, v2, v3, adjacency)
    if (!is.null(edge_result_2)) {
      path <- paste0(path, edge_result_2$path, " ")
      # IMPORTANT: For boundary edges (neighbor_id < 0), use unique key "boundary_2"
      # to avoid overwrites when a piece has multiple boundary edges
      if (edge_result_2$neighbor_id < 0) {
        neighbor_key <- "boundary_2"
      } else {
        neighbor_key <- as.character(edge_result_2$neighbor_id)
      }
      # Include start point to create valid SVG path with "M" command
      edge_segments[[neighbor_key]] <- list(
        path = sprintf("M %.4f %.4f %s", v2[1], v2[2], edge_result_2$path),
        neighbor_id = edge_result_2$neighbor_id,
        is_boundary = edge_result_2$neighbor_id < 0
      )
    } else {
      fallback_path <- sprintf("L %.4f %.4f", v3[1], v3[2])
      path <- paste0(path, fallback_path, " ")
      # Store fallback boundary edge
      edge_segments[["boundary_2"]] <- list(
        path = sprintf("M %.4f %.4f %s", v2[1], v2[2], fallback_path),
        neighbor_id = -1,
        is_boundary = TRUE
      )
    }

    # Edge 3: v3 -> v1
    edge_result_3 <- find_edge_for_segment(edge_map, tri_id, v3, v1, adjacency)
    if (!is.null(edge_result_3)) {
      path <- paste0(path, edge_result_3$path, " ")
      # IMPORTANT: For boundary edges (neighbor_id < 0), use unique key "boundary_3"
      # to avoid overwrites when a piece has multiple boundary edges
      if (edge_result_3$neighbor_id < 0) {
        neighbor_key <- "boundary_3"
      } else {
        neighbor_key <- as.character(edge_result_3$neighbor_id)
      }
      # Include start point to create valid SVG path with "M" command
      edge_segments[[neighbor_key]] <- list(
        path = sprintf("M %.4f %.4f %s", v3[1], v3[2], edge_result_3$path),
        neighbor_id = edge_result_3$neighbor_id,
        is_boundary = edge_result_3$neighbor_id < 0
      )
    } else {
      fallback_path <- sprintf("L %.4f %.4f", v1[1], v1[2])
      path <- paste0(path, fallback_path, " ")
      # Store fallback boundary edge
      edge_segments[["boundary_3"]] <- list(
        path = sprintf("M %.4f %.4f %s", v3[1], v3[2], fallback_path),
        neighbor_id = -1,
        is_boundary = TRUE
      )
    }

    path <- paste0(path, "Z")

    # Parse segments for rendering
    parsed_segments <- tryCatch(
      parse_svg_path(path),
      error = function(e) NULL
    )

    # Determine if this is a boundary piece
    is_boundary_piece <- any(tri_verts <= n_corner)

    pieces[[tri_id]] <- list(
      id = tri_id,
      path = path,
      parsed_segments = parsed_segments,
      edge_segments = edge_segments,  # Neighbor-keyed edge paths for fusion
      center = center,
      random_pos = list(
        vertices = list(v1, v2, v3),
        vertex_indices = tri_verts
      ),
      type = "random",
      is_boundary = is_boundary_piece,
      fusion_group = NA,
      fused_edges = list(),
      fused_neighbor_ids = list()
    )
  }

  pieces
}

# Random positioning is now handled by apply_tessellation_positioning() in piece_positioning.R

# ============================================================================
# Fusion Support
# ============================================================================

#' Process fusion groups for random shape puzzle
#'
#' @param puzzle_result Puzzle result
#' @param fusion_groups Fusion specification
#' @param fusion_style Style for fused edges
#' @param fusion_opacity Opacity for fusion regions
#' @return Updated fusion data
#'
#' @keywords internal
process_random_fusion <- function(puzzle_result, fusion_groups, fusion_style, fusion_opacity) {
  # Fusion support to be added later
  NULL
}

# Random neighbors are now handled by get_tessellation_neighbors() in adjacency_api.R

