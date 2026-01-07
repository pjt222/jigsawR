# Tessellation Edge Generation
#
# Shared utilities for generating bezier edges in tessellation-based puzzles
# (Voronoi, Random shapes, etc.)
#
# This module provides:
# - Generic edge generation between arbitrary points
# - Edge map construction for adjacent cells
# - Boundary edge handling
# - Fermat spiral point generation

# ============================================================================
# Edge Generation
# ============================================================================

#' Generate bezier edge with tab between two arbitrary points
#'
#' Creates a bezier curve path with interlocking tab between two vertices.
#' This is the core function for tessellation-based puzzles where edges
#' can be at any angle (not just horizontal/vertical).
#'
#' @param v1 Start vertex c(x, y)
#' @param v2 End vertex c(x, y)
#' @param seed Random seed for reproducibility
#' @param edge_id Unique edge identifier
#' @param tabsize Tab size percentage (default: 20)
#' @param jitter Jitter percentage (default: 4)
#' @param tab_direction 1 for tab pointing "left" of edge direction,
#'   -1 for "right" (default: 1)
#' @param min_tab_size Minimum absolute tab size in units (default: NULL for no limit).
#'   Prevents tabs from becoming too small on short edges.
#' @param max_tab_size Maximum absolute tab size in units (default: NULL for no limit).
#'   Prevents tabs from becoming too large on long edges and overlapping.
#' @return List with forward and reverse SVG paths, plus metadata
#'
#' @details
#' Uses 3 cubic bezier curves (9 control points) to create a smooth
#' interlocking tab pattern. The tab direction is determined by the
#' `tab_direction` parameter, ensuring adjacent cells have complementary
#' tabs that interlock.
#'
#' For two adjacent cells A and B sharing an edge:
#' - Cell A uses the `forward` path
#' - Cell B uses the `reverse` path
#' The paths are mathematical inverses, creating perfect interlocking.
#'
#' Tab Size Constraints:
#' The actual tab height is approximately `3 * t * edge_length` where t is
#' the tab fraction. With min/max constraints:
#' - If computed tab would be smaller than min_tab_size, it's clamped up
#' - If computed tab would be larger than max_tab_size, it's clamped down
#' - Very short edges (where even min_tab_size would cause overlap) get
#'   straight lines instead of tabs
#'
#' @examples
#' # Horizontal edge
#' edge <- generate_tessellation_edge(c(0, 0), c(100, 0), seed = 42, edge_id = 1)
#' cat(edge$forward)
#'
#' # Diagonal edge with tab size constraints
#' edge <- generate_tessellation_edge(c(0, 0), c(70, 70), seed = 42, edge_id = 2,
#'                                     min_tab_size = 10, max_tab_size = 30)
#'
#' @export
generate_tessellation_edge <- function(v1, v2, seed, edge_id,
                                        tabsize = 20, jitter = 4,
                                        tab_direction = 1,
                                        min_tab_size = NULL,
                                        max_tab_size = NULL) {

  # Calculate edge vector and length
  dx <- v2[1] - v1[1]
  dy <- v2[2] - v1[2]
  edge_length <- sqrt(dx^2 + dy^2)

  # Handle degenerate edges

  if (edge_length < 0.001) {
    return(list(
      forward = sprintf("L %.4f %.4f", v2[1], v2[2]),
      reverse = sprintf("L %.4f %.4f", v1[1], v1[2]),
      start = v1,
      end = v2,
      type = "degenerate"
    ))
  }

  # Unit vectors along edge (tangent) and perpendicular (normal)
  tangent <- c(dx / edge_length, dy / edge_length)
  # Rotate 90 degrees counterclockwise, apply tab_direction
  normal <- c(-tangent[2], tangent[1]) * tab_direction

  # Tab parameters (convert percentages to fractions)
  t_base <- tabsize / 100
  j <- jitter / 100

  # Generate 6 random values using unified batch RNG (Rcpp with R fallback)
  # Uses seed + edge_id for determinism - same edge always gets same tab shape
  rng_vals <- uniform_batch(seed + edge_id, 6)

  # Random tab parameters (same pattern as rectangular puzzles)
  t <- t_base * (0.8 + 0.4 * rng_vals[1])    # Tab size with variation
  a <- j * (rng_vals[2] - 0.5)                # Start jitter
  b <- j * (rng_vals[3] - 0.5)                # Tab position jitter
  c_val <- j * (rng_vals[4] - 0.5)            # Tab offset jitter
  d <- j * (rng_vals[5] - 0.5)                # Tab width jitter
  e <- j * (rng_vals[6] - 0.5)                # End jitter

  # Apply min/max tab size constraints

  # The tab height is approximately 3 * t * edge_length
  # The tab width spans approximately 4 * t * edge_length (from 0.5-2t to 0.5+2t)
  tab_height <- 3.0 * t * edge_length

  if (!is.null(min_tab_size) && tab_height < min_tab_size) {
    # Tab would be too small - scale up
    t <- min_tab_size / (3.0 * edge_length)

    # Check if this would make the tab too wide for the edge
    # Tab spans from 0.5 - 2t to 0.5 + 2t, so total width is 4t
    # It should fit within roughly 0.1 to 0.9 of the edge (leaving margins)
    if (4.0 * t > 0.7) {
      # Edge is too short for even a minimum tab - use straight line
      return(list(
        forward = sprintf("L %.4f %.4f", v2[1], v2[2]),
        reverse = sprintf("L %.4f %.4f", v1[1], v1[2]),
        start = v1,
        end = v2,
        type = "straight_constrained"
      ))
    }
  }

  if (!is.null(max_tab_size) && tab_height > max_tab_size) {
    # Tab would be too large - scale down
    t <- max_tab_size / (3.0 * edge_length)
  }

  # Helper function: position along edge (0 to 1)
  l <- function(frac) {
    v1 + tangent * (frac * edge_length)
  }

  # Helper function: perpendicular offset
  w <- function(offset) {
    normal * (offset * edge_length)
  }

  # Generate 9 control points (bezier curve pattern)
  # Points 1-3: first bezier (approach to tab)
  # Points 4-6: second bezier (tab bulge)
  # Points 7-9: third bezier (exit from tab)
  p1 <- l(0.2) + w(a)
  p2 <- l(0.5 + b + d) + w(-t + c_val)
  p3 <- l(0.5 - t + b) + w(t + c_val)
  p4 <- l(0.5 - 2.0 * t + b - d) + w(3.0 * t + c_val)
  p5 <- l(0.5 + 2.0 * t + b - d) + w(3.0 * t + c_val)
  p6 <- l(0.5 + t + b) + w(t + c_val)
  p7 <- l(0.5 + b + d) + w(-t + c_val)
  p8 <- l(0.8) + w(e)
  p9 <- l(1.0) + w(0.0)  # End point (equals v2)

  # Build forward path (v1 to v2) - 3 cubic bezier curves
  forward <- sprintf("C %.4f %.4f %.4f %.4f %.4f %.4f ",
                     p1[1], p1[2], p2[1], p2[2], p3[1], p3[2])
  forward <- paste0(forward, sprintf("C %.4f %.4f %.4f %.4f %.4f %.4f ",
                                     p4[1], p4[2], p5[1], p5[2], p6[1], p6[2]))
  forward <- paste0(forward, sprintf("C %.4f %.4f %.4f %.4f %.4f %.4f",
                                     p7[1], p7[2], p8[1], p8[2], p9[1], p9[2]))

  # Build reverse path (v2 to v1) - same curves reversed
  # For bezier P0 to P3 with control points P1, P2:
  # Reverse is P3 to P0 with control points P2, P1
  reverse <- sprintf("C %.4f %.4f %.4f %.4f %.4f %.4f ",
                     p8[1], p8[2], p7[1], p7[2], p6[1], p6[2])
  reverse <- paste0(reverse, sprintf("C %.4f %.4f %.4f %.4f %.4f %.4f ",
                                     p5[1], p5[2], p4[1], p4[2], p3[1], p3[2]))
  reverse <- paste0(reverse, sprintf("C %.4f %.4f %.4f %.4f %.4f %.4f",
                                     p2[1], p2[2], p1[1], p1[2], v1[1], v1[2]))

  return(list(
    forward = forward,
    reverse = reverse,
    start = v1,
    end = v2,
    type = "tab"
  ))
}

#' Generate a straight boundary edge
#'
#' Creates a simple line segment for boundary edges (no tabs).
#'
#' @param v1 Start vertex c(x, y)
#' @param v2 End vertex c(x, y)
#' @return List with forward and reverse SVG paths
#'
#' @export
generate_straight_edge <- function(v1, v2) {
  list(
    forward = sprintf("L %.4f %.4f", v2[1], v2[2]),
    reverse = sprintf("L %.4f %.4f", v1[1], v1[2]),
    start = v1,
    end = v2,
    type = "boundary"
  )
}

# ============================================================================
# Edge Map Construction
# ============================================================================

#' Build edge map for tessellation
#'
#' Creates a map of all shared edges between adjacent cells in a tessellation.
#' Each edge is generated once and stored with both forward and reverse paths
#' for use by the two adjacent cells.
#'
#' @param adjacency Data frame with columns: cell_a, cell_b, v1_x, v1_y, v2_x, v2_y
#'   Each row represents an edge between two adjacent cells
#' @param seed Base seed for reproducibility
#' @param tabsize Tab size percentage
#' @param jitter Jitter percentage
#' @param boundary_cells Optional vector of cell IDs that are on boundary
#' @return Named list of edge paths, keyed by "E{min_id}-{max_id}"
#'
#' @details
#' Edge keys use the pattern "E{smaller_id}-{larger_id}" to ensure
#' consistent lookup regardless of which cell queries the edge.
#'
#' @export
build_tessellation_edge_map <- function(adjacency, seed, tabsize = 20, jitter = 4,
                                         boundary_cells = NULL) {
  edge_map <- list()

  for (i in seq_len(nrow(adjacency))) {
    row <- adjacency[i, ]
    cell_a <- row$cell_a
    cell_b <- row$cell_b
    v1 <- c(row$v1_x, row$v1_y)
    v2 <- c(row$v2_x, row$v2_y)

    # Create canonical edge key (smaller ID first)
    min_id <- min(cell_a, cell_b)
    max_id <- max(cell_a, cell_b)
    edge_key <- sprintf("E%d-%d", min_id, max_id)

    # Skip if already generated
    if (!is.null(edge_map[[edge_key]])) {
      next
    }

    # Generate edge ID for deterministic tab generation
    edge_id <- min_id * 10000 + max_id

    # Determine if this is a boundary edge (cell_b == -1 or NA)
    is_boundary <- is.na(cell_b) || cell_b < 0

    if (is_boundary) {
      edge_map[[edge_key]] <- generate_straight_edge(v1, v2)
    } else {
      # Tab direction: consistent for same edge, complementary for reverse
      tab_direction <- if (cell_a == min_id) 1 else -1
      edge_map[[edge_key]] <- generate_tessellation_edge(
        v1, v2, seed, edge_id,
        tabsize = tabsize,
        jitter = jitter,
        tab_direction = tab_direction
      )
    }
  }

  return(edge_map)
}

#' Get edge path for a cell
#'
#' Retrieves the appropriate edge path (forward or reverse) for a specific
#' cell from the edge map.
#'
#' @param edge_map Edge map from build_tessellation_edge_map()
#' @param cell_id ID of the cell requesting the edge
#' @param neighbor_id ID of the adjacent cell (or -1 for boundary)
#' @return SVG path string for the edge
#'
#' @export
get_edge_path <- function(edge_map, cell_id, neighbor_id) {
  # Handle boundary edges
  if (is.na(neighbor_id) || neighbor_id < 0) {
    edge_key <- sprintf("E%d-boundary", cell_id)
    edge <- edge_map[[edge_key]]
    if (is.null(edge)) {
      return(NULL)
    }
    return(edge$forward)
  }

  # Get canonical edge key
  min_id <- min(cell_id, neighbor_id)
  max_id <- max(cell_id, neighbor_id)
  edge_key <- sprintf("E%d-%d", min_id, max_id)

  edge <- edge_map[[edge_key]]
  if (is.null(edge)) {
    return(NULL)
  }

  # Return forward path if cell_id is the smaller ID, reverse otherwise
  if (cell_id == min_id) {
    return(edge$forward)
  } else {
    return(edge$reverse)
  }
}

# ============================================================================
# Point Generation for Voronoi
# ============================================================================

#' Generate Fermat spiral points
#'
#' Creates points distributed on a Fermat spiral (also known as golden spiral).
#' This provides a visually pleasing, relatively uniform distribution that
#' works well for Voronoi tessellation.
#'
#' @param n Number of points to generate
#' @param size Canvas dimensions c(height, width) to match grid c(rows, cols), or c(diameter) for circular
#' @param seed Random seed (for small perturbation if desired)
#' @param center Optional center point c(x, y), default is center of size
#' @return Data frame with x and y columns
#'
#' @details
#' Uses the formula:
#' - r = c * sqrt(i)
#' - theta = i * golden_angle
#'
#' Where golden_angle = pi * (3 - sqrt(5)) radians
#'
#' @examples
#' # Generate 50 points in a 300x300 area
#' pts <- generate_fermat_points(50, c(300, 300))
#' plot(pts$x, pts$y, asp = 1)
#'
#' @export
generate_fermat_points <- function(n, size, seed = NULL, center = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Golden angle in radians
  golden_angle <- pi * (3 - sqrt(5))

  # Determine scale factor based on size
  if (length(size) == 1) {
    # Circular: size is diameter
    radius <- size / 2
    scale <- radius / sqrt(n)
    if (is.null(center)) {
      center <- c(size / 2, size / 2)
    }
  } else {
    # Rectangular: size is c(height, width) to match grid c(rows, cols)
    radius <- min(size) / 2
    scale <- radius / sqrt(n)
    if (is.null(center)) {
      # center is c(x, y) = c(width/2, height/2) = c(size[2]/2, size[1]/2)
      center <- c(size[2] / 2, size[1] / 2)
    }
  }

  # Generate spiral points
  i <- seq_len(n)
  r <- scale * sqrt(i)
  theta <- i * golden_angle

  # Convert to Cartesian coordinates, centered
  x <- center[1] + r * cos(theta)
  y <- center[2] + r * sin(theta)

  data.frame(x = x, y = y)
}

#' Generate uniform random points
#'
#' Creates uniformly distributed random points within a rectangle.
#'
#' @param n Number of points
#' @param size Canvas dimensions c(height, width) to match grid c(rows, cols)
#' @param seed Random seed
#' @return Data frame with x and y columns
#'
#' @export
generate_uniform_points <- function(n, size, seed) {
  set.seed(seed)
  # size = c(height, width), so width = size[2], height = size[1]
  x <- runif(n, min = 0, max = size[2])
  y <- runif(n, min = 0, max = size[1])

  data.frame(x = x, y = y)
}

#' Generate jittered grid points
#'
#' Creates points on a regular grid with random jitter applied.
#' This provides a more uniform distribution than pure random while
#' maintaining some irregularity.
#'
#' @param grid Grid dimensions c(cols, rows) or c(n) for auto-layout
#' @param size Canvas dimensions c(height, width)
#' @param seed Random seed
#' @param jitter_amount Jitter as fraction of cell size (default: 0.3)
#' @return Data frame with x and y columns
#'
#' @export
generate_jittered_grid_points <- function(grid, size, seed, jitter_amount = 0.3) {
  set.seed(seed)
  # size = c(height, width), so width = size[2], height = size[1]
  width <- size[2]
  height <- size[1]

  if (length(grid) == 1) {
    # Auto-calculate grid dimensions
    n <- grid
    aspect <- width / height
    cols <- round(sqrt(n * aspect))
    rows <- ceiling(n / cols)
    grid <- c(cols, rows)
  }

  cols <- grid[1]
  rows <- grid[2]
  n <- cols * rows

  cell_width <- width / cols
  cell_height <- height / rows

  # Generate grid centers
  x_centers <- (seq_len(cols) - 0.5) * cell_width
  y_centers <- (seq_len(rows) - 0.5) * cell_height

  # Expand to all points
  x <- rep(x_centers, times = rows)
  y <- rep(y_centers, each = cols)

  # Add jitter
  jitter_x <- runif(n, -jitter_amount, jitter_amount) * cell_width
  jitter_y <- runif(n, -jitter_amount, jitter_amount) * cell_height

  x <- x + jitter_x
  y <- y + jitter_y

  # Clamp to bounds
  x <- pmax(0.01 * width, pmin(0.99 * width, x))
  y <- pmax(0.01 * height, pmin(0.99 * height, y))

  data.frame(x = x, y = y)
}

# ============================================================================
# Point Generation for Random Shapes
# ============================================================================

#' Generate base polygon vertices
#'
#' Creates vertices for a regular polygon with n corners.
#'
#' @param n_corner Number of corners (3=triangle, 4=rectangle, 5=pentagon, etc.)
#' @param size Dimensions c(height, width) to match grid c(rows, cols)
#' @param center Optional center point c(x, y)
#' @return List with vertices (matrix) and constraint_edges (matrix)
#'
#' @details
#' For n_corner = 4, creates a rectangle (not a rotated square).
#' For other values, creates a regular polygon inscribed in an ellipse.
#'
#' @export
generate_base_polygon <- function(n_corner, size, center = NULL) {
  # size = c(height, width), so width = size[2], height = size[1]
  width <- size[2]
  height <- size[1]

  if (is.null(center)) {
    center <- c(width / 2, height / 2)
  }

  if (n_corner == 4) {
    # Special case: rectangle aligned with axes
    vertices <- matrix(c(
      0, 0,
      width, 0,
      width, height,
      0, height
    ), ncol = 2, byrow = TRUE)
  } else {
    # Regular polygon inscribed in ellipse
    angles <- seq(0, 2 * pi, length.out = n_corner + 1)[1:n_corner]
    # Start from top for visual consistency
    angles <- angles - pi / 2

    rx <- width / 2 * 0.95  # Slight inset
    ry <- height / 2 * 0.95

    x <- center[1] + rx * cos(angles)
    y <- center[2] + ry * sin(angles)

    vertices <- cbind(x, y)
  }

  # Create constraint edges (sequential pairs)
  n <- nrow(vertices)
  constraint_edges <- cbind(
    1:n,
    c(2:n, 1)
  )

  list(
    vertices = vertices,
    constraint_edges = constraint_edges,
    n_corner = n_corner
  )
}

#' Generate interior points for random shape puzzle
#'
#' Creates random points inside a polygon boundary.
#'
#' @param n_points Approximate number of interior points
#' @param boundary Polygon boundary from generate_base_polygon()
#' @param seed Random seed
#' @param min_distance Minimum distance between points (default: NULL for auto)
#' @return Matrix of interior point coordinates
#'
#' @details
#' Uses rejection sampling to generate points inside the polygon.
#' Points are guaranteed to be inside the boundary and at least
#' min_distance apart (Poisson disk sampling variant).
#'
#' @export
generate_interior_points <- function(n_points, boundary, seed,
                                      min_distance = NULL) {
  set.seed(seed)

  vertices <- boundary$vertices
  n_corner <- boundary$n_corner

  # Calculate bounding box
  min_x <- min(vertices[, 1])
  max_x <- max(vertices[, 1])
  min_y <- min(vertices[, 2])
  max_y <- max(vertices[, 2])

  # Auto-calculate min_distance if not provided
  if (is.null(min_distance)) {
    area <- (max_x - min_x) * (max_y - min_y) * 0.7  # Approximate polygon area
    min_distance <- sqrt(area / n_points) * 0.5
  }

  # Point-in-polygon test (ray casting)
  point_in_polygon <- function(pt, poly) {
    n <- nrow(poly)
    inside <- FALSE

    j <- n
    for (i in 1:n) {
      xi <- poly[i, 1]
      yi <- poly[i, 2]
      xj <- poly[j, 1]
      yj <- poly[j, 2]

      if (((yi > pt[2]) != (yj > pt[2])) &&
          (pt[1] < (xj - xi) * (pt[2] - yi) / (yj - yi) + xi)) {
        inside <- !inside
      }
      j <- i
    }
    inside
  }

  # Generate points with rejection sampling
  points <- matrix(nrow = 0, ncol = 2)
  max_attempts <- n_points * 100

  for (attempt in seq_len(max_attempts)) {
    if (nrow(points) >= n_points) break

    # Generate candidate point
    px <- runif(1, min_x + min_distance, max_x - min_distance)
    py <- runif(1, min_y + min_distance, max_y - min_distance)
    candidate <- c(px, py)

    # Check if inside polygon
    if (!point_in_polygon(candidate, vertices)) next

    # Check minimum distance from existing points
    if (nrow(points) > 0) {
      distances <- sqrt((points[, 1] - px)^2 + (points[, 2] - py)^2)
      if (any(distances < min_distance)) next
    }

    # Check minimum distance from boundary vertices
    vertex_distances <- sqrt((vertices[, 1] - px)^2 + (vertices[, 2] - py)^2)
    if (any(vertex_distances < min_distance * 0.5)) next

    points <- rbind(points, candidate)
  }

  points
}

# ============================================================================
# Utility Functions
# ============================================================================

#' Check if deldir package is available
#'
#' @return TRUE if deldir is installed, FALSE otherwise
#' @keywords internal
has_deldir <- function() {
  requireNamespace("deldir", quietly = TRUE)
}

#' Check if RCDT package is available
#'
#' @return TRUE if RCDT is installed, FALSE otherwise
#' @keywords internal
has_rcdt <- function() {
  requireNamespace("RCDT", quietly = TRUE)
}
