# SNIC Superpixel Puzzle Implementation
#
# Generates puzzle pieces using SNIC (Simple Non-Iterative Clustering)
# superpixel segmentation. Unlike geometric puzzle types, SNIC creates
# image-aware pieces where boundaries follow natural features in a photograph.
#
# Implements Issue #80: SNIC superpixel puzzle type

# ============================================================================
# Dependency Check
# ============================================================================

#' Check if snic package is available
#'
#' @return TRUE if snic is installed, FALSE otherwise
#' @keywords internal
has_snic <- function() {
  requireNamespace("snic", quietly = TRUE)
}

#' Check if magick package is available
#'
#' @return TRUE if magick is installed, FALSE otherwise
#' @keywords internal
has_magick <- function() {

  requireNamespace("magick", quietly = TRUE)
}

# ============================================================================
# Image Loading
# ============================================================================

#' Load image for SNIC segmentation
#'
#' Loads an image via magick and converts to a numeric array suitable for
#' the snic package.
#'
#' @param image_path Path to the image file (PNG, JPEG, etc.)
#' @return List with:
#'   - array: numeric array (H, W, 3) with values in 0-1
#'   - width: image width in pixels
#'   - height: image height in pixels
#'
#' @keywords internal
load_image_for_snic <- function(image_path) {
  if (!has_magick()) {
    cli::cli_abort(c(
      "Package {.pkg magick} is required for SNIC puzzles.",
      "i" = "Install with: {.code install.packages('magick')}"
    ))
  }

  if (!file.exists(image_path)) {
    cli::cli_abort("Image file not found: {.file {image_path}}")
  }

  img <- magick::image_read(image_path)
  info <- magick::image_info(img)
  img_width <- info$width
  img_height <- info$height

  # Convert to raw array (H x W x channels)
  arr <- as.numeric(magick::image_data(img, channels = "rgb")) / 255
  dim(arr) <- c(3, img_width, img_height)
  # Rearrange from (channels, W, H) to (H, W, channels)
  arr <- aperm(arr, c(3, 2, 1))

  list(
    array = arr,
    width = img_width,
    height = img_height
  )
}

# ============================================================================
# Main Piece Generation
# ============================================================================

#' Generate SNIC puzzle pieces
#'
#' Creates puzzle pieces using SNIC superpixel segmentation. Boundaries follow
#' natural features in the input image, creating image-aware puzzle pieces.
#'
#' @param seed Random seed for reproducibility
#' @param grid Number of target superpixels (single value)
#' @param size Canvas dimensions c(height, width) in mm
#' @param image_path Path to the image file
#' @param tabsize Tab size percentage (default: 10)
#' @param jitter Jitter percentage for tabs (default: 2)
#' @param compactness SNIC compactness parameter (higher = more regular shapes)
#' @param seed_type Seed grid type: "hexagonal", "rectangular", "diamond", "random"
#' @param min_tab_size Minimum absolute tab size (optional)
#' @param max_tab_size Maximum absolute tab size (optional)
#' @param fusion_groups Fusion group specification
#' @param fusion_style Style for fused edges
#' @param fusion_opacity Opacity for fusion regions
#' @return List with pieces, canvas_size, type, parameters
#'
#' @keywords internal
generate_snic_pieces_internal <- function(seed, grid, size, image_path,
                                           tabsize = 10, jitter = 2,
                                           compactness = 0.5,
                                           seed_type = "hexagonal",
                                           min_tab_size = NULL,
                                           max_tab_size = NULL,
                                           fusion_groups = NULL,
                                           fusion_style = "none",
                                           fusion_opacity = 1.0) {

  # Check dependencies

  if (!has_snic()) {
    cli::cli_abort(c(
      "Package {.pkg snic} is required for SNIC puzzles.",
      "i" = "Install with: {.code install.packages('snic')}"
    ))
  }
  # Determine target superpixel count
  n_target <- if (length(grid) == 1) grid[1] else grid[1]

  # Ensure size is c(height, width)
  if (length(size) == 1) {
    size <- c(size, size)
  }

  if (is.null(image_path) || !nzchar(image_path)) {
    # Synthetic mode: uniform canvas -> regular superpixel grid
    # Use 2 pixels per mm for reasonable resolution
    px_per_mm <- 2
    img_height <- max(round(size[1] * px_per_mm), 20)
    img_width  <- max(round(size[2] * px_per_mm), 20)
    arr <- array(0.5, dim = c(img_height, img_width, 3))
  } else {
    # Image mode: load actual image
    if (!has_magick()) {
      cli::cli_abort(c(
        "Package {.pkg magick} is required for image-based SNIC puzzles.",
        "i" = "Install with: {.code install.packages('magick')}"
      ))
    }
    cli::cli_progress_step("Loading image: {.file {basename(image_path)}}")
    img_data <- load_image_for_snic(image_path)
    img_height <- img_data$height
    img_width  <- img_data$width
    arr <- img_data$array
  }

  # Compute SNIC spacing from target piece count
  spacing <- round(sqrt(img_width * img_height / n_target))

  # Generate SNIC seeds
  cli::cli_progress_step("Generating SNIC seeds (spacing={spacing}, type={seed_type})...")
  seeds <- snic::snic_grid(arr, type = seed_type, spacing = spacing)

  # Run SNIC segmentation
  cli::cli_progress_step("Running SNIC segmentation (compactness={compactness})...")
  snic_result <- snic::snic(arr, seeds, compactness)

  # Extract label matrix (ensure 2D; some platforms return 3D array)
  labels <- snic::snic_get_seg(snic_result)
  if (is.array(labels) && length(dim(labels)) == 3) {
    labels <- labels[, , 1]
  }
  if (!is.matrix(labels)) {
    labels <- matrix(as.integer(labels), nrow = img_height, ncol = img_width)
  }
  actual_n_cells <- length(unique(as.vector(labels)))
  cli::cli_progress_step("Segmentation complete: {actual_n_cells} superpixels")

  # Extract polygon boundaries
  cli::cli_progress_step("Extracting superpixel boundaries...")
  polygon_data <- extract_snic_polygons(labels, size)
  tiles <- polygon_data$tiles
  adjacency <- polygon_data$adjacency

  # Build edge map with tabs
  cli::cli_progress_step("Building edge map with tabs...")
  edge_map <- build_snic_edge_map(tiles, adjacency, seed, tabsize, jitter,
                                   min_tab_size = min_tab_size,
                                   max_tab_size = max_tab_size)

  # Assemble pieces
  cli::cli_progress_step("Assembling {length(tiles)} SNIC pieces...")
  pieces <- assemble_snic_pieces(tiles, edge_map, adjacency, size)

  # Store parameters
  parameters <- list(
    seed = seed,
    grid = grid,
    size = size,
    tabsize = tabsize,
    jitter = jitter,
    n_target = n_target,
    actual_n_cells = actual_n_cells,
    compactness = compactness,
    seed_type = seed_type,
    image_path = image_path,
    image_width = img_width,
    image_height = img_height,
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
      type = "snic",
      parameters = parameters
    )
    fusion_data <- process_snic_fusion(puzzle_result, fusion_groups,
                                        fusion_style, fusion_opacity)
    if (!is.null(fusion_data)) {
      pieces <- fusion_data$pieces
    }
  }

  list(
    pieces = pieces,
    canvas_size = size,
    canvas_offset = c(0, 0),
    type = "snic",
    parameters = parameters,
    fusion_data = fusion_data,
    adjacency = adjacency,
    edge_map = edge_map
  )
}

# ============================================================================
# Boundary Extraction
# ============================================================================

#' Extract SNIC polygons from label matrix
#'
#' Orchestrates boundary extraction from a superpixel label matrix.
#' Converts pixel-level labels to polygon tiles with adjacency information.
#'
#' @param labels Integer matrix (H x W) of superpixel labels
#' @param size Canvas dimensions c(height, width) in mm
#' @return List with tiles, adjacency, junction_vertices
#'
#' @keywords internal
extract_snic_polygons <- function(labels, size) {
  img_height <- nrow(labels)
  img_width <- ncol(labels)

  # Scale factors: map pixel coordinates to mm
  scale_x <- size[2] / img_width
  scale_y <- size[1] / img_height

  # Find junction vertices
  junctions <- find_junction_vertices(labels)

  # Build boundary segments
  boundary_segs <- build_boundary_segments(labels)

  # Trace edges between junction vertices
  edge_data <- trace_snic_edges(boundary_segs, junctions)

  # Build tile polygons
  tiles <- build_snic_tiles(labels, junctions, size)

  # Build adjacency data frame from edge_data
  adjacency <- build_snic_adjacency(edge_data, tiles, size)

  list(
    tiles = tiles,
    adjacency = adjacency,
    junction_vertices = junctions
  )
}

#' Find junction vertices in label matrix
#'
#' Scans all (W+1) x (H+1) grid intersection points. A junction vertex
#' occurs where 3+ unique labels meet at an interior point, or 2+ labels
#' meet at a border point, or at image corners.
#'
#' @param labels Integer matrix (H x W) of superpixel labels
#' @return Data frame with columns: gx, gy, n_labels (grid coordinates)
#'
#' @keywords internal
find_junction_vertices <- function(labels) {
  img_height <- nrow(labels)
  img_width <- ncol(labels)

  junctions <- list()

  # Scan all grid intersection points (0..W, 0..H)
  for (gy in 0:img_height) {
    for (gx in 0:img_width) {
      # Collect labels of up to 4 surrounding pixels
      surrounding_labels <- integer(0)

      # Top-left pixel: (gy-1, gx-1) in matrix coords (row, col)
      if (gy > 0 && gx > 0) {
        surrounding_labels <- c(surrounding_labels, labels[gy, gx])
      }
      # Top-right pixel: (gy-1, gx) in matrix coords
      if (gy > 0 && gx < img_width) {
        surrounding_labels <- c(surrounding_labels, labels[gy, gx + 1])
      }
      # Bottom-left pixel: (gy, gx-1) in matrix coords
      if (gy < img_height && gx > 0) {
        surrounding_labels <- c(surrounding_labels, labels[gy + 1, gx])
      }
      # Bottom-right pixel: (gy, gx) in matrix coords
      if (gy < img_height && gx < img_width) {
        surrounding_labels <- c(surrounding_labels, labels[gy + 1, gx + 1])
      }

      unique_labels <- unique(surrounding_labels)
      n_unique <- length(unique_labels)
      is_border <- (gx == 0 || gx == img_width || gy == 0 || gy == img_height)
      is_corner <- (gx == 0 || gx == img_width) && (gy == 0 || gy == img_height)

      # Junction if: interior with 3+ labels, border with 2+ labels, or corner
      is_junction <- FALSE
      if (is_corner) {
        is_junction <- TRUE
      } else if (is_border && n_unique >= 2) {
        is_junction <- TRUE
      } else if (!is_border && n_unique >= 3) {
        is_junction <- TRUE
      }

      if (is_junction) {
        junctions[[length(junctions) + 1]] <- data.frame(
          gx = gx, gy = gy, n_labels = n_unique,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(junctions) == 0) {
    return(data.frame(gx = integer(), gy = integer(), n_labels = integer(),
                      stringsAsFactors = FALSE))
  }

  do.call(rbind, junctions)
}

#' Build boundary segments between adjacent pixels with different labels
#'
#' For each pair of horizontally or vertically adjacent pixels with
#' different labels, record the boundary segment between their shared
#' grid points. Tags each segment with (label_a, label_b).
#'
#' @param labels Integer matrix (H x W) of superpixel labels
#' @return Data frame with columns: x1, y1, x2, y2, label_a, label_b
#'
#' @keywords internal
build_boundary_segments <- function(labels) {
  img_height <- nrow(labels)
  img_width <- ncol(labels)

  segments <- list()

  # Horizontal boundaries (between vertically adjacent pixels)
  for (row in 1:(img_height - 1)) {
    for (col in 1:img_width) {
      if (labels[row, col] != labels[row + 1, col]) {
        la <- min(labels[row, col], labels[row + 1, col])
        lb <- max(labels[row, col], labels[row + 1, col])
        segments[[length(segments) + 1]] <- data.frame(
          x1 = col - 1, y1 = row, x2 = col, y2 = row,
          label_a = la, label_b = lb,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  # Vertical boundaries (between horizontally adjacent pixels)
  for (row in 1:img_height) {
    for (col in 1:(img_width - 1)) {
      if (labels[row, col] != labels[row, col + 1]) {
        la <- min(labels[row, col], labels[row, col + 1])
        lb <- max(labels[row, col], labels[row, col + 1])
        segments[[length(segments) + 1]] <- data.frame(
          x1 = col, y1 = row - 1, x2 = col, y2 = row,
          label_a = la, label_b = lb,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  # Image border segments (top, bottom, left, right boundaries)
  # Top edge
  for (col in 1:img_width) {
    segments[[length(segments) + 1]] <- data.frame(
      x1 = col - 1, y1 = 0, x2 = col, y2 = 0,
      label_a = labels[1, col], label_b = -1L,
      stringsAsFactors = FALSE
    )
  }
  # Bottom edge
  for (col in 1:img_width) {
    segments[[length(segments) + 1]] <- data.frame(
      x1 = col - 1, y1 = img_height, x2 = col, y2 = img_height,
      label_a = labels[img_height, col], label_b = -1L,
      stringsAsFactors = FALSE
    )
  }
  # Left edge
  for (row in 1:img_height) {
    segments[[length(segments) + 1]] <- data.frame(
      x1 = 0, y1 = row - 1, x2 = 0, y2 = row,
      label_a = labels[row, 1], label_b = -1L,
      stringsAsFactors = FALSE
    )
  }
  # Right edge
  for (row in 1:img_height) {
    segments[[length(segments) + 1]] <- data.frame(
      x1 = img_width, y1 = row - 1, x2 = img_width, y2 = row,
      label_a = labels[row, img_width], label_b = -1L,
      stringsAsFactors = FALSE
    )
  }

  if (length(segments) == 0) {
    return(data.frame(x1 = numeric(), y1 = numeric(),
                      x2 = numeric(), y2 = numeric(),
                      label_a = integer(), label_b = integer(),
                      stringsAsFactors = FALSE))
  }

  do.call(rbind, segments)
}

#' Trace SNIC edges between junction vertices
#'
#' For each label pair, chains boundary segments from junction vertex to
#' junction vertex. Each chain becomes one edge in the adjacency data.
#'
#' @param boundary_segments Data frame from build_boundary_segments()
#' @param junction_vertices Data frame from find_junction_vertices()
#' @return List of edges, each with: cell_a, cell_b, vertices (ordered points)
#'
#' @keywords internal
trace_snic_edges <- function(boundary_segments, junction_vertices) {
  if (nrow(boundary_segments) == 0) {
    return(list())
  }

  # Build a set of junction points for fast lookup
  junction_set <- paste(junction_vertices$gx, junction_vertices$gy, sep = ",")

  # Group segments by label pair (use "|" separator to avoid ambiguity with negative labels)
  seg_key <- paste(boundary_segments$label_a, boundary_segments$label_b, sep = "|")
  seg_groups <- split(seq_len(nrow(boundary_segments)), seg_key)

  edges <- list()

  for (key in names(seg_groups)) {
    group_indices <- seg_groups[[key]]
    group_segs <- boundary_segments[group_indices, , drop = FALSE]

    labels_split <- strsplit(key, "\\|")[[1]]
    la <- as.integer(labels_split[1])
    lb <- as.integer(labels_split[2])

    # Build adjacency graph of segment endpoints
    # Each segment is an edge in a graph of grid points
    adj <- list()
    for (i in seq_len(nrow(group_segs))) {
      p1 <- paste(group_segs$x1[i], group_segs$y1[i], sep = ",")
      p2 <- paste(group_segs$x2[i], group_segs$y2[i], sep = ",")
      adj[[p1]] <- c(adj[[p1]], p2)
      adj[[p2]] <- c(adj[[p2]], p1)
    }

    # Find all junction-to-junction chains
    visited_segs <- logical(nrow(group_segs))

    # Find all junction points that are endpoints in this group
    all_pts <- unique(c(
      paste(group_segs$x1, group_segs$y1, sep = ","),
      paste(group_segs$x2, group_segs$y2, sep = ",")
    ))
    group_junctions <- all_pts[all_pts %in% junction_set]

    # Also treat degree-1 endpoints (dead ends) as chain endpoints
    degree <- table(c(
      paste(group_segs$x1, group_segs$y1, sep = ","),
      paste(group_segs$x2, group_segs$y2, sep = ",")
    ))
    dead_ends <- names(degree)[degree == 1]
    chain_endpoints <- unique(c(group_junctions, dead_ends))

    # Trace chains starting from each junction/endpoint
    visited_points <- character(0)

    for (start_pt in chain_endpoints) {
      neighbors <- adj[[start_pt]]
      if (is.null(neighbors)) next

      for (next_pt in unique(neighbors)) {
        seg_id <- paste(sort(c(start_pt, next_pt)), collapse = "->")
        if (seg_id %in% visited_points) next

        # Trace chain from start_pt through next_pt
        chain <- c(start_pt)
        current <- next_pt
        prev <- start_pt

        while (TRUE) {
          chain <- c(chain, current)
          visited_points <- c(visited_points,
                             paste(sort(c(prev, current)), collapse = "->"))

          # Check if we've reached another junction/endpoint
          if (current %in% chain_endpoints && current != start_pt) {
            break
          }
          if (current == start_pt && length(chain) > 2) {
            break  # Closed loop
          }

          # Find next unvisited neighbor
          next_neighbors <- adj[[current]]
          next_neighbors <- next_neighbors[next_neighbors != prev]

          # Remove already visited
          unvisited <- next_neighbors[!paste(sort(c(rep(current, length(next_neighbors)),
                                                    next_neighbors)),
                                            collapse = "->") %in% visited_points]
          # Simpler check
          found_next <- FALSE
          for (nn in next_neighbors) {
            sid <- paste(sort(c(current, nn)), collapse = "->")
            if (!sid %in% visited_points) {
              prev <- current
              current <- nn
              found_next <- TRUE
              break
            }
          }

          if (!found_next) break
        }

        if (length(chain) >= 2) {
          # Parse chain points back to coordinates
          chain_coords <- do.call(rbind, lapply(chain, function(pt) {
            xy <- as.numeric(strsplit(pt, ",")[[1]])
            data.frame(x = xy[1], y = xy[2])
          }))

          edges[[length(edges) + 1]] <- list(
            cell_a = la,
            cell_b = lb,
            vertices = chain_coords
          )
        }
      }
    }
  }

  edges
}

#' Build SNIC tile polygons from label matrix
#'
#' For each superpixel, collects its junction vertices, orders them
#' counterclockwise (atan2 sort around centroid), and scales to puzzle
#' mm coordinates.
#'
#' @param labels Integer matrix (H x W) of superpixel labels
#' @param junctions Data frame from find_junction_vertices()
#' @param size Canvas dimensions c(height, width) in mm
#' @return List of tiles, each with x, y, bp (boundary point flags),
#'   centroid_x, centroid_y, pt (seed point)
#'
#' @keywords internal
build_snic_tiles <- function(labels, junctions, size) {
  img_height <- nrow(labels)
  img_width <- ncol(labels)
  scale_x <- size[2] / img_width
  scale_y <- size[1] / img_height

  unique_labels <- sort(unique(as.vector(labels)))

  # Build lookup: for each junction, which labels touch it
  junction_labels <- list()
  for (i in seq_len(nrow(junctions))) {
    gx <- junctions$gx[i]
    gy <- junctions$gy[i]

    surrounding <- integer(0)
    if (gy > 0 && gx > 0) surrounding <- c(surrounding, labels[gy, gx])
    if (gy > 0 && gx < img_width) surrounding <- c(surrounding, labels[gy, gx + 1])
    if (gy < img_height && gx > 0) surrounding <- c(surrounding, labels[gy + 1, gx])
    if (gy < img_height && gx < img_width) surrounding <- c(surrounding, labels[gy + 1, gx + 1])

    junction_labels[[i]] <- unique(surrounding)
  }

  tiles <- list()

  for (label_val in unique_labels) {
    # Find junction vertices that touch this label
    touching_indices <- which(sapply(junction_labels, function(jl) label_val %in% jl))

    if (length(touching_indices) < 3) {
      # Too few vertices for a polygon, skip
      next
    }

    verts_gx <- junctions$gx[touching_indices]
    verts_gy <- junctions$gy[touching_indices]

    # Compute centroid of the superpixel (from pixel locations)
    pixel_locs <- which(labels == label_val, arr.ind = TRUE)
    centroid_row <- mean(pixel_locs[, 1]) - 0.5  # Convert to grid coordinates
    centroid_col <- mean(pixel_locs[, 2]) - 0.5

    # Order vertices counterclockwise around centroid
    angles <- atan2(verts_gy - mean(verts_gy), verts_gx - mean(verts_gx))
    order_idx <- order(angles)

    ordered_gx <- verts_gx[order_idx]
    ordered_gy <- verts_gy[order_idx]

    # Scale to mm
    tile_x <- ordered_gx * scale_x
    tile_y <- ordered_gy * scale_y

    # Determine boundary points (on image edge)
    is_boundary <- (ordered_gx == 0 | ordered_gx == img_width |
                    ordered_gy == 0 | ordered_gy == img_height)

    tiles[[as.character(label_val)]] <- list(
      x = tile_x,
      y = tile_y,
      bp = is_boundary,
      centroid_x = centroid_col * scale_x,
      centroid_y = centroid_row * scale_y,
      pt = c(centroid_col * scale_x, centroid_row * scale_y),
      label = label_val
    )
  }

  # Re-index tiles as sequential list (1, 2, 3, ...)
  tile_list <- unname(tiles)
  # Store label-to-index mapping for adjacency building
  attr(tile_list, "label_map") <- setNames(
    seq_along(tiles),
    names(tiles)
  )

  tile_list
}

#' Build SNIC adjacency data frame
#'
#' Converts traced edges to a standard adjacency data frame with mm coordinates.
#'
#' @param edge_data List of edges from trace_snic_edges()
#' @param tiles Tile list from build_snic_tiles()
#' @param size Canvas dimensions c(height, width) in mm
#' @return Data frame with cell_a, cell_b, v1_x, v1_y, v2_x, v2_y columns
#'
#' @keywords internal
build_snic_adjacency <- function(edge_data, tiles, size) {
  if (length(edge_data) == 0) {
    return(data.frame(
      cell_a = integer(), cell_b = integer(),
      v1_x = numeric(), v1_y = numeric(),
      v2_x = numeric(), v2_y = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  label_map <- attr(tiles, "label_map")
  img_height <- max(sapply(edge_data, function(e) max(e$vertices$y)), na.rm = TRUE)
  img_width <- max(sapply(edge_data, function(e) max(e$vertices$x)), na.rm = TRUE)

  # Handle case where img dimensions can't be inferred
  if (img_height == 0) img_height <- 1
  if (img_width == 0) img_width <- 1

  scale_x <- size[2] / img_width
  scale_y <- size[1] / img_height

  adj_list <- list()

  for (edge in edge_data) {
    verts <- edge$vertices

    # Map labels to tile indices
    cell_a_label <- as.character(edge$cell_a)
    cell_b_label <- as.character(edge$cell_b)

    cell_a_idx <- if (!is.null(label_map[[cell_a_label]])) label_map[[cell_a_label]] else -1L
    cell_b_idx <- if (is.na(edge$cell_b) || edge$cell_b == -1L) -1L else {
      if (!is.null(label_map[[cell_b_label]])) label_map[[cell_b_label]] else -1L
    }

    # Use first and last vertices as edge endpoints
    v1_x <- verts$x[1] * scale_x
    v1_y <- verts$y[1] * scale_y
    v2_x <- verts$x[nrow(verts)] * scale_x
    v2_y <- verts$y[nrow(verts)] * scale_y

    adj_list[[length(adj_list) + 1]] <- data.frame(
      cell_a = cell_a_idx,
      cell_b = cell_b_idx,
      v1_x = v1_x, v1_y = v1_y,
      v2_x = v2_x, v2_y = v2_y,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, adj_list)
}

# ============================================================================
# Edge Map Construction
# ============================================================================

#' Build edge map for SNIC puzzle
#'
#' Creates the edge map with bezier tabs for all adjacent cell pairs.
#' Delegates to the shared build_typed_edge_map() implementation.
#'
#' @param tiles Tile list from build_snic_tiles()
#' @param adjacency Adjacency data frame
#' @param seed Random seed
#' @param tabsize Tab size percentage
#' @param jitter Jitter percentage
#' @param min_tab_size Minimum absolute tab size (optional)
#' @param max_tab_size Maximum absolute tab size (optional)
#' @return Named list of edge paths
#'
#' @keywords internal
build_snic_edge_map <- function(tiles, adjacency, seed, tabsize, jitter,
                                 min_tab_size = NULL, max_tab_size = NULL) {
  build_typed_edge_map(adjacency, seed, tabsize, jitter,
                       min_tab_size = min_tab_size,
                       max_tab_size = max_tab_size)
}

# ============================================================================
# Piece Assembly
# ============================================================================

#' Assemble SNIC pieces from tiles
#'
#' Converts tile polygons to standardized puzzle pieces with SVG paths.
#' Follows the same pattern as assemble_voronoi_pieces().
#'
#' @param tiles Tile list from build_snic_tiles()
#' @param edge_map Edge map with bezier paths
#' @param adjacency Adjacency data frame
#' @param size Canvas size
#' @return List of piece objects
#'
#' @keywords internal
assemble_snic_pieces <- function(tiles, edge_map, adjacency, size) {
  assemble_tile_pieces(
    tiles = tiles,
    edge_map = edge_map,
    adjacency = adjacency,
    puzzle_type = "snic",
    get_center = function(tile) c(tile$centroid_x, tile$centroid_y),
    get_type_pos = function(tile, n_verts) list(
      centroid_x = tile$centroid_x, centroid_y = tile$centroid_y,
      n_vertices = n_verts, label = tile$label
    )
  )
}

# SNIC positioning is now handled by apply_tessellation_positioning() in piece_positioning.R

# ============================================================================
# Fusion Support
# ============================================================================

#' Process fusion groups for SNIC puzzle
#'
#' @param puzzle_result Puzzle result
#' @param fusion_groups Fusion specification
#' @param fusion_style Style for fused edges
#' @param fusion_opacity Opacity for fusion regions
#' @return Updated fusion data or NULL
#'
#' @keywords internal
process_snic_fusion <- function(puzzle_result, fusion_groups, fusion_style, fusion_opacity) {
  NULL
}

# SNIC neighbors are now handled by get_tessellation_neighbors() in adjacency_api.R
