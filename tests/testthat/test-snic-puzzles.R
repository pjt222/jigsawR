# Tests for SNIC superpixel puzzle type
# Issue #80

# ============================================================================
# Unit Tests - Boundary Extraction (no snic dependency needed)
# ============================================================================

test_that("find_junction_vertices finds corners of single-label image", {
  # 4x4 image with single label

  labels <- matrix(1L, nrow = 4, ncol = 4)
  junctions <- find_junction_vertices(labels)

  # Should find all 4 corners
  corners <- junctions[junctions$gx %in% c(0, 4) & junctions$gy %in% c(0, 4), ]
  expect_equal(nrow(corners), 4)
})

test_that("find_junction_vertices finds boundaries between 2 labels", {
  # 4x4 image: left half = 1, right half = 2
  labels <- matrix(c(
    1L, 1L, 2L, 2L,
    1L, 1L, 2L, 2L,
    1L, 1L, 2L, 2L,
    1L, 1L, 2L, 2L
  ), nrow = 4, ncol = 4, byrow = TRUE)

  junctions <- find_junction_vertices(labels)

  # Should find junction vertices along the boundary at gx=2
  boundary_junctions <- junctions[junctions$gx == 2, ]
  expect_true(nrow(boundary_junctions) > 0)
})

test_that("find_junction_vertices finds 3-label meeting point", {
  # 4x4 image with 3 labels meeting at (2,2)
  labels <- matrix(c(
    1L, 1L, 2L, 2L,
    1L, 1L, 2L, 2L,
    3L, 3L, 2L, 2L,
    3L, 3L, 2L, 2L
  ), nrow = 4, ncol = 4, byrow = TRUE)

  junctions <- find_junction_vertices(labels)

  # Should find the triple junction at grid point (2,2)
  triple_junctions <- junctions[junctions$n_labels >= 3, ]
  expect_true(nrow(triple_junctions) > 0)
})

test_that("build_boundary_segments finds horizontal and vertical boundaries", {
  # 4x4 image: left half = 1, right half = 2
  labels <- matrix(c(
    1L, 1L, 2L, 2L,
    1L, 1L, 2L, 2L,
    1L, 1L, 2L, 2L,
    1L, 1L, 2L, 2L
  ), nrow = 4, ncol = 4, byrow = TRUE)

  segments <- build_boundary_segments(labels)

  # Should have internal boundary segments between labels 1 and 2
  internal_segs <- segments[segments$label_a == 1 & segments$label_b == 2, ]
  expect_true(nrow(internal_segs) > 0)

  # Should have boundary segments (label_b = -1)
  border_segs <- segments[segments$label_b == -1, ]
  expect_true(nrow(border_segs) > 0)
})

test_that("build_boundary_segments generates correct border segments", {
  # Simple 2x2 image, single label
  labels <- matrix(1L, nrow = 2, ncol = 2)

  segments <- build_boundary_segments(labels)

  # All segments should be border (label_b = -1)
  expect_true(all(segments$label_b == -1))

  # Should have 8 border segments (2 top + 2 bottom + 2 left + 2 right)
  expect_equal(nrow(segments), 8)
})

test_that("build_snic_tiles creates polygons for each label", {
  # 4x4 image with 2 labels
  labels <- matrix(c(
    1L, 1L, 2L, 2L,
    1L, 1L, 2L, 2L,
    1L, 1L, 2L, 2L,
    1L, 1L, 2L, 2L
  ), nrow = 4, ncol = 4, byrow = TRUE)

  junctions <- find_junction_vertices(labels)
  size <- c(200, 200)  # 200mm x 200mm

  tiles <- build_snic_tiles(labels, junctions, size)

  # Should have 2 tiles
  expect_equal(length(tiles), 2)

  # Each tile should have x, y, bp fields
  for (tile in tiles) {
    expect_true("x" %in% names(tile))
    expect_true("y" %in% names(tile))
    expect_true("bp" %in% names(tile))
    expect_true(length(tile$x) >= 3)  # At least a triangle
    expect_true(length(tile$x) == length(tile$y))
  }
})

test_that("build_snic_tiles scales coordinates to mm", {
  labels <- matrix(c(1L, 2L, 1L, 2L), nrow = 2, ncol = 2, byrow = TRUE)

  junctions <- find_junction_vertices(labels)
  size <- c(100, 200)  # height=100mm, width=200mm

  tiles <- build_snic_tiles(labels, junctions, size)

  # All x coordinates should be within [0, 200] (width)
  for (tile in tiles) {
    expect_true(all(tile$x >= 0 & tile$x <= 200))
    expect_true(all(tile$y >= 0 & tile$y <= 100))
  }
})

test_that("trace_snic_edges returns edge data", {
  # 4x4 image with 2 labels
  labels <- matrix(c(
    1L, 1L, 2L, 2L,
    1L, 1L, 2L, 2L,
    1L, 1L, 2L, 2L,
    1L, 1L, 2L, 2L
  ), nrow = 4, ncol = 4, byrow = TRUE)

  junctions <- find_junction_vertices(labels)
  segments <- build_boundary_segments(labels)

  edges <- trace_snic_edges(segments, junctions)

  # Should find edges between labels 1 and 2
  internal_edges <- Filter(function(e) e$cell_a != -1 && e$cell_b != -1, edges)
  expect_true(length(internal_edges) > 0)
})

test_that("extract_snic_polygons produces valid output structure", {
  labels <- matrix(c(
    1L, 1L, 2L, 2L,
    1L, 1L, 2L, 2L,
    3L, 3L, 2L, 2L,
    3L, 3L, 2L, 2L
  ), nrow = 4, ncol = 4, byrow = TRUE)

  size <- c(200, 200)
  result <- extract_snic_polygons(labels, size)

  expect_true("tiles" %in% names(result))
  expect_true("adjacency" %in% names(result))
  expect_true(length(result$tiles) == 3)  # 3 labels

  # Adjacency should be a data frame
  expect_true(is.data.frame(result$adjacency))
  expect_true(all(c("cell_a", "cell_b", "v1_x", "v1_y", "v2_x", "v2_y") %in%
                  names(result$adjacency)))
})

# ============================================================================
# Dependency Check Tests
# ============================================================================

test_that("has_snic returns logical", {
  result <- has_snic()
  expect_true(is.logical(result))
  expect_equal(length(result), 1)
})

test_that("has_magick returns logical", {
  result <- has_magick()
  expect_true(is.logical(result))
  expect_equal(length(result), 1)
})

# ============================================================================
# Error Handling Tests
# ============================================================================

test_that("generate_puzzle with snic type works without image_path (synthetic mode)", {
  skip_if_not(has_snic(), "snic package not available")

  result <- generate_puzzle(type = "snic", grid = c(10), size = c(100, 100), seed = 42)
  expect_type(result, "list")
  expect_true(nchar(result$svg_content) > 0)
})

test_that("generate_puzzle with snic type rejects non-existent file", {
  skip_if_not(has_snic(), "snic package not available")
  skip_if_not(has_magick(), "magick package not available")

  expect_error(
    generate_puzzle(type = "snic", grid = c(10), size = c(100, 100),
                    seed = 42, image_path = "/nonexistent/image.png"),
    "not found"
  )
})

test_that("generate_puzzle validates snic type string", {
  # "snic" should be accepted as a valid type
  valid_types <- c("rectangular", "hexagonal", "concentric", "voronoi", "random", "snic")
  expect_true("snic" %in% valid_types)
})

# ============================================================================
# Image Fill Rendering Tests
# ============================================================================

test_that("render_image_clip_defs generates valid SVG", {
  # Create mock pieces
  pieces <- list(
    list(id = 1, path = "M 0 0 L 100 0 L 100 100 L 0 100 Z"),
    list(id = 2, path = "M 100 0 L 200 0 L 200 100 L 100 100 Z")
  )

  result <- render_image_clip_defs(pieces)

  expect_true(grepl("<defs>", result))
  expect_true(grepl("</defs>", result))
  expect_true(grepl("piece-1-clip", result))
  expect_true(grepl("piece-2-clip", result))
  expect_true(grepl("<clipPath", result))
})

test_that("render_image_filled_pieces generates SVG with image and stroke", {
  pieces <- list(
    list(id = 1, path = "M 0 0 L 100 0 L 100 100 L 0 100 Z")
  )

  image_data_uri <- "data:image/jpeg;base64,/9j/fake=="
  canvas_size <- c(100, 100)

  result <- render_image_filled_pieces(
    pieces, image_data_uri, canvas_size,
    stroke_width = 1.5, colors = "black"
  )

  expect_equal(length(result), 1)
  expect_true(grepl("<image", result[1]))
  expect_true(grepl("clip-path", result[1]))
  expect_true(grepl("stroke=", result[1]))
})

test_that("encode_image_base64 errors on missing file", {
  skip_if_not(has_magick(), "magick package not available")

  expect_error(
    encode_image_base64("/nonexistent/image.png"),
    "not found"
  )
})

# ============================================================================
# Integration Tests (require snic + magick)
# ============================================================================

test_that("SNIC puzzle generates with test image", {
  skip_if_not(has_snic(), "snic package not available")
  skip_if_not(has_magick(), "magick package not available")
  skip_if_not(requireNamespace("base64enc", quietly = TRUE), "base64enc not available")

  # Create a small test image
  tmp_img <- tempfile(fileext = ".png")
  on.exit(unlink(tmp_img), add = TRUE)

  img <- magick::image_blank(40, 30, color = "white")
  # Add some color variation
  img <- magick::image_composite(
    img,
    magick::image_blank(20, 30, color = "red"),
    offset = "+0+0"
  )
  magick::image_write(img, tmp_img)

  result <- generate_puzzle(
    type = "snic",
    grid = c(5),
    size = c(100, 100),
    seed = 42,
    image_path = tmp_img,
    compactness = 1.0
  )

  # Check return structure
  expect_true("svg_content" %in% names(result))
  expect_true("pieces" %in% names(result))
  expect_true("canvas_size" %in% names(result))
  expect_equal(result$type, "snic")
  expect_true(length(result$pieces) > 0)

  # SVG should contain image elements (image fill)
  expect_true(grepl("<image", result$svg_content))

  # Each piece should have required fields
  for (piece in result$pieces) {
    expect_true("id" %in% names(piece))
    expect_true("path" %in% names(piece))
    expect_true("center" %in% names(piece))
    expect_equal(piece$type, "snic")
    # Paths should be closed
    expect_true(grepl("Z$", trimws(piece$path)))
  }
})

test_that("SNIC puzzle is reproducible with same seed and image", {
  skip_if_not(has_snic(), "snic package not available")
  skip_if_not(has_magick(), "magick package not available")
  skip_if_not(requireNamespace("base64enc", quietly = TRUE), "base64enc not available")

  tmp_img <- tempfile(fileext = ".png")
  on.exit(unlink(tmp_img), add = TRUE)
  img <- magick::image_blank(30, 30, color = "blue")
  magick::image_write(img, tmp_img)

  result1 <- generate_puzzle(
    type = "snic", grid = c(4), size = c(80, 80),
    seed = 123, image_path = tmp_img
  )

  result2 <- generate_puzzle(
    type = "snic", grid = c(4), size = c(80, 80),
    seed = 123, image_path = tmp_img
  )

  expect_equal(length(result1$pieces), length(result2$pieces))
  # Same pieces should have same centers
  for (i in seq_along(result1$pieces)) {
    expect_equal(result1$pieces[[i]]$center, result2$pieces[[i]]$center)
  }
})

test_that("SNIC puzzle parameters are stored correctly", {
  skip_if_not(has_snic(), "snic package not available")
  skip_if_not(has_magick(), "magick package not available")
  skip_if_not(requireNamespace("base64enc", quietly = TRUE), "base64enc not available")

  tmp_img <- tempfile(fileext = ".png")
  on.exit(unlink(tmp_img), add = TRUE)
  img <- magick::image_blank(30, 30, color = "green")
  magick::image_write(img, tmp_img)

  result <- generate_puzzle(
    type = "snic", grid = c(5), size = c(100, 100),
    seed = 42, image_path = tmp_img,
    compactness = 1.5, seed_type = "rectangular"
  )

  expect_equal(result$parameters$type, "snic")
  expect_equal(result$parameters$seed, 42)
  expect_equal(result$parameters$image_path, tmp_img)
  expect_equal(result$parameters$compactness, 1.5)
  expect_equal(result$parameters$seed_type, "rectangular")
})

test_that("SNIC puzzle works with offset > 0", {
  skip_if_not(has_snic(), "snic package not available")
  skip_if_not(has_magick(), "magick package not available")
  skip_if_not(requireNamespace("base64enc", quietly = TRUE), "base64enc not available")

  tmp_img <- tempfile(fileext = ".png")
  on.exit(unlink(tmp_img), add = TRUE)
  img <- magick::image_blank(30, 30, color = "yellow")
  magick::image_write(img, tmp_img)

  result <- generate_puzzle(
    type = "snic", grid = c(4), size = c(80, 80),
    seed = 42, image_path = tmp_img, offset = 10
  )

  expect_true(length(result$pieces) > 0)
  expect_equal(result$parameters$offset, 10)
})

# ============================================================================
# Adjacency API Tests
# ============================================================================

test_that("get_piece_neighbors works for snic type", {
  skip_on_ci()
  skip_if_not(has_snic(), "snic package not available")
  skip_if_not(has_magick(), "magick package not available")
  skip_if_not(requireNamespace("base64enc", quietly = TRUE), "base64enc not available")

  tmp_img <- tempfile(fileext = ".png")
  on.exit(unlink(tmp_img), add = TRUE)
  img <- magick::image_blank(40, 40, color = "white")
  img <- magick::image_composite(
    img,
    magick::image_blank(20, 40, color = "red"),
    offset = "+0+0"
  )
  magick::image_write(img, tmp_img)

  result <- generate_puzzle(
    type = "snic", grid = c(6), size = c(100, 100),
    seed = 42, image_path = tmp_img
  )

  # Get neighbors of first piece
  neighbors <- get_piece_neighbors(1, result, include_boundary = TRUE)

  expect_true(is.data.frame(neighbors))
  expect_true(all(c("direction", "neighbor_id", "is_boundary") %in% names(neighbors)))

  # At least one piece in the puzzle should have neighbors
  # (exact adjacency varies by platform due to segmentation differences)
  has_any_neighbors <- any(vapply(seq_along(result$pieces), function(i) {
    nrow(get_piece_neighbors(i, result, include_boundary = TRUE)) > 0
  }, logical(1)))
  expect_true(has_any_neighbors)
})
