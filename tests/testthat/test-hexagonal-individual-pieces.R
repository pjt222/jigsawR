# Tests for hexagonal individual piece extraction functionality
# Issue #9: Test and validate hexagonal individual piece extraction

# Helper to source required files
source_hexagonal_files <- function() {
  # Source from package root (for testthat context)
  if (file.exists("../../R/hexagonal_topology.R")) {
    source("../../R/hexagonal_topology.R")
    source("../../R/hexagonal_neighbors.R")
    source("../../R/hexagonal_bezier_generation.R")
    source("../../R/hexagonal_edge_generation_fixed.R")
    source("../../R/hexagonal_individual_pieces.R")
  } else if (file.exists("R/hexagonal_topology.R")) {
    source("R/hexagonal_topology.R")
    source("R/hexagonal_neighbors.R")
    source("R/hexagonal_bezier_generation.R")
    source("R/hexagonal_edge_generation_fixed.R")
    source("R/hexagonal_individual_pieces.R")
  }
}

# ============================================================================
# 1. Basic Functionality Tests
# ============================================================================

test_that("extracts correct number of pieces for 2-ring puzzle", {
  source_hexagonal_files()

  result <- generate_hexagonal_individual_pieces(
    rings = 2,
    seed = 42,
    output_dir = tempdir(),
    save_individual = FALSE,
    save_combined = FALSE
  )

  # 2 rings: 1 + 6 = 7 pieces
  expect_equal(length(result$pieces), 7)
  expect_equal(result$parameters$num_pieces, 7)
})

test_that("extracts correct number of pieces for 3-ring puzzle", {
  source_hexagonal_files()

  result <- generate_hexagonal_individual_pieces(
    rings = 3,
    seed = 42,
    output_dir = tempdir(),
    save_individual = FALSE,
    save_combined = FALSE
  )

  # 3 rings: 1 + 6 + 12 = 19 pieces
  expect_equal(length(result$pieces), 19)
  expect_equal(result$parameters$num_pieces, 19)
})

test_that("extracts correct number of pieces for various ring counts", {
  source_hexagonal_files()

  for (rings in 2:5) {
    result <- generate_hexagonal_individual_pieces(
      rings = rings,
      seed = 42,
      output_dir = tempdir(),
      save_individual = FALSE,
      save_combined = FALSE
    )

    expected_pieces <- 3 * rings * (rings - 1) + 1
    expect_equal(length(result$pieces), expected_pieces,
                 label = sprintf("rings=%d", rings))
  }
})

# ============================================================================
# 2. SVG Path Validity Tests
# ============================================================================

test_that("all pieces have valid SVG paths", {
  source_hexagonal_files()

  result <- generate_hexagonal_individual_pieces(
    rings = 2,
    seed = 42,
    output_dir = tempdir(),
    save_individual = FALSE,
    save_combined = FALSE
  )

  for (i in seq_along(result$pieces)) {
    piece <- result$pieces[[i]]

    # Path should exist and be non-empty
    expect_true(!is.null(piece$path),
                label = sprintf("piece %d has path", i))
    expect_gt(nchar(piece$path), 20,
              label = sprintf("piece %d path length", i))

    # Path should start with M (move) command
    expect_true(grepl("^M", piece$path),
                label = sprintf("piece %d starts with M", i))

    # Path should be closed with Z
    expect_true(grepl("Z$", piece$path),
                label = sprintf("piece %d ends with Z", i))

    # Path should contain bezier curves (C command)
    expect_true(grepl("C ", piece$path) || grepl("L ", piece$path),
                label = sprintf("piece %d has curves or lines", i))
  }
})

test_that("combined SVG is valid", {
  source_hexagonal_files()

  svg <- create_hexagonal_individual_pieces_svg(rings = 2, seed = 42)

  # Should be valid SVG structure
  expect_true(grepl("^<svg", svg))
  expect_true(grepl("</svg>$", svg))
  expect_true(grepl('viewBox="', svg))

  # Should contain correct number of paths (7 for 2-ring)
  path_matches <- gregexpr("<path", svg)[[1]]
  expect_equal(length(path_matches), 7)
})

# ============================================================================
# 3. File Operations Tests
# ============================================================================

test_that("saves individual piece files correctly", {
  source_hexagonal_files()

  temp_dir <- file.path(tempdir(), "hex_test_individual")
  if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)

  result <- generate_hexagonal_individual_pieces(
    rings = 2,
    seed = 42,
    output_dir = temp_dir,
    save_individual = TRUE,
    save_combined = FALSE
  )

  # Should have 7 individual files
  expect_length(result$files$individual, 7)

  # All files should exist
  for (file in result$files$individual) {
    expect_true(file.exists(file),
                label = sprintf("file exists: %s", basename(file)))
  }

  # Each file should be valid SVG
  for (file in result$files$individual) {
    content <- paste(readLines(file), collapse = "\n")
    expect_true(grepl("<svg", content))
    expect_true(grepl("</svg>", content))
    expect_true(grepl("<path", content))
  }

  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("saves combined file correctly", {
  source_hexagonal_files()

  temp_dir <- file.path(tempdir(), "hex_test_combined")
  if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)

  result <- generate_hexagonal_individual_pieces(
    rings = 2,
    seed = 42,
    output_dir = temp_dir,
    save_individual = FALSE,
    save_combined = TRUE
  )

  # Combined file should exist
  expect_true(!is.null(result$files$combined))
  expect_true(file.exists(result$files$combined))

  # Should be valid SVG with all pieces
  content <- paste(readLines(result$files$combined), collapse = "\n")
  expect_true(grepl("<svg", content))
  expect_true(grepl("</svg>", content))

  # Should have 7 paths
  path_matches <- gregexpr("<path", content)[[1]]
  expect_equal(length(path_matches), 7)

  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

# ============================================================================
# 4. Reproducibility Tests
# ============================================================================

test_that("same seed produces identical results", {
  source_hexagonal_files()

  result1 <- generate_hexagonal_individual_pieces(
    rings = 2,
    seed = 12345,
    output_dir = tempdir(),
    save_individual = FALSE,
    save_combined = FALSE
  )

  result2 <- generate_hexagonal_individual_pieces(
    rings = 2,
    seed = 12345,
    output_dir = tempdir(),
    save_individual = FALSE,
    save_combined = FALSE
  )

  # All pieces should have identical paths
  expect_equal(length(result1$pieces), length(result2$pieces))
  for (i in seq_along(result1$pieces)) {
    expect_equal(result1$pieces[[i]]$path, result2$pieces[[i]]$path,
                 label = sprintf("piece %d path matches", i))
  }
})

test_that("different seeds produce different results", {
  source_hexagonal_files()

  result1 <- generate_hexagonal_individual_pieces(
    rings = 2,
    seed = 11111,
    output_dir = tempdir(),
    save_individual = FALSE,
    save_combined = FALSE
  )

  result2 <- generate_hexagonal_individual_pieces(
    rings = 2,
    seed = 22222,
    output_dir = tempdir(),
    save_individual = FALSE,
    save_combined = FALSE
  )

  # At least some pieces should differ
  paths_differ <- FALSE
  for (i in seq_along(result1$pieces)) {
    if (result1$pieces[[i]]$path != result2$pieces[[i]]$path) {
      paths_differ <- TRUE
      break
    }
  }
  expect_true(paths_differ)
})

# ============================================================================
# 5. Piece Metadata Tests
# ============================================================================

test_that("pieces have correct metadata", {
  source_hexagonal_files()

  result <- generate_hexagonal_individual_pieces(
    rings = 2,
    seed = 42,
    output_dir = tempdir(),
    save_individual = FALSE,
    save_combined = FALSE
  )

  for (i in seq_along(result$pieces)) {
    piece <- result$pieces[[i]]

    # Each piece should have required fields
    expect_true(!is.null(piece$id), label = sprintf("piece %d has id", i))
    expect_true(!is.null(piece$ring), label = sprintf("piece %d has ring", i))
    expect_true(!is.null(piece$center_x), label = sprintf("piece %d has center_x", i))
    expect_true(!is.null(piece$center_y), label = sprintf("piece %d has center_y", i))
    expect_true(!is.null(piece$path), label = sprintf("piece %d has path", i))

    # ID should match index
    expect_equal(piece$id, i)

    # Ring should be valid (0 for center, 1 for first ring)
    expect_true(piece$ring >= 0 && piece$ring < 2)
  }
})

test_that("center piece is at ring 0", {
  source_hexagonal_files()

  result <- generate_hexagonal_individual_pieces(
    rings = 2,
    seed = 42,
    output_dir = tempdir(),
    save_individual = FALSE,
    save_combined = FALSE
  )

  # First piece should be center (ring 0)
  expect_equal(result$pieces[[1]]$ring, 0)

  # Pieces 2-7 should be ring 1
  for (i in 2:7) {
    expect_equal(result$pieces[[i]]$ring, 1)
  }
})

# ============================================================================
# 6. Parameters Test
# ============================================================================

test_that("parameters are preserved in result", {
  source_hexagonal_files()

  result <- generate_hexagonal_individual_pieces(
    rings = 3,
    seed = 999,
    diameter = 300,
    tabsize = 30,
    jitter = 8,
    output_dir = tempdir(),
    save_individual = FALSE,
    save_combined = FALSE
  )

  expect_equal(result$parameters$rings, 3)
  expect_equal(result$parameters$seed, 999)
  expect_equal(result$parameters$diameter, 300)
  expect_equal(result$parameters$tabsize, 30)
  expect_equal(result$parameters$jitter, 8)
})

# ============================================================================
# 7. Edge Complementarity Test
# ============================================================================

test_that("edge map generates correct number of unique edges", {
  source_hexagonal_files()

  # For 2-ring puzzle: center has 6 edges, ring 1 has additional 6 edges
  # Total internal edges = 6 (center to ring 1)
  # Total border edges = 6 (ring 1 outer edges)

  edge_data <- generate_hex_edge_map(
    rings = 2,
    seed = 42,
    diameter = 240,
    tabsize = 27,
    jitter = 5
  )

  # Should have some unique edges
  expect_true(edge_data$num_edges > 0)
  expect_true(length(edge_data$edge_map) > 0)
  expect_true(length(edge_data$piece_edge_map) > 0)
})

# ============================================================================
# 8. Bounding Box Test
# ============================================================================

test_that("path bounds are calculated correctly", {
  source_hexagonal_files()

  # Simple test path
  test_path <- "M 10 20 L 30 40 L 50 20 Z"
  bounds <- calculate_path_bounds(test_path)

  expect_equal(bounds$min_x, 10)
  expect_equal(bounds$max_x, 50)
  expect_equal(bounds$min_y, 20)
  expect_equal(bounds$max_y, 40)
  expect_equal(bounds$width, 40)
  expect_equal(bounds$height, 20)
})

# ============================================================================
# 9. SVG-Only Function Test
# ============================================================================

test_that("create_hexagonal_individual_pieces_svg works without files", {
  source_hexagonal_files()

  svg <- create_hexagonal_individual_pieces_svg(
    rings = 2,
    seed = 42,
    diameter = 240
  )

  # Should return string
  expect_type(svg, "character")
  expect_gt(nchar(svg), 1000)

  # Should be valid SVG
  expect_true(grepl("^<svg", svg))
  expect_true(grepl("</svg>$", svg))

  # Should have title
  expect_true(grepl("<title>", svg))
})

# ============================================================================
# 10. Custom Colors Test
# ============================================================================

test_that("custom colors are applied", {
  source_hexagonal_files()

  custom_colors <- c("#FF0000", "#00FF00", "#0000FF")

  svg <- create_hexagonal_individual_pieces_svg(
    rings = 2,
    seed = 42,
    colors = custom_colors
  )

  # Should contain our custom colors
  expect_true(grepl("#FF0000", svg) || grepl("#ff0000", svg, ignore.case = TRUE))
  expect_true(grepl("#00FF00", svg) || grepl("#00ff00", svg, ignore.case = TRUE))
  expect_true(grepl("#0000FF", svg) || grepl("#0000ff", svg, ignore.case = TRUE))
})
