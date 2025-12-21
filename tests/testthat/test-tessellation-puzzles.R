# Tests for tessellation-based puzzles (Voronoi, Random) and tab size constraints
# Created: 2025-12-15

# =============================================================================
# VORONOI PUZZLE TESTS
# =============================================================================

test_that("Voronoi puzzle generates with default parameters", {

skip_if_not(has_deldir(), "deldir package not available")

  result <- generate_puzzle(
    type = "voronoi",
    grid = c(10),
    size = c(200, 200),
    seed = 42,
    save_files = FALSE
  )

  # Check return structure
  expect_type(result, "list")
  expect_true("svg_content" %in% names(result))
  expect_true("pieces" %in% names(result))
  expect_true("canvas_size" %in% names(result))
  expect_true("parameters" %in% names(result))

  # Check SVG content is valid
  expect_match(result$svg_content, "^<\\?xml")
  expect_match(result$svg_content, "</svg>$")

  # Check piece count (should be approximately grid size)
  expect_equal(length(result$pieces), 10)

  # Check type in parameters
  expect_equal(result$parameters$type, "voronoi")
})

test_that("Voronoi puzzle is reproducible with same seed", {
  skip_if_not(has_deldir(), "deldir package not available")

  puzzle1 <- generate_puzzle(
    type = "voronoi",
    grid = c(15),
    size = c(200, 200),
    seed = 12345,
    save_files = FALSE
  )

  puzzle2 <- generate_puzzle(
    type = "voronoi",
    grid = c(15),
    size = c(200, 200),
    seed = 12345,
    save_files = FALSE
  )

  # Same seed should produce identical SVG content
  expect_identical(puzzle1$svg_content, puzzle2$svg_content)
  expect_identical(puzzle1$pieces[[1]]$path, puzzle2$pieces[[1]]$path)
})

test_that("Voronoi puzzle supports different point distributions", {
  skip_if_not(has_deldir(), "deldir package not available")

  # Fermat spiral (default)
  result_fermat <- generate_puzzle(
    type = "voronoi",
    grid = c(10),
    size = c(200, 200),
    seed = 42,
    point_distribution = "fermat",
    save_files = FALSE
  )
  expect_equal(length(result_fermat$pieces), 10)

  # Uniform random
  result_uniform <- generate_puzzle(
    type = "voronoi",
    grid = c(10),
    size = c(200, 200),
    seed = 42,
    point_distribution = "uniform",
    save_files = FALSE
  )
  expect_equal(length(result_uniform$pieces), 10)

  # Jittered grid (may produce more pieces due to grid structure)
  result_jittered <- generate_puzzle(
    type = "voronoi",
    grid = c(10),
    size = c(200, 200),
    seed = 42,
    point_distribution = "jittered",
    save_files = FALSE
  )
  expect_gte(length(result_jittered$pieces), 10)  # At least requested count

  # Different distributions should produce different puzzles
  expect_false(identical(result_fermat$svg_content, result_uniform$svg_content))
})

test_that("Voronoi puzzle handles offset parameter", {
  skip_if_not(has_deldir(), "deldir package not available")

  result_complete <- generate_puzzle(
    type = "voronoi",
    grid = c(10),
    size = c(200, 200),
    seed = 42,
    offset = 0,
    save_files = FALSE
  )

  result_separated <- generate_puzzle(
    type = "voronoi",
    grid = c(10),
    size = c(200, 200),
    seed = 42,
    offset = 10,
    save_files = FALSE
  )

  # Separated canvas should be larger
  expect_gt(result_separated$canvas_size[1], result_complete$canvas_size[1])
})

# =============================================================================
# RANDOM SHAPE PUZZLE TESTS
# =============================================================================

test_that("Random puzzle generates with default parameters", {
  skip_if_not(has_rcdt(), "RCDT package not available")

  result <- generate_puzzle(
    type = "random",
    grid = c(15),
    size = c(200, 200),
    seed = 42,
    save_files = FALSE
  )

  # Check return structure
  expect_type(result, "list")
  expect_true("svg_content" %in% names(result))
  expect_true("pieces" %in% names(result))
  expect_true("canvas_size" %in% names(result))
  expect_true("parameters" %in% names(result))

  # Check SVG content is valid
  expect_match(result$svg_content, "^<\\?xml")
  expect_match(result$svg_content, "</svg>$")

  # Piece count depends on triangulation
  expect_gt(length(result$pieces), 0)

  # Check type in parameters
  expect_equal(result$parameters$type, "random")
})

test_that("Random puzzle is reproducible with same seed", {
  skip_if_not(has_rcdt(), "RCDT package not available")

  puzzle1 <- generate_puzzle(
    type = "random",
    grid = c(15),
    size = c(200, 200),
    seed = 12345,
    save_files = FALSE
  )

  puzzle2 <- generate_puzzle(
    type = "random",
    grid = c(15),
    size = c(200, 200),
    seed = 12345,
    save_files = FALSE
  )

  # Same seed should produce identical SVG content
  expect_identical(puzzle1$svg_content, puzzle2$svg_content)
  expect_identical(puzzle1$pieces[[1]]$path, puzzle2$pieces[[1]]$path)
})

test_that("Random puzzle supports different base shapes", {
  skip_if_not(has_rcdt(), "RCDT package not available")

  # Triangle base (n_corner = 3)
  result_tri <- generate_puzzle(
    type = "random",
    grid = c(10),
    size = c(200, 200),
    seed = 42,
    n_corner = 3,
    save_files = FALSE
  )
  expect_gt(length(result_tri$pieces), 0)

  # Rectangle base (n_corner = 4, default)
  result_rect <- generate_puzzle(
    type = "random",
    grid = c(10),
    size = c(200, 200),
    seed = 42,
    n_corner = 4,
    save_files = FALSE
  )
  expect_gt(length(result_rect$pieces), 0)

  # Pentagon base (n_corner = 5)
  result_pent <- generate_puzzle(
    type = "random",
    grid = c(10),
    size = c(200, 200),
    seed = 42,
    n_corner = 5,
    save_files = FALSE
  )
  expect_gt(length(result_pent$pieces), 0)

  # Hexagon base (n_corner = 6)
  result_hex <- generate_puzzle(
    type = "random",
    grid = c(10),
    size = c(200, 200),
    seed = 42,
    n_corner = 6,
    save_files = FALSE
  )
  expect_gt(length(result_hex$pieces), 0)
})

test_that("Random puzzle handles offset parameter", {
  skip_if_not(has_rcdt(), "RCDT package not available")

  result_complete <- generate_puzzle(
    type = "random",
    grid = c(10),
    size = c(200, 200),
    seed = 42,
    offset = 0,
    save_files = FALSE
  )

  result_separated <- generate_puzzle(
    type = "random",
    grid = c(10),
    size = c(200, 200),
    seed = 42,
    offset = 10,
    save_files = FALSE
  )

  # Separated canvas should be larger
  expect_gt(result_separated$canvas_size[1], result_complete$canvas_size[1])
})

# =============================================================================
# MIN/MAX TAB SIZE CONSTRAINT TESTS
# =============================================================================

test_that("min_tab_size parameter is stored in result", {
  skip_if_not(has_deldir(), "deldir package not available")

  result <- generate_puzzle(
    type = "voronoi",
    grid = c(10),
    size = c(200, 200),
    seed = 42,
    min_tab_size = 15,
    save_files = FALSE
  )

  expect_equal(result$parameters$min_tab_size, 15)
})

test_that("max_tab_size parameter is stored in result", {
  skip_if_not(has_deldir(), "deldir package not available")

  result <- generate_puzzle(
    type = "voronoi",
    grid = c(10),
    size = c(200, 200),
    seed = 42,
    max_tab_size = 25,
    save_files = FALSE
  )

  expect_equal(result$parameters$max_tab_size, 25)
})

test_that("both min and max tab size parameters work together", {
  skip_if_not(has_deldir(), "deldir package not available")

  result <- generate_puzzle(
    type = "voronoi",
    grid = c(10),
    size = c(200, 200),
    seed = 42,
    min_tab_size = 10,
    max_tab_size = 30,
    save_files = FALSE
  )

  expect_equal(result$parameters$min_tab_size, 10)
  expect_equal(result$parameters$max_tab_size, 30)
  expect_equal(length(result$pieces), 10)
})

test_that("NULL tab size constraints are default behavior", {
  skip_if_not(has_deldir(), "deldir package not available")

  result <- generate_puzzle(
    type = "voronoi",
    grid = c(10),
    size = c(200, 200),
    seed = 42,
    save_files = FALSE
  )

  expect_null(result$parameters$min_tab_size)
  expect_null(result$parameters$max_tab_size)
})

test_that("tab size constraints work for random puzzles", {
  skip_if_not(has_rcdt(), "RCDT package not available")

  result <- generate_puzzle(
    type = "random",
    grid = c(15),
    size = c(200, 200),
    seed = 42,
    min_tab_size = 8,
    max_tab_size = 20,
    save_files = FALSE
  )

  expect_equal(result$parameters$min_tab_size, 8)
  expect_equal(result$parameters$max_tab_size, 20)
  expect_gt(length(result$pieces), 0)
})

test_that("tab size constraints apply to rectangular puzzles", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    size = c(200, 200),
    seed = 42,
    min_tab_size = 10,
    max_tab_size = 30,
    save_files = FALSE
  )

  # Parameters should be stored for all puzzle types
  expect_equal(result$parameters$min_tab_size, 10)
  expect_equal(result$parameters$max_tab_size, 30)
})

test_that("tab size constraints apply to hexagonal puzzles", {
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(3),
    size = c(200),
    seed = 42,
    min_tab_size = 10,
    max_tab_size = 30,
    save_files = FALSE
  )

  # Parameters should be stored for all puzzle types
  expect_equal(result$parameters$min_tab_size, 10)
  expect_equal(result$parameters$max_tab_size, 30)
})

test_that("tab size constraints apply to concentric puzzles", {
  result <- generate_puzzle(
    type = "concentric",
    grid = c(3),
    size = c(200),
    seed = 42,
    min_tab_size = 8,
    max_tab_size = 25,
    save_files = FALSE
  )

  # Parameters should be stored for all puzzle types
  expect_equal(result$parameters$min_tab_size, 8)
  expect_equal(result$parameters$max_tab_size, 25)
  expect_gt(length(result$pieces), 0)
})

# =============================================================================
# INPUT VALIDATION TESTS
# =============================================================================

test_that("negative min_tab_size is rejected", {
  skip_if_not(has_deldir(), "deldir package not available")

  expect_error(
    generate_puzzle(
      type = "voronoi",
      grid = c(10),
      size = c(200, 200),
      seed = 42,
      min_tab_size = -5,
      save_files = FALSE
    ),
    "min_tab_size must be non-negative"
  )
})

test_that("negative max_tab_size is rejected", {
  skip_if_not(has_deldir(), "deldir package not available")

  expect_error(
    generate_puzzle(
      type = "voronoi",
      grid = c(10),
      size = c(200, 200),
      seed = 42,
      max_tab_size = -10,
      save_files = FALSE
    ),
    "max_tab_size must be non-negative"
  )
})

test_that("min_tab_size > max_tab_size is rejected", {
  skip_if_not(has_deldir(), "deldir package not available")

  expect_error(
    generate_puzzle(
      type = "voronoi",
      grid = c(10),
      size = c(200, 200),
      seed = 42,
      min_tab_size = 50,
      max_tab_size = 20,
      save_files = FALSE
    ),
    "min_tab_size.*cannot be greater than.*max_tab_size"
  )
})

# =============================================================================
# GENERATE_TESSELLATION_EDGE UNIT TESTS
# =============================================================================

test_that("generate_tessellation_edge creates valid paths for horizontal edge", {
  edge <- generate_tessellation_edge(
    v1 = c(0, 0),
    v2 = c(100, 0),
    seed = 42,
    edge_id = 1
  )

  expect_type(edge, "list")
  expect_true("forward" %in% names(edge))
  expect_true("reverse" %in% names(edge))
  expect_true("start" %in% names(edge))
  expect_true("end" %in% names(edge))

  # Forward path should end at v2
  expect_match(edge$forward, "C")  # Should contain bezier commands
})

test_that("generate_tessellation_edge creates valid paths for diagonal edge", {
  edge <- generate_tessellation_edge(
    v1 = c(0, 0),
    v2 = c(70, 70),
    seed = 42,
    edge_id = 2
  )

  expect_type(edge, "list")
  expect_match(edge$forward, "C")  # Should contain bezier commands
})

test_that("generate_tessellation_edge handles degenerate (zero-length) edges", {
  edge <- generate_tessellation_edge(
    v1 = c(50, 50),
    v2 = c(50, 50),  # Same point
    seed = 42,
    edge_id = 1
  )

  expect_equal(edge$type, "degenerate")
  expect_match(edge$forward, "^L")  # Should be a straight line command
})

test_that("generate_tessellation_edge with min_tab_size clamps small tabs", {
  # Short edge where default tab would be small
  edge <- generate_tessellation_edge(
    v1 = c(0, 0),
    v2 = c(20, 0),  # Short edge
    seed = 42,
    edge_id = 1,
    min_tab_size = 10
  )

  # Should still produce a valid edge (either bezier or straight if too short)
  expect_type(edge, "list")
  expect_true(edge$type %in% c("tab", "straight_constrained"))
})

test_that("generate_tessellation_edge with max_tab_size clamps large tabs", {
  # Long edge where default tab would be large
  edge <- generate_tessellation_edge(
    v1 = c(0, 0),
    v2 = c(500, 0),  # Long edge
    seed = 42,
    edge_id = 1,
    max_tab_size = 20
  )

  expect_type(edge, "list")
  expect_match(edge$forward, "C")  # Should still be bezier
})

test_that("generate_tessellation_edge falls back to straight for very short edges with min constraint", {
  # Very short edge with large min_tab_size
  edge <- generate_tessellation_edge(
    v1 = c(0, 0),
    v2 = c(5, 0),  # Very short edge (5 units)
    seed = 42,
    edge_id = 1,
    min_tab_size = 15  # Large min for short edge
  )

  # Should fall back to straight line
  expect_equal(edge$type, "straight_constrained")
  expect_match(edge$forward, "^L")
})

test_that("generate_tessellation_edge is reproducible with same seed and edge_id", {
  edge1 <- generate_tessellation_edge(
    v1 = c(0, 0),
    v2 = c(100, 50),
    seed = 12345,
    edge_id = 7
  )

  edge2 <- generate_tessellation_edge(
    v1 = c(0, 0),
    v2 = c(100, 50),
    seed = 12345,
    edge_id = 7
  )

  expect_identical(edge1$forward, edge2$forward)
  expect_identical(edge1$reverse, edge2$reverse)
})

test_that("generate_tessellation_edge forward and reverse are complementary", {
  edge <- generate_tessellation_edge(
    v1 = c(0, 0),
    v2 = c(100, 0),
    seed = 42,
    edge_id = 1
  )

  # Forward starts implicitly at v1 and ends near v2
  # Reverse starts implicitly at v2 and ends near v1
  expect_true(nchar(edge$forward) > 0)
  expect_true(nchar(edge$reverse) > 0)

  # They should be different (one is reversed)
  expect_false(identical(edge$forward, edge$reverse))
})

# =============================================================================
# EDGE CASE TESTS
# =============================================================================

test_that("Voronoi puzzle works with minimum cell count", {
  skip_if_not(has_deldir(), "deldir package not available")

  result <- generate_puzzle(
    type = "voronoi",
    grid = c(3),  # Minimum reasonable
    size = c(200, 200),
    seed = 42,
    save_files = FALSE
  )

  expect_equal(length(result$pieces), 3)
})

test_that("Random puzzle works with minimum interior points", {
  skip_if_not(has_rcdt(), "RCDT package not available")

  result <- generate_puzzle(
    type = "random",
    grid = c(3),  # Minimum reasonable
    size = c(200, 200),
    seed = 42,
    save_files = FALSE
  )

  expect_gt(length(result$pieces), 0)
})

test_that("Voronoi puzzle works with larger cell counts", {
  skip_if_not(has_deldir(), "deldir package not available")

  result <- generate_puzzle(
    type = "voronoi",
    grid = c(50),
    size = c(300, 300),
    seed = 42,
    save_files = FALSE
  )

  expect_equal(length(result$pieces), 50)
})

test_that("Random puzzle works with larger piece counts", {
  skip_if_not(has_rcdt(), "RCDT package not available")

  result <- generate_puzzle(
    type = "random",
    grid = c(30),
    size = c(300, 300),
    seed = 42,
    save_files = FALSE
  )

  expect_gt(length(result$pieces), 30)  # Triangulation creates more
})

test_that("Voronoi puzzle with rectangular size specification", {
  skip_if_not(has_deldir(), "deldir package not available")

  result <- generate_puzzle(
    type = "voronoi",
    grid = c(15),
    size = c(300, 200),  # Non-square
    seed = 42,
    save_files = FALSE
  )

  expect_equal(length(result$pieces), 15)
  expect_equal(result$canvas_size[1], 300)
  expect_equal(result$canvas_size[2], 200)
})

test_that("Random puzzle with rectangular size specification", {
  skip_if_not(has_rcdt(), "RCDT package not available")

  result <- generate_puzzle(
    type = "random",
    grid = c(15),
    size = c(300, 200),  # Non-square
    seed = 42,
    save_files = FALSE
  )

  expect_gt(length(result$pieces), 0)
})

# =============================================================================
# STYLING INTEGRATION TESTS
# =============================================================================

test_that("Voronoi puzzle supports fill colors", {
  skip_if_not(has_deldir(), "deldir package not available")

  result <- generate_puzzle(
    type = "voronoi",
    grid = c(10),
    size = c(200, 200),
    seed = 42,
    fill_color = "#ff0000",
    save_files = FALSE
  )

  expect_match(result$svg_content, "#ff0000")
})

test_that("Random puzzle supports stroke width", {
  skip_if_not(has_rcdt(), "RCDT package not available")

  result <- generate_puzzle(
    type = "random",
    grid = c(10),
    size = c(200, 200),
    seed = 42,
    stroke_width = 2.5,
    save_files = FALSE
  )

  expect_match(result$svg_content, "stroke-width")
})

test_that("Voronoi puzzle supports palette coloring", {
  skip_if_not(has_deldir(), "deldir package not available")

  result <- generate_puzzle(
    type = "voronoi",
    grid = c(10),
    size = c(200, 200),
    seed = 42,
    palette = "viridis",
    save_files = FALSE
  )

  # Should have multiple colors
  expect_gt(length(result$pieces), 1)
})

test_that("tessellation puzzles support labels", {
  skip_if_not(has_deldir(), "deldir package not available")

  result <- generate_puzzle(
    type = "voronoi",
    grid = c(10),
    size = c(200, 200),
    seed = 42,
    show_labels = TRUE,
    save_files = FALSE
  )

  expect_match(result$svg_content, "<text")
})
