# Test hexagonal puzzle generation across multiple ring sizes
# Regression tests for Issue: circle_radius hardcoded values

test_that("circle_radius uses diameter/2 for all ring counts", {

  # Test various ring sizes to ensure circle_radius scales correctly
  test_cases <- list(
    list(rings = 3, diameter = 240, expected_radius = 120),
    list(rings = 5, diameter = 300, expected_radius = 150),
    list(rings = 7, diameter = 400, expected_radius = 200),
    list(rings = 10, diameter = 500, expected_radius = 250)
  )

  for (tc in test_cases) {
    # Generate edge map with circular border
    edge_data <- generate_hex_edge_map(
      rings = tc$rings,
      seed = 42,
      diameter = tc$diameter,
      do_warp = TRUE,
      do_trunc = TRUE,
      do_circular_border = TRUE
    )

    # Find a border edge and extract its arc radius
    border_edges <- Filter(
      function(e) e$type == "border" && grepl("^A", e$forward),
      edge_data$piece_edge_map
    )

    expect_true(
      length(border_edges) > 0,
      info = sprintf("Should have border edges for %d rings", tc$rings)
    )

    # Extract radius from first border arc command
    first_border <- border_edges[[1]]
    arc_parts <- strsplit(first_border$forward, " ")[[1]]
    arc_radius <- as.numeric(arc_parts[2])

    expect_equal(
      arc_radius,
      tc$expected_radius,
      tolerance = 0.01,
      info = sprintf(
        "Arc radius should be diameter/2 for %d rings (expected %g, got %g)",
        tc$rings, tc$expected_radius, arc_radius
      )
    )
  }
})

test_that("generate_puzzle works for 7-ring hexagonal with warp+trunc", {
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(7),
    size = c(400),
    seed = 42,
    offset = 0,
    do_warp = TRUE,
    do_trunc = TRUE,
    do_circular_border = TRUE
  )

  # Check basic structure

  expect_true(!is.null(result$svg_content))
  expect_equal(length(result$pieces), 127)  # 3*7*(7-1)+1 = 127 pieces

  # Check that arcs use correct radius
  arcs <- regmatches(
    result$svg_content,
    gregexpr("A ([0-9.]+) ([0-9.]+)", result$svg_content)
  )
  arc_list <- unlist(arcs)

  expect_true(length(arc_list) > 0, "Should contain arc commands")

  # All arcs should use radius = 200 (diameter/2)
  for (arc in arc_list) {
    parts <- strsplit(arc, " ")[[1]]
    radius <- as.numeric(parts[2])
    expect_equal(radius, 200, tolerance = 0.01)
  }
})

test_that("warp transformation does not invert vertices", {
  # Test that warp doesn't flip vertex direction (dot product stays positive)
  test_points <- list(
    c(-100, 0),   # leftmost
    c(100, 0),    # rightmost
    c(0, 100),    # top
    c(0, -100),   # bottom
    c(70.7, 70.7) # diagonal

  )

  for (pt in test_points) {
    warped <- apply_hex_warp(pt[1], pt[2])

    # Dot product should be positive (same direction)
    dot_product <- pt[1] * warped$x + pt[2] * warped$y
    expect_true(
      dot_product > 0,
      info = sprintf(
        "Warp should not invert direction for point (%.1f, %.1f)",
        pt[1], pt[2]
      )
    )
  }
})

test_that("piece count formula works for various ring sizes", {
  # Formula: 3 * rings * (rings - 1) + 1
  test_cases <- list(
    list(rings = 2, expected = 7),
    list(rings = 3, expected = 19),
    list(rings = 4, expected = 37),
    list(rings = 5, expected = 61),
    list(rings = 6, expected = 91),
    list(rings = 7, expected = 127)
  )

  for (tc in test_cases) {
    result <- generate_puzzle(
      type = "hexagonal",
      grid = c(tc$rings),
      size = c(200),
      seed = 42,
      offset = 0
    )

    expect_equal(
      length(result$pieces),
      tc$expected,
      info = sprintf("Rings=%d should have %d pieces", tc$rings, tc$expected)
    )
  }
})

test_that("separated hexagonal puzzle maintains correct arc radius", {
  # Even with separation, arc radius should still be diameter/2
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(5),
    size = c(300),
    seed = 42,
    offset = 15,  # Separated
    do_warp = TRUE,
    do_trunc = TRUE,
    do_circular_border = TRUE
  )

  # Extract arc radii
  arcs <- regmatches(
    result$svg_content,
    gregexpr("A ([0-9.]+) ([0-9.]+)", result$svg_content)
  )
  arc_list <- unlist(arcs)

  expect_true(length(arc_list) > 0)

  # All arcs should use radius = 150 (diameter/2)
  for (arc in arc_list) {
    parts <- strsplit(arc, " ")[[1]]
    radius <- as.numeric(parts[2])
    expect_equal(radius, 150, tolerance = 0.01)
  }
})
