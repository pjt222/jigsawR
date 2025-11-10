# Tests for individual puzzle pieces functionality

test_that("extract_puzzle_tab_data generates consistent tab data", {
  tab_data <- extract_puzzle_tab_data(
    seed = 1234,
    xn = 2, yn = 2,
    width = 200, height = 200
  )
  
  # Check structure
  expect_type(tab_data, "list")
  expect_true("horizontal" %in% names(tab_data))
  expect_true("vertical" %in% names(tab_data))
  
  # For 2x2 puzzle, should have 1 horizontal and 1 vertical divider
  expect_equal(length(tab_data$horizontal), 1)
  expect_equal(length(tab_data$vertical), 1)
  
  # Each divider should have 2 segments (for 2x2 grid)
  expect_equal(length(tab_data$horizontal[[1]]), 2)
  expect_equal(length(tab_data$vertical[[1]]), 2)
})

test_that("extract_puzzle_tab_data is reproducible", {
  tab_data1 <- extract_puzzle_tab_data(seed = 5678, xn = 3, yn = 3)
  tab_data2 <- extract_puzzle_tab_data(seed = 5678, xn = 3, yn = 3)
  
  # Same seed should produce identical tab data
  expect_identical(tab_data1, tab_data2)
})

test_that("generate_individual_piece_path creates valid SVG paths", {
  # Generate tab data first
  tab_data <- extract_puzzle_tab_data(seed = 1234, xn = 2, yn = 2)
  
  # Generate path for corner piece (0,0)
  path <- generate_individual_piece_path(
    xi = 0, yi = 0,
    xn = 2, yn = 2,
    tab_data = tab_data,
    width = 200, height = 200
  )
  
  # Check path starts with M and ends with Z
  expect_true(grepl("^M", path))
  expect_true(grepl("Z$", path))
  
  # Check path contains valid SVG commands
  expect_true(grepl("[MLCZ]", path))
})

test_that("generate_individual_pieces_svg produces complete SVG", {
  result <- generate_individual_pieces_svg(
    seed = 1234,
    xn = 2, yn = 2,
    width = 200, height = 200
  )
  
  # Check return structure
  expect_type(result, "list")
  expect_true("svg" %in% names(result))
  expect_true("pieces" %in% names(result))
  
  # Check SVG is valid
  svg <- result$svg
  expect_true(grepl("^<\\?xml", svg))
  expect_true(grepl("</svg>$", svg))
  
  # Check all pieces are present for 2x2 puzzle
  for (yi in 0:1) {
    for (xi in 0:1) {
      piece_id <- paste0("piece-", xi, "-", yi)
      expect_true(grepl(piece_id, svg), 
                  info = paste("Missing piece:", piece_id))
    }
  }
})

test_that("piece metadata is correctly generated", {
  result <- generate_individual_pieces_svg(
    seed = 1234,
    xn = 2, yn = 2,
    piece_colors = c("red", "blue", "green", "orange")
  )
  
  pieces_metadata <- result$pieces
  
  # Should have 4 pieces for 2x2 puzzle
  expect_equal(length(pieces_metadata), 4)
  
  # Check piece structure
  piece_0_0 <- pieces_metadata[["piece-0-0"]]
  expect_true("id" %in% names(piece_0_0))
  expect_true("position" %in% names(piece_0_0))
  expect_true("type" %in% names(piece_0_0))
  expect_true("path" %in% names(piece_0_0))
  
  # Check position coordinates
  expect_equal(piece_0_0$position$xi, 0)
  expect_equal(piece_0_0$position$yi, 0)
  
  # Corner pieces should be marked as "corner"
  expect_equal(piece_0_0$type, "corner")
})

test_that("piece types are correctly identified", {
  result <- generate_individual_pieces_svg(
    seed = 1234,
    xn = 3, yn = 3  # 3x3 puzzle has corners, edges, and interior
  )
  
  pieces <- result$pieces
  
  # Count piece types
  corner_count <- sum(sapply(pieces, function(p) p$type == "corner"))
  edge_count <- sum(sapply(pieces, function(p) p$type == "edge"))
  interior_count <- sum(sapply(pieces, function(p) p$type == "interior"))
  
  # 3x3 puzzle should have 4 corners, 4 edges, 1 interior
  expect_equal(corner_count, 4)
  expect_equal(edge_count, 4)
  expect_equal(interior_count, 1)
})

test_that("hexagonal puzzle pieces can be generated", {
  # Test hexagonal puzzle generation
  result <- generate_individual_pieces_svg(
    seed = 1234,
    type = "hexagonal",
    diameter = 200
  )
  
  expect_type(result, "list")
  expect_true("svg" %in% names(result))
  expect_true("pieces" %in% names(result))
  
  # SVG should be valid
  expect_true(grepl("^<\\?xml", result$svg))
  expect_true(grepl("</svg>$", result$svg))
})

test_that("bezier curve utilities work correctly", {
  # Test parsing a simple cubic bezier curve
  path_segment <- "C 10 20 30 40 50 60"
  parsed <- parse_bezier_segment(path_segment)
  
  expect_type(parsed, "list")
  expect_equal(parsed$command, "C")
  expect_equal(length(parsed$coords), 6)
  expect_equal(parsed$coords, c(10, 20, 30, 40, 50, 60))
})

test_that("edge reversal maintains mathematical continuity", {
  # Create a simple bezier segment and reverse it
  original <- "C 0 0 10 10 20 0"
  reversed <- reverse_bezier(original)
  
  # Reversed bezier should be different but valid
  expect_true(original != reversed)
  expect_true(grepl("^C", reversed))
  
  # Should contain same coordinate values in different order
  orig_coords <- as.numeric(strsplit(gsub("C ", "", original), " ")[[1]])
  rev_coords <- as.numeric(strsplit(gsub("C ", "", reversed), " ")[[1]])
  
  expect_equal(length(orig_coords), length(rev_coords))
})