# Tests for PILES (Puzzle Input Line Entry System) notation
#
# PILES is inspired by chemistry's SMILES notation for specifying
# puzzle piece fusion groups in a compact, human-readable format.

test_that("parse_piles handles empty and NULL input", {
  expect_equal(parse_piles(NULL), list())
  expect_equal(parse_piles(""), list())
  expect_equal(parse_piles("   "), list())
})

test_that("parse_piles handles simple pairs", {
  result <- parse_piles("1-2")
  expect_equal(result, list(c(1L, 2L)))

  result <- parse_piles("5-10")
  expect_equal(result, list(c(5L, 10L)))
})

test_that("parse_piles handles linear chains", {
  result <- parse_piles("1-2-3")
  expect_equal(result, list(c(1L, 2L, 3L)))

  result <- parse_piles("1-2-3-4-5")
  expect_equal(result, list(c(1L, 2L, 3L, 4L, 5L)))
})

test_that("parse_piles handles multiple groups with comma separator", {
  result <- parse_piles("1-2,3-4")
  expect_equal(result, list(c(1L, 2L), c(3L, 4L)))

  result <- parse_piles("1-2-3,4-5,6-7-8-9")
  expect_equal(result, list(c(1L, 2L, 3L), c(4L, 5L), c(6L, 7L, 8L, 9L)))
})

test_that("parse_piles handles multiple groups with period separator", {
  result <- parse_piles("1-2.3-4")
  expect_equal(result, list(c(1L, 2L), c(3L, 4L)))
})

test_that("parse_piles handles range notation", {
  result <- parse_piles("1:5")
  expect_equal(result, list(1:5))

  result <- parse_piles("10:15")
  expect_equal(result, list(10:15))

  result <- parse_piles("1:3,5:7")
  expect_equal(result, list(1:3, 5:7))
})

test_that("parse_piles handles branching with parentheses", {
  # 1-2(-3)-4 means: 2 connects to 1, 3, and 4
  result <- parse_piles("1-2(-3)-4")
  pieces <- sort(result[[1]])
  expect_equal(pieces, c(1L, 2L, 3L, 4L))

  # More complex branching
  result <- parse_piles("1(-2)(-3)")
  pieces <- sort(result[[1]])
  expect_equal(pieces, c(1L, 2L, 3L))
})

test_that("parse_piles handles nested parentheses", {
  result <- parse_piles("1-2(-3(-4))-5")
  pieces <- sort(result[[1]])
  expect_equal(pieces, c(1L, 2L, 3L, 4L, 5L))
})

test_that("parse_piles handles ring closures", {
  # 1-2-3-4@1 means piece 4 connects back to piece 1
  result <- parse_piles("1-2-3-4@1")
  pieces <- sort(result[[1]])
  expect_equal(pieces, c(1L, 2L, 3L, 4L))
})

test_that("parse_piles ignores direction specifiers", {
  # Direction specifiers are stripped (for future validation)
  result <- parse_piles("1-2[E]-3")
  pieces <- sort(result[[1]])
  expect_equal(pieces, c(1L, 2L, 3L))

  result <- parse_piles("1[N]-2[S]")
  pieces <- sort(result[[1]])
  expect_equal(pieces, c(1L, 2L))
})

test_that("parse_piles handles center keyword", {
  result <- parse_piles("center")
  expect_equal(result, list(1L))
})

test_that("to_piles converts fusion groups to PILES notation", {
  # Simple pairs
  expect_equal(to_piles(list(c(1, 2))), "1-2")

  # Multiple groups
  expect_equal(to_piles(list(c(1, 2), c(3, 4))), "1-2,3-4")

  # Consecutive range (compact)
  expect_equal(to_piles(list(1:6), compact = TRUE), "1:6")

  # Non-consecutive (no range)
  expect_equal(to_piles(list(c(1, 3, 5))), "1-3-5")

  # Empty
  expect_equal(to_piles(list()), "")
  expect_equal(to_piles(NULL), "")
})

test_that("to_piles sorts pieces within groups", {
  # Consecutive pieces get compacted to range notation by default
  expect_equal(to_piles(list(c(3, 1, 2))), "1:3")
  # Non-consecutive pieces use hyphen notation
  expect_equal(to_piles(list(c(5, 2, 8))), "2-5-8")
  # Disable compact to always use hyphens

  expect_equal(to_piles(list(c(3, 1, 2)), compact = FALSE), "1-2-3")
})

test_that("validate_piles_syntax validates basic syntax", {
  # Valid
  expect_true(validate_piles_syntax("1-2")$valid)
  expect_true(validate_piles_syntax("1-2-3,4-5")$valid)
  expect_true(validate_piles_syntax("1:5")$valid)
  expect_true(validate_piles_syntax("1-2(-3)-4")$valid)
  expect_true(validate_piles_syntax("")$valid)

  # Unbalanced parentheses
  expect_false(validate_piles_syntax("1-2(-3")$valid)
  expect_false(validate_piles_syntax("1-2-3)")$valid)
})

test_that("parse_fusion auto-detects PILES vs legacy format", {
  # PILES format (hyphens)
  result <- parse_fusion("1-2-3")
  expect_equal(result, list(c(1L, 2L, 3L)))

  # Legacy format (parentheses with commas)
  result <- parse_fusion("(1,2),(3,4)")
  expect_equal(result, list(c(1L, 2L), c(3L, 4L)))

  # List pass-through
  result <- parse_fusion(list(c(1, 2), c(3, 4)))
  expect_equal(result, list(c(1L, 2L), c(3L, 4L)))
})

test_that("parse_piles with puzzle_result handles row/column keywords", {
  # Create a 3x3 rectangular puzzle
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    size = c(300, 300),
    seed = 42,
    save_files = FALSE
  )

  # Row 1 should be pieces 1, 2, 3
  row1 <- parse_piles("R1", result)
  expect_equal(row1, list(c(1L, 2L, 3L)))

  # Row 2 should be pieces 4, 5, 6
  row2 <- parse_piles("R2", result)
  expect_equal(row2, list(c(4L, 5L, 6L)))

  # Column 1 should be pieces 1, 4, 7
  col1 <- parse_piles("C1", result)
  expect_equal(col1, list(c(1L, 4L, 7L)))

  # Column 2 should be pieces 2, 5, 8
  col2 <- parse_piles("C2", result)
  expect_equal(col2, list(c(2L, 5L, 8L)))
})

test_that("parse_piles with puzzle_result handles ring keywords for hexagonal", {
  # Create a 3-ring hexagonal puzzle
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(3),
    size = c(200),
    seed = 42,
    save_files = FALSE
  )

  # Ring 0 (center) should be piece 1
  ring0 <- parse_piles("ring0", result)
  expect_equal(ring0, list(1L))

  # Ring 1 should be pieces 2-7 (6 pieces)
  ring1 <- parse_piles("ring1", result)
  expect_equal(ring1, list(2:7))

  # Ring 2 should be pieces 8-19 (12 pieces)
  ring2 <- parse_piles("ring2", result)
  expect_equal(ring2, list(8:19))
})

test_that("parse_piles handles boundary keyword", {
  # Create a 3x3 rectangular puzzle
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    size = c(300, 300),
    seed = 42,
    save_files = FALSE
  )

  # Boundary pieces: 1,2,3,4,6,7,8,9 (all except center piece 5)
  boundary <- parse_piles("boundary", result)
  expect_true(5L %in% setdiff(1:9, boundary[[1]]))
  expect_equal(length(boundary[[1]]), 8L)
})

test_that("parse_piles handles inner keyword", {
  # Create a 3x3 rectangular puzzle
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    size = c(300, 300),
    seed = 42,
    save_files = FALSE
  )

  # Inner pieces: only piece 5 (center) for 3x3
  inner <- parse_piles("inner", result)
  expect_equal(inner, list(5L))
})

test_that("parse_piles handles all keyword", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(2, 2),
    size = c(200, 200),
    seed = 42,
    save_files = FALSE
  )

  all_pieces <- parse_piles("all", result)
  expect_equal(all_pieces, list(1:4))
})

test_that("PILES round-trip: parse then serialize", {
  # Simple cases
  original <- "1-2-3"
  parsed <- parse_piles(original)
  serialized <- to_piles(parsed)
  reparsed <- parse_piles(serialized)
  expect_equal(parsed, reparsed)

  # Multiple groups
  original <- "1-2,3-4-5"
  parsed <- parse_piles(original)
  serialized <- to_piles(parsed)
  reparsed <- parse_piles(serialized)
  expect_equal(parsed, reparsed)

  # Range (may compact)
  original <- "1-2-3-4-5-6"
  parsed <- parse_piles(original)
  serialized <- to_piles(parsed, compact = TRUE)
  expect_equal(serialized, "1:6")
  reparsed <- parse_piles(serialized)
  expect_equal(parsed, reparsed)
})

test_that("PILES handles whitespace gracefully", {
  expect_equal(parse_piles("  1-2  "), list(c(1L, 2L)))
  expect_equal(parse_piles("1 - 2 - 3"), list(c(1L, 2L, 3L)))
  expect_equal(parse_piles("1-2 , 3-4"), list(c(1L, 2L), c(3L, 4L)))
})

test_that("PILES complex structures parse correctly", {
  # T-shape: piece 2 connects to 1, 3, and 5
  result <- parse_piles("1-2(-5)-3")
  pieces <- sort(result[[1]])
  expect_equal(pieces, c(1L, 2L, 3L, 5L))

  # Star pattern: piece 1 connects to 2, 3, 4, 5
  result <- parse_piles("2-1(-3)(-4)(-5)")
  pieces <- sort(result[[1]])
  expect_equal(pieces, c(1L, 2L, 3L, 4L, 5L))
})

test_that("PILES integration with generate_puzzle", {
  # Test that PILES can be used directly with generate_puzzle
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    size = c(300, 300),
    seed = 42,
    fusion_groups = parse_fusion("1-2-3,7-8-9"),
    save_files = FALSE
  )

  expect_true(!is.null(result$svg_content))
  expect_equal(length(result$pieces), 9)
})

# =============================================================================
# EXCLUSION SYNTAX TESTS (Issue #47)
# =============================================================================

test_that("parse_exclusion_syntax handles ALL-N format", {
  # Create a mock puzzle_result for testing
  puzzle <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    size = c(300, 300),
    seed = 42,
    save_files = FALSE
  )

  # ALL-5 should return all pieces except 5
  result <- parse_piles("ALL-5", puzzle)
  expect_equal(length(result), 1)
  expect_equal(sort(result[[1]]), c(1L, 2L, 3L, 4L, 6L, 7L, 8L, 9L))

  # ALL-1-9 should return pieces 2-8
  result <- parse_piles("ALL-1-9", puzzle)
  expect_equal(length(result), 1)
  expect_equal(sort(result[[1]]), c(2L, 3L, 4L, 5L, 6L, 7L, 8L))
})

test_that("parse_exclusion_syntax handles !N format", {
  puzzle <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    size = c(300, 300),
    seed = 42,
    save_files = FALSE
  )

  # !5 should return all pieces except 5
  result <- parse_piles("!5", puzzle)
  expect_equal(length(result), 1)
  expect_equal(sort(result[[1]]), c(1L, 2L, 3L, 4L, 6L, 7L, 8L, 9L))

  # !1!7 should return all pieces except 1 and 7
  result <- parse_piles("!1!7", puzzle)
  expect_equal(length(result), 1)
  expect_equal(sort(result[[1]]), c(2L, 3L, 4L, 5L, 6L, 8L, 9L))
})

test_that("exclusion syntax works with generate_puzzle directly", {
  # Test ALL-N works when passed as string to generate_puzzle
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    size = c(300, 300),
    seed = 42,
    fusion_groups = "ALL-5",
    save_files = FALSE
  )

  expect_true(!is.null(result$fusion_data))
  pieces <- as.integer(names(result$fusion_data$piece_to_group))
  expect_equal(sort(pieces), c(1L, 2L, 3L, 4L, 6L, 7L, 8L, 9L))
})

test_that("keyword 'all' works with generate_puzzle", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    size = c(300, 300),
    seed = 42,
    fusion_groups = "all",
    save_files = FALSE
  )

  expect_true(!is.null(result$fusion_data))
  pieces <- as.integer(names(result$fusion_data$piece_to_group))
  expect_equal(sort(pieces), 1:9)
})

test_that("keyword 'all' sets fusion_group on each piece (Issue #55)", {
  # Test rectangular
  rect <- generate_puzzle(
    type = "rectangular", grid = c(3, 3), size = c(300, 300),
    seed = 42, fusion_groups = "all", save_files = FALSE
  )
  rect_groups <- sapply(rect$pieces, function(p) p$fusion_group)
  expect_true(all(!is.na(rect_groups)), info = "All rectangular pieces should have fusion_group set")
  expect_equal(length(unique(rect_groups)), 1, info = "All rectangular pieces should be in same group")

  # Test hexagonal
  hex <- generate_puzzle(
    type = "hexagonal", grid = c(2), size = c(200),
    seed = 42, fusion_groups = "all", save_files = FALSE
  )
  hex_groups <- sapply(hex$pieces, function(p) p$fusion_group)
  expect_true(all(!is.na(hex_groups)), info = "All hexagonal pieces should have fusion_group set")
  expect_equal(length(unique(hex_groups)), 1, info = "All hexagonal pieces should be in same group")

  # Test concentric
  conc <- generate_puzzle(
    type = "concentric", grid = c(2), size = c(200),
    seed = 42, fusion_groups = "all", save_files = FALSE
  )
  conc_groups <- sapply(conc$pieces, function(p) p$fusion_group)
  expect_true(all(!is.na(conc_groups)), info = "All concentric pieces should have fusion_group set")
  expect_equal(length(unique(conc_groups)), 1, info = "All concentric pieces should be in same group")
})

test_that("row keyword 'R1' works with generate_puzzle", {
  result <- generate_puzzle(
    type = "rectangular",
    grid = c(3, 3),
    size = c(300, 300),
    seed = 42,
    fusion_groups = "R1",
    save_files = FALSE
  )

  expect_true(!is.null(result$fusion_data))
  pieces <- as.integer(names(result$fusion_data$piece_to_group))
  expect_equal(sort(pieces), c(1L, 2L, 3L))
})

test_that("ring keyword 'ring1' works with hexagonal puzzles", {
  result <- generate_puzzle(
    type = "hexagonal",
    grid = c(3),
    size = c(200),
    seed = 42,
    fusion_groups = "ring1",
    save_files = FALSE
  )

  expect_true(!is.null(result$fusion_data))
  pieces <- as.integer(names(result$fusion_data$piece_to_group))
  # Ring 1 should be pieces 2-7
  expect_equal(sort(pieces), 2:7)
})
