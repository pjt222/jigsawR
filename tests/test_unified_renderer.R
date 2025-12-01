#!/usr/bin/env Rscript
# Test unified SVG renderer (Issue #35)

cat("=== Testing Unified SVG Renderer ===\n\n")

# Source dependencies
source("R/logging.R")
source("R/config_utils.R")
source("R/rectangular_puzzle.R")
source("R/puzzle_core_clean.R")
source("R/hexagonal_puzzle.R")
source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")
source("R/unified_piece_generation.R")
source("R/piece_positioning.R")
source("R/unified_renderer.R")

# Test 1: Basic rectangular rendering
cat("--- Test 1: Basic rectangular rendering ---\n")
result1 <- tryCatch({
  pieces <- generate_pieces_internal(
    type = "rectangular",
    seed = 42,
    grid = c(2, 3),
    size = c(150, 100)
  )

  positioned <- apply_piece_positioning(pieces, offset = 0)

  svg <- render_puzzle_svg(positioned, fill = "none", stroke_width = 1.5)

  # Verify SVG structure
  has_header <- grepl("^<\\?xml", svg)
  has_svg_tag <- grepl("<svg", svg)
  has_viewbox <- grepl("viewBox=", svg)
  has_paths <- grepl("<path", svg)
  has_closing <- grepl("</svg>$", svg)

  cat(sprintf("  SVG length: %d characters\n", nchar(svg)))
  cat(sprintf("  Has XML header: %s\n", has_header))
  cat(sprintf("  Has SVG tag: %s\n", has_svg_tag))
  cat(sprintf("  Has viewBox: %s\n", has_viewbox))
  cat(sprintf("  Has paths: %s\n", has_paths))
  cat(sprintf("  Has closing tag: %s\n", has_closing))

  if (has_header && has_svg_tag && has_viewbox && has_paths && has_closing) {
    cat("  PASS: Valid SVG generated\n")
    "PASS"
  } else {
    cat("  FAIL: Invalid SVG structure\n")
    "FAIL"
  }
}, error = function(e) {
  cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
  "FAIL"
})

# Test 2: Hexagonal rendering
cat("\n--- Test 2: Hexagonal rendering ---\n")
result2 <- tryCatch({
  pieces <- generate_pieces_internal(
    type = "hexagonal",
    seed = 42,
    grid = c(3),
    size = c(200)
  )

  positioned <- apply_piece_positioning(pieces, offset = 0)

  svg <- render_puzzle_svg(positioned, fill = "none", stroke_width = 1.5)

  # Count path elements (should be 19 for 3 rings)
  path_count <- length(gregexpr("<path", svg)[[1]])

  cat(sprintf("  SVG length: %d characters\n", nchar(svg)))
  cat(sprintf("  Path elements: %d (expected 19)\n", path_count))

  if (path_count == 19) {
    cat("  PASS: Correct number of paths\n")
    "PASS"
  } else {
    cat("  FAIL: Wrong number of paths\n")
    "FAIL"
  }
}, error = function(e) {
  cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
  "FAIL"
})

# Test 3: Solid color background
cat("\n--- Test 3: Solid color background ---\n")
result3 <- tryCatch({
  pieces <- generate_pieces_internal(
    type = "rectangular",
    seed = 42,
    grid = c(2, 2),
    size = c(100, 100)
  )

  positioned <- apply_piece_positioning(pieces, offset = 0)

  svg <- render_puzzle_svg(positioned, background = "#CCCCCC")

  has_rect <- grepl('<rect[^>]*fill="#CCCCCC"', svg)

  cat(sprintf("  Has background rect: %s\n", has_rect))

  if (has_rect) {
    cat("  PASS: Solid background rendered\n")
    "PASS"
  } else {
    cat("  FAIL: Background not found\n")
    "FAIL"
  }
}, error = function(e) {
  cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
  "FAIL"
})

# Test 4: No background
cat("\n--- Test 4: No background ---\n")
result4 <- tryCatch({
  pieces <- generate_pieces_internal(
    type = "rectangular",
    seed = 42,
    grid = c(2, 2),
    size = c(100, 100)
  )

  positioned <- apply_piece_positioning(pieces, offset = 0)

  svg <- render_puzzle_svg(positioned, background = "none")

  # Should not have a rect element for background
  has_bg_rect <- grepl('<rect[^>]*fill="[^"]*"[^>]*/>', svg)

  cat(sprintf("  Has background rect: %s\n", has_bg_rect))

  if (!has_bg_rect) {
    cat("  PASS: No background when 'none' specified\n")
    "PASS"
  } else {
    cat("  FAIL: Unexpected background element\n")
    "FAIL"
  }
}, error = function(e) {
  cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
  "FAIL"
})

# Test 5: Gradient background
cat("\n--- Test 5: Gradient background ---\n")
result5 <- tryCatch({
  pieces <- generate_pieces_internal(
    type = "rectangular",
    seed = 42,
    grid = c(2, 2),
    size = c(100, 100)
  )

  positioned <- apply_piece_positioning(pieces, offset = 0)

  svg <- render_puzzle_svg(positioned, background = list(
    type = "gradient",
    center_color = "#FFFFFF",
    edge_color = "#000000"
  ))

  has_defs <- grepl("<defs>", svg)
  has_gradient <- grepl("<radialGradient", svg)
  has_stops <- grepl("<stop", svg)
  has_url_fill <- grepl('fill="url\\(#', svg)

  cat(sprintf("  Has defs: %s\n", has_defs))
  cat(sprintf("  Has radialGradient: %s\n", has_gradient))
  cat(sprintf("  Has stops: %s\n", has_stops))
  cat(sprintf("  Has url fill: %s\n", has_url_fill))

  if (has_defs && has_gradient && has_stops && has_url_fill) {
    cat("  PASS: Gradient background rendered\n")
    "PASS"
  } else {
    cat("  FAIL: Gradient not properly rendered\n")
    "FAIL"
  }
}, error = function(e) {
  cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
  "FAIL"
})

# Test 6: Opacity
cat("\n--- Test 6: Opacity control ---\n")
result6 <- tryCatch({
  pieces <- generate_pieces_internal(
    type = "rectangular",
    seed = 42,
    grid = c(2, 2),
    size = c(100, 100)
  )

  positioned <- apply_piece_positioning(pieces, offset = 0)

  svg <- render_puzzle_svg(positioned, opacity = 0.5)

  has_opacity <- grepl('opacity="0.50"', svg)

  cat(sprintf("  Has opacity attribute: %s\n", has_opacity))

  if (has_opacity) {
    cat("  PASS: Opacity applied\n")
    "PASS"
  } else {
    cat("  FAIL: Opacity not found\n")
    "FAIL"
  }
}, error = function(e) {
  cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
  "FAIL"
})

# Test 7: Color palette
cat("\n--- Test 7: Color palette ---\n")
result7 <- tryCatch({
  pieces <- generate_pieces_internal(
    type = "rectangular",
    seed = 42,
    grid = c(2, 2),
    size = c(100, 100)
  )

  positioned <- apply_piece_positioning(pieces, offset = 0)

  svg <- render_puzzle_svg(positioned, palette = "viridis")

  # Viridis colors should appear (hex format #XXXXXX or #XXXXXXXX with alpha)
  has_hex_colors <- grepl('stroke="#[0-9A-Fa-f]{6,8}"', svg)

  cat(sprintf("  Has hex color strokes: %s\n", has_hex_colors))

  if (has_hex_colors) {
    cat("  PASS: Color palette applied\n")
    "PASS"
  } else {
    cat("  FAIL: Colors not applied correctly\n")
    "FAIL"
  }
}, error = function(e) {
  cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
  "FAIL"
})

# Test 8: Separated pieces with offset
cat("\n--- Test 8: Separated pieces rendering ---\n")
result8 <- tryCatch({
  pieces <- generate_pieces_internal(
    type = "rectangular",
    seed = 42,
    grid = c(2, 3),
    size = c(150, 100)
  )

  positioned <- apply_piece_positioning(pieces, offset = 10)

  svg <- render_puzzle_svg(positioned, fill = "#EEEEEE", stroke_width = 2.0)

  # Check that paths are rendered
  path_count <- length(gregexpr("<path", svg)[[1]])

  # Check fill is applied
  has_fill <- grepl('fill="#EEEEEE"', svg)

  cat(sprintf("  Path count: %d (expected 6)\n", path_count))
  cat(sprintf("  Has fill color: %s\n", has_fill))

  if (path_count == 6 && has_fill) {
    cat("  PASS: Separated pieces rendered correctly\n")
    "PASS"
  } else {
    cat("  FAIL: Rendering issue\n")
    "FAIL"
  }
}, error = function(e) {
  cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
  "FAIL"
})

# Test 9: generate_and_render_puzzle convenience function
cat("\n--- Test 9: generate_and_render_puzzle convenience ---\n")
result9 <- tryCatch({
  result <- generate_and_render_puzzle(
    type = "rectangular",
    seed = 42,
    grid = c(2, 2),
    size = c(100, 100),
    offset = 5,
    fill = "none",
    palette = "plasma",
    background = "white"
  )

  has_svg <- !is.null(result$svg) && nchar(result$svg) > 100
  has_pieces <- !is.null(result$pieces) && length(result$pieces$pieces) == 4
  has_params <- !is.null(result$parameters)

  cat(sprintf("  Has SVG: %s\n", has_svg))
  cat(sprintf("  Has pieces: %s\n", has_pieces))
  cat(sprintf("  Has parameters: %s\n", has_params))

  if (has_svg && has_pieces && has_params) {
    cat("  PASS: Convenience function works\n")
    "PASS"
  } else {
    cat("  FAIL: Missing output components\n")
    "FAIL"
  }
}, error = function(e) {
  cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
  "FAIL"
})

# Test 10: Save SVG to file
cat("\n--- Test 10: Save SVG to file ---\n")
result10 <- tryCatch({
  pieces <- generate_pieces_internal(
    type = "rectangular",
    seed = 42,
    grid = c(2, 2),
    size = c(100, 100)
  )

  positioned <- apply_piece_positioning(pieces, offset = 0)

  svg <- render_puzzle_svg(positioned)

  # Save to temp file
  test_file <- "test_renderer_output.svg"
  save_puzzle_svg(svg, test_file)

  # Verify file exists
  full_path <- file.path("output", test_file)
  file_exists <- file.exists(full_path)

  # Read back and verify content
  if (file_exists) {
    content <- paste(readLines(full_path), collapse = "\n")
    content_valid <- grepl("<svg", content) && grepl("</svg>", content)

    # Clean up
    file.remove(full_path)

    cat(sprintf("  File exists: %s\n", file_exists))
    cat(sprintf("  Content valid: %s\n", content_valid))

    if (content_valid) {
      cat("  PASS: SVG saved and verified\n")
      "PASS"
    } else {
      cat("  FAIL: Invalid content\n")
      "FAIL"
    }
  } else {
    cat("  FAIL: File not created\n")
    "FAIL"
  }
}, error = function(e) {
  cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
  "FAIL"
})

# Summary
cat("\n=== Test Summary ===\n")
cat(sprintf("Test 1 (Basic rectangular): %s\n", result1))
cat(sprintf("Test 2 (Hexagonal): %s\n", result2))
cat(sprintf("Test 3 (Solid background): %s\n", result3))
cat(sprintf("Test 4 (No background): %s\n", result4))
cat(sprintf("Test 5 (Gradient background): %s\n", result5))
cat(sprintf("Test 6 (Opacity): %s\n", result6))
cat(sprintf("Test 7 (Color palette): %s\n", result7))
cat(sprintf("Test 8 (Separated pieces): %s\n", result8))
cat(sprintf("Test 9 (Convenience function): %s\n", result9))
cat(sprintf("Test 10 (Save to file): %s\n", result10))

cat("\n=== Tests Complete ===\n")
