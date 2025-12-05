# Test: Fill Modes Verification
# Tests all fill modes for both background and pieces

# Source required files
source("R/logging.R")
source("R/config_utils.R")
source("R/bezier_utils.R")
source("R/rectangular_puzzle.R")  # For init_jigsaw
source("R/puzzle_core_clean.R")
source("R/hexagonal_puzzle.R")
source("R/hexagonal_topology.R")
source("R/hexagonal_edge_generation_fixed.R")
source("R/hexagonal_bezier_generation.R")
source("R/unified_piece_generation.R")
source("R/piece_positioning.R")
source("R/unified_renderer.R")
source("R/jigsawR_clean.R")

log_header("Testing Fill Modes")

# Ensure output directory exists
if (!dir.exists("output")) dir.create("output")

# ===== Test 1: Background Fill Modes =====
log_subheader("Test 1: Background Fill Modes")

# Test 1a: Background = none
log_info("Testing background = 'none'")
result_bg_none <- generate_puzzle(
  type = "rectangular",
  grid = c(2, 2),
  size = c(100, 100),
  seed = 42,
  offset = 10,
  background = "none",
  save_files = TRUE,
  output_dir = "output",
  filename_prefix = "fill_test_bg_none"
)
has_no_rect_bg <- !grepl("rect.*fill=", result_bg_none$svg_content)
log_info("  Background 'none': {if (has_no_rect_bg) 'PASS' else 'FAIL'}")

# Test 1b: Background = solid color
log_info("Testing background = solid color")
result_bg_solid <- generate_puzzle(
  type = "rectangular",
  grid = c(2, 2),
  size = c(100, 100),
  seed = 42,
  offset = 10,
  background = "#ffcccc",
  save_files = TRUE,
  output_dir = "output",
  filename_prefix = "fill_test_bg_solid"
)
has_solid_bg <- grepl('fill="#ffcccc"', result_bg_solid$svg_content)
log_info("  Background solid: {if (has_solid_bg) 'PASS' else 'FAIL'}")

# Test 1c: Background = gradient (3-stop)
log_info("Testing background = gradient (3 colors)")
result_bg_gradient <- generate_puzzle(
  type = "rectangular",
  grid = c(2, 2),
  size = c(100, 100),
  seed = 42,
  offset = 10,
  background = list(
    type = "gradient",
    center = "#ff0000",
    middle = "#00ff00",
    edge = "#0000ff"
  ),
  save_files = TRUE,
  output_dir = "output",
  filename_prefix = "fill_test_bg_gradient"
)
has_gradient_bg <- grepl("radialGradient", result_bg_gradient$svg_content)
has_3_stops <- grepl('offset="50%"', result_bg_gradient$svg_content)
has_correct_colors <- grepl("#ff0000", result_bg_gradient$svg_content) &&
                      grepl("#00ff00", result_bg_gradient$svg_content) &&
                      grepl("#0000ff", result_bg_gradient$svg_content)
log_info("  Background gradient: {if (has_gradient_bg) 'PASS' else 'FAIL'}")
log_info("  Has 3 stops (middle): {if (has_3_stops) 'PASS' else 'FAIL'}")
log_info("  Has correct colors: {if (has_correct_colors) 'PASS' else 'FAIL'}")

# ===== Test 2: Piece Fill Modes =====
log_subheader("Test 2: Piece Fill Modes")

# Test 2a: Piece fill = none
log_info("Testing piece fill = 'none'")
result_pf_none <- generate_puzzle(
  type = "rectangular",
  grid = c(2, 2),
  size = c(100, 100),
  seed = 42,
  offset = 10,
  fill_color = "none",
  save_files = TRUE,
  output_dir = "output",
  filename_prefix = "fill_test_piece_none"
)
has_none_fill <- grepl('fill="none"', result_pf_none$svg_content)
log_info("  Piece fill 'none': {if (has_none_fill) 'PASS' else 'FAIL'}")

# Test 2b: Piece fill = solid color
log_info("Testing piece fill = solid color")
result_pf_solid <- generate_puzzle(
  type = "rectangular",
  grid = c(2, 2),
  size = c(100, 100),
  seed = 42,
  offset = 10,
  fill_color = "#ccffcc",
  save_files = TRUE,
  output_dir = "output",
  filename_prefix = "fill_test_piece_solid"
)
has_solid_fill <- grepl('fill="#ccffcc"', result_pf_solid$svg_content)
log_info("  Piece fill solid: {if (has_solid_fill) 'PASS' else 'FAIL'}")

# Test 2c: Piece fill = gradient (3-stop)
log_info("Testing piece fill = gradient (3 colors)")
result_pf_gradient <- generate_puzzle(
  type = "rectangular",
  grid = c(2, 2),
  size = c(100, 100),
  seed = 42,
  offset = 10,
  fill_color = list(
    type = "gradient",
    center = "#ffffff",
    middle = "#cccccc",
    edge = "#333333"
  ),
  save_files = TRUE,
  output_dir = "output",
  filename_prefix = "fill_test_piece_gradient"
)
has_piece_gradient <- grepl("pieceFillGradient", result_pf_gradient$svg_content)
has_piece_3_stops <- grepl('offset="50%".*stop-color="#cccccc"', result_pf_gradient$svg_content)
pieces_use_gradient <- grepl('fill="url\\(#pieceFillGradient\\)"', result_pf_gradient$svg_content)
log_info("  Piece gradient defined: {if (has_piece_gradient) 'PASS' else 'FAIL'}")
log_info("  Has 3 stops (middle): {if (has_piece_3_stops) 'PASS' else 'FAIL'}")
log_info("  Pieces use gradient: {if (pieces_use_gradient) 'PASS' else 'FAIL'}")

# ===== Test 3: Combined modes =====
log_subheader("Test 3: Combined Fill Modes")

# Test 3: Both background and piece gradient
log_info("Testing both background AND piece gradient")
result_combined <- generate_puzzle(
  type = "hexagonal",
  grid = c(3),
  size = c(200),
  seed = 42,
  offset = 15,
  fill_color = list(
    type = "gradient",
    center = "#ffffff",
    middle = "#888888",
    edge = "#000000"
  ),
  background = list(
    type = "gradient",
    center = "#ffe0e0",
    middle = "#e0ffe0",
    edge = "#e0e0ff"
  ),
  save_files = TRUE,
  output_dir = "output",
  filename_prefix = "fill_test_combined"
)
has_both_gradients <- grepl("pieceFillGradient", result_combined$svg_content) &&
                      grepl("puzzleGradient", result_combined$svg_content)
log_info("  Both gradients present: {if (has_both_gradients) 'PASS' else 'FAIL'}")

# ===== Summary =====
log_header("Test Summary")

all_tests_passed <- has_no_rect_bg && has_solid_bg && has_gradient_bg &&
                    has_3_stops && has_correct_colors &&
                    has_none_fill && has_solid_fill &&
                    has_piece_gradient && pieces_use_gradient &&
                    has_both_gradients

log_info("Generated files in output/:")
log_info("  - fill_test_bg_none_separated.svg")
log_info("  - fill_test_bg_solid_separated.svg")
log_info("  - fill_test_bg_gradient_separated.svg")
log_info("  - fill_test_piece_none_separated.svg")
log_info("  - fill_test_piece_solid_separated.svg")
log_info("  - fill_test_piece_gradient_separated.svg")
log_info("  - fill_test_combined_separated.svg")

if (all_tests_passed) {
  log_success("All fill mode tests PASSED!")
} else {
  log_error("Some tests FAILED - check output above")
}
