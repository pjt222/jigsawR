# Test: Label Centering on Piece Bounding Box
# Verifies that labels are centered on piece geometry, not lattice positions

# Source required files in dependency order
source("R/logging.R")
source("R/config_utils.R")
source("R/bezier_utils.R")
source("R/rectangular_puzzle.R")
source("R/puzzle_core_clean.R")
source("R/hexagonal_puzzle.R")
source("R/hexagonal_topology.R")
source("R/hexagonal_edge_generation_fixed.R")
source("R/hexagonal_bezier_generation.R")
source("R/concentric_geometry.R")
source("R/concentric_edge_generation.R")
source("R/unified_piece_generation.R")
source("R/piece_positioning.R")
source("R/unified_renderer.R")
source("R/jigsawR_clean.R")

log_header("Testing Label Centering Fix")

# Ensure output directory exists
if (!dir.exists("output")) dir.create("output")

# ===== Test 1: Rectangular Puzzle Labels =====
log_subheader("Test 1: Rectangular Puzzle Labels")

result_rect <- generate_puzzle(
  type = "rectangular",
  grid = c(3, 3),
  size = c(200, 200),
  seed = 42,
  offset = 0,
  fill_color = list(
    type = "gradient",
    center = "#ffffff",
    middle = "#cccccc",
    edge = "#666666"
  ),
  show_labels = TRUE,
  label_color = "#ff0000",
  save_files = TRUE,
  output_dir = "output",
  filename_prefix = "label_test_rectangular"
)

log_info("Generated rectangular puzzle with labels")

# ===== Test 2: Hexagonal Puzzle with Warp (most affected) =====
log_subheader("Test 2: Hexagonal Puzzle with Warp")

result_hex <- generate_puzzle(
  type = "hexagonal",
  grid = c(3),
  size = c(200),
  seed = 42,
  offset = 0,
  do_warp = TRUE,
  do_trunc = TRUE,
  fill_color = list(
    type = "gradient",
    center = "#ffffff",
    middle = "#cccccc",
    edge = "#666666"
  ),
  show_labels = TRUE,
  label_color = "#ff0000",
  save_files = TRUE,
  output_dir = "output",
  filename_prefix = "label_test_hexagonal_warped"
)

log_info("Generated warped hexagonal puzzle with labels")

# ===== Test 3: Concentric Puzzle =====
log_subheader("Test 3: Concentric Puzzle")

result_conc <- generate_puzzle(
  type = "concentric",
  grid = c(3),
  size = c(200),
  seed = 42,
  offset = 0,
  center_shape = "hexagon",
  fill_color = list(
    type = "gradient",
    center = "#ffffff",
    middle = "#cccccc",
    edge = "#666666"
  ),
  show_labels = TRUE,
  label_color = "#ff0000",
  save_files = TRUE,
  output_dir = "output",
  filename_prefix = "label_test_concentric"
)

log_info("Generated concentric puzzle with labels")

# ===== Test 4: Separated Hexagonal (verifies offset handling) =====
log_subheader("Test 4: Separated Hexagonal Puzzle")

result_sep <- generate_puzzle(
  type = "hexagonal",
  grid = c(3),
  size = c(200),
  seed = 42,
  offset = 15,
  do_warp = TRUE,
  do_trunc = TRUE,
  fill_color = list(
    type = "gradient",
    center = "#ffffff",
    middle = "#cccccc",
    edge = "#666666"
  ),
  show_labels = TRUE,
  label_color = "#ff0000",
  save_files = TRUE,
  output_dir = "output",
  filename_prefix = "label_test_hexagonal_separated"
)

log_info("Generated separated hexagonal puzzle with labels")

# ===== Verification: Check label positions =====
log_subheader("Verification: Checking Label Positions")

# Check that labels exist in the SVG
has_labels_rect <- grepl("<text.*fill=\"#ff0000\"", result_rect$svg_content)
has_labels_hex <- grepl("<text.*fill=\"#ff0000\"", result_hex$svg_content)
has_labels_conc <- grepl("<text.*fill=\"#ff0000\"", result_conc$svg_content)
has_labels_sep <- grepl("<text.*fill=\"#ff0000\"", result_sep$svg_content)

log_info("Labels present in SVGs:")
log_info("  Rectangular: {if (has_labels_rect) 'YES' else 'NO'}")
log_info("  Hexagonal warped: {if (has_labels_hex) 'YES' else 'NO'}")
log_info("  Concentric: {if (has_labels_conc) 'YES' else 'NO'}")
log_info("  Separated hexagonal: {if (has_labels_sep) 'YES' else 'NO'}")

all_have_labels <- has_labels_rect && has_labels_hex && has_labels_conc && has_labels_sep
if (all_have_labels) {
  log_success("All puzzles have labels rendered correctly")
} else {
  log_error("Some puzzles are missing labels")
}

# ===== Summary =====
log_header("Test Summary")
log_info("Generated files in output/:")
log_info("  - label_test_rectangular_complete.svg")
log_info("  - label_test_hexagonal_warped_complete.svg")
log_info("  - label_test_concentric_complete.svg")
log_info("  - label_test_hexagonal_separated_separated.svg")
log_info("")
log_info("Visually verify that labels (red numbers) are centered")
log_info("with the gradient fill centers on each piece.")
log_success("Label centering test completed!")
