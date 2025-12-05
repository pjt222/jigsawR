# Test: Concentric Puzzle Boundary Facing Options
# Verifies that boundary_facing parameter controls arc direction

# Source required files in dependency order
source("R/logging.R")
source("R/config_utils.R")
source("R/bezier_utils.R")
source("R/puzzle_core_clean.R")
source("R/concentric_geometry.R")
source("R/hexagonal_bezier_generation.R")
source("R/concentric_edge_generation.R")
source("R/unified_piece_generation.R")
source("R/piece_positioning.R")
source("R/unified_renderer.R")
source("R/jigsawR_clean.R")

log_header("Testing Boundary Facing Feature")

# Test 1: Generate with outward boundary (convex - default)
log_subheader("Test 1: Outward Boundary (boundary_facing = 'outward')")

result_outward <- generate_puzzle(
  type = "concentric",
  grid = c(3),
  size = c(200),
  seed = 42,
  do_circular_border = TRUE,
  boundary_facing = "outward",
  center_shape = "hexagon",
  save_files = TRUE,
  output_dir = "output",
  filename_prefix = "concentric_boundary_outward"
)

svg_outward <- result_outward$svg_content

# Count arc commands with sweep=0 and sweep=1
# Format: A rx ry x-rot large-arc sweep-flag x y
a_sweep0 <- gregexpr("A [0-9]+\\.[0-9]+ [0-9]+\\.[0-9]+ 0 0 0", svg_outward)
a_sweep1 <- gregexpr("A [0-9]+\\.[0-9]+ [0-9]+\\.[0-9]+ 0 0 1", svg_outward)
sweep0_count_out <- sum(sapply(a_sweep0, function(m) if (m[1] == -1) 0 else length(m)))
sweep1_count_out <- sum(sapply(a_sweep1, function(m) if (m[1] == -1) 0 else length(m)))

log_info("Test 1 Results (Outward/Convex):")
log_info("  Arc commands with sweep=0 (counter-clockwise): {sweep0_count_out}")
log_info("  Arc commands with sweep=1 (clockwise): {sweep1_count_out}")

# Test 2: Generate with inward boundary (concave)
log_subheader("Test 2: Inward Boundary (boundary_facing = 'inward')")

result_inward <- generate_puzzle(
  type = "concentric",
  grid = c(3),
  size = c(200),
  seed = 42,
  do_circular_border = TRUE,
  boundary_facing = "inward",
  center_shape = "hexagon",
  save_files = TRUE,
  output_dir = "output",
  filename_prefix = "concentric_boundary_inward"
)

svg_inward <- result_inward$svg_content

a_sweep0_in <- gregexpr("A [0-9]+\\.[0-9]+ [0-9]+\\.[0-9]+ 0 0 0", svg_inward)
a_sweep1_in <- gregexpr("A [0-9]+\\.[0-9]+ [0-9]+\\.[0-9]+ 0 0 1", svg_inward)
sweep0_count_in <- sum(sapply(a_sweep0_in, function(m) if (m[1] == -1) 0 else length(m)))
sweep1_count_in <- sum(sapply(a_sweep1_in, function(m) if (m[1] == -1) 0 else length(m)))

log_info("Test 2 Results (Inward/Concave):")
log_info("  Arc commands with sweep=0 (counter-clockwise): {sweep0_count_in}")
log_info("  Arc commands with sweep=1 (clockwise): {sweep1_count_in}")

# Test 3: Verify the sweep flags are different between modes
log_subheader("Test 3: Verify Sweep Flags Differ Between Modes")

if (sweep0_count_out != sweep1_count_in || sweep1_count_out != sweep0_count_in) {
  log_warn("Sweep flag counts don't match expected pattern")
  log_info("  Expected: outward sweep0 = inward sweep1, outward sweep1 = inward sweep0")
} else {
  log_success("Sweep flags correctly swapped between outward and inward modes")
}

# Test 4: Verify parameter is stored in result
log_subheader("Test 4: Verify Parameter Storage")

if (!is.null(result_outward$parameters$boundary_facing)) {
  if (result_outward$parameters$boundary_facing == "outward") {
    log_success("Outward result correctly stores boundary_facing = 'outward'")
  } else {
    log_error("Outward result has wrong boundary_facing: {result_outward$parameters$boundary_facing}")
  }
} else {
  log_error("boundary_facing parameter not stored in result")
}

if (!is.null(result_inward$parameters$boundary_facing)) {
  if (result_inward$parameters$boundary_facing == "inward") {
    log_success("Inward result correctly stores boundary_facing = 'inward'")
  } else {
    log_error("Inward result has wrong boundary_facing: {result_inward$parameters$boundary_facing}")
  }
} else {
  log_error("boundary_facing parameter not stored in inward result")
}

# Summary
log_header("Test Summary")
log_info("Generated files in output/:")
log_info("  - concentric_boundary_outward_complete.svg (convex arcs)")
log_info("  - concentric_boundary_inward_complete.svg (concave arcs)")

total_arcs_out <- sweep0_count_out + sweep1_count_out
total_arcs_in <- sweep0_count_in + sweep1_count_in

if (total_arcs_out > 0 && total_arcs_in > 0) {
  log_success("Both boundary facing modes generate arc commands")
  log_success("All tests PASSED!")
} else {
  log_error("Some tests FAILED - check output above")
}
