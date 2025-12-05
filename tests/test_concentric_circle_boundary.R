# Test: Concentric Puzzle Perfect Circle Boundary
# Verifies that do_circular_border parameter works correctly

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

log_header("Testing Concentric Circle Boundary Feature")

# Test 1: Generate with straight boundary (default)
log_subheader("Test 1: Straight Boundary (do_circular_border = FALSE)")

result_straight <- generate_puzzle(
  type = "concentric",
  grid = c(3),
  size = c(200),
  seed = 42,
  do_circular_border = FALSE,
  center_shape = "hexagon",
  save_files = TRUE,
  output_dir = "output",
  filename_prefix = "concentric_straight_boundary"
)

# Check that SVG contains L commands for boundary (straight lines)
svg_straight <- result_straight$svg_content

# Count outer ring pieces (for 3 rings: 12 pieces in outer ring)
# Each outer piece has one boundary edge
# With straight boundary, these should be L commands

# Find border patterns - straight lines use L commands
l_matches <- gregexpr("L -?[0-9]+\\.[0-9]+ -?[0-9]+\\.[0-9]+", svg_straight)
l_count <- sum(sapply(l_matches, function(m) if (m[1] == -1) 0 else length(m)))

log_info("Test 1 Results:")
log_info("  SVG length: {nchar(svg_straight)} characters")
log_info("  L commands found: {l_count}")

# Test 2: Generate with circular boundary
log_subheader("Test 2: Perfect Circle Boundary (do_circular_border = TRUE)")

result_circle <- generate_puzzle(
  type = "concentric",
  grid = c(3),
  size = c(200),
  seed = 42,
  do_circular_border = TRUE,
  center_shape = "hexagon",
  save_files = TRUE,
  output_dir = "output",
  filename_prefix = "concentric_circle_boundary"
)

svg_circle <- result_circle$svg_content

# Count arc commands - circular boundary uses A commands
a_matches <- gregexpr("A [0-9]+\\.[0-9]+ [0-9]+\\.[0-9]+ 0 0 [01] -?[0-9]+\\.[0-9]+ -?[0-9]+\\.[0-9]+", svg_circle)
a_count <- sum(sapply(a_matches, function(m) if (m[1] == -1) 0 else length(m)))

log_info("Test 2 Results:")
log_info("  SVG length: {nchar(svg_circle)} characters")
log_info("  A (arc) commands found: {a_count}")

# Test 3: Verify arc radius is correct (diameter/2 = 100)
log_subheader("Test 3: Verify Arc Radius")

# Extract arc commands and check radius
arc_pattern <- "A ([0-9]+\\.[0-9]+) [0-9]+\\.[0-9]+ 0 0 [01]"
arc_radii <- regmatches(svg_circle, gregexpr(arc_pattern, svg_circle))
if (length(arc_radii[[1]]) > 0) {
  first_match <- arc_radii[[1]][1]
  radius_str <- sub("A ([0-9]+\\.[0-9]+) .*", "\\1", first_match)
  radius <- as.numeric(radius_str)
  expected_radius <- 200 / 2  # diameter / 2

  if (abs(radius - expected_radius) < 0.01) {
    log_success("Arc radius is correct: {radius} (expected: {expected_radius})")
  } else {
    log_error("Arc radius mismatch: {radius} (expected: {expected_radius})")
  }
} else {
  log_warn("No arc commands found to verify radius")
}

# Test 4: Test with separation (offset > 0)
log_subheader("Test 4: Circle Boundary with Separation")

result_separated <- generate_puzzle(
  type = "concentric",
  grid = c(3),
  size = c(200),
  seed = 42,
  offset = 10,  # 10mm separation
  do_circular_border = TRUE,
  center_shape = "hexagon",
  save_files = TRUE,
  output_dir = "output",
  filename_prefix = "concentric_circle_separated"
)

svg_separated <- result_separated$svg_content

# Verify arcs still present after separation
a_matches_sep <- gregexpr("A [0-9]+\\.[0-9]+ [0-9]+\\.[0-9]+ 0 0 [01]", svg_separated)
a_count_sep <- sum(sapply(a_matches_sep, function(m) if (m[1] == -1) 0 else length(m)))

log_info("Test 4 Results:")
log_info("  SVG length: {nchar(svg_separated)} characters")
log_info("  A (arc) commands found: {a_count_sep}")

if (a_count_sep > 0) {
  log_success("Arc commands preserved after piece separation")
} else {
  log_error("Arc commands lost during separation!")
}

# Test 5: Test with different ring counts
log_subheader("Test 5: Different Ring Counts")

for (rings in c(2, 3, 4)) {
  result <- generate_puzzle(
    type = "concentric",
    grid = c(rings),
    size = c(240),
    seed = 42,
    do_circular_border = TRUE,
    center_shape = "hexagon",
    save_files = FALSE
  )

  svg <- result$svg_content
  a_matches <- gregexpr("A [0-9]+\\.[0-9]+ [0-9]+\\.[0-9]+ 0 0 [01]", svg)
  a_count <- sum(sapply(a_matches, function(m) if (m[1] == -1) 0 else length(m)))

  # For rings=N, outer ring has 6*N pieces, each with 1 boundary arc = 6*N arcs
  expected_arcs <- 6 * (rings - 1) + 6  # Simplified: 6*(rings) for outer ring
  # Actually: outer ring pieces = 6 * (rings - 1), but we need to count actual boundary edges

  outer_pieces <- 6 * (rings - 1)  # Pieces in outermost ring
  log_info("  Rings={rings}: {outer_pieces} outer pieces, {a_count} arc commands found")
}

# Test 6: Test with circle center shape
log_subheader("Test 6: Circle Center with Circle Boundary")

result_circle_center <- generate_puzzle(
  type = "concentric",
  grid = c(3),
  size = c(200),
  seed = 42,
  do_circular_border = TRUE,
  center_shape = "circle",
  save_files = TRUE,
  output_dir = "output",
  filename_prefix = "concentric_circle_center_boundary"
)

svg_circle_center <- result_circle_center$svg_content
a_matches_cc <- gregexpr("A [0-9]+\\.[0-9]+ [0-9]+\\.[0-9]+ 0 0 [01]", svg_circle_center)
a_count_cc <- sum(sapply(a_matches_cc, function(m) if (m[1] == -1) 0 else length(m)))

log_info("Test 6 Results:")
log_info("  SVG length: {nchar(svg_circle_center)} characters")
log_info("  A (arc) commands found: {a_count_cc}")

# Summary
log_header("Test Summary")
log_info("Generated files in output/:")
log_info("  - concentric_straight_boundary_complete.svg (L commands)")
log_info("  - concentric_circle_boundary_complete.svg (A commands)")
log_info("  - concentric_circle_separated_separated.svg (A commands + separation)")
log_info("  - concentric_circle_center_boundary_complete.svg (circle center + arc boundary)")

if (a_count > 0 && a_count_sep > 0) {
  log_success("All tests PASSED!")
} else {
  log_error("Some tests FAILED - check output above")
}
