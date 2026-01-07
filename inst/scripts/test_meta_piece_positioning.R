# Test meta-piece (fused piece) positioning
# Verifies that fused pieces move together when offset > 0

devtools::load_all()

cat("Testing meta-piece positioning\n")
cat("==============================\n\n")

# Helper to extract path start point
get_path_start <- function(path) {
  segments <- parse_svg_path(path)
  if (length(segments) > 0 && segments[[1]]$type == "M") {
    return(c(x = segments[[1]]$x, y = segments[[1]]$y))
  }
  return(c(x = NA, y = NA))
}

# Test 1: Rectangular fused pieces move together
cat("1. Testing rectangular meta-piece positioning...\n")

# Generate with no offset first
result_compact <- generate_puzzle(
  type = "rectangular",
  seed = 42,
  grid = c(2, 2),
  size = c(200, 200),
  offset = 0,
  fusion_groups = list(c(1, 2)),  # Fuse top-left and top-right
  save_files = FALSE
)

# Generate with offset
result_separated <- generate_puzzle(
  type = "rectangular",
  seed = 42,
  grid = c(2, 2),
  size = c(200, 200),
  offset = 30,
  fusion_groups = list(c(1, 2)),
  save_files = FALSE
)

# Pieces 1 and 2 should have same Y offset (horizontal fusion)
# The gap between piece 1 and 2 should be 0 (they move together)
p1_compact <- result_compact$pieces[[1]]
p2_compact <- result_compact$pieces[[2]]
p1_sep <- result_separated$pieces[[1]]
p2_sep <- result_separated$pieces[[2]]

# Get path starts
p1_start_compact <- get_path_start(p1_compact$path)
p2_start_compact <- get_path_start(p2_compact$path)
p1_start_sep <- get_path_start(p1_sep$path)
p2_start_sep <- get_path_start(p2_sep$path)

# Calculate relative positions
rel_compact <- p2_start_compact - p1_start_compact
rel_sep <- p2_start_sep - p1_start_sep

cat(sprintf("   Compact: Piece 2 relative to Piece 1: (%.1f, %.1f)\n", rel_compact["x"], rel_compact["y"]))
cat(sprintf("   Separated: Piece 2 relative to Piece 1: (%.1f, %.1f)\n", rel_sep["x"], rel_sep["y"]))

# For fused pieces, relative position should be the same
if (abs(rel_compact["x"] - rel_sep["x"]) < 1 && abs(rel_compact["y"] - rel_sep["y"]) < 1) {
  cat("   ✓ Rectangular fused pieces maintain relative position\n")
} else {
  cat("   ✗ ERROR: Rectangular fused pieces did NOT maintain relative position!\n")
}

# Test 2: Hexagonal fused pieces move together
cat("\n2. Testing hexagonal meta-piece positioning...\n")

result_hex_compact <- generate_puzzle(
  type = "hexagonal",
  seed = 42,
  grid = c(2),
  size = c(200),
  offset = 0,
  fusion_groups = list(c(1, 2)),  # Fuse center with first ring piece
  save_files = FALSE
)

result_hex_sep <- generate_puzzle(
  type = "hexagonal",
  seed = 42,
  grid = c(2),
  size = c(200),
  offset = 20,
  fusion_groups = list(c(1, 2)),
  save_files = FALSE
)

# Check centers - fused pieces should have same translation
c1_compact <- result_hex_compact$pieces[[1]]$center
c2_compact <- result_hex_compact$pieces[[2]]$center
c1_sep <- result_hex_sep$pieces[[1]]$center
c2_sep <- result_hex_sep$pieces[[2]]$center

rel_compact_hex <- c2_compact - c1_compact
rel_sep_hex <- c2_sep - c1_sep

cat(sprintf("   Compact: Piece 2 relative to Piece 1: (%.1f, %.1f)\n", rel_compact_hex[1], rel_compact_hex[2]))
cat(sprintf("   Separated: Piece 2 relative to Piece 1: (%.1f, %.1f)\n", rel_sep_hex[1], rel_sep_hex[2]))

# For fused pieces, relative position should be the same
if (abs(rel_compact_hex[1] - rel_sep_hex[1]) < 1 && abs(rel_compact_hex[2] - rel_sep_hex[2]) < 1) {
  cat("   ✓ Hexagonal fused pieces maintain relative position\n")
} else {
  cat("   ✗ ERROR: Hexagonal fused pieces did NOT maintain relative position!\n")
}

# Test 3: Concentric fused pieces move together
cat("\n3. Testing concentric meta-piece positioning...\n")

result_conc_compact <- generate_puzzle(
  type = "concentric",
  seed = 42,
  grid = c(2),
  size = c(200),
  offset = 0,
  fusion_groups = list(c(1, 2)),  # Fuse center with first ring piece
  save_files = FALSE
)

result_conc_sep <- generate_puzzle(
  type = "concentric",
  seed = 42,
  grid = c(2),
  size = c(200),
  offset = 20,
  fusion_groups = list(c(1, 2)),
  save_files = FALSE
)

# Check centers
c1_compact_conc <- result_conc_compact$pieces[[1]]$center
c2_compact_conc <- result_conc_compact$pieces[[2]]$center
c1_sep_conc <- result_conc_sep$pieces[[1]]$center
c2_sep_conc <- result_conc_sep$pieces[[2]]$center

rel_compact_conc <- c2_compact_conc - c1_compact_conc
rel_sep_conc <- c2_sep_conc - c1_sep_conc

cat(sprintf("   Compact: Piece 2 relative to Piece 1: (%.1f, %.1f)\n", rel_compact_conc[1], rel_compact_conc[2]))
cat(sprintf("   Separated: Piece 2 relative to Piece 1: (%.1f, %.1f)\n", rel_sep_conc[1], rel_sep_conc[2]))

if (abs(rel_compact_conc[1] - rel_sep_conc[1]) < 1 && abs(rel_compact_conc[2] - rel_sep_conc[2]) < 1) {
  cat("   ✓ Concentric fused pieces maintain relative position\n")
} else {
  cat("   ✗ ERROR: Concentric fused pieces did NOT maintain relative position!\n")
}

# Test 4: Non-fused pieces should separate
cat("\n4. Testing that non-fused pieces DO separate...\n")

# Check piece 3 (not fused) vs piece 1 in rectangular
p3_compact <- result_compact$pieces[[3]]
p3_sep <- result_separated$pieces[[3]]
p3_start_compact <- get_path_start(p3_compact$path)
p3_start_sep <- get_path_start(p3_sep$path)

rel_p3_compact <- p3_start_compact - p1_start_compact
rel_p3_sep <- p3_start_sep - p1_start_sep

cat(sprintf("   Compact: Piece 3 relative to Piece 1: (%.1f, %.1f)\n", rel_p3_compact["x"], rel_p3_compact["y"]))
cat(sprintf("   Separated: Piece 3 relative to Piece 1: (%.1f, %.1f)\n", rel_p3_sep["x"], rel_p3_sep["y"]))

# Non-fused pieces should have different relative positions
if (abs(rel_p3_compact["y"] - rel_p3_sep["y"]) > 1) {
  cat("   ✓ Non-fused pieces correctly separate\n")
} else {
  cat("   ✗ ERROR: Non-fused pieces did NOT separate!\n")
}

cat("\n==============================\n")
cat("Meta-piece positioning tests complete\n")
