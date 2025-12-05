# Debug piece distortion in 7-ring hexagonal puzzle
# Investigating pieces: 19,36,37,59,60,61,125 and 48,73,74,75,107

source("R/logging.R")
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  if (!grepl("scripts_archive", f)) source(f)
}

cat("=== Generating 7-ring hexagonal puzzle (Clean Hexagon mode) ===\n")

# Generate puzzle with clean hexagon mode (do_trunc=TRUE, do_warp=FALSE)
pieces_result <- generate_pieces_internal(
  type = "hexagonal",
  seed = 1234,
  grid = c(7),
  size = c(400),  # 400mm diameter
  tabsize = 20,
  jitter = 4,
  do_warp = FALSE,
  do_trunc = TRUE,
  do_circular_border = FALSE
)

cat("\nTotal pieces:", length(pieces_result$pieces), "\n")
cat("Expected pieces (3*7*(7-1)+1):", 3 * 7 * (7 - 1) + 1, "\n")

# Group 1: pieces 19,36,37,59,60,61,125
group1 <- c(19, 36, 37, 59, 60, 61, 125)
# Group 2: pieces 48,73,74,75,107
group2 <- c(48, 73, 74, 75, 107)

cat("\n=== Group 1 Analysis: pieces", paste(group1, collapse=", "), "===\n")
for (pid in group1) {
  if (pid <= length(pieces_result$pieces)) {
    piece <- pieces_result$pieces[[pid]]
    cat("\nPiece", pid, ":\n")
    cat("  ID:", piece$id, "\n")
    cat("  Center: (", round(piece$center[1], 2), ",", round(piece$center[2], 2), ")\n")
    if (!is.null(piece$ring_pos)) {
      cat("  Ring:", piece$ring_pos$ring, ", Position:", piece$ring_pos$position, "\n")
    }
    # Show first 200 chars of path
    path_preview <- substr(piece$path, 1, 200)
    cat("  Path preview:", path_preview, "...\n")
  } else {
    cat("\nPiece", pid, ": OUT OF RANGE\n")
  }
}

cat("\n=== Group 2 Analysis: pieces", paste(group2, collapse=", "), "===\n")
for (pid in group2) {
  if (pid <= length(pieces_result$pieces)) {
    piece <- pieces_result$pieces[[pid]]
    cat("\nPiece", pid, ":\n")
    cat("  ID:", piece$id, "\n")
    cat("  Center: (", round(piece$center[1], 2), ",", round(piece$center[2], 2), ")\n")
    if (!is.null(piece$ring_pos)) {
      cat("  Ring:", piece$ring_pos$ring, ", Position:", piece$ring_pos$position, "\n")
    }
    # Show first 200 chars of path
    path_preview <- substr(piece$path, 1, 200)
    cat("  Path preview:", path_preview, "...\n")
  } else {
    cat("\nPiece", pid, ": OUT OF RANGE\n")
  }
}

# Generate SVG with labels for visual inspection
cat("\n=== Generating labeled SVG for visual inspection ===\n")
positioned <- apply_piece_positioning(pieces_result, offset = 0)

svg_content <- render_puzzle_svg(
  positioned,
  fill = "none",
  stroke_width = 1.0,
  palette = "viridis",
  background = "white",
  opacity = 1.0,
  show_labels = TRUE,
  label_color = "red",
  label_size = 6
)

output_file <- "output/debug_7ring_clean_hexagon_labeled.svg"
writeLines(svg_content, output_file)
cat("Saved:", output_file, "\n")

# Also generate separated view to see individual pieces better
cat("\n=== Generating separated view ===\n")
positioned_sep <- apply_piece_positioning(pieces_result, offset = 5)

svg_separated <- render_puzzle_svg(
  positioned_sep,
  fill = "#f0f0f0",
  stroke_width = 1.0,
  palette = "viridis",
  background = "white",
  opacity = 1.0,
  show_labels = TRUE,
  label_color = "black",
  label_size = 5
)

output_file_sep <- "output/debug_7ring_clean_hexagon_separated.svg"
writeLines(svg_separated, output_file_sep)
cat("Saved:", output_file_sep, "\n")

# Analyze ring distribution
cat("\n=== Ring Distribution ===\n")
ring_counts <- table(sapply(pieces_result$pieces, function(p) {
  if (!is.null(p$ring_pos)) p$ring_pos$ring else NA
}))
print(ring_counts)

# Check which pieces are on the boundary (ring 7 = outermost)
cat("\n=== Boundary pieces (ring 7) ===\n")
boundary_pieces <- which(sapply(pieces_result$pieces, function(p) {
  if (!is.null(p$ring_pos)) p$ring_pos$ring == 7 else FALSE
}))
cat("Boundary piece indices:", paste(boundary_pieces, collapse=", "), "\n")

# Check if our problem pieces are boundary pieces
cat("\n=== Are problem pieces on boundary? ===\n")
cat("Group 1 on boundary:", paste(intersect(group1, boundary_pieces), collapse=", "), "\n")
cat("Group 2 on boundary:", paste(intersect(group2, boundary_pieces), collapse=", "), "\n")

cat("\n=== Analysis complete ===\n")
