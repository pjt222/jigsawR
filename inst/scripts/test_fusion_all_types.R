# Test fusion functionality for all puzzle types
# Tests that fusion generation and rendering works for rectangular, hexagonal, and concentric puzzles

devtools::load_all()

cat("Testing fusion for all puzzle types\n")
cat("====================================\n\n")

# Test 1: Rectangular fusion
cat("1. Testing rectangular fusion...\n")
result <- generate_puzzle(
  type = "rectangular",
  seed = 42,
  grid = c(2, 2),
  size = c(200, 200),
  offset = 10,
  fusion_groups = list(c(1, 2)),
  fusion_style = "none",
  save_files = FALSE
)

# Check fusion metadata on pieces
stopifnot("pieces" %in% names(result))
stopifnot(length(result$pieces) == 4)

# Check that pieces have fused_edges metadata
piece1 <- result$pieces[[1]]
piece2 <- result$pieces[[2]]
stopifnot(!is.null(piece1$fused_edges))
stopifnot(!is.null(piece2$fused_edges))

# Check that piece 1 (top-left) has fused edge E (right) with piece 2
stopifnot(isTRUE(piece1$fused_edges$E))

# Check that piece 2 has fused edge W (left) with piece 1
stopifnot(isTRUE(piece2$fused_edges$W))

cat("   ✓ Rectangular fusion metadata correct\n")

# Test 2: Hexagonal fusion
cat("\n2. Testing hexagonal fusion...\n")
result_hex <- generate_puzzle(
  type = "hexagonal",
  seed = 42,
  grid = c(2),  # 2 rings = 7 pieces
  size = c(200),
  offset = 10,
  fusion_groups = list(c(1, 2)),
  fusion_style = "none",
  save_files = FALSE
)

stopifnot("pieces" %in% names(result_hex))
stopifnot(length(result_hex$pieces) == 7)  # 3*2*1 + 1 = 7 pieces

# Check that pieces have fused_edges metadata
hex_piece1 <- result_hex$pieces[[1]]
hex_piece2 <- result_hex$pieces[[2]]
stopifnot(!is.null(hex_piece1$fused_edges))
stopifnot(!is.null(hex_piece2$fused_edges))

# Check edge names are correct (0-5 for hexagonal)
edge_names <- names(hex_piece1$fused_edges)
stopifnot(all(edge_names %in% c("0", "1", "2", "3", "4", "5")))

# Check that at least one edge is fused
has_fused <- any(unlist(hex_piece1$fused_edges)) || any(unlist(hex_piece2$fused_edges))
stopifnot(has_fused)

cat("   ✓ Hexagonal fusion metadata correct\n")

# Test 3: Concentric fusion
cat("\n3. Testing concentric fusion...\n")
result_conc <- generate_puzzle(
  type = "concentric",
  seed = 42,
  grid = c(2),  # 2 rings = 7 pieces
  size = c(200),
  offset = 10,
  fusion_groups = list(c(1, 2)),
  fusion_style = "none",
  save_files = FALSE
)

stopifnot("pieces" %in% names(result_conc))
stopifnot(length(result_conc$pieces) == 7)  # 3*2*1 + 1 = 7 pieces

# Check center piece (piece 1, ring 0) has 6 edges
conc_center <- result_conc$pieces[[1]]
stopifnot(!is.null(conc_center$fused_edges))
center_edge_names <- names(conc_center$fused_edges)
stopifnot(all(center_edge_names %in% c("1", "2", "3", "4", "5", "6")))

# Check trapezoid piece (piece 2, ring 1) has 4 edges
conc_trap <- result_conc$pieces[[2]]
stopifnot(!is.null(conc_trap$fused_edges))
trap_edge_names <- names(conc_trap$fused_edges)
stopifnot(all(trap_edge_names %in% c("INNER", "RIGHT", "OUTER", "LEFT")))

# Check that center piece edge to piece 2 is fused
# Center piece edge "1" connects to piece 2's INNER edge
has_fused_conc <- any(unlist(conc_center$fused_edges)) || any(unlist(conc_trap$fused_edges))
stopifnot(has_fused_conc)

cat("   ✓ Concentric fusion metadata correct\n")

# Test 4: Test fusion with offset > 0 (separated pieces)
cat("\n4. Testing fusion with separated pieces...\n")
for (ptype in c("rectangular", "hexagonal", "concentric")) {
  grid <- if (ptype == "rectangular") c(2, 2) else c(2)
  result <- generate_puzzle(
    type = ptype,
    seed = 42,
    grid = grid,
    size = c(200, 200),
    offset = 20,  # Significant separation
    fusion_groups = list(c(1, 2)),
    fusion_style = "dashed",
    fusion_opacity = 0.5,
    save_files = FALSE
  )

  # Check that fused_edges metadata is preserved after positioning
  piece1 <- result$pieces[[1]]
  stopifnot(!is.null(piece1$fused_edges))

  # Check SVG contains content
  stopifnot(nchar(result$svg_content) > 100)

  cat(sprintf("   ✓ %s fusion preserved with offset=20\n", ptype))
}

# Test 5: Test SVG rendering with fusion
cat("\n5. Testing SVG rendering with fusion styling...\n")
for (fusion_style in c("none", "dashed", "solid")) {
  result <- generate_puzzle(
    type = "rectangular",
    seed = 42,
    grid = c(2, 2),
    size = c(200, 200),
    offset = 10,
    fusion_groups = list(c(1, 2)),
    fusion_style = fusion_style,
    fusion_opacity = 0.5,
    save_files = FALSE
  )

  stopifnot(nchar(result$svg_content) > 100)

  # Check that SVG is valid XML
  stopifnot(grepl("^<\\?xml", result$svg_content))
  stopifnot(grepl("</svg>", result$svg_content))

  cat(sprintf("   ✓ fusion_style='%s' renders valid SVG\n", fusion_style))
}

# Test 6: Generate visual output files
cat("\n6. Generating visual test files...\n")

if (!dir.exists("output")) dir.create("output")

# Rectangular with fusion
result_rect <- generate_puzzle(
  type = "rectangular",
  seed = 42,
  grid = c(3, 4),
  size = c(400, 300),
  offset = 15,
  fusion_groups = list(c(1, 2, 5, 6)),  # Fuse top-left 2x2 block
  fusion_style = "none",
  save_files = TRUE,
  output_dir = "output",
  filename_prefix = "test_fusion_rect"
)
cat("   ✓ Saved: output/test_fusion_rect.svg\n")

# Hexagonal with fusion
result_hex2 <- generate_puzzle(
  type = "hexagonal",
  seed = 42,
  grid = c(3),
  size = c(300),
  offset = 15,
  fusion_groups = list(c(1, 2, 3)),  # Fuse center + 2 adjacent
  fusion_style = "dashed",
  fusion_opacity = 0.3,
  save_files = TRUE,
  output_dir = "output",
  filename_prefix = "test_fusion_hex"
)
cat("   ✓ Saved: output/test_fusion_hex.svg\n")

# Concentric with fusion
result_conc2 <- generate_puzzle(
  type = "concentric",
  seed = 42,
  grid = c(3),
  size = c(300),
  offset = 15,
  fusion_groups = list(c(1, 2, 3)),  # Fuse center + 2 adjacent
  fusion_style = "solid",
  fusion_opacity = 0.5,
  save_files = TRUE,
  output_dir = "output",
  filename_prefix = "test_fusion_conc"
)
cat("   ✓ Saved: output/test_fusion_conc.svg\n")

cat("\n====================================\n")
cat("All fusion tests PASSED\n")
cat("====================================\n")
