devtools::load_all()

cat("Fusion Parameters Flow Diagnostic\n")
cat("==================================\n\n")

# Simulate the exact flow from Shiny
# UI values (as they would come from Shiny inputs)
ui_fusion_groups <- "(1,2),(5,6,7)"
ui_fusion_style <- "dashed"  # Could be "none", "dashed", "solid"
ui_fusion_opacity <- 50      # Slider value 0-100

# Server transformation (divide by 100)
server_fusion_opacity <- ui_fusion_opacity / 100

cat("=== STAGE 1: UI INPUT VALUES ===\n")
cat(sprintf("  fusion_groups (text): %s\n", ui_fusion_groups))
cat(sprintf("  fusion_style (radio): %s\n", ui_fusion_style))
cat(sprintf("  fusion_opacity (slider): %d%%\n", ui_fusion_opacity))

cat("\n=== STAGE 2: SERVER TRANSFORMATION ===\n")
cat(sprintf("  fusion_opacity after /100: %.2f\n", server_fusion_opacity))

# Parse fusion groups (as server does)
fusion_groups_parsed <- parse_fusion_input(ui_fusion_groups)
cat(sprintf("  fusion_groups parsed: %s\n",
            paste(sapply(fusion_groups_parsed, function(g) paste0("(", paste(g, collapse=","), ")")),
                  collapse = ", ")))

cat("\n=== STAGE 3: GENERATE_PIECES_INTERNAL ===\n")

# Generate pieces with fusion params
pieces_result <- generate_pieces_internal(
  type = "concentric",
  seed = 42,
  grid = c(2),
  size = c(200),
  fusion_groups = fusion_groups_parsed,
  fusion_style = ui_fusion_style,
  fusion_opacity = server_fusion_opacity
)

cat(sprintf("  Stored fusion_style: %s\n", pieces_result$parameters$fusion_style))
cat(sprintf("  Stored fusion_opacity: %.2f\n", pieces_result$parameters$fusion_opacity))

# Check pieces for fused edges
n_fused <- sum(sapply(pieces_result$pieces, function(p) {
  if (is.null(p$fused_edges)) return(0)
  sum(unlist(p$fused_edges))
}))
cat(sprintf("  Total fused edges marked: %d\n", n_fused))

cat("\n=== STAGE 4: APPLY_PIECE_POSITIONING ===\n")

positioned <- apply_piece_positioning(pieces_result, offset = 40)

cat(sprintf("  Preserved fusion_style: %s\n", positioned$parameters$fusion_style))
cat(sprintf("  Preserved fusion_opacity: %.2f\n", positioned$parameters$fusion_opacity))

cat("\n=== STAGE 5: RENDER (simulation) ===\n")

# Simulate what render_puzzle_svg does
fusion_style <- positioned$parameters$fusion_style
fusion_opacity <- positioned$parameters$fusion_opacity

has_fusion <- any(sapply(positioned$pieces, function(p) {
  !is.null(p$fused_edges) && any(unlist(p$fused_edges))
}))

effective_fusion_opacity <- if (!is.null(fusion_style) && fusion_style == "none") {
  0
} else {
  fusion_opacity %||% 0.3
}

cat(sprintf("  Retrieved fusion_style: %s\n", fusion_style))
cat(sprintf("  Retrieved fusion_opacity: %.2f\n", fusion_opacity))
cat(sprintf("  Has fusion data: %s\n", has_fusion))
cat(sprintf("  Effective opacity (used in SVG): %.2f\n", effective_fusion_opacity))

# Calculate dash pattern
stroke_width <- 1.5
if (fusion_style == "dashed") {
  dash_length <- stroke_width * 3
  gap_length <- stroke_width * 2
  cat(sprintf("  Dash pattern: stroke-dasharray=\"%.1f %.1f\"\n", dash_length, gap_length))
} else {
  cat("  Dash pattern: (none - solid or invisible)\n")
}

cat("\n=== STAGE 6: ACTUAL SVG OUTPUT ===\n")

# Generate actual SVG
svg_content <- render_puzzle_svg(
  positioned,
  fill = "none",
  stroke_width = stroke_width,
  palette = "viridis"
)

# Count fused edge paths in SVG
n_dashed <- length(gregexpr("stroke-dasharray", svg_content)[[1]])
if (n_dashed == -1) n_dashed <- 0
n_opacity <- length(gregexpr('opacity="0\\.[0-9]', svg_content)[[1]])
if (n_opacity == -1) n_opacity <- 0

cat(sprintf("  Dashed paths in SVG: %d\n", n_dashed))
cat(sprintf("  Opacity attributes (0.xx) in SVG: %d\n", n_opacity))

# Extract actual opacity values from SVG
opacity_matches <- regmatches(svg_content, gregexpr('opacity="[0-9.]+', svg_content))[[1]]
unique_opacities <- unique(gsub('opacity="', '', opacity_matches))
cat(sprintf("  Unique opacity values: %s\n", paste(unique_opacities, collapse = ", ")))

cat("\n=== VERIFICATION ===\n")

# Verify the values match expectations
expected_style <- ui_fusion_style
expected_opacity <- server_fusion_opacity

style_ok <- fusion_style == expected_style
opacity_ok <- abs(fusion_opacity - expected_opacity) < 0.001

cat(sprintf("  Style matches expected (%s): %s\n", expected_style, if(style_ok) "PASS" else "FAIL"))
cat(sprintf("  Opacity matches expected (%.2f): %s\n", expected_opacity, if(opacity_ok) "PASS" else "FAIL"))

if (fusion_style == "dashed" && n_dashed > 0) {
  cat("  Dashed pattern applied: PASS\n")
} else if (fusion_style == "dashed" && n_dashed == 0) {
  cat("  Dashed pattern applied: FAIL (expected dashed, got none)\n")
}

if (expected_opacity %in% as.numeric(unique_opacities)) {
  cat(sprintf("  Expected opacity (%.2f) found in SVG: PASS\n", expected_opacity))
} else {
  cat(sprintf("  Expected opacity (%.2f) found in SVG: FAIL (found: %s)\n",
              expected_opacity, paste(unique_opacities, collapse=", ")))
}

cat("\n=== TEST WITH DIFFERENT STYLES ===\n")

for (test_style in c("none", "dashed", "solid")) {
  test_result <- generate_pieces_internal(
    type = "concentric",
    seed = 42,
    grid = c(2),
    size = c(200),
    fusion_groups = fusion_groups_parsed,
    fusion_style = test_style,
    fusion_opacity = 0.5
  )

  test_positioned <- apply_piece_positioning(test_result, offset = 40)
  test_svg <- render_puzzle_svg(test_positioned, fill = "none", stroke_width = 1.5, palette = "viridis")

  n_dash <- length(gregexpr("stroke-dasharray", test_svg)[[1]])
  if (n_dash == -1) n_dash <- 0

  # For "none" style, opacity should be 0.00
  expected_op <- if (test_style == "none") "0.00" else "0.50"
  has_expected_op <- grepl(sprintf('opacity="%s"', expected_op), test_svg)

  cat(sprintf("  Style '%s': dashed_count=%d, has_opacity_%s=%s\n",
              test_style, n_dash, expected_op, has_expected_op))
}
