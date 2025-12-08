devtools::load_all()

cat("Testing fusion style modes\n")
cat("==========================\n\n")

for (style in c("none", "dashed", "solid")) {
  result <- generate_puzzle(
    type = "concentric",
    seed = 42,
    grid = c(2),
    size = c(200),
    offset = 40,
    fusion_groups = list(c(1,2), c(5,6,7)),
    fusion_style = style,
    fusion_opacity = 0.5,
    save_files = FALSE
  )

  svg <- result$svg_content

  # Count dashed patterns
  matches <- gregexpr("stroke-dasharray", svg)[[1]]
  n_dashed <- if (matches[1] == -1) 0 else length(matches)

  # Check for opacity values
  has_opacity_0 <- grepl('opacity="0.00"', svg)
  has_opacity_50 <- grepl('opacity="0.50"', svg)

  cat(sprintf("Style '%s':\n", style))
  cat(sprintf("  - Dashed path count: %d\n", n_dashed))
  cat(sprintf("  - Has opacity 0.00: %s\n", has_opacity_0))
  cat(sprintf("  - Has opacity 0.50: %s\n", has_opacity_50))

  # Extract all fused edge lines
  lines <- strsplit(svg, "\n")[[1]]
  fused_lines <- lines[grepl("opacity=", lines)]
  cat(sprintf("  - Fused edge lines: %d\n", length(fused_lines)))

  if (length(fused_lines) > 0) {
    cat("  - Sample fused edge:\n")
    # Show just attributes, not full path
    sample <- fused_lines[1]
    attrs <- regmatches(sample, gregexpr('(stroke-dasharray|opacity)="[^"]*"', sample))[[1]]
    cat(sprintf("    %s\n", paste(attrs, collapse = " ")))
  }
  cat("\n")
}

cat("Expected behavior:\n")
cat("  - 'none': opacity=0.00, no stroke-dasharray (edges invisible)\n")
cat("  - 'dashed': opacity=0.50, WITH stroke-dasharray (dashed edges)\n")
cat("  - 'solid': opacity=0.50, NO stroke-dasharray (solid edges)\n")
