# Generate workflow diagrams from putior annotations
#
# Produces Mermaid markdown files in man/figures/ for embedding in
# README.md and Quarto documentation.
#
# Usage:
#   Rscript inst/scripts/generate_workflow_diagrams.R

if (!requireNamespace("putior", quietly = TRUE)) {
  stop("putior package is required. Install with: remotes::install_github('pjt222/putior')")
}

library(putior)

# Extract annotations from R source files
workflow <- put("R/")
cat(sprintf("Extracted %d annotations from %d files\n", nrow(workflow), length(unique(workflow$file_name))))

# Ensure output directory exists
dir.create("man/figures", recursive = TRUE, showWarnings = FALSE)

# --- Overview diagram (main pipeline only) ---
# Filter to core pipeline nodes for a clean overview
overview_ids <- c(

  "generate_puzzle", "type_dispatch",
  "rect_gen", "hex_gen", "conc_gen", "vor_gen", "rand_gen", "snic_gen",
  "positioning", "svg_render"
)
overview <- workflow[workflow$id %in% overview_ids, ]

overview_mermaid <- put_diagram(
  overview,
  output = "raw",
  direction = "TD",
  theme = "github",
  node_labels = "label",
  show_workflow_boundaries = TRUE
)

writeLines(
  c("```mermaid", overview_mermaid, "```"),
  "man/figures/workflow-overview.md"
)
cat("Wrote man/figures/workflow-overview.md\n")

# --- Full architecture diagram (all nodes) ---
full_mermaid <- put_diagram(
  workflow,
  output = "raw",
  direction = "LR",
  theme = "github",
  node_labels = "label",
  show_workflow_boundaries = TRUE
)

writeLines(
  c("```mermaid", full_mermaid, "```"),
  "man/figures/workflow-architecture.md"
)
cat("Wrote man/figures/workflow-architecture.md\n")
