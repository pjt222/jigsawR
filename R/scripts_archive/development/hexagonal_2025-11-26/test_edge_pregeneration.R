#!/usr/bin/env Rscript
# Test edge pre-generation system

source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_pregeneration.R")

cat("Testing hexagonal edge pre-generation\n")
cat("======================================\n\n")

rings <- 2  # Start with simple 2-ring puzzle
num_pieces <- 3 * rings * (rings - 1) + 1

cat(sprintf("Generating edges for %d-ring puzzle (%d pieces)...\n", rings, num_pieces))

edges <- generate_all_hex_edges(
  rings = rings,
  seed = 42,
  diameter = 240,
  tabsize = 27,
  jitter = 5
)

cat(sprintf("Generated %d edge entries\n\n", length(edges)))

# Analyze edge types
border_count <- sum(sapply(edges, function(e) e$type == "border"))
tab_count <- sum(sapply(edges, function(e) e$type == "tab"))

cat("Edge type distribution:\n")
cat(sprintf("  Border edges: %d\n", border_count))
cat(sprintf("  Tab edges: %d\n", tab_count))
cat(sprintf("  Total: %d\n\n", border_count + tab_count))

# Test specific edges
cat("Testing center piece edges:\n")
for (side in 0:5) {
  edge_key <- sprintf("1-%d", side)
  edge <- edges[[edge_key]]
  cat(sprintf("  Side %d: type=%s, neighbor=%d\n",
              side, edge$type, edge$neighbor_id))
}

cat("\n")

# Test complementarity
cat("Testing edge complementarity:\n")
cat("Checking piece 1 side 0 and its neighbor (piece 2)...\n")

edge_1_0 <- edges[["1-0"]]
cat(sprintf("  Piece 1, side 0: neighbor=%d\n", edge_1_0$neighbor_id))
cat(sprintf("  Forward path preview: %s...\n", substring(edge_1_0$forward, 1, 50)))

# Find corresponding edge from piece 2's perspective
neighbor_id <- edge_1_0$neighbor_id
for (side in 0:5) {
  edge_key <- sprintf("%d-%d", neighbor_id, side)
  edge <- edges[[edge_key]]
  if (!is.na(edge$neighbor_id) && edge$neighbor_id == 1) {
    cat(sprintf("  Piece %d, side %d: neighbor=%d\n", neighbor_id, side, edge$neighbor_id))
    cat(sprintf("  Forward path preview: %s...\n", substring(edge$forward, 1, 50)))

    # Check if paths are complementary
    if (edge_1_0$forward == edge$reverse) {
      cat("  ✓ PATHS ARE COMPLEMENTARY!\n")
    } else {
      cat("  ✗ Paths do NOT match\n")
      cat("    Piece 1 forward: ", substring(edge_1_0$forward, 1, 40), "...\n")
      cat("    Piece 2 reverse: ", substring(edge$reverse, 1, 40), "...\n")
    }
    break
  }
}

cat("\n")
cat("Edge pre-generation test complete!\n")
