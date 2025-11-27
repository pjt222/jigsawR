#!/usr/bin/env Rscript
# Test hexagonal neighbor mapping

source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")

cat("Testing hexagonal neighbor mapping\n")
cat("====================================\n\n")

rings <- 3

# Test center piece
cat("CENTER PIECE (ID 1):\n")
for (side in 0:5) {
  neighbor <- get_hex_neighbor(1, side, rings)
  cat(sprintf("  Side %d: neighbor = %d\n", side, neighbor))
}

cat("\n")

# Test ring 1 piece
cat("RING 1 PIECE (ID 2, position 0):\n")
for (side in 0:5) {
  neighbor <- get_hex_neighbor(2, side, rings)
  if (is.na(neighbor)) {
    cat(sprintf("  Side %d: BORDER\n", side))
  } else {
    cat(sprintf("  Side %d: neighbor = %d\n", side, neighbor))
  }
}

cat("\n")

# Test another ring 1 piece
cat("RING 1 PIECE (ID 3, position 1):\n")
for (side in 0:5) {
  neighbor <- get_hex_neighbor(3, side, rings)
  if (is.na(neighbor)) {
    cat(sprintf("  Side %d: BORDER\n", side))
  } else {
    cat(sprintf("  Side %d: neighbor = %d\n", side, neighbor))
  }
}

cat("\n")

# Build full neighbor map
cat("Building complete neighbor map...\n")
neighbor_map <- build_hex_neighbor_map(rings)

# Count border edges (NA neighbors)
border_count <- sum(is.na(neighbor_map$neighbor_id))
internal_count <- sum(!is.na(neighbor_map$neighbor_id))

cat(sprintf("Total edges: %d\n", nrow(neighbor_map)))
cat(sprintf("Internal edges: %d\n", internal_count))
cat(sprintf("Border edges: %d\n", border_count))

cat("\n")

# Check reciprocity: if piece A connects to piece B on side X,
# then piece B should connect to piece A
cat("Checking reciprocity...\n")
non_reciprocal <- 0
for (i in 1:nrow(neighbor_map)) {
  row <- neighbor_map[i, ]
  if (!is.na(row$neighbor_id)) {
    # Find the reverse connection
    reverse <- neighbor_map[
      neighbor_map$piece_id == row$neighbor_id &
      neighbor_map$neighbor_id == row$piece_id,
    ]
    if (nrow(reverse) == 0) {
      cat(sprintf("  NON-RECIPROCAL: piece %d side %d → piece %d (no reverse)\n",
                  row$piece_id, row$side, row$neighbor_id))
      non_reciprocal <- non_reciprocal + 1
    }
  }
}

if (non_reciprocal == 0) {
  cat("  ✓ All connections are reciprocal!\n")
} else {
  cat(sprintf("  ✗ Found %d non-reciprocal connections\n", non_reciprocal))
}
