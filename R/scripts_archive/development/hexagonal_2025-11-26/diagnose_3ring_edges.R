#!/usr/bin/env Rscript
source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")

cat("Analyzing 3-ring edge generation\n")
cat("=================================\n\n")

rings <- 3
seed <- 42

edge_data <- generate_hex_edge_map(
  rings = rings,
  seed = seed,
  diameter = 240,
  tabsize = 27,
  jitter = 5
)

cat(sprintf("Total unique edges: %d\n", edge_data$num_edges))
cat(sprintf("Total piece-edge mappings: %d\n\n", length(edge_data$piece_edge_map)))

# Check which pieces have edges
num_pieces <- 3 * rings * (rings - 1) + 1
cat(sprintf("Total pieces: %d\n\n", num_pieces))

# Check each piece
for (piece_id in 1:num_pieces) {
  ring_info <- map_piece_id_to_ring(piece_id, rings)
  
  # Count edges for this piece
  edge_count <- 0
  bezier_count <- 0
  border_count <- 0
  
  for (side in 0:5) {
    edge_key <- sprintf("%d-%d", piece_id, side)
    if (!is.null(edge_data$piece_edge_map[[edge_key]])) {
      edge <- edge_data$piece_edge_map[[edge_key]]
      edge_count <- edge_count + 1
      if (edge$type == "internal") {
        bezier_count <- bezier_count + 1
      } else {
        border_count <- border_count + 1
      }
    }
  }
  
  cat(sprintf("Piece %2d (ring %d): %d edges (%d bezier, %d border)\n",
              piece_id, ring_info$ring, edge_count, bezier_count, border_count))
}

cat("\n")
cat("Analysis: Checking if outer ring has bezier edges...\n")

# Check piece 8 (first piece of outer ring)
piece8_edges <- c()
for (side in 0:5) {
  edge_key <- sprintf("8-%d", side)
  edge <- edge_data$piece_edge_map[[edge_key]]
  if (!is.null(edge)) {
    cat(sprintf("Piece 8, side %d: type=%s\n", side, edge$type))
  } else {
    cat(sprintf("Piece 8, side %d: MISSING!\n", side))
  }
}
