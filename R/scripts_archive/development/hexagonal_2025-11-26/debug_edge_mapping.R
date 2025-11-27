#!/usr/bin/env Rscript
# Debug edge mapping to understand the scrambling issue

source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")

cat("Analyzing edge relationships for 2-ring puzzle\n")
cat("===============================================\n\n")

rings <- 2
num_pieces <- 7

cat("PIECE-BY-PIECE ANALYSIS:\n")
cat("------------------------\n\n")

# For each piece, show its neighbors
for (piece_id in 1:num_pieces) {
  ring_info <- map_piece_id_to_ring(piece_id, rings)
  cat(sprintf("Piece %d (Ring %d, Position %d):\n",
              piece_id, ring_info$ring, ring_info$position))

  for (side in 0:5) {
    neighbor_id <- get_hex_neighbor(piece_id, side, rings)
    if (is.na(neighbor_id)) {
      cat(sprintf("  Side %d: BORDER (no neighbor)\n", side))
    } else {
      cat(sprintf("  Side %d: → Piece %d\n", side, neighbor_id))
    }
  }
  cat("\n")
}

cat("\n")
cat("EDGE MAPPING TABLE:\n")
cat("===================\n")
cat("Each shared edge should appear exactly once\n\n")

# Create edge mapping
edge_map <- list()
edge_counter <- 0

for (piece_id in 1:num_pieces) {
  for (side in 0:5) {
    neighbor_id <- get_hex_neighbor(piece_id, side, rings)

    if (!is.na(neighbor_id)) {
      # Create a unique edge identifier using sorted piece IDs
      pieces <- sort(c(piece_id, neighbor_id))
      edge_key <- sprintf("%d-%d", pieces[1], pieces[2])

      # Check if we've seen this edge already
      if (is.null(edge_map[[edge_key]])) {
        edge_counter <- edge_counter + 1

        # Find which side of the neighbor connects back
        neighbor_side <- NA
        for (ns in 0:5) {
          nn <- get_hex_neighbor(neighbor_id, ns, rings)
          if (!is.na(nn) && nn == piece_id) {
            neighbor_side <- ns
            break
          }
        }

        edge_map[[edge_key]] <- list(
          id = edge_counter,
          piece1 = piece_id,
          side1 = side,
          piece2 = neighbor_id,
          side2 = neighbor_side
        )

        cat(sprintf("Edge %2d: Piece %d (side %d) ←→ Piece %d (side %d)\n",
                    edge_counter, piece_id, side, neighbor_id, neighbor_side))
      }
    }
  }
}

cat(sprintf("\nTotal unique edges: %d\n", edge_counter))
cat(sprintf("Expected: %d pieces × 6 sides / 2 (shared) = %d edges\n",
            num_pieces, num_pieces * 3))
