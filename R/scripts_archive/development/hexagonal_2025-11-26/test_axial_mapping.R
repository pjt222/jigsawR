#!/usr/bin/env Rscript
# Test the new axial coordinate mapping

source("R/hexagonal_topology.R")

rings <- 3
num_pieces <- 3 * rings * (rings - 1) + 1

cat("Testing axial coordinate mapping for", num_pieces, "pieces in", rings, "rings\n\n")

# Map all pieces
for (piece_id in 1:num_pieces) {
  info <- map_piece_id_to_axial(piece_id, rings)
  cat(sprintf("Piece %2d: q=%2d, r=%2d, ring=%d, position=%2d\n",
              piece_id, info$q, info$r, info$ring, info$position))

  if (piece_id == 7 || piece_id == 19) cat("\n")  # Visual separator between rings
}

# Test that pieces 2 and 9 are neighbors (should share 2 vertices)
cat("\n=== Testing neighbor relationship ===\n")
info2 <- map_piece_id_to_axial(2, 3)
info9 <- map_piece_id_to_axial(9, 3)

cat("Piece 2: q=", info2$q, ", r=", info2$r, "\n")
cat("Piece 9: q=", info9$q, ", r=", info9$r, "\n")

# In axial coords, neighbors differ by one of the 6 directions
# Check if they're neighbors
dq <- info9$q - info2$q
dr <- info9$r - info2$r
ds <- -dq - dr  # s coordinate

cat("Difference: dq=", dq, ", dr=", dr, ", ds=", ds, "\n")

# Neighbors have one coordinate difference of ±1 and others 0
is_neighbor <- (abs(dq) + abs(dr) + abs(ds)) == 2
cat("Are neighbors:", is_neighbor, "\n")
