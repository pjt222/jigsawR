# Simple debug for piece 27 inner neighbors

cat("=== Test get_inner_neighbors ===\n\n")

source("R/logging.R")
source("R/concentric_geometry.R")
source("R/concentric_edge_generation.R")

rings <- 7

# Test piece 27
cat("Piece 27 (ring 3, pos 7):\n")
inner_neighbors <- get_inner_neighbors(27, rings)
cat(sprintf("  Inner neighbors: %s\n", paste(inner_neighbors, collapse=", ")))
cat("  Expected: 12, 13\n\n")

# Verify angular ranges
piece_height <- get_concentric_piece_height(500, rings)

# Ring 3 piece 27: position 7
pieces_in_ring3 <- 6 * 3
arc3 <- 360 / pieces_in_ring3
cat(sprintf("Ring 3: %d pieces, each %.1f degrees\n", pieces_in_ring3, arc3))
cat(sprintf("  Piece 27 (pos 7): %.1f to %.1f degrees\n", 7 * arc3, 8 * arc3))

# Ring 2 pieces 12 and 13
pieces_in_ring2 <- 6 * 2
arc2 <- 360 / pieces_in_ring2
cat(sprintf("\nRing 2: %d pieces, each %.1f degrees\n", pieces_in_ring2, arc2))
cat(sprintf("  Piece 12 (pos 4): %.1f to %.1f degrees\n", 4 * arc2, 5 * arc2))
cat(sprintf("  Piece 13 (pos 5): %.1f to %.1f degrees\n", 5 * arc2, 6 * arc2))

# Check overlap
cat("\nOverlap analysis:\n")
cat(sprintf("  Piece 27: 140 to 160 degrees\n"))
cat(sprintf("  Piece 12: 120 to 150 degrees -> overlaps 140-150\n"))
cat(sprintf("  Piece 13: 150 to 180 degrees -> overlaps 150-160\n"))

cat("\n=== Done ===\n")
