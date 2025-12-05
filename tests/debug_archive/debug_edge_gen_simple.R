# Simple debug of edge generation

cat("=" , rep("=", 70), "\n", sep = "")
cat("Debug: Simple Edge Generation Test\n")
cat("=" , rep("=", 70), "\n\n", sep = "")

source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")

rings <- 3
diameter <- 240
seed <- 1234

cat("Testing with do_warp=TRUE, do_trunc=TRUE:\n\n")

edge_data <- generate_hex_edge_map(rings, seed, diameter, tabsize = 20, jitter = 4,
                                    do_warp = TRUE, do_trunc = TRUE)

cat(sprintf("\nNumber of unique edges: %d\n", edge_data$num_edges))
cat(sprintf("Number of piece_edge_map entries: %d\n", length(edge_data$piece_edge_map)))

# Count border vs internal edges
border_count <- 0
internal_count <- 0
warped_count <- 0

for (key in names(edge_data$piece_edge_map)) {
  edge <- edge_data$piece_edge_map[[key]]
  if (edge$type == "border") {
    border_count <- border_count + 1
    if (!is.null(edge$warped) && edge$warped) {
      warped_count <- warped_count + 1
    }
  } else {
    internal_count <- internal_count + 1
  }
}

cat(sprintf("\nEdge counts:\n"))
cat(sprintf("  Border edges: %d\n", border_count))
cat(sprintf("  Internal edges: %d\n", internal_count))
cat(sprintf("  Warped border edges: %d\n", warped_count))

# Show first few border edges
cat("\nFirst few border edges:\n")
count <- 0
for (key in names(edge_data$piece_edge_map)) {
  edge <- edge_data$piece_edge_map[[key]]
  if (edge$type == "border" && count < 5) {
    v1 <- edge$start
    v2 <- edge$end
    dist1 <- sqrt(v1[1]^2 + v1[2]^2)
    dist2 <- sqrt(v2[1]^2 + v2[2]^2)
    cat(sprintf("  %s: v1_dist=%.2f, v2_dist=%.2f, warped=%s\n",
                key, dist1, dist2, if(!is.null(edge$warped)) edge$warped else "NA"))
    cat(sprintf("    forward: %s\n", substr(edge$forward, 1, 60)))
    count <- count + 1
  }
}
