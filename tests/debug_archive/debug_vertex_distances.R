# Debug: Analyze vertex distances after warp transformation

cat("=" , rep("=", 70), "\n", sep = "")
cat("Debug: Vertex Distances After Warp\n")
cat("=" , rep("=", 70), "\n\n", sep = "")

source("R/hexagonal_topology.R")
source("R/hexagonal_neighbors.R")
source("R/hexagonal_bezier_generation.R")
source("R/hexagonal_edge_generation_fixed.R")

rings <- 3
diameter <- 240
seed <- 1234
piece_radius <- diameter / (rings * 4)

cat(sprintf("Parameters: rings=%d, diameter=%d, piece_radius=%.2f\n\n", rings, diameter, piece_radius))

# Generate edge map with do_warp + do_trunc
edge_data <- generate_hex_edge_map(rings, seed, diameter, tabsize = 20, jitter = 4,
                                    do_warp = TRUE, do_trunc = TRUE)

# Analyze the piece_edge_map for border edges
cat("Border edge analysis:\n")
cat(sprintf("%-15s %-15s %-15s %-15s %-15s\n", "Edge Key", "V1 Dist", "V2 Dist", "Arc Radius", "Type"))
cat(sprintf("%-15s %-15s %-15s %-15s %-15s\n", "--------", "-------", "-------", "----------", "----"))

border_edges <- list()
for (key in names(edge_data$piece_edge_map)) {
  edge <- edge_data$piece_edge_map[[key]]
  if (edge$type == "border") {
    v1 <- edge$start
    v2 <- edge$end
    dist1 <- sqrt(v1[1]^2 + v1[2]^2)
    dist2 <- sqrt(v2[1]^2 + v2[2]^2)

    # Extract arc radius from forward command
    arc_match <- regmatches(edge$forward, regexpr("A ([0-9.]+)", edge$forward))
    arc_radius <- if (length(arc_match) > 0) {
      as.numeric(gsub("A ", "", arc_match))
    } else {
      NA
    }

    cat(sprintf("%-15s %-15.2f %-15.2f %-15.2f %-15s\n",
                key, dist1, dist2, arc_radius, if(edge$warped) "warped" else "straight"))

    border_edges[[key]] <- list(dist1 = dist1, dist2 = dist2, arc_radius = arc_radius)
  }
}

cat("\n")

# Summary
all_dists <- unlist(lapply(border_edges, function(e) c(e$dist1, e$dist2)))
cat(sprintf("Boundary vertex distance range: [%.2f, %.2f]\n", min(all_dists), max(all_dists)))
cat(sprintf("Boundary vertex distance variation: %.2f%%\n",
            (max(all_dists) - min(all_dists)) / mean(all_dists) * 100))

cat("\n")
cat("=" , rep("=", 70), "\n", sep = "")
cat("The problem: Warped vertices are at DIFFERENT distances from origin!\n")
cat("The warp transformation doesn't create a perfect circle.\n")
cat("=" , rep("=", 70), "\n", sep = "")
