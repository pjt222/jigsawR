devtools::load_all()

cat("Checking if adjacent pieces share vertices correctly\n")
cat("=====================================================\n\n")

# Calculate vertices for pieces 5 and 6 directly
rings <- 3
diameter <- 300
piece_height <- diameter / (2 * rings)  # 50

cat(sprintf("Configuration: rings=%d, diameter=%d, piece_height=%.2f\n\n",
            rings, diameter, piece_height))

# Piece 5: ring 1, position 3
# Piece 6: ring 1, position 4
info5 <- map_concentric_piece_id(5, rings)
info6 <- map_concentric_piece_id(6, rings)

cat("Piece 5 info:\n")
cat(sprintf("  Ring: %d, Position: %d\n", info5$ring, info5$position))

cat("Piece 6 info:\n")
cat(sprintf("  Ring: %d, Position: %d\n", info6$ring, info6$position))

# Calculate vertices manually
arc_angle <- 2 * pi / 6  # 60 degrees per piece in ring 1
inner_radius <- 1 * piece_height  # 50
outer_radius <- 2 * piece_height  # 100

# Piece 5 (position 3)
p5_start_angle <- 3 * arc_angle
p5_end_angle <- 4 * arc_angle

p5_v1 <- c(inner_radius * cos(p5_start_angle), inner_radius * sin(p5_start_angle))
p5_v2 <- c(inner_radius * cos(p5_end_angle), inner_radius * sin(p5_end_angle))
p5_v3 <- c(outer_radius * cos(p5_end_angle), outer_radius * sin(p5_end_angle))
p5_v4 <- c(outer_radius * cos(p5_start_angle), outer_radius * sin(p5_start_angle))

cat("\nPiece 5 vertices (manual calculation):\n")
cat(sprintf("  V1 (inner-start): (%.2f, %.2f) at angle %.2f rad\n", p5_v1[1], p5_v1[2], p5_start_angle))
cat(sprintf("  V2 (inner-end):   (%.2f, %.2f) at angle %.2f rad\n", p5_v2[1], p5_v2[2], p5_end_angle))
cat(sprintf("  V3 (outer-end):   (%.2f, %.2f) at angle %.2f rad\n", p5_v3[1], p5_v3[2], p5_end_angle))
cat(sprintf("  V4 (outer-start): (%.2f, %.2f) at angle %.2f rad\n", p5_v4[1], p5_v4[2], p5_start_angle))
cat(sprintf("  RIGHT edge: V2->V3 = (%.2f, %.2f) -> (%.2f, %.2f)\n", p5_v2[1], p5_v2[2], p5_v3[1], p5_v3[2]))

# Piece 6 (position 4)
p6_start_angle <- 4 * arc_angle
p6_end_angle <- 5 * arc_angle

p6_v1 <- c(inner_radius * cos(p6_start_angle), inner_radius * sin(p6_start_angle))
p6_v2 <- c(inner_radius * cos(p6_end_angle), inner_radius * sin(p6_end_angle))
p6_v3 <- c(outer_radius * cos(p6_end_angle), outer_radius * sin(p6_end_angle))
p6_v4 <- c(outer_radius * cos(p6_start_angle), outer_radius * sin(p6_start_angle))

cat("\nPiece 6 vertices (manual calculation):\n")
cat(sprintf("  V1 (inner-start): (%.2f, %.2f) at angle %.2f rad\n", p6_v1[1], p6_v1[2], p6_start_angle))
cat(sprintf("  V2 (inner-end):   (%.2f, %.2f) at angle %.2f rad\n", p6_v2[1], p6_v2[2], p6_end_angle))
cat(sprintf("  V3 (outer-end):   (%.2f, %.2f) at angle %.2f rad\n", p6_v3[1], p6_v3[2], p6_end_angle))
cat(sprintf("  V4 (outer-start): (%.2f, %.2f) at angle %.2f rad\n", p6_v4[1], p6_v4[2], p6_start_angle))
cat(sprintf("  LEFT edge: V4->V1 = (%.2f, %.2f) -> (%.2f, %.2f)\n", p6_v4[1], p6_v4[2], p6_v1[1], p6_v1[2]))

# Check if they match
cat("\n=== VERTEX SHARING CHECK ===\n")
cat(sprintf("Piece 5 V2 == Piece 6 V1? %s\n",
            all(abs(p5_v2 - p6_v1) < 0.01)))
cat(sprintf("Piece 5 V3 == Piece 6 V4? %s\n",
            all(abs(p5_v3 - p6_v4) < 0.01)))

# Now get the actual vertices from the geometry module
cat("\n=== ACTUAL VERTICES FROM GEOMETRY MODULE ===\n")
v5 <- calculate_concentric_vertices(5, rings, diameter, "hexagon")
v6 <- calculate_concentric_vertices(6, rings, diameter, "hexagon")

cat("\nPiece 5 actual vertices:\n")
cat(sprintf("  V1: (%.2f, %.2f)\n", v5$vertices[[1]][1], v5$vertices[[1]][2]))
cat(sprintf("  V2: (%.2f, %.2f)\n", v5$vertices[[2]][1], v5$vertices[[2]][2]))
cat(sprintf("  V3: (%.2f, %.2f)\n", v5$vertices[[3]][1], v5$vertices[[3]][2]))
cat(sprintf("  V4: (%.2f, %.2f)\n", v5$vertices[[4]][1], v5$vertices[[4]][2]))

cat("\nPiece 6 actual vertices:\n")
cat(sprintf("  V1: (%.2f, %.2f)\n", v6$vertices[[1]][1], v6$vertices[[1]][2]))
cat(sprintf("  V2: (%.2f, %.2f)\n", v6$vertices[[2]][1], v6$vertices[[2]][2]))
cat(sprintf("  V3: (%.2f, %.2f)\n", v6$vertices[[3]][1], v6$vertices[[3]][2]))
cat(sprintf("  V4: (%.2f, %.2f)\n", v6$vertices[[4]][1], v6$vertices[[4]][2]))

# Check sharing
cat("\n=== ACTUAL SHARING CHECK ===\n")
cat(sprintf("Actual P5.V2 == P6.V1? %s\n",
            all(abs(v5$vertices[[2]] - v6$vertices[[1]]) < 0.01)))
cat(sprintf("Actual P5.V3 == P6.V4? %s\n",
            all(abs(v5$vertices[[3]] - v6$vertices[[4]]) < 0.01)))

# Now generate actual pieces and check their edge paths
cat("\n=== ACTUAL GENERATED PIECES ===\n")
result <- generate_puzzle(
  type = "concentric",
  seed = 42,
  grid = c(3),
  size = c(300),
  tabsize = 7,
  jitter = 2,
  offset = 0,
  fusion_groups = list(c(1, 2), c(5, 6, 7)),
  save_files = FALSE
)

p5 <- result$pieces[[5]]
p6 <- result$pieces[[6]]

cat("\nPiece 5 path first 200 chars:\n")
cat(substr(p5$path, 1, 200), "\n...\n")

cat("\nPiece 6 path first 200 chars:\n")
cat(substr(p6$path, 1, 200), "\n...\n")

# Parse and analyze paths
p5_segs <- parse_svg_path(p5$path)
p6_segs <- parse_svg_path(p6$path)

cat("\n=== PATH SEGMENT ANALYSIS ===\n")
cat(sprintf("\nPiece 5: %d segments\n", length(p5_segs)))
for (i in seq_along(p5_segs)) {
  seg <- p5_segs[[i]]
  if (seg$type == "M") {
    cat(sprintf("  %2d: M (%.2f, %.2f)\n", i, seg$x, seg$y))
  } else if (seg$type == "C") {
    cat(sprintf("  %2d: C -> (%.2f, %.2f)\n", i, seg$x, seg$y))
  } else if (seg$type == "L") {
    cat(sprintf("  %2d: L -> (%.2f, %.2f)\n", i, seg$x, seg$y))
  } else if (seg$type == "Z") {
    cat(sprintf("  %2d: Z\n", i))
  }
}

cat(sprintf("\nPiece 6: %d segments\n", length(p6_segs)))
for (i in seq_along(p6_segs)) {
  seg <- p6_segs[[i]]
  if (seg$type == "M") {
    cat(sprintf("  %2d: M (%.2f, %.2f)\n", i, seg$x, seg$y))
  } else if (seg$type == "C") {
    cat(sprintf("  %2d: C -> (%.2f, %.2f)\n", i, seg$x, seg$y))
  } else if (seg$type == "L") {
    cat(sprintf("  %2d: L -> (%.2f, %.2f)\n", i, seg$x, seg$y))
  } else if (seg$type == "Z") {
    cat(sprintf("  %2d: Z\n", i))
  }
}

# Now check the edge splitting
cat("\n=== EDGE SPLITTING ===\n")
p5_edges <- get_piece_edge_paths(p5)
p6_edges <- get_piece_edge_paths(p6)

cat("\nPiece 5 edge paths:\n")
for (name in names(p5_edges)) {
  path <- p5_edges[[name]]
  if (!is.null(path) && nzchar(path)) {
    segs <- parse_svg_path(path)
    start <- sprintf("(%.2f, %.2f)", segs[[1]]$x, segs[[1]]$y)

    # Find end
    end_seg <- NULL
    for (j in length(segs):2) {
      if (segs[[j]]$type != "Z") {
        end_seg <- segs[[j]]
        break
      }
    }
    end <- if (!is.null(end_seg)) sprintf("(%.2f, %.2f)", end_seg$x, end_seg$y) else "?"

    is_fused <- isTRUE(p5$fused_edges[[name]])
    cat(sprintf("  %s: %s -> %s %s\n", name, start, end, if(is_fused) "[FUSED]" else ""))
  }
}

cat("\nPiece 6 edge paths:\n")
for (name in names(p6_edges)) {
  path <- p6_edges[[name]]
  if (!is.null(path) && nzchar(path)) {
    segs <- parse_svg_path(path)
    start <- sprintf("(%.2f, %.2f)", segs[[1]]$x, segs[[1]]$y)

    # Find end
    end_seg <- NULL
    for (j in length(segs):2) {
      if (segs[[j]]$type != "Z") {
        end_seg <- segs[[j]]
        break
      }
    }
    end <- if (!is.null(end_seg)) sprintf("(%.2f, %.2f)", end_seg$x, end_seg$y) else "?"

    is_fused <- isTRUE(p6$fused_edges[[name]])
    cat(sprintf("  %s: %s -> %s %s\n", name, start, end, if(is_fused) "[FUSED]" else ""))
  }
}

cat("\n=== EXPECTED EDGE MATCH ===\n")
cat("Piece 5 RIGHT should match Piece 6 LEFT (reversed)\n")
cat(sprintf("P5 V2->V3 expected: (%.2f, %.2f) -> (%.2f, %.2f)\n", p5_v2[1], p5_v2[2], p5_v3[1], p5_v3[2]))
cat(sprintf("P6 V4->V1 expected: (%.2f, %.2f) -> (%.2f, %.2f)\n", p6_v4[1], p6_v4[2], p6_v1[1], p6_v1[2]))
