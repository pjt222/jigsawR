devtools::load_all()

cat("Debugging ring 2 piece edge splitting\n")
cat("======================================\n\n")

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

# Pick a ring 2 piece (piece 8 is first in ring 2)
piece <- result$pieces[[8]]
cat("Piece 8:\n")
cat(sprintf("  Ring: %d, Position: %d\n", piece$ring_pos$ring, piece$ring_pos$position))

# Show the full path
cat("\nFull path:\n")
cat(piece$path, "\n\n")

# Parse and analyze
segs <- parse_svg_path(piece$path)
cat(sprintf("Total segments: %d\n\n", length(segs)))

cat("Segment breakdown:\n")
for (i in seq_along(segs)) {
  seg <- segs[[i]]
  if (seg$type == "M") {
    cat(sprintf("  %2d: M (%.2f, %.2f)\n", i, seg$x, seg$y))
  } else if (seg$type == "C") {
    cat(sprintf("  %2d: C -> (%.2f, %.2f)\n", i, seg$x, seg$y))
  } else if (seg$type == "L") {
    cat(sprintf("  %2d: L -> (%.2f, %.2f) **BOUNDARY**\n", i, seg$x, seg$y))
  } else if (seg$type == "A") {
    cat(sprintf("  %2d: A -> (%.2f, %.2f)\n", i, seg$x, seg$y))
  } else if (seg$type == "Z") {
    cat(sprintf("  %2d: Z\n", i))
  }
}

# Calculate expected vertices
cat("\n\nExpected vertices for ring 2, position 0:\n")
diameter <- 300
rings <- 3
piece_height <- diameter / (2 * rings)  # 50
ring <- 2
position <- 0
pieces_in_ring <- 6 * ring  # 12
arc_angle <- 2 * pi / pieces_in_ring  # 30 degrees
start_angle <- position * arc_angle
end_angle <- (position + 1) * arc_angle
inner_radius <- ring * piece_height  # 100
outer_radius <- (ring + 1) * piece_height  # 150

v1 <- c(inner_radius * cos(start_angle), inner_radius * sin(start_angle))
v2 <- c(inner_radius * cos(end_angle), inner_radius * sin(end_angle))
v3 <- c(outer_radius * cos(end_angle), outer_radius * sin(end_angle))
v4 <- c(outer_radius * cos(start_angle), outer_radius * sin(start_angle))

cat(sprintf("  V1 (inner-start): (%.2f, %.2f)\n", v1[1], v1[2]))
cat(sprintf("  V2 (inner-end):   (%.2f, %.2f)\n", v2[1], v2[2]))
cat(sprintf("  V3 (outer-end):   (%.2f, %.2f)\n", v3[1], v3[2]))
cat(sprintf("  V4 (outer-start): (%.2f, %.2f)\n", v4[1], v4[2]))

# Identify where path matches vertices
cat("\n\nFinding vertex matches in path:\n")
tolerance <- 1.0  # Larger tolerance for bigger pieces
for (i in 2:length(segs)) {
  seg <- segs[[i]]
  if (seg$type == "Z") next

  seg_end <- c(seg$x, seg$y)

  if (sqrt(sum((seg_end - v1)^2)) < tolerance) {
    cat(sprintf("  Segment %d endpoint matches V1\n", i))
  }
  if (sqrt(sum((seg_end - v2)^2)) < tolerance) {
    cat(sprintf("  Segment %d endpoint matches V2\n", i))
  }
  if (sqrt(sum((seg_end - v3)^2)) < tolerance) {
    cat(sprintf("  Segment %d endpoint matches V3\n", i))
  }
  if (sqrt(sum((seg_end - v4)^2)) < tolerance) {
    cat(sprintf("  Segment %d endpoint matches V4\n", i))
  }
}

# Now test the edge splitting function
cat("\n\nEdge splitting result:\n")
edge_paths <- get_piece_edge_paths(piece)
for (name in c("INNER", "RIGHT", "OUTER", "LEFT")) {
  path <- edge_paths[[name]]
  if (!is.null(path) && nzchar(path)) {
    segs2 <- parse_svg_path(path)
    cat(sprintf("  %s: %d segments, ", name, length(segs2)))
    start <- sprintf("(%.2f, %.2f)", segs2[[1]]$x, segs2[[1]]$y)
    end_seg <- NULL
    for (j in length(segs2):2) {
      if (segs2[[j]]$type != "Z") {
        end_seg <- segs2[[j]]
        break
      }
    }
    end <- if (!is.null(end_seg)) sprintf("(%.2f, %.2f)", end_seg$x, end_seg$y) else "?"
    cat(sprintf("%s -> %s\n", start, end))
  } else {
    cat(sprintf("  %s: empty\n", name))
  }
}
