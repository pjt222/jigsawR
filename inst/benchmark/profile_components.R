# Profile individual components of geometry processing
suppressMessages(devtools::load_all(quiet = TRUE))

cat("=== Component Profiling ===\n\n")

# Generate hex pieces using the internal function
rings <- 5
diameter <- 200
tabsize <- 15
jitter <- 4

hex_pieces <- generate_hex_pieces_with_edge_map(
  seed = 42,
  rings = rings,
  diameter = diameter,
  tabsize = tabsize,
  jitter = jitter,
  do_warp = TRUE,
  do_trunc = TRUE,
  do_circular_border = TRUE
)

num_pieces <- length(hex_pieces)
cat("Testing with", num_pieces, "pieces\n\n")

# 1. Time parse_svg_path
cat("1. parse_svg_path():\n")
t1 <- system.time({
  for (rep in 1:10) {
    for (hp in hex_pieces) {
      parsed <- parse_svg_path(hp$path)
    }
  }
})
cat("   ", num_pieces, "x10 calls:", round(t1[["elapsed"]] * 1000), "ms\n")
cat("   Per call:", round(t1[["elapsed"]] / (num_pieces * 10) * 1000, 2), "ms\n\n")

# 2. Time get_hex_neighbors_for_fusion
cat("2. get_hex_neighbors_for_fusion():\n")
t2 <- system.time({
  for (rep in 1:100) {
    for (piece_id in 1:num_pieces) {
      nb <- get_hex_neighbors_for_fusion(piece_id, rings)
    }
  }
})
cat("   ", num_pieces, "x100 calls:", round(t2[["elapsed"]] * 1000), "ms\n")
cat("   Per call:", round(t2[["elapsed"]] / (num_pieces * 100) * 1000, 3), "ms\n\n")

# 3. Time extract_path_coords (includes re-parse)
cat("3. extract_path_coords() [includes parse_svg_path]:\n")
t3 <- system.time({
  for (rep in 1:10) {
    for (hp in hex_pieces) {
      coords <- extract_path_coords(hp$path)
    }
  }
})
cat("   ", num_pieces, "x10 calls:", round(t3[["elapsed"]] * 1000), "ms\n")
cat("   Per call:", round(t3[["elapsed"]] / (num_pieces * 10) * 1000, 2), "ms\n\n")

# 4. Time calculate_pieces_bounds
cat("4. calculate_pieces_bounds():\n")
# First create pieces with parsed_segments (like the real code does)
pieces <- lapply(hex_pieces, function(hp) {
  list(
    id = hp$id,
    path = hp$path,
    parsed_segments = parse_svg_path(hp$path),
    center = c(hp$center_x, hp$center_y)
  )
})
t4 <- system.time({
  for (rep in 1:10) {
    bounds <- calculate_pieces_bounds(pieces)
  }
})
cat("   10 calls:", round(t4[["elapsed"]] * 1000), "ms\n")
cat("   Per call:", round(t4[["elapsed"]] / 10 * 1000, 1), "ms\n\n")

# 5. Time bounds calculation WITHOUT re-parsing (using cached parsed_segments)
cat("5. Bounds calculation using cached parsed_segments:\n")
extract_coords_from_segments <- function(segments) {
  if (is.null(segments) || length(segments) == 0) {
    return(list(x = numeric(0), y = numeric(0)))
  }
  x_coords <- c()
  y_coords <- c()
  for (seg in segments) {
    if (seg$type %in% c("M", "L")) {
      x_coords <- c(x_coords, seg$x)
      y_coords <- c(y_coords, seg$y)
    } else if (seg$type == "C") {
      x_coords <- c(x_coords, seg$cp1_x, seg$cp2_x, seg$x)
      y_coords <- c(y_coords, seg$cp1_y, seg$cp2_y, seg$y)
    } else if (seg$type == "A") {
      x_coords <- c(x_coords, seg$x)
      y_coords <- c(y_coords, seg$y)
    }
  }
  list(x = x_coords, y = y_coords)
}

t5 <- system.time({
  for (rep in 1:10) {
    all_x <- c()
    all_y <- c()
    for (p in pieces) {
      coords <- extract_coords_from_segments(p$parsed_segments)
      all_x <- c(all_x, coords$x)
      all_y <- c(all_y, coords$y)
    }
    bounds <- list(min_x = min(all_x), max_x = max(all_x),
                   min_y = min(all_y), max_y = max(all_y))
  }
})
cat("   10 calls:", round(t5[["elapsed"]] * 1000), "ms\n")
cat("   Per call:", round(t5[["elapsed"]] / 10 * 1000, 1), "ms\n\n")

# Summary
cat("=== Summary ===\n")
cat("Current bounds calc (t4):", round(t4[["elapsed"]] / 10 * 1000, 1), "ms\n")
cat("Optimized bounds calc (t5):", round(t5[["elapsed"]] / 10 * 1000, 1), "ms\n")
speedup <- (t4[["elapsed"]] / t5[["elapsed"]])
cat("Potential speedup:", round(speedup, 1), "x\n")
