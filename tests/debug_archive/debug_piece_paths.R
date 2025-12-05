# Debug piece paths - look at coordinate ranges and anomalies
# Focus on pieces with suspected distortion

source("R/logging.R")
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  if (!grepl("scripts_archive", f)) source(f)
}

# Generate puzzle
pieces_result <- generate_pieces_internal(
  type = "hexagonal",
  seed = 1234,
  grid = c(7),
  size = c(400),
  tabsize = 20,
  jitter = 4,
  do_warp = FALSE,
  do_trunc = TRUE,
  do_circular_border = FALSE
)

# Function to extract all numeric coordinates from a path
extract_coords <- function(path) {
  # Extract all numbers (including negative and decimal)
  nums <- as.numeric(unlist(regmatches(path, gregexpr("-?[0-9]+\\.?[0-9]*", path))))
  nums <- nums[!is.na(nums)]
  return(nums)
}

# Function to analyze a piece's path
analyze_piece <- function(piece, pid) {
  coords <- extract_coords(piece$path)
  if (length(coords) < 2) return(NULL)

  # Separate x and y (assuming pairs)
  n <- length(coords)
  # Get coordinate ranges
  x_coords <- coords[seq(1, n, 2)]
  y_coords <- coords[seq(2, n, 2)]

  # Calculate expected bounds based on center
  cx <- piece$center[1]
  cy <- piece$center[2]

  # For a hexagon with ~30.77mm piece radius, coords should be within ~40-50mm of center
  piece_radius <- 400 / (4 * 7 - 2)  # ~15.38mm
  expected_range <- piece_radius * 3  # Allow for tabs

  x_range <- range(x_coords)
  y_range <- range(y_coords)
  x_span <- diff(x_range)
  y_span <- diff(y_range)

  # Check for anomalies
  max_dist_from_center_x <- max(abs(x_coords - cx))
  max_dist_from_center_y <- max(abs(y_coords - cy))

  is_anomalous <- max_dist_from_center_x > expected_range * 2 ||
                  max_dist_from_center_y > expected_range * 2 ||
                  x_span > expected_range * 3 ||
                  y_span > expected_range * 3

  list(
    pid = pid,
    center = c(cx, cy),
    ring = piece$ring_pos$ring,
    x_range = x_range,
    y_range = y_range,
    x_span = x_span,
    y_span = y_span,
    max_dist_x = max_dist_from_center_x,
    max_dist_y = max_dist_from_center_y,
    expected_range = expected_range,
    is_anomalous = is_anomalous
  )
}

# Analyze all pieces
cat("=== Analyzing all 127 pieces ===\n\n")

all_analyses <- lapply(seq_along(pieces_result$pieces), function(i) {
  analyze_piece(pieces_result$pieces[[i]], i)
})

# Find anomalous pieces
anomalous <- Filter(function(a) !is.null(a) && a$is_anomalous, all_analyses)

cat("Found", length(anomalous), "anomalous pieces:\n\n")

for (a in anomalous) {
  cat("Piece", a$pid, "(ring", a$ring, "):\n")
  cat("  Center:", round(a$center[1], 2), ",", round(a$center[2], 2), "\n")
  cat("  X range:", round(a$x_range[1], 2), "to", round(a$x_range[2], 2),
      "(span:", round(a$x_span, 2), ")\n")
  cat("  Y range:", round(a$y_range[1], 2), "to", round(a$y_range[2], 2),
      "(span:", round(a$y_span, 2), ")\n")
  cat("  Max dist from center - X:", round(a$max_dist_x, 2),
      ", Y:", round(a$max_dist_y, 2), "\n")
  cat("  Expected range:", round(a$expected_range, 2), "\n")
  cat("\n")
}

# Check if the problem pieces are in the anomalous list
group1 <- c(19, 36, 37, 59, 60, 61, 125)
group2 <- c(48, 73, 74, 75, 107)
all_problem <- c(group1, group2)

anomalous_ids <- sapply(anomalous, function(a) a$pid)
cat("=== Problem pieces status ===\n")
cat("Group 1 anomalous:", paste(intersect(group1, anomalous_ids), collapse=", "), "\n")
cat("Group 2 anomalous:", paste(intersect(group2, anomalous_ids), collapse=", "), "\n")
cat("All anomalous piece IDs:", paste(anomalous_ids, collapse=", "), "\n")

# Look at specific piece 36 in detail (it had crazy coordinates)
cat("\n=== Detailed analysis of piece 36 ===\n")
p36 <- pieces_result$pieces[[36]]
cat("Full path:\n")
cat(p36$path, "\n\n")

# Parse the path commands
path_commands <- strsplit(p36$path, "(?=[MLCZ])", perl=TRUE)[[1]]
path_commands <- path_commands[nchar(path_commands) > 0]
cat("Path commands breakdown:\n")
for (i in seq_along(path_commands)) {
  cmd <- path_commands[i]
  cat(i, ":", substr(cmd, 1, 80), if(nchar(cmd) > 80) "..." else "", "\n")
}
