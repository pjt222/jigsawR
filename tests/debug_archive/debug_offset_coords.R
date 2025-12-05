#!/usr/bin/env Rscript
# Debug the offset_path_coords function

cat("=== Debugging offset_path_coords ===\n\n")

# The function from hexagonal_edge_generation_fixed.R
offset_path_coords <- function(path_segment, offset_x, offset_y) {
  # Get the command type
  cmd <- substr(trimws(path_segment), 1, 1)

  cat("Input path_segment:", substr(path_segment, 1, 100), "...\n")
  cat("Command:", cmd, "\n")
  cat("Offset:", offset_x, offset_y, "\n")

  if (cmd == "A") {
    # Arc command handling...
    return(path_segment)
  } else {
    # L or C commands - all values are coordinates
    numbers <- as.numeric(unlist(strsplit(path_segment, "[CLM ]+")))
    numbers <- numbers[!is.na(numbers)]

    cat("Extracted numbers:", length(numbers), "\n")
    cat("Numbers:", head(numbers, 12), "...\n")

    # Offset x coordinates (odd indices) and y coordinates (even indices)
    for (i in seq_along(numbers)) {
      if (i %% 2 == 1) {
        numbers[i] <- numbers[i] + offset_x  # x coordinate
      } else {
        numbers[i] <- numbers[i] + offset_y  # y coordinate
      }
    }

    # Rebuild path segment
    coords <- sprintf("%.2f", numbers)
    result <- paste(cmd, paste(coords, collapse = " "))
    cat("Result:", substr(result, 1, 100), "...\n\n")
    return(result)
  }
}

# Test with actual bezier edge
v1 <- c(0, 0)
v2 <- c(10, 0)

# Simulate a bezier path like: "C 2 1 5 -2 3 3 C 4 5 6 5 7 3 C 8 -1 9 0 10 0"
test_path <- "C 2.00 1.00 5.00 -2.00 3.00 3.00 C 4.00 5.00 6.00 5.00 7.00 3.00 C 8.00 -1.00 9.00 0.00 10.00 0.00"

cat("=== Test 1: Simple offset ===\n")
result <- offset_path_coords(test_path, 100, 50)

cat("\n=== Test 2: What the edge map generates ===\n")
# Source to get actual edge
source("R/hexagonal_bezier_generation.R")
edge <- generate_hex_bezier_edge(
  v1 = c(0, 0),
  v2 = c(16.67, 0),
  seed = 42,
  edge_id = 1,
  tab_params = list(tabsize = 27, jitter = 5)
)
cat("Forward edge:\n", edge$forward, "\n\n")

cat("Applying offset (100, 50):\n")
result2 <- offset_path_coords(edge$forward, 100, 50)

# The issue: the edge$forward has multiple C commands but cmd = "C" only once
# Let's check what happens
cat("\n=== Problem Analysis ===\n")
cat("The path has MULTIPLE C commands, but we only add ONE 'C' prefix\n")
cat("This is the bug - need to handle multiple commands in sequence\n")

# Count C commands in the original
c_count <- length(gregexpr("C", test_path)[[1]])
cat("Number of C commands in test_path:", c_count, "\n")
