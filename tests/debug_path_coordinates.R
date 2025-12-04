# Debug path coordinates - check if edge start/end match vertex positions
# This will identify if paths are being drawn in wrong order or to wrong vertices

cat("=== Debugging Path Coordinates ===\n\n")

source("R/logging.R")
source("R/concentric_geometry.R")
source("R/hexagonal_bezier_generation.R")
source("R/concentric_edge_generation.R")

# Parameters
rings <- 3
diameter <- 240
seed <- 42
tabsize <- diameter * 0.10  # 10%

cat(sprintf("Rings: %d, Diameter: %d mm, Tab size: %.1f mm\n\n", rings, diameter, tabsize))

# Generate edge map
edge_data <- generate_concentric_edge_map(
  rings = rings,
  seed = seed,
  diameter = diameter,
  tabsize = tabsize,
  jitter = 4,
  center_shape = "hexagon"
)

# Helper to extract first coordinate from path segment
extract_start <- function(path_segment) {
  # Path segment starts with command letter then coordinates
  # E.g., "C 10.00 20.00 30.00 40.00 50.00 60.00" -> end is (50, 60)
  # Or "L 10.00 20.00" -> end is (10, 20)
  nums <- as.numeric(strsplit(gsub("[A-Za-z]", "", path_segment), "\\s+")[[1]])
  nums <- nums[!is.na(nums)]
  if (length(nums) >= 2) {
    return(c(nums[1], nums[2]))  # First two numbers after command
  }
  return(NULL)
}

extract_end <- function(path_segment) {
  nums <- as.numeric(strsplit(gsub("[A-Za-z]", "", path_segment), "\\s+")[[1]])
  nums <- nums[!is.na(nums)]
  if (length(nums) >= 2) {
    return(c(nums[length(nums)-1], nums[length(nums)]))  # Last two numbers
  }
  return(NULL)
}

# Check each piece
cat("=== CHECKING EDGE-VERTEX ALIGNMENT ===\n\n")

tolerance <- 0.01

for (piece_id in 1:7) {  # Check center and ring 1 first
  piece_info <- edge_data$piece_vertices[[piece_id]]
  piece_edges <- edge_data$piece_edges[[piece_id]]

  cat(sprintf("--- Piece %d (%s) ---\n", piece_id, piece_info$type))

  if (piece_info$type == "hexagon") {
    vertices <- piece_info$vertices

    # Center piece has 6 outer edges connecting to ring 1
    # Each edge should go from V[i] to V[i+1]
    outer_edges <- piece_edges[sapply(piece_edges, function(e) e$type == "outer")]

    cat(sprintf("Has %d outer edges (expected 6)\n", length(outer_edges)))

    for (i in seq_along(outer_edges)) {
      edge_info <- outer_edges[[i]]
      edge <- edge_data$edge_map[[edge_info$edge_ref]]

      # Expected: edge from V[i] to V[i+1]
      v_start <- vertices[[i]]
      v_end <- vertices[[(i %% 6) + 1]]

      # Actual: depends on is_forward
      if (edge_info$is_forward) {
        actual_start <- edge$start
        actual_end <- edge$end
      } else {
        actual_start <- edge$end
        actual_end <- edge$start
      }

      start_match <- abs(v_start[1] - actual_start[1]) < tolerance &&
                     abs(v_start[2] - actual_start[2]) < tolerance
      end_match <- abs(v_end[1] - actual_end[1]) < tolerance &&
                   abs(v_end[2] - actual_end[2]) < tolerance

      status <- if (start_match && end_match) "OK" else "MISMATCH"

      cat(sprintf("  Edge %d (%s): %s\n", i, edge_info$edge_ref, status))
      if (!start_match || !end_match) {
        cat(sprintf("    Expected: (%.2f,%.2f) -> (%.2f,%.2f)\n", v_start[1], v_start[2], v_end[1], v_end[2]))
        cat(sprintf("    Actual:   (%.2f,%.2f) -> (%.2f,%.2f)\n", actual_start[1], actual_start[2], actual_end[1], actual_end[2]))
      }
    }

  } else if (piece_info$type == "trapezoid") {
    vertices <- piece_info$vertices
    v1 <- vertices[[1]]  # inner-start
    v2 <- vertices[[2]]  # inner-end
    v3 <- vertices[[3]]  # outer-end
    v4 <- vertices[[4]]  # outer-start

    cat(sprintf("Vertices: V1(%.1f,%.1f) V2(%.1f,%.1f) V3(%.1f,%.1f) V4(%.1f,%.1f)\n",
                v1[1], v1[2], v2[1], v2[2], v3[1], v3[2], v4[1], v4[2]))

    # Check INNER edge (V1 -> V2)
    inner_edges <- piece_edges[sapply(piece_edges, function(e) e$type == "inner")]
    if (length(inner_edges) > 0) {
      edge_info <- inner_edges[[1]]
      edge <- edge_data$edge_map[[edge_info$edge_ref]]

      if (edge_info$is_forward) {
        actual_start <- edge$start
        actual_end <- edge$end
      } else {
        actual_start <- edge$end
        actual_end <- edge$start
      }

      start_match <- abs(v1[1] - actual_start[1]) < tolerance && abs(v1[2] - actual_start[2]) < tolerance
      end_match <- abs(v2[1] - actual_end[1]) < tolerance && abs(v2[2] - actual_end[2]) < tolerance

      cat(sprintf("  INNER (V1->V2): %s\n", if (start_match && end_match) "OK" else "MISMATCH"))
      if (!start_match || !end_match) {
        cat(sprintf("    Expected: (%.2f,%.2f) -> (%.2f,%.2f)\n", v1[1], v1[2], v2[1], v2[2]))
        cat(sprintf("    Actual:   (%.2f,%.2f) -> (%.2f,%.2f) (forward=%s)\n",
                    actual_start[1], actual_start[2], actual_end[1], actual_end[2], edge_info$is_forward))
        cat(sprintf("    Edge stored: start=(%.2f,%.2f) end=(%.2f,%.2f)\n",
                    edge$start[1], edge$start[2], edge$end[1], edge$end[2]))
      }
    } else {
      cat("  INNER: MISSING!\n")
    }

    # Check RIGHT edge (V2 -> V3)
    right_edges <- piece_edges[sapply(piece_edges, function(e) e$type == "right")]
    if (length(right_edges) > 0) {
      edge_info <- right_edges[[1]]
      edge <- edge_data$edge_map[[edge_info$edge_ref]]

      if (edge_info$is_forward) {
        actual_start <- edge$start
        actual_end <- edge$end
      } else {
        actual_start <- edge$end
        actual_end <- edge$start
      }

      start_match <- abs(v2[1] - actual_start[1]) < tolerance && abs(v2[2] - actual_start[2]) < tolerance
      end_match <- abs(v3[1] - actual_end[1]) < tolerance && abs(v3[2] - actual_end[2]) < tolerance

      cat(sprintf("  RIGHT (V2->V3): %s\n", if (start_match && end_match) "OK" else "MISMATCH"))
      if (!start_match || !end_match) {
        cat(sprintf("    Expected: (%.2f,%.2f) -> (%.2f,%.2f)\n", v2[1], v2[2], v3[1], v3[2]))
        cat(sprintf("    Actual:   (%.2f,%.2f) -> (%.2f,%.2f) (forward=%s)\n",
                    actual_start[1], actual_start[2], actual_end[1], actual_end[2], edge_info$is_forward))
      }
    }

    # Check OUTER edge segments (V3 -> V4)
    outer_edges <- piece_edges[sapply(piece_edges, function(e) e$type %in% c("outer_segment", "border"))]
    cat(sprintf("  OUTER (V3->V4): %d segments\n", length(outer_edges)))

    # For outer_segments, we need to check if they chain from V3 to V4
    # The issue might be that the segments are not in the right order
    if (length(outer_edges) > 0 && outer_edges[[1]]$type == "outer_segment") {
      for (j in seq_along(outer_edges)) {
        seg <- outer_edges[[j]]
        edge <- edge_data$edge_map[[seg$edge_ref]]

        if (seg$is_forward) {
          actual_start <- edge$start
          actual_end <- edge$end
        } else {
          actual_start <- edge$end
          actual_end <- edge$start
        }

        cat(sprintf("    Segment %d -> neighbor %d: (%.2f,%.2f) -> (%.2f,%.2f)\n",
                    j, seg$neighbor, actual_start[1], actual_start[2], actual_end[1], actual_end[2]))
      }
    }

    # Check LEFT edge (V4 -> V1)
    left_edges <- piece_edges[sapply(piece_edges, function(e) e$type == "left")]
    if (length(left_edges) > 0) {
      edge_info <- left_edges[[1]]
      edge <- edge_data$edge_map[[edge_info$edge_ref]]

      if (edge_info$is_forward) {
        actual_start <- edge$start
        actual_end <- edge$end
      } else {
        actual_start <- edge$end
        actual_end <- edge$start
      }

      start_match <- abs(v4[1] - actual_start[1]) < tolerance && abs(v4[2] - actual_start[2]) < tolerance
      end_match <- abs(v1[1] - actual_end[1]) < tolerance && abs(v1[2] - actual_end[2]) < tolerance

      cat(sprintf("  LEFT (V4->V1): %s\n", if (start_match && end_match) "OK" else "MISMATCH"))
      if (!start_match || !end_match) {
        cat(sprintf("    Expected: (%.2f,%.2f) -> (%.2f,%.2f)\n", v4[1], v4[2], v1[1], v1[2]))
        cat(sprintf("    Actual:   (%.2f,%.2f) -> (%.2f,%.2f) (forward=%s)\n",
                    actual_start[1], actual_start[2], actual_end[1], actual_end[2], edge_info$is_forward))
      }
    }
  }

  cat("\n")
}

cat("\n=== VISUAL PATH ANALYSIS FOR PIECE 2 ===\n\n")

piece_id <- 2
piece_info <- edge_data$piece_vertices[[piece_id]]
piece_edges <- edge_data$piece_edges[[piece_id]]
path <- build_concentric_piece_path(piece_id, edge_data)

cat("Full path:\n")
cat(path, "\n\n")

cat("Expected vertex sequence: V1 -> V2 -> V3 -> V4 -> V1\n")
cat(sprintf("V1: (%.2f, %.2f)\n", piece_info$vertices[[1]][1], piece_info$vertices[[1]][2]))
cat(sprintf("V2: (%.2f, %.2f)\n", piece_info$vertices[[2]][1], piece_info$vertices[[2]][2]))
cat(sprintf("V3: (%.2f, %.2f)\n", piece_info$vertices[[3]][1], piece_info$vertices[[3]][2]))
cat(sprintf("V4: (%.2f, %.2f)\n", piece_info$vertices[[4]][1], piece_info$vertices[[4]][2]))

cat("\n=== Debug Complete ===\n")
