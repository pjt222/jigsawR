# Comprehensive Profiling for Hexagonal Puzzle Generation
# Identifies bottlenecks for optimization targeting
#
# Usage: Rscript inst/benchmark/profile_hex_generation.R

devtools::load_all(quiet = TRUE)
library(cli)

# =============================================================================
# CONFIGURATION
# =============================================================================

# Test configurations: (rings, with_fusion)
test_configs <- list(
  small = list(rings = 3, fusion = FALSE),
  small_fusion = list(rings = 3, fusion = TRUE),
  medium = list(rings = 5, fusion = FALSE),
  medium_fusion = list(rings = 5, fusion = TRUE),
  large = list(rings = 7, fusion = FALSE),
  large_fusion = list(rings = 7, fusion = TRUE)
)

# Number of timing iterations for stable measurements
N_ITER <- 3

# =============================================================================
# PROFILING HELPERS
# =============================================================================

#' Time a function with multiple iterations
#' Returns median time in milliseconds
time_fn <- function(fn, n = N_ITER) {
  times <- numeric(n)
  for (i in seq_len(n)) {
    start <- Sys.time()
    result <- fn()
    end <- Sys.time()
    times[i] <- as.numeric(end - start, units = "secs") * 1000
  }
  list(
    median_ms = median(times),
    min_ms = min(times),
    max_ms = max(times),
    result = result
  )
}

#' Create fusion groups for testing (fuse adjacent pairs)
make_fusion_groups <- function(num_pieces) {
  # Create fusion groups covering ~20% of pieces
  n_groups <- max(1, num_pieces %/% 10)
  groups <- list()
  for (i in seq_len(n_groups)) {
    base_piece <- (i - 1) * 5 + 1
    if (base_piece + 1 <= num_pieces) {
      groups[[i]] <- c(base_piece, base_piece + 1)
    }
  }
  groups
}

# =============================================================================
# MICRO-BENCHMARKS: Individual Operations
# =============================================================================

cli_h1("Micro-Benchmarks: Individual Operations")

# Test 1: Trig function performance
cli_h2("Trigonometric Functions")

n_ops <- 100000
angles <- runif(n_ops, 0, 2 * pi)

# Standard trig
t1 <- time_fn(function() {
  x <- cos(angles)
  y <- sin(angles)
  list(x = x, y = y)
})

# atan2 (used heavily in direction calculations)
t2 <- time_fn(function() {
  atan2(sin(angles), cos(angles))
})

cli_alert_info("cos/sin ({n_ops} ops): {round(t1$median_ms, 2)}ms")
cli_alert_info("atan2 ({n_ops} ops): {round(t2$median_ms, 2)}ms")

# Test 2: String operations (sprintf vs paste)
cli_h2("String Operations")

n_strings <- 10000
x_vals <- runif(n_strings)
y_vals <- runif(n_strings)

t_sprintf <- time_fn(function() {
  sapply(seq_len(n_strings), function(i) {
    sprintf("%.1f,%.1f", x_vals[i], y_vals[i])
  })
})

t_paste <- time_fn(function() {
  paste(round(x_vals, 1), round(y_vals, 1), sep = ",")
})

cli_alert_info("sprintf loop ({n_strings} ops): {round(t_sprintf$median_ms, 2)}ms")
cli_alert_info("paste vectorized ({n_strings} ops): {round(t_paste$median_ms, 2)}ms")
cli_alert_success("Vectorized paste is {round(t_sprintf$median_ms / t_paste$median_ms, 1)}x faster")

# Test 3: Environment vs list for hash maps
cli_h2("Hash Map Performance")

n_items <- 5000
keys <- paste0("key_", seq_len(n_items))
values <- seq_len(n_items)

# Environment-based hash map
t_env_write <- time_fn(function() {
  env <- new.env(hash = TRUE, parent = emptyenv())
  for (i in seq_len(n_items)) {
    env[[keys[i]]] <- values[i]
  }
  env
})

t_env_read <- time_fn(function() {
  env <- t_env_write$result
  total <- 0
  for (key in keys) {
    total <- total + env[[key]]
  }
  total
})

# Named list
t_list_write <- time_fn(function() {
  lst <- list()
  for (i in seq_len(n_items)) {
    lst[[keys[i]]] <- values[i]
  }
  lst
})

t_list_read <- time_fn(function() {
  lst <- t_list_write$result
  total <- 0
  for (key in keys) {
    total <- total + lst[[key]]
  }
  total
})

cli_alert_info("env write ({n_items} items): {round(t_env_write$median_ms, 2)}ms")
cli_alert_info("env read ({n_items} items): {round(t_env_read$median_ms, 2)}ms")
cli_alert_info("list write ({n_items} items): {round(t_list_write$median_ms, 2)}ms")
cli_alert_info("list read ({n_items} items): {round(t_list_read$median_ms, 2)}ms")

# Test 4: Regex vs simple string operations
cli_h2("Regex Performance")

svg_paths <- replicate(1000, {
  paste0("M 0 0 C ", paste(runif(18), collapse = " "), " Z")
})

t_regex <- time_fn(function() {
  lapply(svg_paths, function(path) {
    as.numeric(unlist(regmatches(path, gregexpr("-?[0-9]+\\.?[0-9]*", path))))
  })
})

# Alternative: strsplit approach
t_strsplit <- time_fn(function() {
  lapply(svg_paths, function(path) {
    # Remove M, C, Z commands
    clean <- gsub("[MCZ]", "", path)
    as.numeric(unlist(strsplit(trimws(clean), "\\s+")))
  })
})

cli_alert_info("gregexpr ({length(svg_paths)} paths): {round(t_regex$median_ms, 2)}ms")
cli_alert_info("strsplit ({length(svg_paths)} paths): {round(t_strsplit$median_ms, 2)}ms")

# =============================================================================
# COMPONENT BENCHMARKS: Puzzle Generation Steps
# =============================================================================

cli_h1("Component Benchmarks: Generation Steps")

for (config_name in names(test_configs)) {
  config <- test_configs[[config_name]]
  rings <- config$rings
  with_fusion <- config$fusion
  num_pieces <- 3 * rings * (rings - 1) + 1

  cli_h2("{config_name}: {rings} rings, {num_pieces} pieces, fusion={with_fusion}")

  fusion_groups <- if (with_fusion) make_fusion_groups(num_pieces) else NULL

  # Step 1: Edge map generation
  t_edge_map <- time_fn(function() {
    generate_hex_edge_map(
      rings = rings,
      seed = 42,
      diameter = 200,
      tabsize = 20,
      jitter = 4,
      do_warp = TRUE,
      do_trunc = TRUE
    )
  }, n = 1)  # Only 1 iteration for large puzzles

  cli_alert_info("generate_hex_edge_map: {round(t_edge_map$median_ms, 1)}ms")

  # Step 2: Full piece generation with edge map
  t_pieces <- time_fn(function() {
    generate_hex_pieces_with_edge_map(
      rings = rings,
      seed = 42,
      diameter = 200,
      tabsize = 20,
      jitter = 4,
      separated = FALSE,
      separation_factor = 1.0,
      do_warp = TRUE,
      do_trunc = TRUE
    )
  }, n = 1)

  cli_alert_info("generate_hex_pieces_with_edge_map: {round(t_pieces$median_ms, 1)}ms")

  # Step 3: Fusion edge computation
  if (with_fusion) {
    t_fusion <- time_fn(function() {
      compute_hex_fused_edges_fast(fusion_groups, rings)
    })
    cli_alert_info("compute_hex_fused_edges_fast: {round(t_fusion$median_ms, 2)}ms")
  }

  # Step 4: Full generate_puzzle (end-to-end)
  t_full <- time_fn(function() {
    generate_puzzle(
      type = "hexagonal",
      seed = 42,
      grid = c(rings),
      size = c(200),
      do_warp = TRUE,
      do_trunc = TRUE,
      fusion_groups = fusion_groups,
      fusion_style = if (with_fusion) "dashed" else "none",
      save_files = FALSE
    )
  }, n = 1)

  cli_alert_success("TOTAL generate_puzzle: {round(t_full$median_ms, 1)}ms")

  # Calculate overhead
  measured <- t_edge_map$median_ms + t_pieces$median_ms
  if (with_fusion) measured <- measured + t_fusion$median_ms
  overhead <- t_full$median_ms - measured
  cli_alert_warning("Unmeasured overhead: {round(overhead, 1)}ms ({round(100 * overhead / t_full$median_ms, 1)}%)")

  cat("\n")
}

# =============================================================================
# HOTSPOT ANALYSIS: Where is time actually spent?
# =============================================================================

cli_h1("Hotspot Analysis with Rprof")

# Profile a medium-sized puzzle in detail
rings <- 5
num_pieces <- 3 * rings * (rings - 1) + 1
fusion_groups <- make_fusion_groups(num_pieces)

cli_alert_info("Profiling {rings}-ring hexagonal puzzle with fusion...")

# Enable profiling
Rprof(tmp <- tempfile(), interval = 0.01)

# Run the generation
result <- generate_puzzle(
  type = "hexagonal",
  seed = 42,
  grid = c(rings),
  size = c(200),
  do_warp = TRUE,
  do_trunc = TRUE,
  fusion_groups = fusion_groups,
  fusion_style = "dashed",
  save_files = FALSE
)

# Stop profiling
Rprof(NULL)

# Summarize
prof_summary <- summaryRprof(tmp)

cli_h2("Top Functions by Self Time")
if (nrow(prof_summary$by.self) > 0) {
  top_self <- head(prof_summary$by.self, 15)
  for (i in seq_len(nrow(top_self))) {
    fn_name <- rownames(top_self)[i]
    self_time <- top_self[i, "self.time"]
    self_pct <- top_self[i, "self.pct"]
    cli_alert("{fn_name}: {self_time}s ({self_pct}%)")
  }
}

cli_h2("Top Functions by Total Time")
if (nrow(prof_summary$by.total) > 0) {
  top_total <- head(prof_summary$by.total, 15)
  for (i in seq_len(nrow(top_total))) {
    fn_name <- rownames(top_total)[i]
    total_time <- top_total[i, "total.time"]
    total_pct <- top_total[i, "total.pct"]
    cli_alert("{fn_name}: {total_time}s ({total_pct}%)")
  }
}

# Clean up
unlink(tmp)

# =============================================================================
# OPTIMIZATION OPPORTUNITIES SUMMARY
# =============================================================================

cli_h1("Optimization Opportunities")

cli_alert_info("Based on profiling results:")
cli_bullets(c(
  "i" = "String operations (sprintf, paste) - Consider vectorization or pre-allocation",
  "i" = "Regex parsing (gregexpr) - Heavy for SVG path extraction",
  "i" = "Trigonometric functions - Consider lookup tables or approximations",
  "i" = "Loop overhead in R - Consider Rcpp for hot paths",
  "i" = "Memory allocation - Pre-allocate vectors/lists where possible"
))

cli_h2("Quake III-style Optimizations to Consider")
cli_bullets(c(
  "*" = "Fast inverse sqrt for distance calculations (IEEE 754 bit manipulation)",
  "*" = "Lookup tables for sin/cos (256-entry table with linear interpolation)",
  "*" = "Fixed-point arithmetic for coordinate calculations",

  "*" = "SIMD vectorization via Rcpp + RcppParallel",
  "*" = "Memory-mapped pre-computed edge tables"
))

cli_alert_success("Profiling complete!")
