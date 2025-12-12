# jigsawR Benchmark Suite

This directory contains performance benchmarking tools for the jigsawR package.

## Files

- `baseline_benchmark.R` - Comprehensive benchmark script for all puzzle types
- `results/` - Output directory for benchmark results (auto-created)

## Running Benchmarks

### From Command Line (WSL)

```bash
# Using Rscript wrapper
Rscript tests/benchmark/baseline_benchmark.R

# Using full path
"/mnt/c/Program Files/R/R-4.5.0/bin/Rscript.exe" tests/benchmark/baseline_benchmark.R
```

### From RStudio

```r
source("tests/benchmark/baseline_benchmark.R")
```

## Requirements

The benchmark script requires the following packages:

```r
install.packages(c("bench", "profvis", "htmlwidgets"))
```

## What Gets Benchmarked

### Puzzle Types and Sizes

1. **Rectangular Puzzles**
   - Small: 3x3 grid (9 pieces)
   - Medium: 5x5 grid (25 pieces)
   - Large: 8x8 grid (64 pieces)

2. **Hexagonal Puzzles**
   - Small: 2 rings (7 pieces)
   - Medium: 3 rings (19 pieces)
   - Large: 5 rings (61 pieces)

3. **Concentric Puzzles**
   - Small: 2 rings (7 pieces)
   - Medium: 3 rings (13 pieces)
   - Large: 5 rings (25 pieces)

### Metrics Collected

- **Timing**: min, median, mean, max execution time
- **Memory**: total memory allocated
- **Garbage Collection**: GC calls per iteration
- **Profiling**: CPU and memory profiling with `profvis`

## Output Files

All results are saved to `tests/benchmark/results/` with timestamps:

- `benchmark_YYYYMMDD_HHMMSS.rds` - Full benchmark object (can be loaded with `readRDS()`)
- `summary_YYYYMMDD_HHMMSS.csv` - Summary table (can be opened in Excel/spreadsheets)
- `profile_rect_5x5_YYYYMMDD_HHMMSS.html` - Interactive profiling for rectangular 5x5
- `profile_hex_3rings_YYYYMMDD_HHMMSS.html` - Interactive profiling for hexagonal 3 rings
- `profile_conc_3rings_YYYYMMDD_HHMMSS.html` - Interactive profiling for concentric 3 rings

## Viewing Profiling Results

Open the HTML profiling files in a web browser:

```r
# From R
browseURL("tests/benchmark/results/profile_rect_5x5_TIMESTAMP.html")

# From command line
xdg-open tests/benchmark/results/profile_rect_5x5_TIMESTAMP.html  # Linux
open tests/benchmark/results/profile_rect_5x5_TIMESTAMP.html      # Mac
start tests/benchmark/results/profile_rect_5x5_TIMESTAMP.html     # Windows
```

The profvis HTML files show:
- Flame graph of function calls
- Time spent in each function
- Memory allocation by function
- Source code with line-level timing

## Analyzing Results

### Load Previous Results

```r
# Load full benchmark object
results <- readRDS("tests/benchmark/results/benchmark_20250101_120000.rds")

# View summary
print(results)

# Extract specific metrics
median_times <- results$median
memory_usage <- results$mem_alloc
```

### Compare Over Time

```r
# Load multiple benchmark runs
old_results <- readRDS("tests/benchmark/results/benchmark_OLD.rds")
new_results <- readRDS("tests/benchmark/results/benchmark_NEW.rds")

# Compare median times
comparison <- data.frame(
  type = old_results$type,
  label = old_results$label,
  old_median = old_results$median,
  new_median = new_results$median,
  speedup = old_results$median / new_results$median
)
print(comparison)
```

## Interpreting Results

### Performance Characteristics

- **Rectangular puzzles**: Linear scaling with piece count
- **Hexagonal puzzles**: Higher overhead due to warping/truncation
- **Concentric puzzles**: Moderate complexity, circular geometry calculations

### Common Hotspots (from profiling)

1. Bezier curve generation
2. SVG path string concatenation
3. Coordinate transformations (hexagonal)
4. Edge detection and adjacency calculations

### Expected Performance (Baseline)

On a typical development machine:
- Small puzzles: < 100ms
- Medium puzzles: 100-500ms
- Large puzzles: 500ms-2s

*Note: Actual times depend on hardware and system load.*

## Tips for Performance Optimization

1. **Profile before optimizing**: Use profvis to identify actual bottlenecks
2. **Focus on hot paths**: Optimize functions called repeatedly
3. **Memory matters**: Reducing allocations can improve speed
4. **Vectorize**: Replace loops with vectorized operations where possible
5. **Re-benchmark**: Always verify that optimizations improve performance

## Troubleshooting

### "Package 'bench' is required"

```r
install.packages("bench")
```

### "Package 'profvis' is required"

```r
install.packages("profvis")
```

### Script runs but no output files

Check that you have write permissions in `tests/benchmark/results/`.

### Profiling HTML files won't open

Profvis requires an interactive R session. If running from command line, the HTML files are still generated but won't auto-open.

## Custom Benchmarks

To benchmark custom configurations:

```r
devtools::load_all()
library(bench)

# Custom benchmark
result <- bench::mark(
  generate_puzzle(
    type = "rectangular",
    seed = 42,
    grid = c(10, 10),  # Custom size
    size = c(1000, 1000),
    offset = 0
  ),
  iterations = 5,
  check = FALSE,
  memory = TRUE
)

print(result)
```

## Contributing

When making performance improvements:

1. Run baseline benchmark before changes
2. Make your changes
3. Run benchmark again
4. Document speedup in commit message
5. Save before/after benchmark results

## References

- [bench package documentation](https://bench.r-lib.org/)
- [profvis package documentation](https://rstudio.github.io/profvis/)
