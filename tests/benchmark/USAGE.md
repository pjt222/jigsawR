# Quick Start Guide for Benchmarking

## Basic Usage

### Run the benchmark script

```bash
# From project root directory
Rscript tests/benchmark/baseline_benchmark.R
```

**Note**: The script takes approximately 5-10 minutes to complete all benchmarks and profiling.

## What Happens When You Run It

1. **Setup**: Loads the jigsawR package and checks dependencies
2. **Benchmarking**: Runs 9 puzzle configurations (3 types × 3 sizes)
   - Each configuration is run 10 times
   - Timing and memory statistics are collected
3. **Profiling**: Generates detailed performance profiles for 3 medium-sized puzzles
4. **Analysis**: Computes comparative metrics and scaling behavior
5. **Output**: Saves results to `tests/benchmark/results/`

## Expected Output

### Console Output

You'll see:
- Progress messages for each benchmark
- Summary tables with timing and memory statistics
- Comparative analysis between puzzle types
- Scaling analysis showing performance growth
- File paths for saved results

### Generated Files

```
tests/benchmark/results/
├── benchmark_YYYYMMDD_HHMMSS.rds          # Full benchmark data
├── summary_YYYYMMDD_HHMMSS.csv            # CSV summary (Excel-friendly)
├── profile_rect_5x5_YYYYMMDD_HHMMSS.html  # Interactive profile
├── profile_hex_3rings_YYYYMMDD_HHMMSS.html
└── profile_conc_3rings_YYYYMMDD_HHMMSS.html
```

## Viewing Results

### 1. Summary Table (CSV)

Open in Excel, Google Sheets, or R:

```r
results <- read.csv("tests/benchmark/results/summary_TIMESTAMP.csv")
print(results)
```

### 2. Full Benchmark Object (RDS)

```r
benchmark <- readRDS("tests/benchmark/results/benchmark_TIMESTAMP.rds")

# View all metrics
print(benchmark)

# Extract specific columns
timing <- benchmark[, c("type", "label", "median", "mean")]
memory <- benchmark[, c("type", "label", "mem_alloc")]
```

### 3. Interactive Profiling (HTML)

Open in your web browser to see:
- Flame graphs of function calls
- Time spent in each function
- Memory allocation patterns
- Line-by-line source code timing

```r
# Open in browser from R
browseURL("tests/benchmark/results/profile_rect_5x5_TIMESTAMP.html")
```

Or double-click the HTML files in your file manager.

## Example Output

### Summary Table

```
type         label       median     mean       mem_alloc  n_gc
rectangular  3x3         45.2ms     47.1ms     12.5MB     0
rectangular  5x5         156.3ms    158.9ms    42.3MB     1
rectangular  8x8         523.7ms    531.2ms    145.8MB    3
hexagonal    2 rings     38.6ms     39.8ms     10.2MB     0
hexagonal    3 rings     142.8ms    145.3ms    38.7MB     1
hexagonal    5 rings     687.3ms    695.1ms    178.3MB    4
concentric   2 rings     42.1ms     43.5ms     11.8MB     0
concentric   3 rings     128.4ms    131.2ms    35.2MB     1
concentric   5 rings     445.6ms    451.8ms    122.4MB    2
```

*Note: Times are examples and will vary by system*

### Comparative Analysis

```
Comparative Analysis: Medium-Sized Puzzles
========================================================================

Rectangular (5x5):       156.3ms
Hexagonal (3 rings):     142.8ms
Concentric (3 rings):    128.4ms

Fastest type (medium):   Concentric

Memory - Rectangular:    42.3MB
Memory - Hexagonal:      38.7MB
Memory - Concentric:     35.2MB

Most memory efficient:   Concentric
```

### Scaling Analysis

```
Rectangular Scaling:
  3x3 (9 pieces):      45.2ms
  5x5 (25 pieces):     156.3ms (3.46x)
  8x8 (64 pieces):     523.7ms (11.58x)

Hexagonal Scaling:
  2 rings (7 pieces):    38.6ms
  3 rings (19 pieces):   142.8ms (3.70x)
  5 rings (61 pieces):   687.3ms (17.80x)

Concentric Scaling:
  2 rings (7 pieces):    42.1ms
  3 rings (13 pieces):   128.4ms (3.05x)
  5 rings (25 pieces):   445.6ms (10.58x)
```

## Interpreting Profiling Results

### Flame Graph

- **Width**: Time spent in function
- **Height**: Call stack depth
- **Color**: Different functions (for visibility)

**Look for**:
- Wide bars = hotspots (optimization targets)
- Deep stacks = complex call chains
- Repeated patterns = opportunities for caching

### Source Code View

- Line numbers with timing information
- Memory allocation per line
- Identifies specific bottlenecks

**Tips**:
- Focus on lines with high cumulative time
- Look for unnecessary memory allocations
- Check for repeated expensive operations

## Troubleshooting

### "Package 'bench' is required"

```r
install.packages("bench")
```

### "Package 'profvis' is required"

```r
install.packages("profvis")
```

### Script fails with "cannot open file"

Make sure you're running from the project root:

```bash
cd /path/to/jigsawR
Rscript tests/benchmark/baseline_benchmark.R
```

### Results directory not created

Check write permissions:

```bash
ls -la tests/benchmark/
mkdir -p tests/benchmark/results/
```

### Profvis HTML won't open interactively

This is normal when running from command line. The files are still generated - open them manually in a browser.

## Next Steps

1. **Baseline**: Run benchmark on current version
2. **Optimize**: Make performance improvements
3. **Re-benchmark**: Run again to measure impact
4. **Compare**: Use saved RDS files to compare before/after
5. **Document**: Note improvements in commit messages

## Advanced Usage

### Custom Configurations

Edit `baseline_benchmark.R` to add custom test cases:

```r
# Add to configuration section
custom_configs <- list(
  huge = list(grid = c(10, 10), size = c(1000, 1000), label = "10x10")
)

# Add to benchmark section
custom_results <- lapply(names(custom_configs), function(name) {
  config <- custom_configs[[name]]
  run_single_benchmark("rectangular", config, config$label)
})
```

### Benchmark Specific Functionality

```r
# Benchmark just piece generation
bench::mark(
  generate_pieces_internal(
    puzzle_type = "rectangular",
    grid = c(5, 5),
    size = c(500, 500),
    rng = create_rng(42),
    canvas_size = list(width = 500, height = 500)
  ),
  iterations = 20
)
```

### Memory Profiling Only

```r
profvis::profvis({
  result <- generate_puzzle(
    type = "hexagonal",
    seed = 42,
    grid = c(5),
    size = c(500)
  )
}, prof_output = "memory-profile.html")
```

## Performance Goals

### Target Times (Development Machine)

- Small puzzles: < 100ms
- Medium puzzles: 100-300ms
- Large puzzles: 300-1000ms

### Acceptable Scaling

- ~O(n) for piece count (linear)
- ~O(n) for memory (linear)
- Minimal GC overhead (< 1 per iteration)

### Red Flags

- Exponential scaling with puzzle size
- Memory leaks (increasing over iterations)
- High GC frequency (> 5 per iteration)
- Outlier times (max >> median)

## Resources

- [bench package](https://bench.r-lib.org/)
- [profvis package](https://rstudio.github.io/profvis/)
- [R Performance Book](https://adv-r.hadley.nz/perf-improve.html)
