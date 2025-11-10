# Clean jigsawR Implementation

## Overview

This document describes the clean, reproducible implementation of the jigsawR puzzle generation system. The implementation follows software engineering best practices with no hard-coded adjustments or output tinkering.

## Core Principles

### 1. Reproducibility
- **Seed-based generation**: Same seed always produces identical output
- **Deterministic algorithms**: No random variations outside seed control
- **Version stability**: Code changes maintain backward compatibility

### 2. Clean Architecture
- **Separation of concerns**: Core algorithms, edge generation, and rendering are separate
- **No hard-coded values**: All adjustments are algorithmic, not manual
- **Modular design**: Each component has a single responsibility

### 3. Mathematical Correctness
- **Shared edges**: Adjacent pieces share the EXACT SAME edge path
- **Proper reversal**: Edges are traversed in opposite directions by adjacent pieces
- **Bezier continuity**: Curves maintain mathematical continuity

## File Structure

```
R/
├── puzzle_core_clean.R      # Core puzzle generation with shared edges
├── jigsawR_clean.R          # Main API and pipeline
└── rectangular_puzzle.R     # Original JS translation (used for helpers)

tests/
└── test_clean_2x2.R         # Comprehensive test suite

inst/examples/
└── clean_usage_example.R    # Usage demonstrations
```

## Key Components

### 1. Core Generation (`puzzle_core_clean.R`)

```r
generate_puzzle_core(seed, grid, size, tabsize, jitter)
```
- Generates complete puzzle structure with shared edges
- Pre-calculates all edge paths once
- Returns structure used by both complete and individual modes

### 2. Edge Sharing Algorithm

The key insight: **Adjacent pieces share the SAME edge path**

- Edge defined once: `edges$horizontal[[row]][[col]]`
- Piece above uses: `edge$forward` (left to right)
- Piece below uses: `edge$reverse` (right to left)

### 3. Edge Reversal Mathematics

For a cubic Bezier curve with control points:
- Forward: `P0 -> (P1, P2) -> P3`
- Reverse: `P3 -> (P2, P1) -> P0`

The reversal swaps control point order within each segment and reverses segment order.

### 4. Main API (`jigsawR_clean.R`)

```r
generate_puzzle(
  type = "rectangular",
  grid = c(2, 2),
  size = c(200, 200),
  seed = 1234,
  output = "both",      # "complete", "individual", or "both"
  background = "none"   # "none", "gradient", or color
)
```

## Usage Examples

### Simple 2x2 Puzzle
```r
source("R/rectangular_puzzle.R")
source("R/puzzle_core_clean.R")
source("R/jigsawR_clean.R")

puzzle <- generate_puzzle(
  grid = c(2, 2),
  seed = 1234,
  output = "both"
)
```

### Colored Individual Pieces
```r
puzzle <- generate_puzzle(
  grid = c(3, 3),
  seed = 5678,
  output = "individual",
  colors = c("red", "blue", "green", "orange")
)
```

### Batch Generation
```r
variations <- list(
  list(name = "easy", seed = 100, grid = c(2, 2)),
  list(name = "medium", seed = 200, grid = c(3, 3)),
  list(name = "hard", seed = 300, grid = c(4, 4))
)

results <- generate_puzzle_batch(variations)
```

## Validation

The implementation includes comprehensive validation:

1. **Reproducibility**: Same seed produces identical output
2. **Edge Sharing**: Adjacent pieces reference same edge data
3. **Path Integrity**: All paths start with M, end with Z
4. **Mathematical Correctness**: Reversed edges maintain continuity

Run tests with:
```bash
Rscript tests/test_clean_2x2.R
```

## Output Files

The system generates:
- `*_complete.svg`: All pieces as connected puzzle
- `*_individual.svg`: Each piece as separate path element
- `*_pieces/`: Directory with individual piece files
- `*_background.svg`: Optional background layer

## Comparison with Previous Implementation

| Aspect | Previous | Clean Implementation |
|--------|----------|---------------------|
| Edge Generation | Generated multiple times | Generated once, shared |
| Edge Reversal | Approximated or skipped | Mathematically correct |
| Hard-coded Fixes | Many debug scripts | None |
| Reproducibility | Uncertain | Guaranteed |
| Architecture | Mixed concerns | Clean separation |

## Best Practices

1. **Always use seeds** for reproducible output
2. **Validate puzzles** with the provided validation function
3. **Test edge cases** (2x2, 1x1, large grids)
4. **Maintain backward compatibility** when extending

## Future Extensions

The clean architecture makes it easy to add:
- Hexagonal puzzle support
- Custom piece shapes
- Variable piece sizes
- Export to other formats (DXF, PDF)
- Kerf compensation for laser cutting

## Summary

This clean implementation provides:
- ✅ Reproducible, deterministic output
- ✅ Mathematically correct edge sharing
- ✅ No hard-coded adjustments
- ✅ Clean, maintainable architecture
- ✅ Comprehensive testing
- ✅ Professional API design

The code is production-ready for generating jigsaw puzzles for various applications including laser cutting, educational materials, and digital puzzles.