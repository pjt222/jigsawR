# jigsawR (development version)

# jigsawR 0.4.0

## New Features

* **SNIC superpixel puzzle type** with image-aware boundary segmentation and
  synthetic mode for use without input images (#80)
* **Voronoi puzzle type** using Fermat spiral cell generation (#42)
* **Random shape puzzle type** via Delaunay triangulation (#41)
* **ggplot2 integration**: `geom_puzzle_rect()`, `geom_puzzle_hex()`,
  `geom_puzzle_conc()`, `geom_puzzle_vor()`, `geom_puzzle_rnd()`,
  `geom_puzzle_snic()` layer functions with `stat_puzzle()` and
  `theme_puzzle()` (#54)
* **Fusion rendering** for all puzzle types including voronoi and random,
  with `fusion_style` and `fusion_opacity` parameters (#68)
* **`fill_direction` parameter** for spatial color ordering across piece fills
* **`fill_palette` parameter** for ggpuzzle geom functions
* **`show_labels` parameter** for ggpuzzle label display
* **Min/max tab size constraints** for all puzzle types (#41, #42, #76)
* **Tabsize normalization** with unified formula and defaults across all
  puzzle types
* **Rcpp C++ optimizations** for Bezier curve computation and SVG path
  translation
* **RNG batch optimization** integrated into puzzle generators (#65)
* **Cached segment extraction** for ~40x faster coordinate parsing
* **Quarto documentation site** with dark mode toggle and gallery pages
  (#62, #69)

## Shiny Application

* Dark mode support with CSS contrast improvements
* SNIC puzzle type in navigation
* Nested accordion UI with slider enhancements (#75)
* Fusion styling updates without regeneration
* Voronoi and Random puzzle types in UI (#41, #42)
* Tab size constraint controls

## Bug Fixes

* Fixed blank PNG rendering in Quarto with `theme_puzzle()` addition
* Fixed Y-axis inversion for voronoi/random `fill_direction`
* Fixed fusion edge rendering with lower Bezier resolution for dashed edges
* Fixed missing boundary strokes and edge segment tracking for voronoi/random
* Fixed offset parameter wiring through geom functions
* Fixed blank images when `ggplot()` has no data
* Fixed gradient test compatibility with ggplot2 4.0 S7 objects
* Fixed PILES keyword tokenization and row/column piece lookups
* Fixed size parameter order standardized to `c(height, width)`
* Fixed `%||%` operator to use rlang
* Resolved 15 pre-existing test failures
* SNIC: handle non-matrix labels, platform-robust adjacency tests

## CI/CD

* Full R-CMD-check matrix: windows, macOS, ubuntu (release, devel, oldrel-1)
* Quarto site publishing workflow
* Shiny app deployment workflow
* RCDT archived package handled via `Remotes` field
* Increased CI timeout for oldrel-1 source compilation

## Documentation

* Quarto documentation site with API/ggpuzzle side-by-side tabsets
* Responsive SVG embedding with `htmltools`
* Gallery pages with fusion group examples
* README redesigned as attractive entry point (#63)
* GPL-3 license boilerplate expanded

## Internal

* Split `unified_renderer.R` into modular renderer files
* Consolidated duplicated code across tessellation puzzle types
* CRAN compliance remediation (#81-#85)
* Namespace cleanup and performance refactoring
* Test suite expanded to 2,200+ tests

# jigsawR 0.3.0

## Features

* Concentric ring puzzle type with configurable center shapes
* PILES notation (Puzzle Input Line Entry System) for piece fusion groups
* Noise-based fill patterns
* Individual piece extraction and export
* Repel layout for separated piece positioning

# jigsawR 0.2.0

## Features

* Hexagonal puzzle type with circular warping and edge truncation
* Gradient background support
* Seed-based deterministic generation

# jigsawR 0.1.0

## Initial Release

* Rectangular jigsaw puzzles with configurable grid dimensions
* `generate_puzzle()` unified API
* High-quality SVG output with configurable piece separation
* Shiny web application with live preview and PNG/SVG downloads
