# jigsawR 0.1.0

## Initial Release

### Puzzle Types
* Rectangular jigsaw puzzles with configurable grid dimensions
* Hexagonal puzzles with circular warping and edge truncation
* Concentric ring puzzles with configurable center shapes
* Voronoi puzzles using Fermat spiral cell generation
* Random shape puzzles via Delaunay triangulation
* SNIC superpixel puzzles with image-aware segmentation

### Core Features
* `generate_puzzle()` unified API for all puzzle types
* High-quality SVG output with configurable piece separation
* PILES notation (Puzzle Input Line Entry System) for piece fusion groups
* Seed-based deterministic generation for reproducibility
* Gradient and noise-based fill patterns
* Individual piece extraction and export

### ggplot2 Integration
* `geom_puzzle_rect()`, `geom_puzzle_hex()`, `geom_puzzle_conc()`,
  `geom_puzzle_vor()`, `geom_puzzle_rnd()`, `geom_puzzle_snic()` layer functions
* `stat_puzzle()` for computing puzzle geometries
* `theme_puzzle()` for clean puzzle visualization
* Fusion-aware rendering with configurable fill and stroke palettes

### Shiny Application
* Interactive puzzle generator with live preview
* Dark mode support
* PNG and SVG download options
* Configurable styling (fills, strokes, labels, backgrounds)

### Performance
* Rcpp-accelerated Bezier curve computation and SVG path translation
* Optional parallel processing support via future/furrr
