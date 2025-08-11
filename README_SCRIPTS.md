# Script Organization

## Current Structure

All R scripts have been organized into appropriate directories:

### Production Code (`R/`)
- **Core Functions**: 
  - `rectangular_puzzle.R` - Rectangular puzzle generation
  - `hexagonal_puzzle.R` - Hexagonal puzzle generation
  - `gradient_background.R` - Background generation
  - `svg_utils.R` - SVG utilities
  - `image_processing.R` - PNG conversion
  - `main_generator.R` - Main orchestration

- **Clean Implementation**:
  - `puzzle_core_clean.R` - Clean core with shared edges
  - `jigsawR_clean.R` - Clean API

- **Individual Pieces**:
  - `individual_pieces_final.R` - Current individual pieces implementation
  - `bezier_utils.R` - Bezier curve utilities

### Archived Scripts (`R/scripts_archive/`)
Development and debugging scripts that were previously at top level:
- `debug_*.R` - Debugging scripts
- `test_*.R` - Test scripts  
- `implement_curve_reversal.R`
- `run_individual_pieces_generation.R`
- `update_package_exports.R`

### Examples (`inst/examples/`)
- `clean_usage_example.R` - Clean API usage examples
- `legacy_compatibility.R` - Backward compatibility (replaces `svg_to_png_overlay.R`)
- `individual_pieces_example.R` - Individual pieces demonstration
- `generate_puzzles.R` - General puzzle generation examples

### Tests (`tests/`)
- `test_clean_2x2.R` - Clean implementation tests

### Debug Scripts (`debug_scripts/`)
Historical debugging scripts for reference

## Migration Notes

- **svg_to_png_overlay.R** → `inst/examples/legacy_compatibility.R`
  - Maintains backward compatibility
  - New projects should use clean API

- All debug and test scripts moved to `R/scripts_archive/`
  - Preserved for reference
  - Not part of production code

## Usage

For new projects, use the clean API:
```r
source("R/puzzle_core_clean.R")
source("R/jigsawR_clean.R")

puzzle <- generate_puzzle(
  grid = c(2, 2),
  seed = 1234,
  output = "both"
)
```

For legacy compatibility:
```r
source("inst/examples/legacy_compatibility.R")
```