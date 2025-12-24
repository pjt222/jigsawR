# Parameter Consistency Audit Report

**Issue**: #74
**Date**: 2024-12-24
**Status**: Audit Complete

## Executive Summary

This audit compares all parameters across 4 sources:
1. **R API** (`R/jigsawR_clean.R`) - The implementation
2. **Config** (`inst/config.yml`) - Single source of truth for defaults
3. **Shiny UI** (`inst/shiny-app/app.R`) - User interface
4. **Quarto Docs** (`quarto/api/*.qmd`, `quarto/gallery/*.qmd`) - Documentation

**Key Findings**: 12 discrepancies identified, categorized by severity.

---

## Master Comparison Table

### Core Shape Parameters

| Parameter | Config.yml | R API Default | Shiny UI | API Docs | Gallery Docs | Status |
|-----------|------------|---------------|----------|----------|--------------|--------|
| **tabsize** | 10 (%) | 20 (%) | 10 (%) | 15 | 20 | MISMATCH |
| **jitter** | 2 (%) | 4 (%) | 2 (%) | 4 | 4 | MISMATCH |
| **offset** | 0 (mm) | 0 (mm) | 0 (mm) | 0 | 0 | OK |
| **seed** | 1234 | NULL | 1234 | NULL | "random" | MISMATCH |
| **stroke_width** | 1 | 1.5 | 1 | - | - | MISMATCH |
| **opacity** | 100 (%) | 1.0 (0-1) | 100 (%) | - | - | UNIT DIFF |

### Rectangular Parameters

| Parameter | Config.yml | R API Default | Shiny UI | API Docs | Gallery Docs | Status |
|-----------|------------|---------------|----------|----------|--------------|--------|
| **rows** | 3 | 2 (via grid) | 3 | required | 3 | MISMATCH |
| **cols** | 4 | 2 (via grid) | 4 | required | 3 | MISMATCH |
| **width** | 400 (mm) | 200 (mm) | 400 (mm) | required | - | MISMATCH |
| **height** | 300 (mm) | 200 (mm) | 300 (mm) | required | - | MISMATCH |

### Hexagonal Parameters

| Parameter | Config.yml | R API Default | Shiny UI | API Docs | Gallery Docs | Status |
|-----------|------------|---------------|----------|----------|--------------|--------|
| **rings** | 3 | - (via grid) | 3 | required | 3 | OK |
| **diameter** | 360 (mm) | 200 (via size) | 360 (mm) | required | - | MISMATCH |
| **do_warp** | zigzag mode | FALSE | zigzag mode | TRUE | TRUE | MISMATCH |
| **do_trunc** | zigzag mode | FALSE | zigzag mode | TRUE | TRUE | MISMATCH |

### Concentric Parameters

| Parameter | Config.yml | R API Default | Shiny UI | API Docs | Gallery Docs | Status |
|-----------|------------|---------------|----------|----------|--------------|--------|
| **center_shape** | hexagon | hexagon | hexagon | hexagon | hexagon | OK |
| **boundary_facing** | outward | outward | outward | - | - | OK |

### Voronoi/Random Parameters

| Parameter | Config.yml | R API Default | Shiny UI | API Docs | Gallery Docs | Status |
|-----------|------------|---------------|----------|----------|--------------|--------|
| **n_cells** | - | - | 20 | - | 12 | MISMATCH |
| **n_corner** | - | 4 | 4 | 5 | 4 | MISMATCH |
| **point_distribution** | - | fermat | fermat | - | fermat | OK |
| **min_tab_size** | null | NULL | 0 (=null) | - | not documented | GAP |
| **max_tab_size** | null | NULL | 0 (=null) | - | not documented | GAP |

---

## Discrepancy Details

### HIGH PRIORITY - Default Value Conflicts

#### 1. tabsize Default
| Source | Value | Unit |
|--------|-------|------|
| config.yml | 10 | % |
| R API | 20 | % |
| Shiny UI | 10 | % (from config) |
| generate-puzzle.qmd | 15 | (no unit) |
| Gallery docs | 20 | (no unit) |

**Impact**: Users will get different results depending on which interface they use.
**Recommendation**: Standardize to **config.yml value (10%)** and update all documentation.

#### 2. jitter Default
| Source | Value | Unit |
|--------|-------|------|
| config.yml | 2 | % |
| R API | 4 | % |
| Shiny UI | 2 | % (from config) |
| Quarto docs | 4 | (no unit) |

**Impact**: Visual inconsistency between API and Shiny app results.
**Recommendation**: Standardize to **config.yml value (2%)** and update R API default.

#### 3. stroke_width Default
| Source | Value |
|--------|-------|
| config.yml | 1 |
| R API | 1.5 |

**Recommendation**: Update R API to use **1** (matching config.yml).

#### 4. grid/size Defaults (Rectangular)
| Source | grid | size |
|--------|------|------|
| config.yml | rows=3, cols=4 | 400x300 |
| R API | c(2, 2) | c(200, 200) |

**Recommendation**: Update R API defaults to match config.yml: `grid = c(3, 4)`, `size = c(400, 300)`.

### MEDIUM PRIORITY - Documentation Gaps

#### 5. do_warp/do_trunc Defaults
| Source | do_warp | do_trunc |
|--------|---------|----------|
| R API generate_puzzle() | FALSE | FALSE |
| R API geom_puzzle_hex() | TRUE | TRUE |
| Quarto docs | TRUE | TRUE |

**Issue**: ggplot2 functions have different defaults than main API.
**Recommendation**: Document the difference clearly or align defaults.

#### 6. n_corner Default
| Source | Value |
|--------|-------|
| R API | 4 |
| generate-puzzle.qmd | 5 |
| Gallery (random.qmd) | 4 |

**Recommendation**: Standardize to **4** (matches R API and gallery).

#### 7. min_tab_size / max_tab_size Documentation
- **Not documented** in gallery parameter tables
- Only mentioned in API reference for voronoi/random types
- Missing from README entirely

**Recommendation**: Add to gallery tables and README.

### LOW PRIORITY - Unit Clarification Needed

#### 8. tabsize Unit
| Source | Unit Stated |
|--------|-------------|
| config.yml | % (implied) |
| R API docs | "percentage" |
| Shiny UI | % (slider suffix) |
| Gallery docs | **none** |

**Recommendation**: Explicitly state "%" in all gallery parameter tables.

#### 9. opacity Unit Difference
| Source | Range | Representation |
|--------|-------|----------------|
| config.yml | 0-100 | percentage |
| R API | 0.0-1.0 | decimal |
| Shiny UI | 0-100 | percentage |

**Note**: This is handled by conversion in Shiny server. Document the difference.

#### 10. Grid Parameter Order
Some docs say "cols, rows" vs "rows, columns".

**Recommendation**: Standardize documentation to always use **"rows, cols"** order (R convention: row-major).

---

## Missing Documentation

### Parameters Not in README
1. `layout` (grid vs repel)
2. `repel_margin`, `repel_max_iter`
3. `min_tab_size`, `max_tab_size`
4. `fusion_groups`, `fusion_style`, `fusion_opacity`
5. `fill_color`, `fills`
6. `background` (gradient options)
7. `do_circular_border`, `boundary_facing`

### Parameters Not in Gallery Docs
1. `min_tab_size`, `max_tab_size` - Only in API reference
2. `layout`, `repel_margin`, `repel_max_iter` - Not documented
3. `do_circular_border` - Missing from hexagonal gallery

---

## Recommended Actions

### Phase 1: Align R API with config.yml (Source of Truth)

Update `R/jigsawR_clean.R` generate_puzzle() defaults:
```r
# Current → Proposed
tabsize = 20 → tabsize = 10
jitter = 4 → jitter = 2
stroke_width = 1.5 → stroke_width = 1
grid = c(2, 2) → grid = c(3, 4)  # rectangular
size = c(200, 200) → size = c(400, 300)  # rectangular
```

### Phase 2: Update Quarto Documentation

1. **generate-puzzle.qmd**: Fix tabsize default (15 → 10)
2. **All gallery files**: Add unit "%" to tabsize and jitter
3. **Gallery files**: Add min_tab_size/max_tab_size to parameter tables
4. **random.qmd**: Verify n_corner default matches API

### Phase 3: Update README

Add a "Quick Parameter Reference" section with:
- Core parameters table (tabsize, jitter, offset, seed)
- Type-specific parameter summaries
- Link to full API documentation

---

## Verification Checklist

After fixes are applied:

- [ ] `generate_puzzle(type="rectangular")` uses config.yml defaults
- [ ] `generate_puzzle(type="hexagonal")` uses config.yml defaults
- [ ] Shiny app and API produce identical output with same parameters
- [ ] All Quarto docs show correct default values
- [ ] All gallery parameter tables include units
- [ ] README has parameter quick-reference section

---

## Files Modified

This audit examined:
- `R/jigsawR_clean.R`
- `R/geom_puzzle.R`
- `inst/config.yml`
- `inst/shiny-app/app.R`
- `quarto/api/generate-puzzle.qmd`
- `quarto/api/ggpuzzle.qmd`
- `quarto/gallery/rectangular.qmd`
- `quarto/gallery/hexagonal.qmd`
- `quarto/gallery/concentric.qmd`
- `quarto/gallery/voronoi.qmd`
- `quarto/gallery/random.qmd`
- `quarto/getting-started.qmd`
- `README.md`
