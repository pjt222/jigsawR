# jigsawR Parameter Consistency Report

**Generated**: 2025-01-07
**Purpose**: Compare parameters across API, ggpuzzle, and Shiny app for consistency

## Executive Summary

Analysis of 60+ parameters across three interfaces reveals generally good consistency, with several areas for improvement:

- **Good**: Core puzzle parameters (type, grid, size, seed) are consistent
- **Good**: Tab styling (tabsize, jitter) and fusion parameters align well
- **Concern**: Size parameter ordering (now `c(height, width)`) needs Shiny UI update
- **Concern**: Several API parameters missing from ggpuzzle/Shiny
- **Concern**: Some default values differ between interfaces

---

## 1. Parameter Coverage Matrix

### 1.1 Core Puzzle Parameters

| Parameter | API | ggpuzzle | Shiny | Status |
|-----------|-----|----------|-------|--------|
| type | `type` | `puzzle_type` (stat) | `puzzle_type` | **Name differs** |
| grid (rows/cols) | `grid = c(rows, cols)` | `rows`, `cols` | `rows`, `cols` | OK (expanded) |
| grid (rings) | `grid = c(rings)` | `rings` | `rings` | OK |
| size (rect) | `size = c(height, width)` | `width`, `height` | `width`, `height` | **Order differs!** |
| size (hex/conc) | `size = c(diameter)` | `diameter` | `diameter` | OK |
| seed | `seed` | `seed` | `seed` | OK |

**Issue**: Shiny UI uses `width`, `height` as separate inputs, but the API now expects `size = c(height, width)`. The mapping in Shiny may be inverted.

### 1.2 Tab Styling Parameters

| Parameter | API | ggpuzzle | Shiny | Default (API) | Default (Shiny) | Status |
|-----------|-----|----------|-------|---------------|-----------------|--------|
| tabsize | `tabsize` | `tabsize` | `tabsize` | 10 | 10 | OK |
| jitter | `jitter` | `jitter` | `jitter` | 2 | 2 | OK |
| min_tab_size | `min_tab_size` | `min_tab_size` | `min_tab_size` | NULL | 0 | **Default differs** |
| max_tab_size | `max_tab_size` | `max_tab_size` | `max_tab_size` | NULL | 0 | **Default differs** |

### 1.3 Layout/Positioning Parameters

| Parameter | API | ggpuzzle | Shiny | Status |
|-----------|-----|----------|-------|--------|
| offset | `offset` | `offset` | `offset` | OK |
| layout | `layout` | - | `layout` | **Missing in ggpuzzle** |
| repel_margin | `repel_margin` | - | `repel_margin` | **Missing in ggpuzzle** |
| repel_max_iter | `repel_max_iter` | - | `repel_max_iter` | **Missing in ggpuzzle** |

**Note**: ggpuzzle uses ggplot2's coordinate system, so repel layout isn't applicable.

### 1.4 Fill/Color Parameters

| Parameter | API | ggpuzzle | Shiny | Status |
|-----------|-----|----------|-------|--------|
| fill_color | `fill_color` | - (use `fill` aes) | `fill_color` | ggpuzzle uses aes |
| fill_palette | `fill_palette` | - (use `scale_fill_*`) | `fill_palette` | ggpuzzle uses scales |
| fills | `fills` | - (use `fill` aes) | - | ggpuzzle uses aes |
| colors | `colors` | - (use `colour` aes) | `stroke_color` | ggpuzzle uses aes |
| palette | `palette` | - (use `scale_colour_*`) | `stroke_palette` | ggpuzzle uses scales |
| palette_invert | `palette_invert` | - (use scale direction) | `fill_palette_invert` | ggpuzzle uses scales |
| stroke_width | `stroke_width` | `linewidth` (aes) | `stroke_width` | **Name differs** |
| opacity | `opacity` | `alpha` (aes) | `opacity` | **Name differs** |

**Design Decision**: ggpuzzle follows ggplot2 conventions (aesthetics + scales). This is intentional and correct.

### 1.5 Background Parameters

| Parameter | API | ggpuzzle | Shiny | Status |
|-----------|-----|----------|-------|--------|
| background | `background` | - (use `theme()`) | `background_type` + colors | ggpuzzle uses theme |

### 1.6 Label Parameters

| Parameter | API | ggpuzzle | Shiny | Default (API) | Default (Shiny) | Status |
|-----------|-----|----------|-------|---------------|-----------------|--------|
| show_labels | `show_labels` | `show_labels` | `show_labels` | FALSE | TRUE | **Default differs** |
| label_color | `label_color` | `label_color`/`label_colour` | `label_color` | "black" | "#000000" | OK (same) |
| label_size | `label_size` | `label_size` | `label_size` | NULL (auto) | 0 (auto) | OK (both auto) |

### 1.7 Fusion Parameters

| Parameter | API | ggpuzzle | Shiny | Default | Status |
|-----------|-----|----------|-------|---------|--------|
| fusion_groups | `fusion_groups` | `fusion_groups` | `fusion_groups` | NULL/"" | OK |
| fusion_style | `fusion_style` | `fusion_style` | `fusion_style` | "none" | OK |
| fusion_opacity | `fusion_opacity` | `fusion_opacity` | `fusion_opacity` | 0.3 | OK |

### 1.8 Hexagonal Parameters

| Parameter | API | ggpuzzle | Shiny | Default | Status |
|-----------|-----|----------|-------|---------|--------|
| do_warp | `do_warp` | `do_warp` | (via hex_boundary) | FALSE/TRUE | **Default differs** |
| do_trunc | `do_trunc` | `do_trunc` | (via hex_boundary) | FALSE/TRUE | **Default differs** |
| do_circular_border | `do_circular_border` | `do_circular_border` | (via hex_boundary) | FALSE | OK |

**Note**: API defaults are FALSE, ggpuzzle defaults are TRUE. Shiny uses radio buttons that map to combinations.

### 1.9 Concentric Parameters

| Parameter | API | ggpuzzle | Shiny | Default | Status |
|-----------|-----|----------|-------|---------|--------|
| center_shape | `center_shape` | `center_shape` | - (hardcoded) | "hexagon" | **Missing in Shiny** |
| boundary_facing | `boundary_facing` | `boundary_facing` | `conc_boundary_facing` | "outward" | OK |

### 1.10 Voronoi Parameters

| Parameter | API | ggpuzzle | Shiny | Status |
|-----------|-----|----------|-------|--------|
| n_cells | `grid` | `n_cells` | `n_cells` | OK |
| point_distribution | `point_distribution` | `point_distribution` | `point_distribution` | OK |

### 1.11 Random Shape Parameters

| Parameter | API | ggpuzzle | Shiny | Status |
|-----------|-----|----------|-------|--------|
| n_pieces | `grid` | `n_pieces` | `n_interior` | **Name differs** |
| n_corner | `n_corner` | `n_corner` | `n_corner` | OK |

---

## 2. Identified Issues

### 2.1 Critical: Size Parameter Order - FIXED ✓

**Issue**: The API was changed from `size = c(width, height)` to `size = c(height, width)` to match `grid = c(rows, cols)`.

**Resolution**: Fixed in `inst/shiny-app/app.R` - all 6 locations now use `c(height, width)` order:
- Lines 1355, 1358, 1361 (Generate button handler)
- Lines 2069, 2072, 2075 (Download handler)

```r
# Now correct:
size_param <- c(input$height, input$width)  # height first
size_param <- c(input$vor_height, input$vor_width)
size_param <- c(input$rnd_height, input$rnd_width)
```

### 2.2 Medium: Default Value Mismatches

| Parameter | API Default | Shiny Default | Impact |
|-----------|-------------|---------------|--------|
| do_warp | FALSE | (via "zigzag") FALSE | OK |
| do_trunc | FALSE | (via "zigzag") FALSE | OK |
| show_labels | FALSE | TRUE | Visual difference |
| min_tab_size | NULL | 0 | Functionally same |
| max_tab_size | NULL | 0 | Functionally same |

### 2.3 Minor: Naming Inconsistencies

| Concept | API | ggpuzzle | Shiny | Recommendation |
|---------|-----|----------|-------|----------------|
| Puzzle type | `type` | `puzzle_type` | `puzzle_type` | Consider aligning |
| Random interior points | `grid` | `n_pieces` | `n_interior` | Standardize name |
| Stroke width | `stroke_width` | `linewidth` | `stroke_width` | ggpuzzle uses ggplot2 convention (correct) |
| Opacity | `opacity` | `alpha` | `opacity` | ggpuzzle uses ggplot2 convention (correct) |

### 2.4 Missing Features

| Feature | API | ggpuzzle | Shiny | Priority |
|---------|-----|----------|-------|----------|
| center_shape UI | ✓ | ✓ | ✗ | Medium |
| repel layout | ✓ | ✗ | ✓ | Low (design choice) |
| min_piece_size | ✓ | ✗ | ✗ | Low |

---

## 3. Recommendations

### 3.1 Immediate Fixes (High Priority)

1. ~~**Fix Shiny size parameter order**~~: ✓ DONE - Updated `inst/shiny-app/app.R` to use `c(height, width)` in all 6 locations.

2. **Add center_shape to Shiny**: Add a radio button for concentric puzzles to choose between "hexagon" and "circle" center shapes.

### 3.2 Short-term Improvements (Medium Priority)

3. **Standardize "n_interior" naming**: Consider renaming Shiny's `n_interior` to match the API/ggpuzzle naming pattern.

4. **Document default differences**: Add a note in documentation explaining why show_labels defaults differ (API = FALSE for programmatic use, Shiny = TRUE for visual exploration).

### 3.3 Long-term Considerations (Low Priority)

5. **Consider type/puzzle_type alignment**: The stat/geom uses `puzzle_type` internally but this is an implementation detail. Document the relationship.

6. **min_tab_size/max_tab_size semantics**: Consider whether `0` should mean "no constraint" (like Shiny) or keep `NULL` (API) for the "no constraint" semantic.

---

## 4. Verification Checklist

After implementing fixes, verify:

- [x] `generate_puzzle(grid=c(3,4), size=c(300,400))` produces 3 rows, 4 cols, height=300, width=400
- [x] Shiny app with rows=3, cols=4, height=300, width=400 matches API output
- [x] ggpuzzle with rows=3, cols=4, height=300, width=400 matches API output
- [ ] Concentric puzzles with center_shape UI work correctly
- [x] All 1986 tests still pass

---

## 5. Files to Review/Modify

1. **inst/shiny-app/app.R**: Size parameter order fix
2. **R/stat_puzzle.R**: Verify size handling
3. **docs/**: Update documentation for size parameter semantics
4. **quarto/**: Update examples if needed

---

*This report was generated by analyzing three subagent outputs examining API, ggpuzzle, and Shiny app parameters.*
