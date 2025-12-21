# Implementation Plan: Consolidate Aspect Ratio Documentation

**GitHub Issue**: #73
**Date**: 2025-12-21
**Status**: COMPLETED

## Executive Summary

Consolidate 3 nearly-identical "Aspect Ratios" sections from gallery files into a single authoritative guide, reducing documentation maintenance burden and improving user experience.

## Current State Analysis

### Duplicate Sections Found

| File | Lines | Content |
|------|-------|---------|
| `quarto/gallery/rectangular.qmd` | 148-195 | Wide/Square/Tall with `geom_puzzle_rect()` |
| `quarto/gallery/voronoi.qmd` | 37-84 | Wide/Square/Tall with `geom_puzzle_voronoi()` |
| `quarto/gallery/random.qmd` | 132-179 | Wide/Square/Tall with `geom_puzzle_random()` |

### What's Identical (100% structural match)
- Same 3 aspect ratios demonstrated: Wide (16:9), Square (1:1), Tall (9:16)
- Same layout: `layout-ncol: 3` with labeled figures
- Same technique: `coord_fixed()` to preserve aspect ratio
- Same explanatory concept: "X puzzles handle any aspect ratio"

### What Differs (Type-specific)
- Function name: `geom_puzzle_rect/voronoi/random()`
- Parameters: `cols/rows` vs `n_cells` vs `n_pieces`
- Seeds: rectangular/voronoi use 123, random uses 42
- Palettes: cividis, plasma, magma

### Missing Coverage
- `hexagonal.qmd` - No aspect ratio section
- `concentric.qmd` - No aspect ratio section (circular, so N/A)

## Proposed Solution

### Option A: Consolidate into Tutorials Section (RECOMMENDED)

Create `quarto/tutorials/aspect-ratios.qmd` with:
1. Overview explaining aspect ratio flexibility
2. Multi-type comparison showing all puzzle types
3. Technical guidance on calculating aspect ratios
4. Cross-references back to gallery pages

Then update gallery files with brief cross-references.

### Option B: Keep in Gallery, Standardize

Keep sections in gallery files but:
1. Standardize seeds, palettes
2. Add cross-references between files
3. Add missing section to hexagonal.qmd

### Option C: Integrate with Size Parameter Docs

Merge aspect ratio discussion into `quarto/api/generate-puzzle.qmd` under size parameter section.

## Recommended Approach: Option A

### Rationale
1. **Single source of truth** - One place to update
2. **DRY principle** - Don't Repeat Yourself
3. **Better UX** - Users learn concept once, see type-specific examples
4. **Easier maintenance** - Future changes in one file

## Implementation Steps

### Phase 1: Create Consolidated Guide

**File**: `quarto/tutorials/aspect-ratios.qmd`

```yaml
---
title: "Working with Aspect Ratios"
description: "How jigsawR puzzles adapt to any canvas shape"
---
```

**Content Structure**:
1. Introduction - What aspect ratio means for puzzles
2. Quick reference - Common ratios (16:9, 4:3, 1:1, 3:4, 9:16)
3. Multi-type comparison - One grid showing rect/hex/voronoi/random
4. Technical notes - How `coord_fixed()` works with ggpuzzle

### Phase 2: Update Gallery Files

For `rectangular.qmd`, `voronoi.qmd`, `random.qmd`:
- Remove full aspect ratio sections
- Add brief cross-reference callout box

```markdown
::: {.callout-tip}
## Aspect Ratio Flexibility
Rectangular puzzles adapt to any aspect ratio. See the
[Aspect Ratios Guide](../tutorials/aspect-ratios.qmd) for examples.
:::
```

### Phase 3: Update Navigation

**File**: `quarto/_quarto.yml`
- Add new tutorial to sidebar navigation

### Phase 4: Verify and Test

- Render full site: `quarto render quarto`
- Check all cross-links work
- Verify removed sections don't break page flow

## Files to Modify

| File | Action |
|------|--------|
| `quarto/tutorials/aspect-ratios.qmd` | CREATE - New consolidated guide |
| `quarto/gallery/rectangular.qmd` | EDIT - Remove lines 148-195, add callout |
| `quarto/gallery/voronoi.qmd` | EDIT - Remove lines 37-84, add callout |
| `quarto/gallery/random.qmd` | EDIT - Remove lines 132-179, add callout |
| `quarto/_quarto.yml` | EDIT - Add to navigation |

## Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Broken cross-references | Low | Medium | Test all links after changes |
| User confusion from removed content | Low | Low | Clear callout directing to new location |
| Gallery pages feel incomplete | Low | Medium | Callout makes it clear concept is covered elsewhere |

## Success Criteria

1. Single `aspect-ratios.qmd` file contains all aspect ratio documentation
2. Gallery files reference the consolidated guide
3. No duplicate content across files
4. All cross-links functional
5. Quarto site renders without errors

## Alternative Consideration

If the team prefers keeping examples in gallery files, Option B (standardize) would:
- Keep sections in each gallery file
- Ensure consistency (same seeds, similar structure)
- Add cross-references between files
- Add hexagonal section for completeness

This is less disruptive but doesn't solve the DRY problem.
