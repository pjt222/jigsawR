# Warp/Trunc Debugging Scripts

Archived debug scripts from the implementation of circular warp (`do_warp`) and edge truncation (`do_trunc`) for separated hexagonal mode.

## Date Archived
2025-12-01

## Related Issues
- #29 - Implement Circular Warp (do_warp) for Separated Hexagonal Mode (CLOSED)
- #30 - Implement Truncate Edges (do_trunc) for Separated Hexagonal Mode (CLOSED)
- #31 - Epic: Circular Warp and Truncate Integration (CLOSED)

## Related Commits
- `f193679` - Feat: Implement circular warp for separated hexagonal mode
- `efcac31` - Fix: Circular warp boundary detection and arc handling
- `a62cedb` - Feat: Implement do_trunc for separated hexagonal mode
- `852cc2e` - Fix: Match do_warp/do_trunc semantics

## Files
- `debug_boundary_angles.R` - Debug boundary vertex angles
- `debug_edge_map.R` - Debug edge map structure
- `debug_trunc_geometry.R` - Debug truncation geometry
- `debug_vertex_warp.R` - Debug vertex warping transformation
- `debug_warp_detailed.R` - Detailed warp formula analysis
- `debug_warp_formula.R` - Step-by-step warp calculation debug
- `quick_warp_check.R` - Quick verification of warp implementation
- `test_hex_trunc_function.R` - Test apply_hex_trunc function
- `test_separated_trunc.R` - Test do_trunc in separated mode

## Note
The canonical test for warp/trunc functionality is now `tests/test_warp_trunc.R` which is committed to the repository.
