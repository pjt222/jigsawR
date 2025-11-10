# Individual Puzzle Piece Extraction - Implementation Plan

## Current State Analysis

### Current SVG Structure
The puzzle currently generates 3 continuous paths:
1. **Horizontal dividers** (`gen_dh()`): All horizontal piece boundaries as one path
2. **Vertical dividers** (`gen_dv()`): All vertical piece boundaries as one path  
3. **Border** (`gen_db()`): Outer puzzle boundary as one path

### Current Grid Logic
- Grid dimensions: `xn` columns × `yn` rows
- Horizontal loop: `yi` from 1 to `yn-1` (between-row boundaries)
- Vertical loop: `xi` from 1 to `xn-1` (between-column boundaries)
- Each line segment has tabs/blanks generated with Bézier curves

## Target State Design

### Individual Piece SVG Structure
```xml
<svg>
  <g id="puzzle-pieces">
    <path id="piece-0-0" d="M ... Z" />  <!-- Top-left corner -->
    <path id="piece-1-0" d="M ... Z" />  <!-- Top edge -->
    <path id="piece-0-1" d="M ... Z" />  <!-- Left edge -->
    <path id="piece-1-1" d="M ... Z" />  <!-- Interior piece -->
    <!-- ... one path per piece ... -->
  </g>
  <path id="border" d="..." />  <!-- Optional outer border -->
</svg>
```

### Piece Identification System
- **Piece coordinates**: `(xi, yi)` where `xi ∈ [0, xn-1]`, `yi ∈ [0, yn-1]`
- **Piece types**:
  - Corner pieces: 4 total (2 straight edges each)
  - Edge pieces: `2*(xn-2) + 2*(yn-2)` total (1 straight edge each)  
  - Interior pieces: `(xn-2)*(yn-2)` total (4 curved edges each)

## Algorithm Design

### Piece Boundary Tracing Algorithm

For each piece at position `(xi, yi)`, trace boundary **clockwise**:

1. **Top edge**: From `(xi, yi)` to `(xi+1, yi)`
   - If `yi == 0`: straight line (puzzle edge)
   - Else: curved line with tab/blank (matches horizontal divider `yi`)

2. **Right edge**: From `(xi+1, yi)` to `(xi+1, yi+1)` 
   - If `xi == xn-1`: straight line (puzzle edge)
   - Else: curved line with tab/blank (matches vertical divider `xi+1`)

3. **Bottom edge**: From `(xi+1, yi+1)` to `(xi, yi+1)`
   - If `yi == yn-1`: straight line (puzzle edge) 
   - Else: curved line with tab/blank (matches horizontal divider `yi+1`, **reversed**)

4. **Left edge**: From `(xi, yi+1)` to `(xi, yi)`
   - If `xi == 0`: straight line (puzzle edge)
   - Else: curved line with tab/blank (matches vertical divider `xi`, **reversed**)

### Tab/Blank Direction Consistency

**Critical insight**: Adjacent pieces must have complementary tab/blank patterns.

If piece A has a tab going right → piece B has a blank going left
If piece A has a blank going right → piece B has a tab going left

**Current tab generation**: 
- Each divider line has consistent tab/blank pattern along its length
- `rbool()` determines tab direction, `flip` variable controls consistency

**For individual pieces**:
- Need to reference the **same** tab/blank data used by current system
- Bottom/left edges must be **reversed** because we're tracing from the opposite direction

## Implementation Phases

### Phase 1: Tab/Blank Data Extraction
- Modify existing `gen_dh()` and `gen_dv()` to store tab/blank metadata
- Create data structure: `tabs[divider_type][line_index][segment_index] = {direction, parameters}`

### Phase 2: Straight Edge Generation  
- Implement functions for puzzle border edges
- Handle corner radius for corner pieces

### Phase 3: Curved Edge Generation
- Extract curved edge generation from existing Bézier functions
- Implement edge reversal for bottom/left edges

### Phase 4: Piece Assembly
- Create `trace_piece_boundary(xi, yi)` function
- Combine 4 edges into closed path with `Z` command

### Phase 5: SVG Generation
- Generate individual `<path>` elements for each piece
- Add piece metadata (ID, coordinates)
- Maintain backward compatibility option

## Technical Challenges

### Challenge 1: Tab/Blank Consistency
**Problem**: Current system generates tabs as it goes. Individual pieces need to reference consistent tab data.

**Solution**: Pre-generate all tab/blank data into lookup tables, then reference during piece tracing.

### Challenge 2: Edge Direction Reversal  
**Problem**: Bottom/left edges trace in opposite direction to original generation.

**Solution**: Implement path reversal functions for Bézier curves.

### Challenge 3: Coordinate System Alignment
**Problem**: Ensuring individual pieces align perfectly when reassembled.

**Solution**: Use exact same coordinate calculations as current system.

### Challenge 4: Performance
**Problem**: Generating `xn*yn` individual paths vs 3 combined paths.

**Solution**: Acceptable trade-off for functionality. Optimize if needed.

## Testing Strategy

### Test Cases
1. **2×2 puzzle**: 4 pieces (4 corners, 0 edges, 0 interior)
2. **3×3 puzzle**: 9 pieces (4 corners, 4 edges, 1 interior)  
3. **4×4 puzzle**: 16 pieces (4 corners, 8 edges, 4 interior)

### Validation Methods
1. **Visual inspection**: Pieces should fit together perfectly
2. **Coordinate validation**: Adjacent edges should be identical
3. **SVG validation**: Well-formed SVG with valid paths
4. **Boundary check**: Total piece area should equal puzzle area

## Implementation Priority

### High Priority (Core functionality)
- [ ] Tab/blank data extraction and storage
- [ ] Piece boundary tracing algorithm  
- [ ] Individual piece path generation
- [ ] Basic SVG output with piece paths

### Medium Priority (Polish)
- [ ] Piece identification and metadata
- [ ] Backward compatibility with combined paths
- [ ] Performance optimization

### Low Priority (Features)  
- [ ] Piece separation/offset for cutting
- [ ] Advanced metadata (neighbors, piece types)
- [ ] Custom piece styling options

## Success Criteria

1. **Functional**: Each puzzle piece exported as separate SVG path
2. **Accurate**: Pieces fit together with perfect alignment  
3. **Compatible**: Maintains existing API and functionality
4. **Testable**: Comprehensive test suite with visual validation
5. **Documented**: Clear usage examples and API documentation

---

This plan provides the foundation for implementing individual piece extraction while maintaining the precision and compatibility of the existing system.