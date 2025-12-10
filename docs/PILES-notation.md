# PILES: Puzzle Input Line Entry System

A SMILES-inspired notation for specifying puzzle piece fusion groups in jigsawR.

## Overview

PILES (Puzzle Input Line Entry System) is a domain-specific notation for describing which puzzle pieces should be fused together. It is inspired by chemistry's [SMILES notation](https://en.wikipedia.org/wiki/Simplified_Molecular_Input_Line_Entry_System) for representing molecular structures.

Just as SMILES uses simple ASCII strings to represent complex molecular bonds, PILES uses simple strings to represent puzzle piece connections.

## Quick Reference

| Syntax | Meaning | Example |
|--------|---------|---------|
| `n` | Piece ID | `1`, `15`, `42` |
| `-` | Fusion bond | `1-2` (fuse pieces 1 and 2) |
| `,` | Separate groups | `1-2,3-4` (two independent groups) |
| `.` | Separate groups (alt) | `1-2.3-4` (same as comma) |
| `n:m` | Range | `1:6` (pieces 1 through 6) |
| `()` | Branching | `1-2(-3)-4` (2 bonds to 1, 3, and 4) |
| `@n` | Ring closure | `1-2-3-4@1` (4 connects back to 1) |
| `[D]` | Direction hint | `1-2[E]-3` (optional validation) |
| `ALL-n` | Exclusion | `ALL-5` (all except piece 5) |
| `!n` | Exclusion (alt) | `!5` (same as ALL-5) |

## Basic Syntax

### Piece IDs
Numbers represent piece IDs (1-indexed):
```
1       # Piece 1 alone
15      # Piece 15 alone
```

### Fusion Bonds (-)
Hyphens indicate pieces that should be fused together:
```
1-2         # Fuse pieces 1 and 2
1-2-3       # Linear chain: 1 + 2 + 3 fused together
1-2-3-4-5   # Longer chain
```

### Multiple Groups (,)
Commas separate independent fusion groups:
```
1-2,3-4         # Two groups: (1,2) and (3,4)
1-2-3,4-5,6-7   # Three groups
```

### Range Notation (:)
Colons specify consecutive piece ranges:
```
1:6         # Pieces 1, 2, 3, 4, 5, 6 fused together
10:15       # Pieces 10 through 15
```

## Advanced Syntax

### Branching
Parentheses define branches from a junction piece:
```
1-2(-3)-4   # Piece 2 connects to 1, 3, AND 4
            # Forms a T-junction

1-2(-3)(-4)-5   # Piece 2 connects to 1, 3, 4, AND 5
                # Multiple branches from piece 2
```

### Ring Closures
The `@` symbol with a piece number indicates a ring closure (connecting back to form a cycle):
```
1-2-3-4@1       # Piece 4 connects back to piece 1
                # Forms a closed ring: 1-2-3-4-1

1-2-3-4-5-6@1   # Hexagonal ring closure
```

### Direction Specifiers (Optional)
Square brackets can specify connection directions for validation:
```
# Rectangular puzzles
1-2[E]-3        # 2's East edge connects to 3
1[S]-5          # 1's South edge connects to 5
# Directions: [N]orth, [E]ast, [S]outh, [W]est

# Hexagonal puzzles
1-2[0]-3        # Side 0 connection
# Sides: [0] through [5]

# Concentric puzzles
1-2[O]-3        # Outer edge connection
# Directions: [I]nner, [R]ight, [O]uter, [L]eft
```

## Special Keywords

PILES supports special keywords that expand to groups of pieces based on puzzle structure:

### Universal Keywords
| Keyword | Meaning |
|---------|---------|
| `all` | All pieces in the puzzle |
| `boundary` | All boundary/edge pieces |
| `border` | Same as `boundary` |
| `edge` | Same as `boundary` |
| `inner` | All non-boundary pieces |
| `center` | Center piece (hexagonal/concentric) |

### Hexagonal & Concentric Keywords
| Keyword | Meaning |
|---------|---------|
| `ring0` | Center piece (same as `center`) |
| `ring1` | All pieces in ring 1 |
| `ring2` | All pieces in ring 2 |
| `ringN` | All pieces in ring N |

### Rectangular Keywords
| Keyword | Meaning |
|---------|---------|
| `R1` or `row1` | All pieces in row 1 (top) |
| `R2` or `row2` | All pieces in row 2 |
| `C1` or `col1` | All pieces in column 1 (left) |
| `C2` or `col2` | All pieces in column 2 |

### Keyword Examples
```r
# Fuse all boundary pieces together
parse_piles("boundary", puzzle_result)

# Fuse center with first ring
parse_piles("center-ring1", puzzle_result)

# Fuse entire first row
parse_piles("R1", puzzle_result)

# Fuse columns 1 and 2
parse_piles("C1,C2", puzzle_result)
```

## Exclusion Syntax

PILES supports exclusion patterns to fuse "all pieces except..." This is useful when you want to fuse most pieces but leave a few separate.

### Exclusion Patterns

| Syntax | Meaning | Example |
|--------|---------|---------|
| `ALL-n` | All pieces except n | `ALL-5` (all except piece 5) |
| `ALL-n-m` | All pieces except n and m | `ALL-1-9` (all except 1 and 9) |
| `!n` | All pieces except n | `!5` (same as `ALL-5`) |
| `!n!m` | All pieces except n and m | `!1!7` (all except 1 and 7) |

### Exclusion Examples

```r
# Create a 3x3 puzzle (9 pieces)
puzzle <- generate_puzzle(
  type = "rectangular",
  grid = c(3, 3),
  seed = 42
)

# Fuse all except the center piece (piece 5)
parse_piles("ALL-5", puzzle)
# Returns: list(c(1L, 2L, 3L, 4L, 6L, 7L, 8L, 9L))

# Fuse all except corners (pieces 1, 3, 7, 9)
parse_piles("!1!3!7!9", puzzle)
# Returns: list(c(2L, 4L, 5L, 6L, 8L))

# Direct use with generate_puzzle
result <- generate_puzzle(
  type = "rectangular",
  grid = c(3, 3),
  fusion_groups = "ALL-5",  # Fuse all except center
  save_files = FALSE
)
```

### Combining Exclusion with Other Syntax

Exclusion patterns create single fusion groups. To create multiple groups with exclusions, use comma separation:

```r
# NOT YET SUPPORTED: combining exclusion with regular groups
# "ALL-5,1-2" would need future implementation
```

**Note**: Exclusion syntax requires puzzle context to know the total number of pieces. It cannot be used with `parse_piles()` without providing a `puzzle_result`.

## R Functions

### Core Functions

#### `parse_piles()`
Parse PILES notation into fusion groups:
```r
parse_piles("1-2-3,4-5")
# Returns: list(c(1L, 2L, 3L), c(4L, 5L))

parse_piles("1:6")
# Returns: list(c(1L, 2L, 3L, 4L, 5L, 6L))

# With puzzle context for keywords
puzzle <- generate_puzzle(type = "rectangular", grid = c(3, 2))
parse_piles("R1", puzzle)
# Returns: list(c(1L, 2L, 3L))  # First row
```

#### `to_piles()`
Convert fusion groups back to PILES notation:
```r
to_piles(list(c(1, 2), c(3, 4, 5)))
# Returns: "1-2,3-4-5"

to_piles(list(1:6))
# Returns: "1:6"  # Compact range notation

to_piles(list(c(3, 1, 2)), compact = FALSE)
# Returns: "1-2-3"  # Sorted, hyphen-separated
```

#### `parse_fusion()`
Universal parser that auto-detects format:
```r
# PILES format
parse_fusion("1-2-3,4-5")

# Legacy format (also supported)
parse_fusion("(1,2),(3,4)")

# List format (also supported)
parse_fusion(list(c(1, 2), c(3, 4)))
```

#### `validate_piles_syntax()`
Validate PILES string syntax:
```r
validate_piles_syntax("1-2-3,4-5")
# Returns: list(valid = TRUE, message = "Valid PILES syntax", warnings = character())

validate_piles_syntax("1-2((3")
# Returns: list(valid = FALSE, message = "Unbalanced parentheses: 2 open, 1 close")
```

## Examples

### Rectangular Puzzle Fusions
```r
puzzle <- generate_puzzle(type = "rectangular", grid = c(4, 3), seed = 42)

# Fuse horizontal pairs
parse_piles("1-2,3-4,5-6,7-8,9-10,11-12", puzzle)

# Fuse entire rows
parse_piles("R1,R2,R3", puzzle)

# Create 2x2 meta-pieces
parse_piles("1-2-5-6,3-4-7-8,9-10,11-12", puzzle)
```

### Hexagonal Puzzle Fusions
```r
puzzle <- generate_puzzle(type = "hexagonal", grid = c(3), seed = 42)

# Fuse center with first ring
parse_piles("center-ring1", puzzle)

# Fuse adjacent pairs in outer ring
parse_piles("8-9,10-11,12-13,14-15,16-17,18-19", puzzle)
```

### Concentric Puzzle Fusions
```r
puzzle <- generate_puzzle(type = "concentric", grid = c(3), seed = 42)

# Fuse entire rings
parse_piles("ring0,ring1,ring2", puzzle)

# Fuse center to all neighbors
parse_piles("1-2-3-4-5-6-7", puzzle)
```

## Comparison with SMILES

| Feature | SMILES (Chemistry) | PILES (Puzzles) |
|---------|-------------------|-----------------|
| Atoms/Pieces | C, N, O... | 1, 2, 3... |
| Bonds | `-`, `=`, `#` | `-` only |
| Branching | `()` | `()` |
| Ring closure | Digit pairs | `@n` |
| Groups | `.` | `,` or `.` |

### Key Differences
1. **PILES uses explicit ring closure syntax** (`@n`) instead of SMILES' implicit digit matching
2. **PILES has puzzle-specific keywords** (`ring1`, `R1`, `boundary`, etc.)
3. **PILES only has single bonds** (fusion is binary: connected or not)
4. **PILES uses numeric IDs** instead of element symbols

## Technical Details

### Parsing Order
1. Split by group separators (`,` or `.`)
2. Check for special keywords
3. Handle range notation (`:`)
4. Process ring closures (`@n`)
5. Remove direction specifiers (`[...]`)
6. Parse structure with branching
7. Validate adjacency (optional)

### Auto-Detection
The `parse_fusion()` function auto-detects format:
- **Legacy format**: Starts with `(` or contains `(n,n)` pattern
- **PILES format**: Contains `-` or keywords

### Performance
- Tokenization is O(n) where n is string length
- Keyword expansion may require puzzle structure lookup
- Pre-allocated vectors used for O(n) performance

## References

- [SMILES Notation (Wikipedia)](https://en.wikipedia.org/wiki/Simplified_Molecular_Input_Line_Entry_System)
- [OpenSMILES Specification](http://opensmiles.org/opensmiles.html)
- [jigsawR Package Documentation](../README.md)
