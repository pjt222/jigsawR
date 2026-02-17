# PILES: Puzzle Input Line Entry System
#
# A SMILES-inspired notation for specifying puzzle piece fusion groups.
# Inspired by chemistry's SMILES notation for molecular structure.
#
# PILES Syntax Reference:
# =======================
#
# Basic Elements:
# - Numbers represent piece IDs (1, 2, 3, ...)
# - Hyphen (-) represents adjacency/fusion bond between pieces
# - Comma (,) separates independent fusion groups
# - Period (.) separates disconnected structures (same as comma)
#
# Branching:
# - Parentheses () define branches from a junction piece
# - Example: 1-2(-3)-4 means piece 2 connects to 1, 3, and 4
#
# Ring Closures:
# - Numbers after @ indicate ring closure points
# - Example: 1-2-3-4@1 forms a ring (piece 4 connects back to piece 1)
#
# Direction Specifiers (optional, for validation):
# - Square brackets with direction: [N], [E], [S], [W] for rectangular
# - [0]-[5] for hexagonal sides
# - [I], [R], [O], [L] for concentric (Inner, Right, Outer, Left)
#
# Wildcards and Patterns:
# - Range notation: 1:5 means pieces 1, 2, 3, 4, 5
# - Row/column for rectangular: R1 (row 1), C2 (column 2)
#
# Keywords (require puzzle_result):
# - "all"                   -> All pieces in the puzzle
# - "center"                -> Center piece (hexagonal/concentric)
# - "ring1", "ring2"        -> All pieces in ring N
# - "R1", "R2", "row1"      -> Row N (rectangular)
# - "C1", "C2", "col1"      -> Column N (rectangular)
# - "boundary", "border"    -> All boundary pieces
# - "inner"                 -> All non-boundary pieces
#
# Exclusion Syntax (require puzzle_result):
# - "ALL-N"                 -> All pieces EXCEPT piece N
# - "ALL-N-M"               -> All pieces EXCEPT pieces N and M
# - "!N" or "!N!M"          -> All pieces EXCEPT N (and M)
# - "-N" or "-N-M"          -> All pieces EXCEPT N (and M) [standalone]
#
# Examples:
# ---------
# "1-2"                    -> Fuse pieces 1 and 2
# "1-2-3"                  -> Linear chain: 1 fused to 2 fused to 3
# "1-2,3-4"                -> Two separate groups: (1,2) and (3,4)
# "1-2(-3)-4"              -> Branched: 2 connects to 1, 3, and 4
# "1-2-3-4@1"              -> Ring: 4 connects back to 1
# "1:6"                    -> Range: pieces 1 through 6 in a line
# "1-2[E]-3"               -> 1 connects to 2's East, 2 connects to 3
# "center"                 -> Special: center piece (hexagonal)
# "ring1"                  -> Special: all pieces in ring 1 (hex/concentric)
# "R1"                     -> Row 1 (rectangular)
# "C2-C3"                  -> Fuse columns 2 and 3 (rectangular)
# "all"                    -> Fuse ALL pieces into one meta-piece
# "ALL-1"                  -> Fuse all EXCEPT center piece
# "!1!7"                   -> Fuse all EXCEPT pieces 1 and 7
#
# Sources:
# - SMILES notation: https://en.wikipedia.org/wiki/Simplified_Molecular_Input_Line_Entry_System
# - OpenSMILES: http://opensmiles.org/opensmiles.html

# =============================================================================
# PILES PARSER
# =============================================================================

#' Parse PILES notation into fusion groups
#'
#' Converts PILES (Puzzle Input Line Entry System) notation into a list of
#' fusion groups. PILES is inspired by chemistry's SMILES notation.
#'
#' @param piles Character string in PILES notation
#' @param puzzle_result Optional puzzle result for validation and special keywords
#' @param validate If TRUE, validate that all pieces are adjacent (default TRUE)
#'
#' @return List of integer vectors, each representing a fusion group
#'
#' @details
#' PILES Syntax:
#' \itemize{
#'   \item Numbers: Piece IDs (e.g., "1", "2", "15")
#'   \item Hyphen (-): Fusion bond between pieces (e.g., "1-2")
#'   \item Comma (,) or Period (.): Separate groups (e.g., "1-2,3-4")
#'   \item Parentheses: Branching (e.g., "1-2(-3)-4" = 2 bonds to 1, 3, 4)
#'   \item @n: Ring closure (e.g., "1-2-3-4@1" = 4 connects to 1)
#'   \item n:m: Range (e.g., "1:5" = pieces 1-5)
#'   \item Special keywords: "center", "ring1", "R1", "C2", etc.
#' }
#'
#' @examples
#' # Simple pair
#' parse_piles("1-2")
#'
#' # Linear chain
#' parse_piles("1-2-3-4")
#'
#' # Multiple groups
#' parse_piles("1-2,3-4-5")
#'
#' # Branched structure (piece 2 connects to 1, 3, and 4)
#' parse_piles("1-2(-3)-4")
#'
#' # Ring structure
#' parse_piles("1-2-3-4@1")
#'
#' # Range notation
#' parse_piles("1:6")
#'
#' @export
parse_piles <- function(piles, puzzle_result = NULL, validate = TRUE) {
  if (is.null(piles) || !is.character(piles) || nchar(trimws(piles)) == 0) {
    return(list())
  }

  piles <- trimws(piles)

  # Split by comma or period (group separators)
  group_strings <- strsplit(piles, "[,.]")[[1]]
  group_strings <- trimws(group_strings)
  group_strings <- group_strings[nchar(group_strings) > 0]

  if (length(group_strings) == 0) {
    return(list())
  }

  # Parse each group
  groups <- lapply(group_strings, function(gs) {
    parse_piles_group(gs, puzzle_result)
  })

  # Flatten any nested lists and remove empty

  groups <- groups[lengths(groups) > 0]

  # Validate adjacency if requested

if (validate && !is.null(puzzle_result) && length(groups) > 0) {
    for (i in seq_along(groups)) {
      result <- validate_fusion_group(groups[[i]], puzzle_result)
      if (!result$valid) {
        log_warn("PILES group {i} validation: {result$message}")
      }
    }
  }

  return(groups)
}

#' Parse a single PILES group (internal)
#'
#' @param group_str Single group string (no commas)
#' @param puzzle_result Optional puzzle result for special keywords
#' @return Integer vector of piece IDs
#' @keywords internal
parse_piles_group <- function(group_str, puzzle_result = NULL) {
  # Handle special keywords first
  keyword_result <- parse_piles_keyword(group_str, puzzle_result)
  if (!is.null(keyword_result)) {
    return(keyword_result)
  }

  # Handle negative/exclusion syntax: ALL-N, ALL-N-M, !N, -N
  # These require puzzle_result to determine total piece count
  exclusion_result <- parse_exclusion_syntax(group_str, puzzle_result)
  if (!is.null(exclusion_result)) {
    return(exclusion_result)
  }

  # Handle range notation: "1:5"
  if (grepl("^\\d+:\\d+$", group_str)) {
    parts <- as.integer(strsplit(group_str, ":")[[1]])
    return(seq(parts[1], parts[2]))
  }

  # Handle ring closures: collect @n markers
  ring_closures <- list()
  if (grepl("@", group_str)) {
    # Extract ring closure markers
    matches <- gregexpr("@(\\d+)", group_str, perl = TRUE)
    if (matches[[1]][1] != -1) {
      closure_nums <- regmatches(group_str, matches)[[1]]
      closure_nums <- as.integer(gsub("@", "", closure_nums))
      # Store positions for later processing
      ring_closures <- closure_nums
    }
    # Remove ring closure markers for main parsing
    group_str <- gsub("@\\d+", "", group_str)
  }

  # Handle direction specifiers: remove [N], [E], etc. for now
  # (could be used for validation in future)
  group_str <- gsub("\\[[^]]+\\]", "", group_str)

  # Parse the main structure with branching
  pieces <- parse_piles_structure(group_str, puzzle_result)

  # Apply ring closures (connect last piece to closure points)
  # For simplicity, ring closures just ensure those pieces are included
  pieces <- unique(c(pieces, ring_closures))

  return(as.integer(pieces))
}

#' Parse PILES structure with branching (internal)
#'
#' @param str Structure string potentially with parentheses
#' @param puzzle_result Optional puzzle result
#' @return Integer vector of piece IDs
#' @keywords internal
parse_piles_structure <- function(str, puzzle_result = NULL) {
  pieces <- integer()

  # Tokenize: split into numbers, hyphens, and parenthetical groups
  # Use a state machine approach for nested parentheses

  tokens <- tokenize_piles(str)

  for (token in tokens) {
    if (grepl("^\\d+$", token)) {
      # Simple number
      pieces <- c(pieces, as.integer(token))
    } else if (grepl("^\\(", token)) {
      # Parenthetical group - recursive parse
      inner <- substr(token, 2, nchar(token) - 1)
      branch_pieces <- parse_piles_structure(inner, puzzle_result)
      pieces <- c(pieces, branch_pieces)
    } else if (grepl("^\\d+:\\d+$", token)) {
      # Range within structure
      parts <- as.integer(strsplit(token, ":")[[1]])
      pieces <- c(pieces, seq(parts[1], parts[2]))
    } else if (grepl("^[a-zA-Z]", token)) {
      # Keyword token - parse using keyword handler
      keyword_pieces <- parse_piles_keyword(token, puzzle_result)
      if (!is.null(keyword_pieces) && length(keyword_pieces) > 0) {
        pieces <- c(pieces, keyword_pieces)
      }
    }
    # Ignore hyphens (just separators)
  }

  return(unique(pieces))
}

#' Tokenize PILES string (internal)
#'
#' @param str PILES string
#' @return Character vector of tokens
#' @keywords internal
tokenize_piles <- function(str) {
  tokens <- character()
  i <- 1
  n <- nchar(str)

  while (i <= n) {
    char <- substr(str, i, i)

    if (char == "(") {
      # Find matching closing paren
      depth <- 1
      j <- i + 1
      while (j <= n && depth > 0) {
        c <- substr(str, j, j)
        if (c == "(") depth <- depth + 1
        else if (c == ")") depth <- depth - 1
        j <- j + 1
      }
      tokens <- c(tokens, substr(str, i, j - 1))
      i <- j
    } else if (char == "-") {
      # Skip hyphen (separator)
      i <- i + 1
    } else if (grepl("[0-9]", char)) {
      # Number (possibly multi-digit or range)
      j <- i
      while (j <= n && grepl("[0-9:]", substr(str, j, j))) {
        j <- j + 1
      }
      tokens <- c(tokens, substr(str, i, j - 1))
      i <- j
    } else if (grepl("[a-zA-Z]", char)) {
      # Keyword (alphabetic characters followed by alphanumeric)
      j <- i
      while (j <= n && grepl("[a-zA-Z0-9]", substr(str, j, j))) {
        j <- j + 1
      }
      tokens <- c(tokens, substr(str, i, j - 1))
      i <- j
    } else {
      # Skip other characters (spaces, commas handled elsewhere)
      i <- i + 1
    }
  }

  return(tokens)
}

#' Parse PILES special keywords (internal)
#'
#' @param keyword Keyword string
#' @param puzzle_result Puzzle result (required for some keywords)
#' @return Integer vector of piece IDs, or NULL if not a keyword
#' @keywords internal
parse_piles_keyword <- function(keyword, puzzle_result = NULL) {
  keyword <- tolower(trimws(keyword))

  # "center" - center piece of hexagonal puzzle
  if (keyword == "center") {
    return(1L)  # Center is always piece 1
  }

  # "ring<n>" - all pieces in ring n
  if (grepl("^ring\\d+$", keyword)) {
    ring_num <- as.integer(gsub("ring", "", keyword))
    return(get_ring_pieces(ring_num, puzzle_result))
  }

  # "R<n>" or "row<n>" - row n of rectangular puzzle
  if (grepl("^r\\d+$", keyword) || grepl("^row\\d+$", keyword)) {
    row_num <- as.integer(gsub("^r(ow)?", "", keyword))
    return(get_row_pieces(row_num, puzzle_result))
  }

  # "C<n>" or "col<n>" - column n of rectangular puzzle
  if (grepl("^c\\d+$", keyword) || grepl("^col\\d+$", keyword)) {
    col_num <- as.integer(gsub("^c(ol)?", "", keyword))
    return(get_col_pieces(col_num, puzzle_result))
  }

  # "all" - all pieces
  if (keyword == "all") {
    if (is.null(puzzle_result)) return(NULL)
    return(seq_len(length(puzzle_result$pieces)))
  }

  # "boundary" or "border" - all boundary pieces
  if (keyword %in% c("boundary", "border", "edge")) {
    return(get_boundary_pieces(puzzle_result))
  }

  # "inner" - all non-boundary pieces
  if (keyword == "inner") {
    return(get_inner_pieces(puzzle_result))
  }

  return(NULL)  # Not a keyword
}

#' Parse exclusion/negative syntax (internal)
#'
#' Handles various exclusion syntaxes:
#' - "ALL-N" or "ALL-N-M" - all pieces except N (and M)
#' - "!N" or "!N!M" - all pieces except N (and M)
#' - "-N" at start of group - all pieces except N (standalone negation)
#'
#' @param group_str Group string potentially containing exclusion syntax
#' @param puzzle_result Puzzle result (required for total piece count)
#' @return Integer vector of included piece IDs, or NULL if not exclusion syntax
#' @keywords internal
parse_exclusion_syntax <- function(group_str, puzzle_result = NULL) {
  str <- trimws(group_str)
  str_lower <- tolower(str)

  # Pattern 1: "ALL-N-M..." - ALL with exclusions
  if (grepl("^all-", str_lower)) {
    if (is.null(puzzle_result)) {
      log_warn("Cannot resolve 'ALL-...' without puzzle_result")
      return(NULL)
    }
    total_pieces <- length(puzzle_result$pieces)
    # Extract exclusions after "ALL-"
    excl_part <- sub("^all-", "", str_lower)
    # Split by hyphen to get individual exclusions
    excl_nums <- as.integer(strsplit(excl_part, "-")[[1]])
    excl_nums <- excl_nums[!is.na(excl_nums)]
    if (length(excl_nums) == 0) {
      return(seq_len(total_pieces))  # ALL with no valid exclusions
    }
    # Return all pieces except excluded ones
    return(setdiff(seq_len(total_pieces), excl_nums))
  }

  # Pattern 2: "!N" or "!N!M!..." - exclamation mark syntax
  if (grepl("^!", str)) {
    if (is.null(puzzle_result)) {
      log_warn("Cannot resolve '!...' exclusion without puzzle_result")
      return(NULL)
    }
    total_pieces <- length(puzzle_result$pieces)
    # Extract all numbers after ! marks
    excl_matches <- gregexpr("!(\\d+)", str)
    if (excl_matches[[1]][1] == -1) {
      return(NULL)  # No valid exclusions found
    }
    excl_strs <- regmatches(str, excl_matches)[[1]]
    excl_nums <- as.integer(gsub("!", "", excl_strs))
    # Return all pieces except excluded ones
    return(setdiff(seq_len(total_pieces), excl_nums))
  }

  # Pattern 3: Standalone "-N" (only if it's just negation, not a chain like "1-2")
  # This pattern is tricky - we only match if it starts with - and only has numbers
  if (grepl("^-\\d+$", str) || grepl("^-\\d+(-\\d+)*$", str)) {
    # Check if this looks like just negations (e.g., "-1-7" means exclude 1 and 7)
    # vs a chain like "1-2-3" which is handled elsewhere
    if (is.null(puzzle_result)) {
      log_warn("Cannot resolve '-N' exclusion without puzzle_result")
      return(NULL)
    }
    total_pieces <- length(puzzle_result$pieces)
    # Extract all numbers (the pieces to exclude)
    excl_nums <- as.integer(strsplit(gsub("^-", "", str), "-")[[1]])
    excl_nums <- excl_nums[!is.na(excl_nums)]
    if (length(excl_nums) == 0) {
      return(NULL)
    }
    # Return all pieces except excluded ones
    return(setdiff(seq_len(total_pieces), excl_nums))
  }

  return(NULL)  # Not exclusion syntax
}

#' Get pieces in a specific ring (internal)
#'
#' @param ring_num Ring number (0 = center, 1 = first ring, etc.)
#' @param puzzle_result Puzzle result
#' @return Integer vector of piece IDs
#' @keywords internal
get_ring_pieces <- function(ring_num, puzzle_result = NULL) {
  if (is.null(puzzle_result)) {
    log_warn("Cannot resolve 'ring{ring_num}' without puzzle_result")
    return(integer())
  }

  type <- puzzle_result$type %||% puzzle_result$parameters$type

  if (type == "hexagonal") {
    # Get rings from parameters (might be stored as grid or rings)
    rings <- puzzle_result$parameters$rings %||% puzzle_result$parameters$grid[1]
    if (is.null(rings) || length(rings) == 0) {
      log_warn("Cannot determine rings from hexagonal puzzle")
      return(integer())
    }
    if (ring_num == 0) return(1L)  # Center
    if (ring_num >= rings) return(integer())

    # Calculate piece range for this ring
    # Ring 0: piece 1
    # Ring 1: pieces 2-7 (6 pieces)
    # Ring r: 6*r pieces
    if (ring_num == 0) return(1L)

    start_piece <- 1L + 1L + sum(6L * seq_len(ring_num - 1L))
    if (ring_num == 1) start_piece <- 2L
    end_piece <- start_piece + 6L * ring_num - 1L
    return(seq(start_piece, end_piece))

  } else if (type == "concentric") {
    # Get rings from parameters (might be stored as rings or grid[1])
    rings <- puzzle_result$parameters$rings %||% puzzle_result$parameters$grid[1]
    if (is.null(rings) || length(rings) == 0) {
      log_warn("Cannot determine rings from concentric puzzle")
      return(integer())
    }
    if (ring_num == 0) return(1L)  # Center
    if (ring_num >= rings) return(integer())

    # Concentric has different structure
    # Ring 0: piece 1 (center)
    # Ring r: has varying number of pieces
    pieces_info <- puzzle_result$pieces
    ring_pieces <- integer()
    for (i in seq_along(pieces_info)) {
      piece_ring <- pieces_info[[i]]$ring_pos$ring
      if (!is.null(piece_ring) && piece_ring == ring_num) {
        ring_pieces <- c(ring_pieces, i)
      }
    }
    return(ring_pieces)
  }

  return(integer())
}

#' Get pieces in a specific row (rectangular puzzles) (internal)
#'
#' @param row_num Row number (1-indexed from top)
#' @param puzzle_result Puzzle result
#' @return Integer vector of piece IDs
#' @keywords internal
get_row_pieces <- function(row_num, puzzle_result = NULL) {

  if (is.null(puzzle_result)) {
    log_warn("Cannot resolve row {row_num} without puzzle_result")
    return(integer())
  }

  type <- puzzle_result$type %||% puzzle_result$parameters$type
  if (type != "rectangular") {
    log_warn("Row notation only valid for rectangular puzzles")
    return(integer())
  }

  # grid = c(rows, cols) per API documentation
  n_rows <- puzzle_result$parameters$grid[1]
  n_cols <- puzzle_result$parameters$grid[2]

  if (row_num < 1 || row_num > n_rows) return(integer())

  # Row numbering: row 1 is y=0 (top)
  # Pieces are numbered left-to-right, top-to-bottom: 1,2,3 / 4,5,6 / ...
  y <- row_num - 1
  pieces <- (y * n_cols + 1):(y * n_cols + n_cols)
  return(as.integer(pieces))
}

#' Get pieces in a specific column (rectangular puzzles) (internal)
#'
#' @param col_num Column number (1-indexed from left)
#' @param puzzle_result Puzzle result
#' @return Integer vector of piece IDs
#' @keywords internal
get_col_pieces <- function(col_num, puzzle_result = NULL) {

  if (is.null(puzzle_result)) {
    log_warn("Cannot resolve column {col_num} without puzzle_result")
    return(integer())
  }

  type <- puzzle_result$type %||% puzzle_result$parameters$type
  if (type != "rectangular") {
    log_warn("Column notation only valid for rectangular puzzles")
    return(integer())
  }

  # grid = c(rows, cols) per API documentation
  n_rows <- puzzle_result$parameters$grid[1]
  n_cols <- puzzle_result$parameters$grid[2]

  if (col_num < 1 || col_num > n_cols) return(integer())

  # Column numbering: col 1 is x=0 (left)
  # Pieces are numbered left-to-right, top-to-bottom: 1,2,3 / 4,5,6 / ...
  x <- col_num - 1
  pieces <- x + 1 + (0:(n_rows - 1)) * n_cols
  return(as.integer(pieces))
}

#' Get boundary pieces (internal)
#'
#' @param puzzle_result Puzzle result
#' @return Integer vector of boundary piece IDs
#' @keywords internal
get_boundary_pieces <- function(puzzle_result = NULL) {
  if (is.null(puzzle_result)) {
    return(integer())
  }

  type <- puzzle_result$type %||% puzzle_result$parameters$type
  num_pieces <- length(puzzle_result$pieces)

  boundary <- integer()
  for (id in seq_len(num_pieces)) {
    neighbors <- get_piece_neighbors(id, puzzle_result, include_boundary = TRUE)
    if (any(neighbors$is_boundary)) {
      boundary <- c(boundary, id)
    }
  }
  return(boundary)
}

#' Get inner (non-boundary) pieces (internal)
#'
#' @param puzzle_result Puzzle result
#' @return Integer vector of inner piece IDs
#' @keywords internal
get_inner_pieces <- function(puzzle_result = NULL) {
  if (is.null(puzzle_result)) {
    return(integer())
  }

  num_pieces <- length(puzzle_result$pieces)
  boundary <- get_boundary_pieces(puzzle_result)
  inner <- setdiff(seq_len(num_pieces), boundary)
  return(as.integer(inner))
}

# =============================================================================
# PILES SERIALIZATION (fusion groups to PILES string)
# =============================================================================

#' Convert fusion groups to PILES notation
#'
#' Serializes a list of fusion groups back to PILES string format.
#'
#' @param fusion_groups List of integer vectors
#' @param puzzle_result Optional puzzle result for structure-aware serialization
#' @param compact If TRUE, use range notation where possible
#'
#' @return Character string in PILES notation
#'
#' @examples
#' to_piles(list(c(1, 2), c(3, 4, 5)))
#' # Returns: "1-2,3-4-5"
#'
#' to_piles(list(1:6))
#' # Returns: "1:6" (with compact=TRUE)
#'
#' @export
to_piles <- function(fusion_groups, puzzle_result = NULL, compact = TRUE) {
  if (is.null(fusion_groups) || length(fusion_groups) == 0) {
    return("")
  }

  group_strings <- vapply(fusion_groups, function(group) {
    group <- sort(unique(as.integer(group)))

    if (length(group) == 0) return("")
    if (length(group) == 1) return(as.character(group))

    # Check if it's a consecutive range
    if (compact && length(group) > 2) {
      if (all(diff(group) == 1)) {
        return(sprintf("%d:%d", group[1], group[length(group)]))
      }
    }

    # Default: hyphen-separated
    return(paste(group, collapse = "-"))
  }, character(1))

  group_strings <- group_strings[nchar(group_strings) > 0]
  return(paste(group_strings, collapse = ","))
}

# =============================================================================
# PILES VALIDATION
# =============================================================================

#' Validate PILES notation syntax
#'
#' Checks if a PILES string has valid syntax without evaluating special keywords.
#'
#' @param piles Character string in PILES notation
#'
#' @return List with:
#'   \describe{
#'     \item{valid}{TRUE if syntax is valid}
#'     \item{message}{Error message if invalid}
#'     \item{warnings}{Vector of warning messages}
#'   }
#'
#' @examples
#' validate_piles_syntax("1-2-3,4-5")
#' validate_piles_syntax("1:6")
#' validate_piles_syntax("1-2(-3)-4")
#' validate_piles_syntax("((unbalanced")
#'
#' @export
validate_piles_syntax <- function(piles) {
  if (is.null(piles) || !is.character(piles)) {
    return(list(valid = FALSE, message = "PILES must be a character string"))
  }

  piles <- trimws(piles)
  if (nchar(piles) == 0) {
    return(list(valid = TRUE, message = "Empty PILES string"))
  }

  warnings <- character()

  # Check for balanced parentheses
  open_parens <- sum(strsplit(piles, "")[[1]] == "(")
  close_parens <- sum(strsplit(piles, "")[[1]] == ")")
  if (open_parens != close_parens) {
    return(list(
      valid = FALSE,
      message = sprintf("Unbalanced parentheses: %d open, %d close", open_parens, close_parens)
    ))
  }

  # Check for invalid characters
  # Define valid characters explicitly to avoid regex escaping issues
  valid_chars <- c(
    as.character(0:9),
    letters,
    LETTERS,
    "-", ",", ".", ":", "(", ")", "@", "[", "]", " "
  )
  chars <- strsplit(piles, "")[[1]]
  invalid <- chars[!chars %in% valid_chars]
  if (length(invalid) > 0) {
    return(list(
      valid = FALSE,
      message = sprintf("Invalid characters: %s", paste(unique(invalid), collapse = ", "))
    ))
  }

  # Check for empty groups
  if (grepl(",,|\\.\\.|-{2,}", piles)) {
    warnings <- c(warnings, "Empty groups detected (consecutive separators)")
  }

  # Check for dangling operators
  if (grepl("^-|-$", gsub("\\([^)]*\\)", "", piles))) {
    warnings <- c(warnings, "Dangling hyphen at start or end")
  }

  return(list(
    valid = TRUE,
    message = "Valid PILES syntax",
    warnings = warnings
  ))
}

# =============================================================================
# INTEGRATION WITH EXISTING API
# =============================================================================

#' Enhanced fusion input parser with PILES support
#'
#' Extended version of parse_fusion_input that also accepts PILES notation.
#' Automatically detects format based on input.
#'
#' @param input Fusion input in any format:
#'   \itemize{
#'     \item PILES string: "1-2-3,4-5"
#'     \item Legacy string: "(1,2),(3,4)"
#'     \item List of vectors: list(c(1,2), c(3,4))
#'   }
#' @param puzzle_result Optional puzzle result for validation
#' @param auto_merge If TRUE, merge overlapping groups
#'
#' @return List of integer vectors (fusion groups)
#'
#' @examples
#' # PILES notation
#' parse_fusion("1-2-3,4-5")
#'
#' # Legacy parenthesized format
#' parse_fusion("(1,2),(3,4)")
#'
#' # List input (pass-through)
#' parse_fusion(list(c(1, 2), c(3, 4)))
#'
#' @export
parse_fusion <- function(input, puzzle_result = NULL, auto_merge = TRUE) {
  if (is.null(input) || length(input) == 0) {
    return(list())
  }

  # If already a list, use existing parser
  if (is.list(input)) {
    return(parse_fusion_input(input, puzzle_result, auto_merge))
  }

  # If string, detect format
  if (is.character(input)) {
    input <- trimws(input)
    if (nchar(input) == 0) return(list())

    # Detect PILES vs legacy format
    # Legacy format: starts with "(" or contains "(n,n)"
    # PILES format: contains hyphens or keywords like "ring", "R1", etc.

    is_legacy <- grepl("^\\(\\d+,\\d+", input) ||
                 (grepl("\\(", input) && !grepl("-", input))

    if (is_legacy) {
      result <- parse_fusion_input(input, puzzle_result, auto_merge)
    } else {
      result <- parse_piles(input, puzzle_result, validate = !is.null(puzzle_result))
    }

    # Apply auto-merge if requested
    if (auto_merge && length(result) > 0) {
      result <- merge_fusion_groups(result)
    }

    return(result)
  }

  stop("Fusion input must be a string or list")
}
