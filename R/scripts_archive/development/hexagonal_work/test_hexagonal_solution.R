#!/usr/bin/env Rscript
# Test Script for Hexagonal Puzzle Separation and Piece Boundary Solution
# Validates that both individual piece extraction and separation work correctly

cat(paste(rep("=", 60), collapse=""), "\n")
cat("TESTING HEXAGONAL PUZZLE SOLUTION\n")
cat("Testing individual piece extraction and separation integration\n")
cat(paste(rep("=", 60), collapse=""), "\n")

# Load all necessary functions
if (!require("devtools", quietly = TRUE)) {
  install.packages("devtools")
  library(devtools)
}

# Load package functions
load_all()

# Test parameters
test_cases <- list(
  list(rings = 2, seed = 1234, name = "2-ring basic"),
  list(rings = 3, seed = 5678, name = "3-ring standard"),
  list(rings = 2, seed = 9999, name = "2-ring circular", do_warp = TRUE, do_trunc = TRUE)
)

test_results <- list()

for (i in seq_along(test_cases)) {
  test_case <- test_cases[[i]]
  
  cat(sprintf("\n--- Test %d: %s ---\n", i, test_case$name))
  cat(sprintf("Parameters: rings=%d, seed=%d\n", test_case$rings, test_case$seed))
  
  # Set defaults
  do_warp <- if (is.null(test_case$do_warp)) FALSE else test_case$do_warp
  do_trunc <- if (is.null(test_case$do_trunc)) FALSE else test_case$do_trunc
  
  # Test 1: Individual piece extraction
  cat("\n1. Testing individual piece extraction...\n")
  
  tryCatch({
    individual_result <- generate_hexagonal_individual_pieces(
      rings = test_case$rings,
      seed = test_case$seed,
      diameter = 100,  # Smaller for testing
      do_warp = do_warp,
      do_trunc = do_trunc,
      save_files = FALSE
    )
    
    num_pieces <- length(individual_result$pieces)
    expected_pieces <- 3 * test_case$rings * (test_case$rings - 1) + 1
    
    cat(sprintf("   Generated %d pieces (expected %d)\n", num_pieces, expected_pieces))
    
    # Check piece validity
    valid_pieces <- 0
    for (piece in individual_result$pieces) {
      if (!is.null(piece$path) && nchar(piece$path) > 10) {
        valid_pieces <- valid_pieces + 1
      }
    }
    
    cat(sprintf("   %d pieces have valid paths\n", valid_pieces))
    
    individual_success <- (num_pieces == expected_pieces && valid_pieces > 0)
    
    # Save test result
    filename <- sprintf("output/test_individual_%s_rings%d_seed%d.svg", 
                       gsub(" ", "_", test_case$name), test_case$rings, test_case$seed)
    
    if (!is.null(individual_result$svg_content)) {
      if (!dir.exists("output")) dir.create("output")
      writeLines(individual_result$svg_content, filename)
      cat(sprintf("   Saved: %s\n", filename))
    }
    
  }, error = function(e) {
    cat(sprintf("   ERROR in individual pieces: %s\n", e$message))
    individual_success <- FALSE
  })
  
  # Test 2: Separation with real pieces
  cat("\n2. Testing separation with real piece shapes...\n")
  
  tryCatch({
    separated_svg <- generate_separated_hexagonal_svg(
      rings = test_case$rings,
      seed = test_case$seed,
      diameter = 100,
      offset = 15,  # Good separation for testing
      arrangement = "rectangular",
      do_warp = do_warp,
      do_trunc = do_trunc,
      colors = c("#FF6B6B", "#4ECDC4", "#45B7D1", "#FFA07A"),
      stroke_width = 1
    )
    
    separation_success <- (!is.null(separated_svg) && nchar(separated_svg) > 100)
    cat(sprintf("   Generated separated SVG (%d characters)\n", nchar(separated_svg)))
    
    # Save separated result
    sep_filename <- sprintf("output/test_separated_%s_rings%d_seed%d.svg", 
                           gsub(" ", "_", test_case$name), test_case$rings, test_case$seed)
    
    writeLines(separated_svg, sep_filename)
    cat(sprintf("   Saved: %s\n", sep_filename))
    
  }, error = function(e) {
    cat(sprintf("   ERROR in separation: %s\n", e$message))
    separation_success <- FALSE
  })
  
  # Test 3: Validate paths are different (not all identical)
  cat("\n3. Testing piece uniqueness...\n")
  
  if (exists("individual_result") && !is.null(individual_result$pieces)) {
    unique_paths <- length(unique(sapply(individual_result$pieces, function(p) p$path)))
    total_pieces <- length(individual_result$pieces)
    
    uniqueness_ratio <- unique_paths / total_pieces
    cat(sprintf("   %d unique paths out of %d pieces (ratio: %.2f)\n", 
                unique_paths, total_pieces, uniqueness_ratio))
    
    uniqueness_success <- (uniqueness_ratio > 0.5)  # At least 50% should be unique
  } else {
    uniqueness_success <- FALSE
    cat("   Could not test uniqueness - no pieces available\n")
  }
  
  # Store results
  test_results[[i]] <- list(
    name = test_case$name,
    individual_success = individual_success,
    separation_success = separation_success,
    uniqueness_success = uniqueness_success,
    overall_success = individual_success && separation_success && uniqueness_success
  )
  
  cat(sprintf("\n   Result: %s\n", 
              if(test_results[[i]]$overall_success) "✓ PASS" else "✗ FAIL"))
}

# Summary
cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("TEST SUMMARY\n")
cat(paste(rep("=", 60), collapse=""), "\n")

overall_pass <- 0
for (i in seq_along(test_results)) {
  result <- test_results[[i]]
  status <- if(result$overall_success) "✓ PASS" else "✗ FAIL"
  
  cat(sprintf("Test %d (%s): %s\n", i, result$name, status))
  cat(sprintf("  - Individual pieces: %s\n", if(result$individual_success) "✓" else "✗"))
  cat(sprintf("  - Separation: %s\n", if(result$separation_success) "✓" else "✗"))  
  cat(sprintf("  - Uniqueness: %s\n", if(result$uniqueness_success) "✓" else "✗"))
  
  if (result$overall_success) overall_pass <- overall_pass + 1
}

cat(sprintf("\nOverall: %d/%d tests passed\n", overall_pass, length(test_results)))

if (overall_pass == length(test_results)) {
  cat("\n🎉 ALL TESTS PASSED! The hexagonal puzzle solution is working correctly.\n")
} else {
  cat(sprintf("\n⚠️  %d tests failed. Check the error messages above for details.\n", 
              length(test_results) - overall_pass))
}

cat("\nGenerated test files in output/ directory:\n")
test_files <- list.files("output", pattern = "test_.*\\.svg$", full.names = FALSE)
for (file in test_files) {
  cat(sprintf("  - %s\n", file))
}

cat("\n", paste(rep("=", 60), collapse=""), "\n")