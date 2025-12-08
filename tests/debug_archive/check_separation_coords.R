# Check if separation is being applied to piece paths
devtools::load_all()

result <- generate_puzzle(
  type = "rectangular",
  grid = c(2, 2),
  size = c(200, 200),
  seed = 42,
  offset = 50,
  fusion_groups = list(c(1, 2)),
  save_files = FALSE
)

cat("=== Piece Positions (should be offset by 50mm) ===\n")
for (i in 1:4) {
  piece <- result$pieces[[i]]
  cat(sprintf("Piece %d: position=(%s, %s)\n", 
              i, 
              piece$position$x %||% "NULL",
              piece$position$y %||% "NULL"))
  
  # Check first M command in path
  m_match <- regmatches(piece$path, regexpr("M [0-9.-]+ [0-9.-]+", piece$path))
  cat(sprintf("  First M command: %s\n", m_match))
}

cat("\n=== Expected Positions for 2x2 with offset=50 ===\n")
cat("Piece 1 (0,0): Should start near (0, 0) - actually separated\n")
cat("Piece 2 (1,0): Should start near (100+margin, 0)\n")
cat("Piece 3 (0,1): Should start near (0, 100+margin)\n")
cat("Piece 4 (1,1): Should start near (100+margin, 100+margin)\n")
