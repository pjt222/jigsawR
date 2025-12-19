suppressMessages(devtools::load_all(quiet = TRUE))

adj <- get_hex_adjacency_matrix(3)

cat("Verifying: neighbor_side = (side + 3) %% 6\n\n")
all_match <- TRUE

for (piece_id in 1:7) {
  for (side in 0:5) {
    neighbor_id <- adj[piece_id, side + 1]
    if (!is.na(neighbor_id)) {
      expected_nb_side <- (side + 3) %% 6
      actual_nb_side <- NA
      
      for (nb_side in 0:5) {
        back_ref <- adj[neighbor_id, nb_side + 1]
        if (!is.na(back_ref) && back_ref == piece_id) {
          actual_nb_side <- nb_side
          break
        }
      }
      
      match <- (actual_nb_side == expected_nb_side)
      if (!match) all_match <- FALSE
      cat(sprintf("P%d side %d -> P%d: actual=%d, expected=%d %s\n",
                  piece_id, side, neighbor_id, actual_nb_side, expected_nb_side,
                  if(match) "OK" else "MISMATCH"))
    }
  }
}

cat("\n", if(all_match) "All sides follow (side+3)%%6 pattern!" else "Pattern mismatch found!", "\n")
