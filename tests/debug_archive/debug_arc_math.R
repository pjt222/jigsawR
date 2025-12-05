# Calculate arc centers to demonstrate the translation problem

calculate_arc_center <- function(x1, y1, x2, y2, r) {
  mx <- (x1 + x2) / 2
  my <- (y1 + y2) / 2
  d <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  
  if (d > 2*r) {
    return(list(x = NA, y = NA))
  }
  
  h <- sqrt(r^2 - (d/2)^2)
  dx_perp <- -(y2 - y1) / d
  dy_perp <- (x2 - x1) / d
  cx <- mx + h * dx_perp
  cy <- my + h * dy_perp
  
  list(x = cx, y = cy)
}

cat("Arc Translation Problem Analysis\n")
cat("=================================\n\n")

cat("Original arc (centered at origin):\n")
cat("  Path: M 0 100 A 100 100 0 0 1 86.6 50\n")
original_center <- calculate_arc_center(0, 100, 86.6, 50, 100)
cat(sprintf("  Calculated center: (%.2f, %.2f)\n\n", original_center$x, original_center$y))

cat("Translated arc (simple endpoint translation by dx=50, dy=50):\n")
cat("  Path: M 50 150 A 100 100 0 0 1 136.6 100\n")
translated_center <- calculate_arc_center(50, 150, 136.6, 100, 100)
cat(sprintf("  Calculated center: (%.2f, %.2f)\n\n", translated_center$x, translated_center$y))

cat("Expected translated center: (50, 50)\n")
cat(sprintf("Actual translated center: (%.2f, %.2f)\n", translated_center$x, translated_center$y))
cat(sprintf("ERROR: (%.2f, %.2f)\n\n", 
            translated_center$x - 50, translated_center$y - 50))

cat("CONCLUSION:\n")
cat("SVG arcs defined by radius + endpoints do NOT translate correctly\n")
cat("when you only translate the endpoints and keep the radius constant.\n")
cat("The arc center implicitly moves by a DIFFERENT amount than (dx, dy).\n")
