# Debug arc translation issue
# When we have an arc A rx ry ... x y centered at origin,
# and we translate the entire piece by (dx, dy),
# the arc endpoints move correctly but the arc itself doesn't!

# Example: Arc from (0, 100) to (86.6, 50) with radius 100
# This is a 60-degree arc centered at origin (0, 0)

library(ggplot2)

# Original arc (centered at origin)
arc_original <- data.frame(
  x = c(0, 86.6),
  y = c(100, 50),
  label = c("Start", "End")
)

# SVG path would be: M 0 100 A 100 100 0 0 1 86.6 50
# The arc center is implicitly at (0, 0)

# Now translate by (50, 50)
dx <- 50
dy <- 50

arc_translated <- data.frame(
  x = c(0 + dx, 86.6 + dx),
  y = c(100 + dy, 50 + dy),
  label = c("Start", "End")
)

# The translated SVG path is: M 50 150 A 100 100 0 0 1 136.6 100
# BUT: The arc still has radius 100 centered at... where?

# The issue: SVG arcs are defined by:
# - Start point (from current position)
# - rx, ry (radii)
# - End point
# - Flags for which of the 4 possible arcs to use

# The CENTER of the arc is CALCULATED from these parameters
# For the original arc: center is at (0, 0)
# For the translated endpoints with SAME radius: center is NOT at (50, 50)!

# Let's calculate the actual centers
calculate_arc_center <- function(x1, y1, x2, y2, r) {
  # For an arc from (x1,y1) to (x2,y2) with radius r
  # The center (cx, cy) satisfies:
  # (x1 - cx)^2 + (y1 - cy)^2 = r^2
  # (x2 - cx)^2 + (y2 - cy)^2 = r^2
  
  # Midpoint
  mx <- (x1 + x2) / 2
  my <- (y1 + y2) / 2
  
  # Distance between points
  d <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  
  if (d > 2*r) {
    return(list(x = NA, y = NA, note = "Points too far apart"))
  }
  
  # Distance from midpoint to center
  h <- sqrt(r^2 - (d/2)^2)
  
  # Direction perpendicular to line
  dx_perp <- -(y2 - y1) / d
  dy_perp <- (x2 - x1) / d
  
  # Two possible centers (we want the one on the right/clockwise side)
  cx <- mx + h * dx_perp
  cy <- my + h * dy_perp
  
  list(x = cx, y = cy)
}

original_center <- calculate_arc_center(0, 100, 86.6, 50, 100)
translated_center <- calculate_arc_center(50, 150, 136.6, 100, 100)

cat("Original arc:\n")
cat(sprintf("  Start: (0, 100)\n"))
cat(sprintf("  End: (86.6, 50)\n"))
cat(sprintf("  Radius: 100\n"))
cat(sprintf("  Center: (%.2f, %.2f)\n", original_center$x, original_center$y))

cat("\nTranslated arc (simple translation of endpoints):\n")
cat(sprintf("  Start: (50, 150)\n"))
cat(sprintf("  End: (136.6, 100)\n"))
cat(sprintf("  Radius: 100 (unchanged)\n"))
cat(sprintf("  Center: (%.2f, %.2f)\n", translated_center$x, translated_center$y))

cat("\nExpected translated center: (50, 50)\n")
cat(sprintf("Actual translated center: (%.2f, %.2f)\n", translated_center$x, translated_center$y))
cat(sprintf("Difference: (%.2f, %.2f)\n", 
            translated_center$x - 50, translated_center$y - 50))

# Visualization
plot_data <- rbind(
  cbind(arc_original, type = "Original"),
  cbind(arc_translated, type = "Translated")
)

centers <- data.frame(
  x = c(original_center$x, translated_center$x, 50),
  y = c(original_center$y, translated_center$y, 50),
  label = c("Original\nCenter", "Actual\nTranslated\nCenter", "Expected\nTranslated\nCenter"),
  type = c("Original", "Translated", "Expected")
)

p <- ggplot() +
  geom_point(data = plot_data, aes(x, y, color = type, shape = label), size = 3) +
  geom_text(data = plot_data, aes(x, y, label = label), vjust = -1, size = 3) +
  geom_point(data = centers, aes(x, y, color = type), size = 4, shape = 4, stroke = 2) +
  geom_text(data = centers, aes(x, y, label = label), vjust = -1.5, size = 3) +
  coord_equal() +
  theme_minimal() +
  labs(title = "SVG Arc Translation Problem",
       subtitle = "Simple endpoint translation doesn't preserve arc geometry")

print(p)

ggsave("/mnt/d/dev/p/jigsawR/output/arc_translation_problem.png", 
       p, width = 10, height = 8)

cat("\nCONCLUSION:\n")
cat("When you translate arc endpoints but keep the same radius,\n")
cat("the arc center does NOT translate by the same amount!\n")
cat("This is because the arc center is implicitly calculated\n")
cat("from the endpoints and radius.\n")
