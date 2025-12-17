# Visual test for ggpuzzle
# Run manually to verify output looks correct

suppressPackageStartupMessages({
  devtools::load_all()
  library(ggplot2)
})

cat("=== Visual Test: geom_puzzle_rect() ===\n\n")

# Test 1: Basic 3x3 puzzle with numeric fill
cat("Test 1: 3x3 puzzle with numeric fill\n")
df <- data.frame(value = 1:9)
p1 <- ggplot(df, aes(fill = value)) +
  geom_puzzle_rect(rows = 3, cols = 3, seed = 42) +
  scale_fill_viridis_c() +
  theme_void() +
  labs(title = "3x3 Puzzle - Numeric Fill")

ggsave("output/ggpuzzle_test_1.png", p1, width = 6, height = 6)
cat("  Saved: output/ggpuzzle_test_1.png\n")

# Test 2: 2x2 puzzle with categorical fill
cat("Test 2: 2x2 puzzle with categorical fill\n")
sales <- data.frame(
  region = c("North", "South", "East", "West"),
  value = c(100, 150, 80, 120)
)
p2 <- ggplot(sales, aes(fill = value)) +
  geom_puzzle_rect(rows = 2, cols = 2, seed = 123) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_void() +
  labs(title = "Sales by Region")

ggsave("output/ggpuzzle_test_2.png", p2, width = 5, height = 5)
cat("  Saved: output/ggpuzzle_test_2.png\n")

# Test 3: Larger puzzle
cat("Test 3: 5x5 puzzle\n")
df3 <- data.frame(value = 1:25)
p3 <- ggplot(df3, aes(fill = value)) +
  geom_puzzle_rect(rows = 5, cols = 5, seed = 42, tabsize = 15, jitter = 3) +
  scale_fill_viridis_c(option = "plasma") +
  theme_void() +
  labs(title = "5x5 Puzzle")

ggsave("output/ggpuzzle_test_3.png", p3, width = 8, height = 8)
cat("  Saved: output/ggpuzzle_test_3.png\n")

cat("\n=== All visual tests completed ===\n")
cat("Check the output/ directory for PNG files.\n")
