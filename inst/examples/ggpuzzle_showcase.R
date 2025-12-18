# ggpuzzle Showcase Script
# Generates PNG examples for all puzzle types

library(ggplot2)
devtools::load_all()

# Create output directory if it doesn't exist
if (!dir.exists("output")) dir.create("output")

# 1. Rectangular puzzle (3x4 = 12 pieces)
cli::cli_alert_info("Creating rectangular puzzle...")
df_rect <- data.frame(value = 1:12, label = LETTERS[1:12])
p_rect <- ggplot(df_rect, aes(fill = value)) +
  geom_puzzle_rect(rows = 3, cols = 4, seed = 42) +
  scale_fill_viridis_c(option = "plasma") +
  theme_void() +
  labs(title = "Rectangular Puzzle (3x4)") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave("output/ggpuzzle_rectangular.png", p_rect, width = 8, height = 6, dpi = 150)

# 2. Hexagonal puzzle (3 rings = 19 pieces)
cli::cli_alert_info("Creating hexagonal puzzle...")
df_hex <- data.frame(value = 1:19)
p_hex <- ggplot(df_hex, aes(fill = value)) +
  geom_puzzle_hex(rings = 3, seed = 42) +
  scale_fill_viridis_c(option = "viridis") +
  theme_void() +
  coord_fixed() +
  labs(title = "Hexagonal Puzzle (3 rings, 19 pieces)") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave("output/ggpuzzle_hexagonal.png", p_hex, width = 8, height = 8, dpi = 150)

# 3. Concentric puzzle (3 rings = 19 pieces)
cli::cli_alert_info("Creating concentric puzzle...")
df_conc <- data.frame(value = 1:19)
p_conc <- ggplot(df_conc, aes(fill = value)) +
  geom_puzzle_conc(rings = 3, seed = 42) +
  scale_fill_viridis_c(option = "magma") +
  theme_void() +
  coord_fixed() +
  labs(title = "Concentric Puzzle (3 rings, 19 pieces)") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave("output/ggpuzzle_concentric.png", p_conc, width = 8, height = 8, dpi = 150)

# 4. Voronoi puzzle (15 cells)
cli::cli_alert_info("Creating voronoi puzzle...")
df_vor <- data.frame(value = 1:15)
p_vor <- ggplot(df_vor, aes(fill = value)) +
  geom_puzzle_voronoi(n_cells = 15, seed = 42, point_distribution = "fermat") +
  scale_fill_viridis_c(option = "turbo") +
  theme_void() +
  labs(title = "Voronoi Puzzle (15 cells, Fermat spiral)") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave("output/ggpuzzle_voronoi.png", p_vor, width = 8, height = 8, dpi = 150)

# 5. Random puzzle (12 pieces)
cli::cli_alert_info("Creating random puzzle...")
df_rnd <- data.frame(value = 1:20)
p_rnd <- ggplot(df_rnd, aes(fill = value)) +
  geom_puzzle_random(n_pieces = 12, seed = 42) +
  scale_fill_viridis_c(option = "cividis") +
  theme_void() +
  labs(title = "Random Shape Puzzle") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave("output/ggpuzzle_random.png", p_rnd, width = 8, height = 8, dpi = 150)

# 6. Combined showcase - all types in one image
cli::cli_alert_info("Creating combined showcase...")
if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)
  p_showcase <- (p_rect + p_hex) / (p_conc + p_vor) +
    plot_annotation(
      title = "jigsawR ggpuzzle Extension - All Puzzle Types",
      theme = theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
    )
  ggsave("output/ggpuzzle_showcase.png", p_showcase, width = 14, height = 14, dpi = 150)
}

cli::cli_alert_success("All PNG files created in output/")
print(list.files("output", pattern = "ggpuzzle.*\\.png"))
