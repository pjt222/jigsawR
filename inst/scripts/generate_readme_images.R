# Generate README images for jigsawR
# Run from package root: Rscript inst/scripts/generate_readme_images.R

library(ggplot2)
library(patchwork)

# Load package
devtools::load_all()

# Output directory
out_dir <- "man/figures"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

cli::cli_h1("Generating README images")

# 1. Hero image - 2x2 grid of puzzle types
cli::cli_alert_info("Creating hero image (2x2 grid)...")

p_rect <- ggplot() +
geom_puzzle_rect(
    aes(fill = after_stat(piece_id)),
    cols = 4, rows = 3, seed = 42
  ) +
  scale_fill_viridis_c(option = "plasma", guide = "none") +
  coord_fixed() +
  theme_void() +
  labs(title = "Rectangular")

p_hex <- ggplot() +
  geom_puzzle_hex(
    aes(fill = after_stat(piece_id)),
    rings = 3, seed = 42
  ) +
  scale_fill_viridis_c(option = "viridis", guide = "none") +
  coord_fixed() +
  theme_void() +
  labs(title = "Hexagonal")

p_conc <- ggplot() +
  geom_puzzle_conc(
    aes(fill = after_stat(ring)),
    rings = 3, seed = 42
  ) +
  scale_fill_viridis_c(option = "magma", guide = "none") +
  coord_fixed() +
  theme_void() +
  labs(title = "Concentric")

p_vor <- ggplot() +
  geom_puzzle_voronoi(
    aes(fill = after_stat(piece_id)),
    n_cells = 20, seed = 42
  ) +
  scale_fill_viridis_c(option = "turbo", guide = "none") +
  coord_fixed() +
  theme_void() +
  labs(title = "Voronoi")

hero <- (p_rect | p_hex) / (p_conc | p_vor) +
  plot_annotation(
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
    )
  )

ggsave(
  file.path(out_dir, "readme-hero.png"),
  hero,
  width = 8, height = 8, dpi = 150, bg = "white"
)
cli::cli_alert_success("Saved readme-hero.png")

# 2. Quick start example - simple hexagonal puzzle
cli::cli_alert_info("Creating quick start image...")

p_quickstart <- ggplot() +
  geom_puzzle_hex(
    aes(fill = after_stat(piece_id)),
    rings = 3, seed = 42
  ) +
  scale_fill_viridis_c(option = "plasma", guide = "none") +
  coord_fixed() +
  theme_void()

ggsave(
  file.path(out_dir, "readme-quickstart.png"),
  p_quickstart,
  width = 4, height = 4, dpi = 150, bg = "white"
)
cli::cli_alert_success("Saved readme-quickstart.png")

# 3. ggpuzzle example - rectangular with viridis
cli::cli_alert_info("Creating ggpuzzle example image...")

p_ggpuzzle <- ggplot() +
  geom_puzzle_rect(
    aes(fill = after_stat(piece_id)),
    cols = 5, rows = 4, seed = 123
  ) +
  scale_fill_viridis_c(option = "turbo") +
  coord_fixed() +
  theme_void() +
  theme(legend.position = "right")

ggsave(
  file.path(out_dir, "readme-ggpuzzle.png"),
  p_ggpuzzle,
  width = 6, height = 4, dpi = 150, bg = "white"
)
cli::cli_alert_success("Saved readme-ggpuzzle.png")

# 4. Gallery thumbnails - individual puzzle types
cli::cli_alert_info("Creating gallery thumbnails...")

thumbnails <- list(
  rectangular = ggplot() +
    geom_puzzle_rect(aes(fill = after_stat(piece_id)), cols = 4, rows = 3, seed = 42) +
    scale_fill_viridis_c(option = "plasma", guide = "none") +
    coord_fixed() + theme_void(),

  hexagonal = ggplot() +
    geom_puzzle_hex(aes(fill = after_stat(piece_id)), rings = 3, seed = 42) +
    scale_fill_viridis_c(option = "viridis", guide = "none") +
    coord_fixed() + theme_void(),

  concentric = ggplot() +
    geom_puzzle_conc(aes(fill = after_stat(ring)), rings = 3, seed = 42) +
    scale_fill_viridis_c(option = "magma", guide = "none") +
    coord_fixed() + theme_void(),

  voronoi = ggplot() +
    geom_puzzle_voronoi(aes(fill = after_stat(piece_id)), n_cells = 20, seed = 42) +
    scale_fill_viridis_c(option = "turbo", guide = "none") +
    coord_fixed() + theme_void(),

  random = ggplot() +
    geom_puzzle_random(aes(fill = after_stat(piece_id)), n_pieces = 15, seed = 42) +
    scale_fill_viridis_c(option = "inferno", guide = "none") +
    coord_fixed() + theme_void()
)

for (name in names(thumbnails)) {
  ggsave(
    file.path(out_dir, paste0("thumb-", name, ".png")),
    thumbnails[[name]],
    width = 3, height = 3, dpi = 100, bg = "white"
  )
}
cli::cli_alert_success("Saved 5 gallery thumbnails")

cli::cli_h1("Done!")
cli::cli_alert_success("All images saved to {.path {out_dir}}")
