# Generate favicon for jigsawR documentation site
# Uses jigsawR's own puzzle generation to create the icon
#
# Usage: Rscript inst/scripts/generate_favicon.R
# Requires: devtools (for load_all), ggplot2, magick (optional, for resize)

devtools::load_all(quiet = TRUE)
library(ggplot2)

output_path <- file.path("quarto", "favicon.png")

favicon_plot <- ggplot() +
  geom_puzzle_hex(
    aes(fill = after_stat(piece_id)),
    rings = 2,
    seed = 42,
    tabsize = 15,
    do_warp = TRUE,
    do_trunc = TRUE
  ) +
  scale_fill_viridis_c(option = "plasma", guide = "none") +
  coord_fixed() +
  theme_void() +
  theme(
    plot.margin = margin(2, 2, 2, 2),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )

ggsave(
  output_path,
  plot = favicon_plot,
  width = 4, height = 4, units = "in",
  dpi = 150,
  bg = "transparent"
)

# Resize to 256x256 for favicon
if (requireNamespace("magick", quietly = TRUE)) {
  img <- magick::image_read(output_path)
  img_resized <- magick::image_resize(img, "256x256")
  magick::image_write(img_resized, output_path, format = "png")
}

cat("Favicon generated:", output_path, "\n")
