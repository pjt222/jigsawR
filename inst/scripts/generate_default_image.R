# Resize a source photo to a small default sample image for SNIC puzzle demos
# Run once: Rscript inst/scripts/generate_default_image.R
#
# Source: PJT_6805.jpg (purple asters, Norway 2017)
# Photo by Philipp Thoss

if (!requireNamespace("magick", quietly = TRUE)) {
  stop("magick package required: install.packages('magick')")
}

source_path <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(source_path) || !file.exists(source_path)) {
  stop("Usage: Rscript inst/scripts/generate_default_image.R <path-to-source-image>")
}
output_path <- file.path("inst", "extdata", "sample_image.jpg")

img <- magick::image_read(source_path)
img <- magick::image_resize(img, "400x")
magick::image_write(img, output_path, format = "jpeg", quality = 85)

cat("Default image saved to:", output_path, "\n")
cat("Dimensions:", paste(magick::image_info(img)$width, "x",
    magick::image_info(img)$height), "\n")
cat("File size:", file.size(output_path), "bytes\n")
