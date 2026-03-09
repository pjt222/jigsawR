# ===========================================================================
# Stage 1: Dependencies (cached layer -- only rebuilds when DESCRIPTION changes)
# ===========================================================================
FROM rocker/r-ver:4.4.2 AS deps

# Headless rgl (no X11 display in Docker)
ENV RGL_USE_NULL=TRUE

# System libraries needed by R packages
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libmagick++-dev \
    librsvg2-dev \
    libpng-dev \
    libgl1-mesa-dev \
    libglu1-mesa-dev \
    libfreetype6-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfontconfig1-dev \
    libtiff5-dev \
    libjpeg-dev \
    libwebp-dev \
    zlib1g-dev \
    git \
    && rm -rf /var/lib/apt/lists/*

# Copy only dependency-defining file for cache optimization
COPY DESCRIPTION /tmp/pkg/DESCRIPTION

# Install all R packages (PPM binaries where available)
RUN Rscript -e " \
  install.packages('remotes', repos='https://cloud.r-project.org'); \
  # Imports \
  install.packages(c('Rcpp', 'ggplot2', 'ggfx', 'viridis', 'config', 'cli', 'rlang'), \
    repos='https://cloud.r-project.org'); \
  # Shiny runtime dependencies \
  install.packages(c('shiny', 'bslib', 'bsicons', 'shinyjs', 'colourpicker', 'waiter', \
    'rsvg', 'magick', 'zip', 'scales', 'ambient', 'png', 'base64enc', \
    'deldir', 'snic', 'ggforce', 'data.table'), \
    repos='https://cloud.r-project.org'); \
  # GitHub-only packages \
  remotes::install_github('stla/RCDT'); \
"

# ===========================================================================
# Stage 2: Application (fast rebuild on code changes)
# ===========================================================================
FROM deps AS app

# Create app user (HF Spaces runs as uid 1000)
RUN useradd -m -u 1000 appuser

# Copy package source and install (compiles Rcpp C++)
COPY . /tmp/jigsawR-src
RUN cd /tmp/jigsawR-src && \
    R CMD INSTALL --no-test-load . && \
    rm -rf /tmp/jigsawR-src

# Set up the Shiny app directory
RUN mkdir -p /app/output
COPY inst/shiny-app/ /app/
COPY inst/config.yml /app/config.yml

RUN chown -R appuser:appuser /app

USER appuser
WORKDIR /app

EXPOSE 7860

CMD ["Rscript", "-e", "shiny::runApp('/app', host='0.0.0.0', port=7860, launch.browser=FALSE)"]
