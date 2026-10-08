# Reproduction environment for the panel analysis (panel/analysis).
#
# Build from the committed state only, as a fresh checkout would see it:
#   git archive --format=tar HEAD | docker build -t zm-panel -
# Run the tests, then the smoke profile of the pipeline on the synthetic data:
#   docker run --rm zm-panel Rscript -e 'source("R/config.R"); zm_setup(); testthat::test_dir("tests/testthat")'
#   docker run --rm zm-panel
# The full profile (preregistered settings, about 40 minutes, at least 24 GB):
#   docker run --rm -e ZM_PROFILE=full zm-panel
# The preregistration as HTML and PDF, written to ./renders on the host:
#   docker run --rm -v "$PWD/renders:/opt/zm/panel/preregistration/renders" zm-panel \
#     bash -c 'cd ../preregistration && quarto render preregistration.qmd --to html && quarto render preregistration.qmd --to pdf && { mv preregistration.html preregistration.pdf renders/ 2>/dev/null || true; }'

# The base image by digest (amd64 and arm64), so that a later rebuild of the
# 4.5.2 tag does not change the system underneath.
FROM rocker/r-ver:4.5.2@sha256:fd4ccdd3a4a6f7ef805e2daeee2a0fe3bf126bc231f36351223baecf5a595a4c

ARG QUARTO_VERSION=1.9.37
ARG CMDSTAN_VERSION=2.36.0
ARG TARGETARCH
# Parallel compile jobs; each C++ job for CmdStan needs about 1-2 GB of memory.
ARG BUILD_JOBS=4

# System libraries for the locked R packages (the list Posit Package Manager
# gives for them on Ubuntu 24.04), CmdStan's toolchain and git for the GitHub
# package in the lockfile.
RUN apt-get update && apt-get install -y --no-install-recommends \
      build-essential cmake git curl ca-certificates librsvg2-bin \
      libcurl4-openssl-dev libssl-dev libxml2-dev libgit2-dev \
      libglpk-dev libgmp-dev libmpfr-dev libgsl-dev pari-gp \
      libpng-dev libjpeg-dev libtiff-dev libwebp-dev libcairo2-dev libx11-dev \
      libfontconfig1-dev libfreetype6-dev libharfbuzz-dev libfribidi-dev \
      zlib1g-dev libbz2-dev liblzma-dev libicu-dev libuv1-dev libnode-dev \
    && rm -rf /var/lib/apt/lists/* \
    && command -v rsvg-convert

# Quarto uses rsvg-convert for the preregistration SVG figures in PDF output.
# Quarto renders the results report.
RUN curl -fsSL -o /tmp/quarto.deb \
      "https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-${TARGETARCH}.deb" \
    && apt-get install -y /tmp/quarto.deb \
    && rm /tmp/quarto.deb

# CmdStan, the backend of every brms fit. zm_setup() reads ZM_CMDSTAN_PATH;
# cmdstanr reads CMDSTAN in worker processes that do not call zm_setup().
# The release bundles an x86-64 stanc only, so ARM builds fetch the arm64 one.
ENV ZM_CMDSTAN_PATH=/opt/cmdstan/cmdstan-${CMDSTAN_VERSION} \
    CMDSTAN=/opt/cmdstan/cmdstan-${CMDSTAN_VERSION}
RUN mkdir -p /opt/cmdstan \
    && curl -fsSL "https://github.com/stan-dev/cmdstan/releases/download/v${CMDSTAN_VERSION}/cmdstan-${CMDSTAN_VERSION}.tar.gz" \
       | tar -xz -C /opt/cmdstan \
    && if [ "${TARGETARCH}" = "arm64" ]; then \
         curl -fsSL -o "${ZM_CMDSTAN_PATH}/bin/stanc" \
           "https://github.com/stan-dev/stanc3/releases/download/v${CMDSTAN_VERSION}/linux-arm64-stanc" \
         && chmod +x "${ZM_CMDSTAN_PATH}/bin/stanc"; \
       fi \
    && make -C "${ZM_CMDSTAN_PATH}" build -j"${BUILD_JOBS}"

# R packages from the lockfile, into a library outside the project folder so
# that copying the source over it leaves the library in place. The dated Posit
# Package Manager snapshot serves Linux builds of most locked versions; the
# rest compile from source.
ENV RENV_PATHS_LIBRARY=/opt/renv/library \
    RENV_PATHS_CACHE=/opt/renv/cache \
    RENV_CONFIG_REPOS_OVERRIDE=https://packagemanager.posit.co/cran/__linux__/noble/2026-10-04
WORKDIR /opt/zm/panel/analysis
COPY panel/analysis/.Rprofile panel/analysis/renv.lock ./
COPY panel/analysis/renv/activate.R panel/analysis/renv/settings.json renv/
RUN R -q -e 'renv::restore(prompt = FALSE)'

# TinyTeX renders the preregistration and the equations in the report PDF.
# standalone supplies the tightly cropped equation pages; XeLaTeX and amsmath
# are already part of TinyTeX. Verify both rather than adding a second TeX setup.
# The binaries share one PATH folder regardless of the architecture.
RUN quarto install tinytex --no-prompt \
    && ln -s "$(ls -d /root/.TinyTeX/bin/*)" /opt/tinytex-bin \
    && /opt/tinytex-bin/tlmgr install luatexbase standalone \
    && test -x /opt/tinytex-bin/xelatex \
    && /opt/tinytex-bin/kpsewhich amsmath.sty \
    && /opt/tinytex-bin/kpsewhich standalone.cls
ENV PATH=/opt/tinytex-bin:$PATH

# The report PDF uses the same HTML tables and histogram images as its online
# version. Keep its two Python packages separate from Ubuntu's system Python
# and from the locked R environment. Versions match the validated local export.
RUN apt-get update && apt-get install -y --no-install-recommends python3-venv \
    && rm -rf /var/lib/apt/lists/* \
    && python3 -m venv /opt/report-pdf \
    && /opt/report-pdf/bin/pip install --no-cache-dir PyMuPDF==1.27.2.2 Pillow==12.0.0 \
    && /opt/report-pdf/bin/python -c "import fitz; from PIL import Image"

# The study source: analysis, preregistration and codebooks.
COPY panel /opt/zm/panel

ENV ZM_PROFILE=smoke \
    ZM_DATA=synthetic
CMD ["Rscript", "-e", "targets::tar_make(callr_function = NULL)"]
