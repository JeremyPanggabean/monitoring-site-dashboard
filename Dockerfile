# Optimized Dockerfile for Railway deployment
FROM rocker/shiny:4.3.2

# Install system dependencies (minimal)
RUN apt-get update && apt-get install -y \
    libpq-dev \
    postgresql-client \
    && rm -rf /var/lib/apt/lists/*

# Set CRAN repository with binary packages
ENV CRAN_REPO=https://packagemanager.rstudio.com/cran/__linux__/jammy/latest

# Install essential R packages only (using binary packages)
RUN R -e "install.packages(c(\
    'shinydashboard', \
    'DT', \
    'plotly', \
    'dplyr', \
    'RPostgreSQL' \
    ), repos='${CRAN_REPO}', type='binary')"

# Copy app files
COPY app.R /srv/shiny-server/

# Expose port
EXPOSE 3838

# Run Shiny Server
CMD ["/usr/bin/shiny-server"]
