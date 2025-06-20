# Enhanced Dockerfile for Railway deployment
FROM rocker/shiny:4.3.2

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libpq-dev \
    postgresql-client \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages in logical order (dependencies first)
RUN R -e "install.packages(c('remotes', 'devtools'), repos='https://cran.rstudio.com/')"

# Install core packages
RUN R -e "install.packages(c(\
    'shiny', \
    'shinydashboard', \
    'DT', \
    'plotly', \
    'dplyr', \
    'ggplot2', \
    'RPostgreSQL' \
    ), repos='https://cran.rstudio.com/')"

# Install enhanced UI packages
RUN R -e "install.packages(c(\
    'shinydashboardPlus', \
    'fresh', \
    'shinyWidgets', \
    'htmltools', \
    'htmlwidgets' \
    ), repos='https://cran.rstudio.com/')"

# Create app directory and set permissions
RUN mkdir -p /srv/shiny-server/
RUN chown -R shiny:shiny /srv/shiny-server/

# Copy app files
COPY app.R /srv/shiny-server/

# Set working directory
WORKDIR /srv/shiny-server/

# Set proper permissions
RUN chown -R shiny:shiny /srv/shiny-server/

# Expose port
EXPOSE 3838

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
    CMD curl -f http://localhost:3838/ || exit 1

# Run Shiny Server
CMD ["/usr/bin/shiny-server"]
