# Simple Dockerfile for Railway deployment
FROM rocker/shiny:4.3.2

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libpq-dev \
    postgresql-client \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('shinydashboard', 'DT', 'plotly', 'dplyr', 'RPostgreSQL'), repos='https://cran.rstudio.com/')"

# Copy app
COPY app.R /srv/shiny-server/

# Expose port
EXPOSE 3838

# Run Shiny
CMD ["/usr/bin/shiny-server"]
