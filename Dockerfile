FROM rocker/shiny:4.3.2

# Install dependencies
RUN apt-get update && apt-get install -y \
    libpq-dev \
    postgresql-client \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('shinydashboard', 'DT', 'plotly', 'dplyr', 'RPostgreSQL'), repos='https://packagemanager.rstudio.com/cran/__linux__/jammy/latest')"

# Copy the app
COPY app.R /srv/shiny-server/

# Expose port
EXPOSE 3838

# Use default shiny user (don't change USER)
CMD ["/usr/bin/shiny-server"]
