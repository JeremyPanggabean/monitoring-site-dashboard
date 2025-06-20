FROM rocker/shiny:4.3.2

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libpq-dev \
    build-essential \
    postgresql-client \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages (tanpa 'type = binary')
RUN R -e "install.packages(c('shinydashboard', 'DT', 'plotly', 'dplyr', 'RPostgreSQL'), repos='https://packagemanager.rstudio.com/cran/__linux__/jammy/latest')"

# Copy Shiny app
COPY app.R /srv/shiny-server/

# Expose port
EXPOSE 3838

# Launch the Shiny app
CMD ["/usr/bin/shiny-server"]
