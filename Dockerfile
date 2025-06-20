FROM rocker/shiny:4.3.2

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libpq-dev \
    build-essential \
    postgresql-client \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN R -e "install.packages(c('shinydashboard', 'DT', 'plotly', 'dplyr', 'RPostgreSQL'), repos='https://packagemanager.rstudio.com/cran/__linux__/jammy/latest')"

# Create a non-root user
RUN useradd -m shinyuser

# Add the user to the shiny group
RUN usermod -aG shiny shinyuser

# Copy Shiny app
COPY app.R /srv/shiny-server/

# Change ownership and permissions of the Shiny app directory
RUN chown -R shinyuser:shiny /srv/shiny-server
RUN chmod g+rwx /srv/shiny-server

# Expose port
EXPOSE 3838

# Switch to non-root user and launch the Shiny app
USER shinyuser
CMD ["/usr/bin/shiny-server"]
