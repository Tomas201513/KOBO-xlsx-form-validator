# XLS-Validator Dockerfile
# R + Python + Java environment for ODK XLSForm validation

FROM rocker/shiny:4.3.0

LABEL maintainer="XLS-Validator"
LABEL description="ODK XLSForm Validation Platform"

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    python3 \
    python3-pip \
    openjdk-17-jre-headless \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install Python pyxform
RUN pip3 install pyxform

# Verify installations
RUN python3 --version && \
    pip3 show pyxform && \
    java -version

# Install R packages
RUN R -e "install.packages(c( \
    'shiny', \
    'shinyjs', \
    'bslib', \
    'DT', \
    'rhandsontable', \
    'readxl', \
    'writexl', \
    'dplyr', \
    'tibble', \
    'stringr', \
    'processx', \
    'purrr' \
  ), repos='https://cloud.r-project.org/')"

# Create app directory
WORKDIR /srv/shiny-server/xls-validator

# Copy application files
COPY . .

# Download ODK Validate JAR if not present
RUN if [ ! -f tools/ODK_Validate.jar ]; then \
    mkdir -p tools && \
    curl -L -o tools/ODK_Validate.zip \
      "https://github.com/getodk/validate/releases/download/v3.0.1/ODK_Validate.zip" && \
    unzip tools/ODK_Validate.zip -d tools/ && \
    rm tools/ODK_Validate.zip; \
  fi

# Set permissions
RUN chown -R shiny:shiny /srv/shiny-server/xls-validator

# Expose Shiny port
EXPOSE 3838

# Run Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/xls-validator', host='0.0.0.0', port=3838)"]


