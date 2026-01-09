# XLS-Validator R Project Profile
# Automatically loads when R session starts in this directory

# Source renv if available
if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
}

# Set options
options(
  shiny.maxRequestSize = 50 * 1024^2,  # Allow up to 50MB uploads
  scipen = 999,  # Avoid scientific notation
  stringsAsFactors = FALSE
)

message("XLS-Validator environment loaded")
message("Run shiny::runApp() to start the application")

