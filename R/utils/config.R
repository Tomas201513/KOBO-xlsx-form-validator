# Configuration Management for XLS-Validator
# Manages paths to external tools and application settings

#' Get application configuration
#' @return List of configuration values
get_config <- function() {
  list(
    # External tool paths
    pyxform_cmd = find_pyxform(),
    java_cmd = find_java(),
    odk_validate_jar = find_odk_validate_jar(),
    
    # Temp directory for processing
    temp_dir = file.path(tempdir(), "xls_validator"),
    
    # Validation settings
    max_file_size_mb = 50,
    allowed_extensions = c("xls", "xlsx"),
    
    # Sheets to validate
    xlsform_sheets = c("survey", "choices", "settings"),
    
    # Required columns per sheet
    required_columns = list(
      survey = c("type", "name"),
      choices = c("list_name", "name"),
      settings = c()
    )
  )
}

#' Find pyxform executable
#' @return Path to pyxform or NULL if not found
find_pyxform <- function() {
  # Try direct pyxform command
  result <- tryCatch({
    processx::run("pyxform", "--version", error_on_status = FALSE, timeout = 5)
    "pyxform"
  }, error = function(e) NULL)
  
  if (!is.null(result)) return(result)
  
  # Try xls2xform (alternative name)
  result <- tryCatch({
    processx::run("xls2xform", "--help", error_on_status = FALSE, timeout = 5)
    "xls2xform"
  }, error = function(e) NULL)
  
  if (!is.null(result)) return(result)
  
  # Try python -m pyxform
  result <- tryCatch({
    processx::run("python", c("-m", "pyxform", "--version"), error_on_status = FALSE, timeout = 5)
    "python -m pyxform"
  }, error = function(e) NULL)
  
  result
}

#' Find Java executable
#' @return Path to java or NULL if not found
find_java <- function() {
  # Check JAVA_HOME first
  java_home <- Sys.getenv("JAVA_HOME")
  if (nzchar(java_home)) {
    java_path <- file.path(java_home, "bin", "java")
    if (.Platform$OS.type == "windows") {
      java_path <- paste0(java_path, ".exe")
    }
    if (file.exists(java_path)) {
      return(normalizePath(java_path, winslash = "/"))
    }
  }
  
  # Check system JAVA_HOME (in case R session has stale environment)
  if (.Platform$OS.type == "windows") {
    sys_java_home <- tryCatch({
      system2("cmd", c("/c", "echo %JAVA_HOME%"), stdout = TRUE, stderr = FALSE)
    }, error = function(e) "")
    sys_java_home <- trimws(sys_java_home)
    if (nzchar(sys_java_home) && sys_java_home != "%JAVA_HOME%") {
      java_path <- file.path(sys_java_home, "bin", "java.exe")
      if (file.exists(java_path)) {
        return(normalizePath(java_path, winslash = "/"))
      }
    }
    
    # Check common Adoptium/Eclipse Temurin installation paths
    adoptium_paths <- list.dirs("C:/Program Files/Eclipse Adoptium", recursive = FALSE)
    for (adoptium_path in adoptium_paths) {
      java_path <- file.path(adoptium_path, "bin", "java.exe")
      if (file.exists(java_path)) {
        return(normalizePath(java_path, winslash = "/"))
      }
    }
  }
  
  # Try java in PATH
  result <- tryCatch({
    processx::run("java", "-version", error_on_status = FALSE, timeout = 5)
    "java"
  }, error = function(e) NULL)
  
  result
}

#' Find ODK Validate JAR file
#' @return Path to JAR or NULL if not found
find_odk_validate_jar <- function() {
  # Check common locations
  possible_paths <- c(
    "tools/ODK_Validate.jar",
    "tools/ODK-Validate.jar",
    "tools/odk-validate.jar",
    file.path(Sys.getenv("HOME"), "ODK_Validate.jar")
  )
  
  for (path in possible_paths) {
    if (file.exists(path)) {
      return(normalizePath(path, winslash = "/"))
    }
  }
  
  NULL
}

#' Validate configuration
#' @param config Configuration list from get_config()
#' @return List with valid (logical) and messages (character vector)
validate_config <- function(config = get_config()) {
  messages <- character()
  
  if (is.null(config$pyxform_cmd)) {
    messages <- c(messages, "pyxform not found. Install with: pip install pyxform")
  }
  
  if (is.null(config$java_cmd)) {
    messages <- c(messages, "Java not found. Install Java 8+ from https://adoptium.net/")
  }
  
  if (is.null(config$odk_validate_jar)) {
    messages <- c(messages, "ODK_Validate.jar not found. Place it in the 'tools' folder.")
  }
  
  list(
    valid = length(messages) == 0,
    messages = messages
  )
}

#' Ensure temp directory exists
#' @param config Configuration list
#' @return Path to temp directory
ensure_temp_dir <- function(config = get_config()) {
  if (!dir.exists(config$temp_dir)) {
    dir.create(config$temp_dir, recursive = TRUE)
  }
  config$temp_dir
}

