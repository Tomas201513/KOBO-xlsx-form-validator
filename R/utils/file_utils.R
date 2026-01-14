# File Utilities for XLS-Validator
# Helper functions for file handling

#' Validate uploaded file
#' @param file_path Path to uploaded file
#' @param config Application configuration
#' @return List with valid (logical) and message (character)
validate_upload <- function(file_path, config = get_config()) {
  # Check file exists
 if (!file.exists(file_path)) {
    return(list(valid = FALSE, message = "File not found"))
  }
  
  # Check extension
  ext <- tolower(tools::file_ext(file_path))
  if (!ext %in% config$allowed_extensions) {
    return(list(
      valid = FALSE, 
      message = paste("Invalid file type. Allowed:", paste(config$allowed_extensions, collapse = ", "))
    ))
  }
  
  # Check file size
  size_mb <- file.info(file_path)$size / (1024^2)
  if (size_mb > config$max_file_size_mb) {
    return(list(
      valid = FALSE,
      message = paste("File too large. Maximum size:", config$max_file_size_mb, "MB")
    ))
  }
  
  list(valid = TRUE, message = "File is valid")
}

#' Copy uploaded file to temp directory with safe name
#' @param file_path Original file path
#' @param original_name Original file name
#' @param config Application configuration
#' @return Path to copied file
copy_to_temp <- function(file_path, original_name, config = get_config()) {
  temp_dir <- ensure_temp_dir(config)
  
  # Create safe filename
  safe_name <- gsub("[^a-zA-Z0-9._-]", "_", original_name)
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  new_name <- paste0(timestamp, "_", safe_name)
  
  new_path <- file.path(temp_dir, new_name)
  file.copy(file_path, new_path, overwrite = TRUE)
  
  normalizePath(new_path, winslash = "/")
}

#' Clean up old temp files
#' @param max_age_hours Maximum age of files to keep (default 24 hours)
#' @param config Application configuration
cleanup_temp_files <- function(max_age_hours = 24, config = get_config()) {
  temp_dir <- config$temp_dir
  
  if (!dir.exists(temp_dir)) return(invisible(NULL))
  
  files <- list.files(temp_dir, full.names = TRUE)
  if (length(files) == 0) return(invisible(NULL))
  
  # Get file info
  file_info <- file.info(files)
  file_info$path <- files
  
  # Calculate age
  age_hours <- as.numeric(difftime(Sys.time(), file_info$mtime, units = "hours"))
  
  # Remove old files
  old_files <- files[age_hours > max_age_hours]
  if (length(old_files) > 0) {
    file.remove(old_files)
  }
  
  invisible(length(old_files))
}

#' Get list of sheets in an Excel file
#' @param file_path Path to Excel file
#' @return Character vector of sheet names
get_excel_sheets <- function(file_path) {
  readxl::excel_sheets(file_path)
}

#' Check if file is a valid XLSForm (has required sheets)
#' @param file_path Path to Excel file
#' @return List with valid (logical) and message (character)
is_valid_xlsform <- function(file_path) {
  tryCatch({
    sheets <- tolower(get_excel_sheets(file_path))
    
    # Must have 'survey' sheet
    if (!"survey" %in% sheets) {
      return(list(valid = FALSE, message = "Missing required 'survey' sheet"))
    }
    
    # 'choices' is recommended but not strictly required
    if (!"choices" %in% sheets) {
      message <- "Warning: 'choices' sheet not found. This may cause issues if your form uses choice lists."
    } else {
      message <- "Valid XLSForm structure"
    }
    
    list(valid = TRUE, message = message)
  }, error = function(e) {
    list(valid = FALSE, message = paste("Error reading file:", e$message))
  })
}



