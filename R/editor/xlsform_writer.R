# XLSForm Writer for XLS-Validator
# Writes corrected XLSForm back to Excel file

#' Write XLSForm data to Excel file
#' @param xlsform_data List from read_xlsform (with or without applied changes)
#' @param output_path Path for output file
#' @param original_path Original file path (to preserve sheet order and any extra sheets)
#' @return Path to written file
write_xlsform <- function(xlsform_data, output_path, original_path = NULL) {
  # Prepare sheets list for writexl
  sheets_to_write <- list()
  
  # Determine sheet order
  if (!is.null(original_path) && file.exists(original_path)) {
    sheet_order <- readxl::excel_sheets(original_path)
  } else {
    # Default order: survey, choices, settings, then others
    standard_sheets <- c("survey", "choices", "settings")
    other_sheets <- setdiff(names(xlsform_data$sheets), standard_sheets)
    sheet_order <- c(standard_sheets, other_sheets)
  }
  
  # Build sheets list
  for (sheet_name in sheet_order) {
    sheet_key <- tolower(sheet_name)
    
    if (sheet_key %in% names(xlsform_data$sheets)) {
      df <- xlsform_data$sheets[[sheet_key]]
      
      # Remove internal columns before writing
      internal_cols <- names(df)[startsWith(names(df), ".")]
      if (length(internal_cols) > 0) {
        df <- df[, !names(df) %in% internal_cols, drop = FALSE]
      }
      
      # Only include non-empty sheets
      if (ncol(df) > 0) {
        sheets_to_write[[sheet_name]] <- df
      }
    }
  }
  
  # Write to file
  writexl::write_xlsx(sheets_to_write, path = output_path)
  
  normalizePath(output_path, winslash = "/")
}

#' Apply changes and write to file
#' @param xlsform_data Original XLSForm data
#' @param tracker Change tracker with pending changes
#' @param output_path Path for output file
#' @return Path to written file
write_with_changes <- function(xlsform_data, tracker, output_path) {
  # Apply all pending changes
  updated_data <- apply_changes(tracker, xlsform_data)
  
  # Write to file
  write_xlsform(updated_data, output_path, xlsform_data$file_path)
}

#' Create a download filename based on original
#' @param original_name Original filename
#' @param suffix Suffix to add (default: "_corrected")
#' @return New filename
create_download_filename <- function(original_name, suffix = "_corrected") {
  ext <- tools::file_ext(original_name)
  base <- tools::file_path_sans_ext(original_name)
  
  # Add timestamp
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  paste0(base, suffix, "_", timestamp, ".", ext)
}

#' Prepare XLSForm for download (creates temp file)
#' @param xlsform_data XLSForm data
#' @param tracker Change tracker (optional)
#' @param config Application configuration
#' @return List with path and suggested filename
prepare_download <- function(xlsform_data, tracker = NULL, config = get_config()) {
  # Create temp directory
  temp_dir <- ensure_temp_dir(config)
  
  # Generate filename
  original_name <- xlsform_data$file_name
  if (is.null(original_name) || nchar(original_name) == 0) {
    original_name <- "xlsform.xlsx"
  }
  
  download_name <- create_download_filename(original_name)
  temp_path <- file.path(temp_dir, download_name)
  
  # Write file
  if (!is.null(tracker) && count_changes(tracker) > 0) {
    write_with_changes(xlsform_data, tracker, temp_path)
  } else {
    write_xlsform(xlsform_data, temp_path)
  }
  
  list(
    path = temp_path,
    filename = download_name
  )
}

#' Validate written file by attempting to read it back
#' @param file_path Path to written file
#' @return List with valid (logical) and message (character)
validate_written_file <- function(file_path) {
  tryCatch({
    sheets <- readxl::excel_sheets(file_path)
    
    if (length(sheets) == 0) {
      return(list(valid = FALSE, message = "Written file has no sheets"))
    }
    
    # Try to read first sheet
    df <- readxl::read_excel(file_path, sheet = 1)
    
    list(valid = TRUE, message = "File written successfully")
  }, error = function(e) {
    list(valid = FALSE, message = paste("Error validating written file:", e$message))
  })
}


