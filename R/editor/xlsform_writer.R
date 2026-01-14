# XLSForm Writer for XLS-Validator
# Writes corrected XLSForm back to Excel file
# Supports format-preserving export using openxlsx when original file is available

#' Write XLSForm data to Excel file
#' @param xlsform_data List from read_xlsform (with or without applied changes)
#' @param output_path Path for output file
#' @param original_path Original file path (to preserve sheet order and formatting)
#' @param preserve_format If TRUE and openxlsx is available, preserve original formatting
#' @return Path to written file
write_xlsform <- function(xlsform_data, output_path, original_path = NULL, 
                          preserve_format = TRUE) {
  # Try format-preserving write if original exists and openxlsx is available
  if (preserve_format && !is.null(original_path) && file.exists(original_path)) {
    if (requireNamespace("openxlsx", quietly = TRUE)) {
      result <- tryCatch({
        write_xlsform_styled(xlsform_data, output_path, original_path)
        return(normalizePath(output_path, winslash = "/"))
      }, error = function(e) {
        warning("Format-preserving write failed, falling back to basic write: ", e$message)
        NULL
      })
      if (!is.null(result)) return(result)
    }
  }
  
  # Fallback to basic writexl (no formatting preservation)
  write_xlsform_basic(xlsform_data, output_path, original_path)
}

#' Write XLSForm with style preservation using openxlsx
#' Preserves colors, fonts, column widths, and other formatting from original
#' @param xlsform_data List from read_xlsform
#' @param output_path Path for output file
#' @param original_path Original file path (required)
#' @return Path to written file
write_xlsform_styled <- function(xlsform_data, output_path, original_path) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("openxlsx package is required for format-preserving export")
  }
  
  # Load original workbook to preserve styles
  wb <- openxlsx::loadWorkbook(original_path)
  original_sheets <- openxlsx::sheets(wb)
  
  # Update each sheet with new data while keeping formatting
  for (sheet_name in original_sheets) {
    sheet_key <- tolower(sheet_name)
    
    if (sheet_key %in% names(xlsform_data$sheets)) {
      df <- xlsform_data$sheets[[sheet_key]]
      
      # Remove internal columns before writing
      internal_cols <- names(df)[startsWith(names(df), ".")]
      if (length(internal_cols) > 0) {
        df <- df[, !names(df) %in% internal_cols, drop = FALSE]
      }
      
      if (nrow(df) > 0 && ncol(df) > 0) {
        # Clear existing data (but keep header row formatting)
        # Write data starting from row 2 (after header)
        openxlsx::writeData(
          wb, 
          sheet = sheet_name, 
          x = df, 
          startRow = 2, 
          startCol = 1,
          colNames = FALSE,
          keepNA = FALSE,
          na.string = ""
        )
        
        # Update headers if columns changed
        openxlsx::writeData(
          wb,
          sheet = sheet_name,
          x = as.data.frame(t(names(df))),
          startRow = 1,
          startCol = 1,
          colNames = FALSE
        )
      }
    }
  }
  
  # Add any new sheets that weren't in original
  new_sheets <- setdiff(names(xlsform_data$sheets), tolower(original_sheets))
  for (sheet_key in new_sheets) {
    df <- xlsform_data$sheets[[sheet_key]]
    
    # Remove internal columns
    internal_cols <- names(df)[startsWith(names(df), ".")]
    if (length(internal_cols) > 0) {
      df <- df[, !names(df) %in% internal_cols, drop = FALSE]
    }
    
    if (ncol(df) > 0) {
      # Add new sheet with default styling
      openxlsx::addWorksheet(wb, sheetName = sheet_key)
      openxlsx::writeData(wb, sheet = sheet_key, x = df, startRow = 1, colNames = TRUE)
      
      # Apply basic header styling
      header_style <- openxlsx::createStyle(
        textDecoration = "bold",
        fgFill = "#D9E1F2",
        border = "Bottom"
      )
      openxlsx::addStyle(wb, sheet = sheet_key, style = header_style, 
                         rows = 1, cols = 1:ncol(df), gridExpand = TRUE)
    }
  }
  
  # Save workbook
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  
  normalizePath(output_path, winslash = "/")
}

#' Basic XLSForm write without formatting (using writexl)
#' @param xlsform_data List from read_xlsform
#' @param output_path Path for output file
#' @param original_path Original file path (for sheet order)
#' @return Path to written file
write_xlsform_basic <- function(xlsform_data, output_path, original_path = NULL) {
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


