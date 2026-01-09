# XLSForm Reader for XLS-Validator
# Reads XLSForm Excel files into editable data structures

#' Read XLSForm into a list of data frames
#' @param file_path Path to XLSForm Excel file
#' @param config Application configuration
#' @return List with survey, choices, settings data frames and metadata
read_xlsform <- function(file_path, config = get_config()) {
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  
  # Get sheet names
  sheets <- readxl::excel_sheets(file_path)
  sheets_lower <- tolower(sheets)
  
  result <- list(
    file_path = normalizePath(file_path, winslash = "/"),
    file_name = basename(file_path),
    sheets = list(),
    sheet_names = sheets,
    row_counts = list()
  )
  
  # Read each sheet
  for (i in seq_along(sheets)) {
    sheet_name <- sheets[i]
    sheet_key <- tolower(sheet_name)
    
    tryCatch({
      df <- readxl::read_excel(
        file_path, 
        sheet = sheet_name,
        col_types = "text",  # Read all as text to preserve formatting
        .name_repair = "minimal"
      )
      
      # Convert to data frame
      df <- as.data.frame(df, stringsAsFactors = FALSE)
      
      # Add row numbers (1-based, excluding header)
      if (nrow(df) > 0) {
        df$.row_num <- seq_len(nrow(df)) + 1  # +1 for header row in Excel
      } else {
        df$.row_num <- integer()
      }
      
      result$sheets[[sheet_key]] <- df
      result$row_counts[[sheet_key]] <- nrow(df)
      
    }, error = function(e) {
      warning(paste("Failed to read sheet:", sheet_name, "-", e$message))
      result$sheets[[sheet_key]] <- data.frame()
      result$row_counts[[sheet_key]] <- 0
    })
  }
  
  # Ensure standard sheets exist (even if empty)
  for (standard_sheet in c("survey", "choices", "settings")) {
    if (!standard_sheet %in% names(result$sheets)) {
      result$sheets[[standard_sheet]] <- data.frame()
      result$row_counts[[standard_sheet]] <- 0
    }
  }
  
  result
}

#' Get a specific sheet from XLSForm data
#' @param xlsform_data List from read_xlsform
#' @param sheet_name Sheet name (case insensitive)
#' @return Data frame for the sheet or empty data frame
get_sheet <- function(xlsform_data, sheet_name) {
  sheet_key <- tolower(sheet_name)
  if (sheet_key %in% names(xlsform_data$sheets)) {
    return(xlsform_data$sheets[[sheet_key]])
  }
  data.frame()
}

#' Get column names for a sheet
#' @param xlsform_data List from read_xlsform
#' @param sheet_name Sheet name
#' @return Character vector of column names (excluding internal columns)
get_sheet_columns <- function(xlsform_data, sheet_name) {
  df <- get_sheet(xlsform_data, sheet_name)
  cols <- names(df)
  # Remove internal columns
  cols[!startsWith(cols, ".")]
}

#' Get a cell value
#' @param xlsform_data List from read_xlsform
#' @param sheet_name Sheet name
#' @param row Row number (Excel row, including header = row 1)
#' @param column Column name
#' @return Cell value as string
get_cell_value <- function(xlsform_data, sheet_name, row, column) {
  df <- get_sheet(xlsform_data, sheet_name)
  
  # Convert Excel row to data frame row (subtract 1 for header)
  df_row <- row - 1
  
  if (df_row < 1 || df_row > nrow(df)) {
    return(NA_character_)
  }
  
  if (!column %in% names(df)) {
    return(NA_character_)
  }
  
  as.character(df[df_row, column])
}

#' Set a cell value (returns new data, doesn't modify in place)
#' @param xlsform_data List from read_xlsform
#' @param sheet_name Sheet name
#' @param row Row number (Excel row)
#' @param column Column name
#' @param value New value
#' @return Updated xlsform_data
set_cell_value <- function(xlsform_data, sheet_name, row, column, value) {
  sheet_key <- tolower(sheet_name)
  
  if (!sheet_key %in% names(xlsform_data$sheets)) {
    warning(paste("Sheet not found:", sheet_name))
    return(xlsform_data)
  }
  
  # Convert Excel row to data frame row
  df_row <- row - 1
  
  if (df_row < 1 || df_row > nrow(xlsform_data$sheets[[sheet_key]])) {
    warning(paste("Row out of range:", row))
    return(xlsform_data)
  }
  
  if (!column %in% names(xlsform_data$sheets[[sheet_key]])) {
    warning(paste("Column not found:", column))
    return(xlsform_data)
  }
  
  xlsform_data$sheets[[sheet_key]][df_row, column] <- as.character(value)
  
  xlsform_data
}

#' Get sheet as display-ready data frame (for UI)
#' @param xlsform_data List from read_xlsform
#' @param sheet_name Sheet name
#' @param include_row_nums Include row number column
#' @return Data frame ready for display
get_sheet_for_display <- function(xlsform_data, sheet_name, include_row_nums = TRUE) {
  df <- get_sheet(xlsform_data, sheet_name)
  
  if (nrow(df) == 0) {
    return(df)
  }
  
  if (include_row_nums && ".row_num" %in% names(df)) {
    # Move row_num to first column and rename
    row_nums <- df$.row_num
    df$.row_num <- NULL
    df <- cbind(Row = row_nums, df)
  } else {
    # Remove internal columns
    internal_cols <- names(df)[startsWith(names(df), ".")]
    df[internal_cols] <- NULL
  }
  
  df
}

#' Get available sheet names
#' @param xlsform_data List from read_xlsform
#' @return Character vector of sheet names
get_available_sheets <- function(xlsform_data) {
  names(xlsform_data$sheets)
}


