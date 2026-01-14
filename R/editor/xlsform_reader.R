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
    row_counts = list(),
    name_index = list()
  )
  
  # Read each sheet
  for (i in seq_along(sheets)) {
    sheet_name <- sheets[i]
    sheet_key <- tolower(sheet_name)
    
    tryCatch({
      df <- readxl::read_excel(
        file_path, 
        sheet = sheet_name,
        col_types = "text",
        .name_repair = "minimal"
      )
      
      # Convert to data frame
      df <- as.data.frame(df, stringsAsFactors = FALSE)
      
      # Add row numbers (1-based, excluding header)
      if (nrow(df) > 0) {
        df$.row_num <- seq_len(nrow(df)) + 1
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
  
  # Build name-to-row index for fast lookups
  result$name_index <- build_name_index(result)
  
  result
}

#' Build a name-to-row index for fast field lookups
#' Maps field names to their Excel row numbers for accurate navigation
#' @param xlsform_data List from read_xlsform (partially built)
#' @return Named list of indices per sheet
build_name_index <- function(xlsform_data) {
  index <- list()
  
  for (sheet_name in names(xlsform_data$sheets)) {
    df <- xlsform_data$sheets[[sheet_name]]
    if (is.null(df) || nrow(df) == 0) next
    
    sheet_index <- list()
    
    # Index by 'name' column (primary identifier in XLSForm)
    if ("name" %in% names(df)) {
      for (i in seq_len(nrow(df))) {
        name_val <- df$name[i]
        if (!is.na(name_val) && nchar(trimws(name_val)) > 0) {
          # Use .row_num if available, otherwise calculate
          row_num <- if (".row_num" %in% names(df)) df$.row_num[i] else (i + 1)
          sheet_index[[name_val]] <- row_num
        }
      }
    }
    
    # For choices sheet, also index by list_name:name combination
    if (sheet_name == "choices" && "list_name" %in% names(df)) {
      for (i in seq_len(nrow(df))) {
        list_name_val <- df$list_name[i]
        name_val <- if ("name" %in% names(df)) df$name[i] else NA
        if (!is.na(list_name_val) && nchar(trimws(list_name_val)) > 0) {
          row_num <- if (".row_num" %in% names(df)) df$.row_num[i] else (i + 1)
          # Index by list_name alone (first occurrence)
          if (!list_name_val %in% names(sheet_index)) {
            sheet_index[[list_name_val]] <- row_num
          }
          # Index by list_name:name combination
          if (!is.na(name_val) && nchar(trimws(name_val)) > 0) {
            combined_key <- paste0(list_name_val, ":", name_val)
            sheet_index[[combined_key]] <- row_num
          }
        }
      }
    }
    
    if (length(sheet_index) > 0) {
      index[[sheet_name]] <- sheet_index
    }
  }
  
  index
}

#' Look up row number by field name
#' @param xlsform_data List from read_xlsform
#' @param field_name Name of the field to look up
#' @param sheet_name Optional sheet name to search in (searches all if NULL)
#' @return List with row (integer) and sheet (character), or NULL if not found
lookup_field_row <- function(xlsform_data, field_name, sheet_name = NULL) {
  if (is.null(xlsform_data$name_index) || is.na(field_name) || nchar(field_name) == 0) {
    return(NULL)
  }
  
  # If sheet specified, search only that sheet
  if (!is.null(sheet_name) && sheet_name %in% names(xlsform_data$name_index)) {
    if (field_name %in% names(xlsform_data$name_index[[sheet_name]])) {
      return(list(
        row = xlsform_data$name_index[[sheet_name]][[field_name]],
        sheet = sheet_name
      ))
    }
    return(NULL)
  }
  
  # Search all sheets (survey first, then others)
  search_order <- c("survey", setdiff(names(xlsform_data$name_index), "survey"))
  
  for (sheet in search_order) {
    if (sheet %in% names(xlsform_data$name_index)) {
      if (field_name %in% names(xlsform_data$name_index[[sheet]])) {
        return(list(
          row = xlsform_data$name_index[[sheet]][[field_name]],
          sheet = sheet
        ))
      }
    }
  }
  
  NULL
}

#' Get all field names from the index
#' @param xlsform_data List from read_xlsform
#' @param sheet_name Optional sheet name to get fields from
#' @return Character vector of field names
get_indexed_fields <- function(xlsform_data, sheet_name = NULL) {
  if (is.null(xlsform_data$name_index)) {
    return(character())
  }
  
  if (!is.null(sheet_name)) {
    if (sheet_name %in% names(xlsform_data$name_index)) {
      return(names(xlsform_data$name_index[[sheet_name]]))
    }
    return(character())
  }
  
  unique(unlist(lapply(xlsform_data$name_index, names)))
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

# =============================================================================
# Pagination / Lazy Loading Functions
# =============================================================================

#' Get paginated sheet data for display
#' Useful for large forms to avoid loading everything into the UI
#' @param xlsform_data List from read_xlsform
#' @param sheet_name Sheet name
#' @param page Page number (1-based)
#' @param page_size Number of rows per page
#' @param include_row_nums Include row number column
#' @return List with data, total_rows, total_pages, current_page
get_sheet_paginated <- function(xlsform_data, sheet_name, page = 1, 
                                page_size = 100, include_row_nums = TRUE) {
  df <- get_sheet(xlsform_data, sheet_name)
  
  total_rows <- nrow(df)
  total_pages <- ceiling(total_rows / page_size)
  
  if (total_rows == 0) {
    return(list(
      data = df,
      total_rows = 0,
      total_pages = 0,
      current_page = 1,
      page_size = page_size,
      start_row = 0,
      end_row = 0
    ))
  }
  
  # Ensure page is within bounds
  page <- max(1, min(page, total_pages))
  
  # Calculate row range
  start_row <- (page - 1) * page_size + 1
  end_row <- min(page * page_size, total_rows)
  
  # Slice data
  page_data <- df[start_row:end_row, , drop = FALSE]
  
  # Format for display
  if (include_row_nums && ".row_num" %in% names(page_data)) {
    row_nums <- page_data$.row_num
    page_data$.row_num <- NULL
    page_data <- cbind(Row = row_nums, page_data)
  } else {
    # Remove internal columns
    internal_cols <- names(page_data)[startsWith(names(page_data), ".")]
    page_data[internal_cols] <- NULL
  }
  
  list(
    data = page_data,
    total_rows = total_rows,
    total_pages = total_pages,
    current_page = page,
    page_size = page_size,
    start_row = start_row,
    end_row = end_row
  )
}

#' Get rows around a specific row (for context when navigating to issues)
#' @param xlsform_data List from read_xlsform
#' @param sheet_name Sheet name
#' @param target_row Excel row number to center on
#' @param context_rows Number of rows to show before and after
#' @param include_row_nums Include row number column
#' @return List with data, start_row, end_row, target_index
get_rows_around <- function(xlsform_data, sheet_name, target_row, 
                           context_rows = 10, include_row_nums = TRUE) {
  df <- get_sheet(xlsform_data, sheet_name)
  
  if (nrow(df) == 0) {
    return(list(
      data = df,
      start_row = 0,
      end_row = 0,
      target_index = NA
    ))
  }
  
  # Convert Excel row to data frame row
  target_df_row <- target_row - 1
  
  # Calculate range
  start_df_row <- max(1, target_df_row - context_rows)
  end_df_row <- min(nrow(df), target_df_row + context_rows)
  
  # Slice data
  context_data <- df[start_df_row:end_df_row, , drop = FALSE]
  
  # Calculate target index within the slice
  target_index <- target_df_row - start_df_row + 1
  
  # Format for display
  if (include_row_nums && ".row_num" %in% names(context_data)) {
    row_nums <- context_data$.row_num
    context_data$.row_num <- NULL
    context_data <- cbind(Row = row_nums, context_data)
  } else {
    internal_cols <- names(context_data)[startsWith(names(context_data), ".")]
    context_data[internal_cols] <- NULL
  }
  
  list(
    data = context_data,
    start_row = start_df_row + 1,
    end_row = end_df_row + 1,
    target_index = target_index
  )
}

#' Check if a form is considered "large" and should use pagination
#' @param xlsform_data List from read_xlsform
#' @param threshold Number of total rows to consider "large"
#' @return Logical
is_large_form <- function(xlsform_data, threshold = 500) {
  total_rows <- sum(unlist(xlsform_data$row_counts))
  total_rows > threshold
}

#' Get form size summary
#' @param xlsform_data List from read_xlsform
#' @return List with row counts and total
get_form_size <- function(xlsform_data) {
  list(
    by_sheet = xlsform_data$row_counts,
    total = sum(unlist(xlsform_data$row_counts)),
    sheets = length(xlsform_data$sheets)
  )
}

#' Read sheet with lazy loading (only headers initially)
#' For very large forms, this allows showing structure without loading all data
#' @param file_path Path to XLSForm Excel file
#' @param sheet_name Sheet name
#' @param max_preview_rows Maximum rows to load for preview
#' @return List with columns, preview_data, total_rows
read_sheet_lazy <- function(file_path, sheet_name, max_preview_rows = 50) {
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  
  # Read just the first few rows to get structure
  preview <- tryCatch({
    readxl::read_excel(
      file_path,
      sheet = sheet_name,
      col_types = "text",
      n_max = max_preview_rows,
      .name_repair = "minimal"
    )
  }, error = function(e) {
    NULL
  })
  
  if (is.null(preview)) {
    return(list(
      columns = character(),
      preview_data = data.frame(),
      total_rows = 0,
      is_preview = TRUE
    ))
  }
  
  # Get total row count (this is fast even for large files)
  # Note: This reads the whole column but is still faster than reading all columns
  total_rows <- tryCatch({
    nrow(readxl::read_excel(
      file_path,
      sheet = sheet_name,
      col_types = "text",
      range = readxl::cell_cols(1),
      .name_repair = "minimal"
    ))
  }, error = function(e) {
    nrow(preview)
  })
  
  list(
    columns = names(preview),
    preview_data = as.data.frame(preview, stringsAsFactors = FALSE),
    total_rows = total_rows,
    is_preview = total_rows > max_preview_rows
  )
}


