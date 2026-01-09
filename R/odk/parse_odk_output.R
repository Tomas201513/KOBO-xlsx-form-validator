# ODK Output Parser for XLS-Validator
# Utilities for parsing and enhancing ODK tool output

#' Enhance ODK issues with row numbers from XLSForm
#' Maps field names/XPaths to actual row numbers in the spreadsheet
#' @param issues Tibble of issues from ODK tools
#' @param xlsform_data List of data frames from xlsform_reader
#' @return Enhanced issues tibble with row numbers
enhance_issues_with_rows <- function(issues, xlsform_data) {
  if (nrow(issues) == 0 || is.null(xlsform_data)) {
    return(issues)
  }
  
  # Build a lookup of field names to row numbers
  field_row_map <- build_field_row_map(xlsform_data)
  
  # Update issues that have field but no row
  for (i in seq_len(nrow(issues))) {
    if (!is.na(issues$field[i]) && is.na(issues$row[i])) {
      field <- issues$field[i]
      sheet <- issues$sheet[i]
      
      # Try to find row number
      if (!is.na(sheet) && sheet %in% names(field_row_map)) {
        if (field %in% names(field_row_map[[sheet]])) {
          issues$row[i] <- field_row_map[[sheet]][[field]]
        }
      } else {
        # Search all sheets
        for (sheet_name in names(field_row_map)) {
          if (field %in% names(field_row_map[[sheet_name]])) {
            issues$row[i] <- field_row_map[[sheet_name]][[field]]
            issues$sheet[i] <- sheet_name
            break
          }
        }
      }
    }
  }
  
  issues
}

#' Build a map of field names to row numbers
#' @param xlsform_data List of data frames from xlsform_reader
#' @return Named list of lists (sheet -> field -> row)
build_field_row_map <- function(xlsform_data) {
  result <- list()
  
  for (sheet_name in names(xlsform_data)) {
    df <- xlsform_data[[sheet_name]]
    if (is.null(df) || nrow(df) == 0) next
    
    result[[sheet_name]] <- list()
    
    # Look for 'name' column (primary identifier in XLSForm)
    if ("name" %in% names(df)) {
      for (i in seq_len(nrow(df))) {
        name_val <- df$name[i]
        if (!is.na(name_val) && nchar(trimws(name_val)) > 0) {
          # Row number is 1-based (add 1 for header row)
          result[[sheet_name]][[name_val]] <- i + 1
        }
      }
    }
    
    # Also index by list_name for choices sheet
    if (sheet_name == "choices" && "list_name" %in% names(df)) {
      for (i in seq_len(nrow(df))) {
        list_name_val <- df$list_name[i]
        name_val <- df$name[i]
        if (!is.na(list_name_val) && !is.na(name_val)) {
          combined_key <- paste0(list_name_val, ":", name_val)
          result[[sheet_name]][[combined_key]] <- i + 1
        }
      }
    }
  }
  
  result
}

#' Extract field name from XPath expression
#' @param xpath XPath string like /data/field_name or /html/body/input[@ref='/data/field']
#' @return Extracted field name or NA
extract_field_from_xpath <- function(xpath) {
  if (is.na(xpath) || nchar(trimws(xpath)) == 0) {
    return(NA_character_)
  }
  
  # Try to extract from @ref attribute
  ref_match <- regmatches(xpath, regexpr("@ref=['\"][^'\"]+['\"]", xpath))
  if (length(ref_match) > 0 && nchar(ref_match) > 0) {
    xpath <- gsub("@ref=['\"]|['\"]$", "", ref_match)
  }
  
  # Get last segment of path
  segments <- strsplit(xpath, "/")[[1]]
  segments <- segments[nchar(segments) > 0]
  
  if (length(segments) == 0) return(NA_character_)
  
  last_segment <- segments[length(segments)]
  
  # Remove any predicates [...]
  last_segment <- gsub("\\[.*\\]", "", last_segment)
  
  # Remove data/ prefix if present
  last_segment <- gsub("^data$", "", last_segment)
  
  if (nchar(last_segment) == 0 && length(segments) > 1) {
    last_segment <- segments[length(segments) - 1]
  }
  
  if (nchar(last_segment) == 0) NA_character_ else last_segment
}

#' Clean and standardize error messages
#' @param message Raw error message
#' @return Cleaned message
clean_error_message <- function(message) {
  if (is.na(message)) return(NA_character_)
  
  # Remove file paths
  message <- gsub("C:[\\\\][^\\s]+|/[^\\s]+\\.(xml|xlsx|xls)", "[file]", message)
  
  # Remove excessive whitespace
  message <- gsub("\\s+", " ", message)
  
  # Trim
  message <- trimws(message)
  
  message
}

#' Deduplicate issues that may appear from multiple sources
#' @param issues Tibble of validation issues
#' @return Deduplicated tibble
deduplicate_issues <- function(issues) {
  if (nrow(issues) == 0) return(issues)
  
  # Create a key for deduplication
  issues <- issues |>
    dplyr::mutate(
      .dedup_key = paste(sheet, row, field, level, sep = "|")
    ) |>
    dplyr::distinct(.dedup_key, .keep_all = TRUE) |>
    dplyr::select(-.dedup_key)
  
  # Renumber IDs
  issues$id <- seq_len(nrow(issues))
  
  issues
}


