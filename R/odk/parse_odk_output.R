# ODK Output Parser for XLS-Validator
# Utilities for parsing and enhancing ODK tool output

#' Enhance ODK issues with row numbers from XLSForm
#' Maps field names/XPaths to actual row numbers in the spreadsheet
#' Uses pre-built name_index from xlsform_reader for fast lookups
#' @param issues Tibble of issues from ODK tools
#' @param xlsform_data List of data frames from xlsform_reader (with name_index)
#' @return Enhanced issues tibble with row numbers
enhance_issues_with_rows <- function(issues, xlsform_data) {
  if (nrow(issues) == 0 || is.null(xlsform_data)) {
    return(issues)
  }
  
  # Use pre-built index if available, otherwise build it
  field_row_map <- if (!is.null(xlsform_data$name_index)) {
    xlsform_data$name_index
  } else if (!is.null(xlsform_data$sheets)) {
    # Fallback: build from sheets (for backward compatibility)
    build_field_row_map(xlsform_data$sheets)
  } else {
    # Legacy: xlsform_data is the sheets list directly
    build_field_row_map(xlsform_data)
  }
  
  # Update issues that have field but no row
  for (i in seq_len(nrow(issues))) {
    if (!is.na(issues$field[i]) && is.na(issues$row[i])) {
      field <- issues$field[i]
      sheet <- issues$sheet[i]
      
      # Try to find row number using the index
      if (!is.na(sheet) && sheet %in% names(field_row_map)) {
        if (field %in% names(field_row_map[[sheet]])) {
          issues$row[i] <- field_row_map[[sheet]][[field]]
        }
      } else {
        # Search all sheets (survey first for performance)
        search_order <- c("survey", setdiff(names(field_row_map), "survey"))
        for (sheet_name in search_order) {
          if (field %in% names(field_row_map[[sheet_name]])) {
            issues$row[i] <- field_row_map[[sheet_name]][[field]]
            if (is.na(sheet)) {
              issues$sheet[i] <- sheet_name
            }
            break
          }
        }
      }
    }
    
    # Also try to extract field from message if field is NA
    if (is.na(issues$field[i]) && !is.na(issues$message[i])) {
      extracted_field <- extract_field_from_message(issues$message[i])
      if (!is.na(extracted_field)) {
        # Try to look up this field
        search_order <- c("survey", setdiff(names(field_row_map), "survey"))
        for (sheet_name in search_order) {
          if (extracted_field %in% names(field_row_map[[sheet_name]])) {
            issues$field[i] <- extracted_field
            issues$row[i] <- field_row_map[[sheet_name]][[extracted_field]]
            if (is.na(issues$sheet[i])) {
              issues$sheet[i] <- sheet_name
            }
            break
          }
        }
      }
    }
  }
  
  issues
}

#' Extract field name from error message
#' Looks for common patterns in pyxform/ODK Validate error messages
#' @param message Error message string
#' @return Extracted field name or NA
extract_field_from_message <- function(message) {
  if (is.na(message) || nchar(message) == 0) {
    return(NA_character_)
  }
  
  # Pattern 1: ${field_name} reference
  match <- regmatches(message, regexpr("\\$\\{([^}]+)\\}", message))
  if (length(match) > 0 && nchar(match) > 0) {
    return(gsub("^\\$\\{|\\}$", "", match))
  }
  
  # Pattern 2: 'field_name' in quotes (pyxform style)
  match <- regmatches(message, regexpr("'([a-zA-Z_][a-zA-Z0-9_]*)'", message))
  if (length(match) > 0 && nchar(match) > 0) {
    field <- gsub("^'|'$", "", match)
    # Filter out common non-field strings
    if (!field %in% c("survey", "choices", "settings", "name", "type", "label")) {
      return(field)
    }
  }
  
  # Pattern 3: XPath /data/field_name
  match <- regmatches(message, regexpr("/data/([a-zA-Z_][a-zA-Z0-9_]*)", message))
  if (length(match) > 0 && nchar(match) > 0) {
    return(gsub("^/data/", "", match))
  }
  
  NA_character_
}

#' Build a map of field names to row numbers (fallback for legacy data)
#' @param xlsform_sheets List of data frames (sheets only, not full xlsform_data)
#' @return Named list of lists (sheet -> field -> row)
build_field_row_map <- function(xlsform_sheets) {
  result <- list()
  
  for (sheet_name in names(xlsform_sheets)) {
    df <- xlsform_sheets[[sheet_name]]
    if (is.null(df) || nrow(df) == 0) next
    
    result[[sheet_name]] <- list()
    
    # Look for 'name' column (primary identifier in XLSForm)
    if ("name" %in% names(df)) {
      for (i in seq_len(nrow(df))) {
        name_val <- df$name[i]
        if (!is.na(name_val) && nchar(trimws(name_val)) > 0) {
          # Use .row_num if available, otherwise calculate
          row_num <- if (".row_num" %in% names(df)) df$.row_num[i] else (i + 1)
          result[[sheet_name]][[name_val]] <- row_num
        }
      }
    }
    
    # Also index by list_name for choices sheet
    if (sheet_name == "choices" && "list_name" %in% names(df)) {
      for (i in seq_len(nrow(df))) {
        list_name_val <- df$list_name[i]
        name_val <- if ("name" %in% names(df)) df$name[i] else NA
        if (!is.na(list_name_val) && !is.na(name_val)) {
          combined_key <- paste0(list_name_val, ":", name_val)
          row_num <- if (".row_num" %in% names(df)) df$.row_num[i] else (i + 1)
          result[[sheet_name]][[combined_key]] <- row_num
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



