# Pyxform Wrapper for XLS-Validator
# Converts XLSForm (XLSX) to XForm (XML) using pyxform

#' Run pyxform to convert XLSForm to XForm
#' @param xlsform_path Path to XLSForm Excel file
#' @param output_path Path for output XML (optional, auto-generated if NULL)
#' @param config Application configuration
#' @return List with success, xml_path, stdout, stderr, and issues
run_pyxform <- function(xlsform_path, output_path = NULL, config = get_config()) {
  # Validate pyxform is available
  if (is.null(config$pyxform_cmd)) {
    return(list(
      success = FALSE,
      xml_path = NULL,
      stdout = "",
      stderr = "pyxform not found. Install with: pip install pyxform",
      issues = create_pyxform_error_issue("pyxform not installed")
    ))
  }
  
  # Generate output path if not provided
  if (is.null(output_path)) {
    temp_dir <- ensure_temp_dir(config)
    base_name <- tools::file_path_sans_ext(basename(xlsform_path))
    output_path <- file.path(temp_dir, paste0(base_name, ".xml"))
  }
  
  # Build command arguments
  # pyxform / xls2xform syntax: xls2xform input.xlsx output.xml
  # Using --skip_validate to bypass Java requirement for pyxform validation
  # Note: Custom validation rules and ODK Validate (if Java installed) will still run
  if (config$pyxform_cmd == "python -m pyxform") {
    cmd <- "python"
    args <- c("-m", "pyxform", xlsform_path, output_path, "--skip_validate")
  } else {
    cmd <- config$pyxform_cmd
    args <- c(xlsform_path, output_path, "--skip_validate")
  }
  
  # Run pyxform
  result <- tryCatch({
    processx::run(
      cmd,
      args,
      error_on_status = FALSE,
      timeout = 120  # 2 minute timeout
    )
  }, error = function(e) {
    list(
      status = -1,
      stdout = "",
      stderr = paste("Failed to execute pyxform:", e$message)
    )
  })
  
  # Check if conversion succeeded
  success <- result$status == 0 && file.exists(output_path)
  
  # Parse any errors/warnings from output
  issues <- parse_pyxform_output(result$stdout, result$stderr, xlsform_path)
  
  list(
    success = success,
    xml_path = if (success) normalizePath(output_path, winslash = "/") else NULL,
    stdout = result$stdout,
    stderr = result$stderr,
    issues = issues
  )
}

#' Parse pyxform output for errors and warnings
#' @param stdout Standard output from pyxform
#' @param stderr Standard error from pyxform
#' @param xlsform_path Original XLSForm path (for context)
#' @return Tibble of validation issues
parse_pyxform_output <- function(stdout, stderr, xlsform_path) {
  issues <- create_empty_results()
  combined_output <- paste(stdout, stderr, sep = "\n")
  
  if (nchar(trimws(combined_output)) == 0) {
    return(issues)
  }
  
  lines <- strsplit(combined_output, "\n")[[1]]
  lines <- lines[nchar(trimws(lines)) > 0]
  
  issue_id <- 0
  
  for (line in lines) {
    # Skip non-error lines
    if (grepl("^(Traceback|File |\\s+)", line)) next
    
    # Skip header-only lines (just labels without content)
    if (grepl("^(Warnings?:|Errors?:|Notes?:|Info:)\\s*$", line, ignore.case = TRUE)) next
    
    # Detect error level
    level <- "info"
    if (grepl("error|Error|ERROR|exception|Exception", line, ignore.case = FALSE)) {
      level <- "error"
    } else if (grepl("warning|Warning|WARNING", line, ignore.case = FALSE)) {
      level <- "warning"
    }
    
    # Try to extract row number from pyxform messages
    # Common patterns: "row X", "[row : X]", "on row X", "at row X", "row: X"
    row_match <- regmatches(line, regexpr("(row|Row|ROW)\\s*:?\\s*(\\d+)", line))
    row_num <- NA_integer_
    if (length(row_match) > 0 && nchar(row_match) > 0) {
      row_num <- as.integer(gsub("\\D", "", row_match))
    }
    
    # Try to extract sheet name
    sheet <- NA_character_
    if (grepl("survey", line, ignore.case = TRUE)) sheet <- "survey"
    else if (grepl("choices", line, ignore.case = TRUE)) sheet <- "choices"
    else if (grepl("settings", line, ignore.case = TRUE)) sheet <- "settings"
    
    # Try to extract field name
    # Look for patterns like: the 'name' value, 'field_name' column, field: field_name
    field <- NA_character_
    
    # Pattern 1: "the 'X' value" or "'X' column" (pyxform style)
    field_pattern <- regexpr("the '([^']+)' (value|column|field)", line)
    if (field_pattern > 0) {
      field_match <- regmatches(line, field_pattern)
      field <- gsub("^the '|' (value|column|field)$", "", field_match)
    }
    
    # Pattern 2: fallback to quoted strings, skip sheet names
    if (is.na(field) || nchar(field) == 0) {
      all_quoted <- regmatches(line, gregexpr("'[^']+'", line))[[1]]
      # Filter out sheet names only
      sheet_names <- c("'survey'", "'choices'", "'settings'")
      field_candidates <- all_quoted[!tolower(all_quoted) %in% sheet_names]
      if (length(field_candidates) > 0) {
        field <- gsub("^'|'$", "", field_candidates[1])
      }
    }
    
    issue_id <- issue_id + 1
    new_issue <- create_issue(
      id = issue_id,
      source = "odk",
      level = level,
      sheet = sheet,
      row = row_num,
      field = field,
      message = trimws(line),
      rule_id = "pyxform_conversion"
    )
    
    issues <- dplyr::bind_rows(issues, new_issue)
  }
  
  issues
}

#' Create a single error issue for pyxform failures
#' @param message Error message
#' @return Single-row tibble
create_pyxform_error_issue <- function(message) {
  create_issue(
    id = 1,
    source = "odk",
    level = "error",
    message = message,
    rule_id = "pyxform_conversion"
  )
}

