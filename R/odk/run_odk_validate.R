# ODK Validate Wrapper for XLS-Validator
# Runs ODK Validate JAR to validate XForm XML

#' Run ODK Validate on an XForm XML file
#' @param xml_path Path to XForm XML file
#' @param config Application configuration
#' @return List with success, stdout, stderr, and issues
run_odk_validate <- function(xml_path, config = get_config()) {
  # Validate Java is available
  if (is.null(config$java_cmd)) {
    return(list(
      success = FALSE,
      stdout = "",
      stderr = "Java not found. Install Java 8+ to use ODK Validate.",
      issues = create_odk_validate_error_issue("Java not installed")
    ))
  }
  
  # Validate JAR exists
  if (is.null(config$odk_validate_jar) || !file.exists(config$odk_validate_jar)) {
    return(list(
      success = FALSE,
      stdout = "",
      stderr = "ODK_Validate.jar not found in tools folder.",
      issues = create_odk_validate_error_issue("ODK_Validate.jar not found")
    ))
  }
  
  # Validate XML file exists
  if (!file.exists(xml_path)) {
    return(list(
      success = FALSE,
      stdout = "",
      stderr = paste("XML file not found:", xml_path),
      issues = create_odk_validate_error_issue("XForm XML file not found")
    ))
  }
  
  # Run ODK Validate
  # Syntax: java -jar ODK_Validate.jar form.xml
  result <- tryCatch({
    processx::run(
      config$java_cmd,
      c("-jar", config$odk_validate_jar, xml_path),
      error_on_status = FALSE,
      timeout = 120,  # 2 minute timeout
      stderr_to_stdout = FALSE
    )
  }, error = function(e) {
    list(
      status = -1,
      stdout = "",
      stderr = paste("Failed to execute ODK Validate:", e$message)
    )
  })
  
  # ODK Validate returns 0 for valid forms, non-zero for invalid
  # But we also need to check for "Passes" in output
  success <- result$status == 0 || 
    grepl("Passes|passed|valid", result$stdout, ignore.case = TRUE)
  
  # Parse output for errors/warnings
  issues <- parse_odk_validate_output(result$stdout, result$stderr)
  
  list(
    success = success,
    stdout = result$stdout,
    stderr = result$stderr,
    issues = issues
  )
}

#' Parse ODK Validate output for errors and warnings
#' @param stdout Standard output from ODK Validate
#' @param stderr Standard error from ODK Validate
#' @return Tibble of validation issues
parse_odk_validate_output <- function(stdout, stderr) {
  issues <- create_empty_results()
  combined_output <- paste(stdout, stderr, sep = "\n")
  
  if (nchar(trimws(combined_output)) == 0) {
    return(issues)
  }
  
  # Check for success message
  if (grepl("Passes!|passes!|Form is valid", combined_output, ignore.case = TRUE)) {
    return(issues)  # No issues
  }
  
  lines <- strsplit(combined_output, "\n")[[1]]
  lines <- lines[nchar(trimws(lines)) > 0]
  
  issue_id <- 0
  
  for (line in lines) {
    # Skip info/header lines
    if (grepl("^(Validating|>>|ODK Validate|JavaRosa)", line)) next
    
    # Detect error level
    level <- "warning"
    if (grepl("error|Error|ERROR|Fatal|FATAL|failed|Failed", line)) {
      level <- "error"
    } else if (grepl("warning|Warning|WARNING", line)) {
      level <- "warning"
    } else if (grepl("info|Info|INFO|note|Note", line)) {
      level <- "info"
    }
    
    # Try to extract XPath reference (often contains field path)
    # Common patterns: /data/field_name, /html/body/select1[@ref='/data/field']
    xpath_match <- regmatches(line, regexpr("/[a-zA-Z0-9_/\\[\\]@=']+", line))
    field <- NA_character_
    if (length(xpath_match) > 0 && nchar(xpath_match) > 0) {
      # Extract field name from XPath
      field <- gsub("^.*/", "", xpath_match)
      field <- gsub("\\[.*\\]", "", field)
    }
    
    # ODK Validate doesn't typically give row numbers, just XPath
    # We'll need to map XPath to row in post-processing if needed
    
    issue_id <- issue_id + 1
    new_issue <- create_issue(
      id = issue_id,
      source = "odk",
      level = level,
      sheet = "survey",  # Most ODK Validate errors relate to survey
      row = NA_integer_,
      field = field,
      message = trimws(line),
      rule_id = "odk_validate"
    )
    
    issues <- dplyr::bind_rows(issues, new_issue)
  }
  
  issues
}

#' Create a single error issue for ODK Validate failures
#' @param message Error message
#' @return Single-row tibble
create_odk_validate_error_issue <- function(message) {
  create_issue(
    id = 1,
    source = "odk",
    level = "error",
    message = message,
    rule_id = "odk_validate"
  )
}


