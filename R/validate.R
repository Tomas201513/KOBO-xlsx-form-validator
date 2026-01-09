# Main Validation Orchestrator for XLS-Validator
# Coordinates the entire validation pipeline

#' Run full validation on an XLSForm
#' @param xlsform_path Path to XLSForm Excel file
#' @param run_odk Run ODK validation (pyxform + ODK Validate)
#' @param run_custom Run custom R validation rules
#' @param config Application configuration
#' @return List with results, xlsform_data, and metadata
validate_xlsform <- function(
  xlsform_path,
  run_odk = TRUE,
  run_custom = TRUE,
  config = get_config()
) {
  results <- list(
    success = TRUE,
    xlsform_path = xlsform_path,
    xlsform_data = NULL,
    issues = create_empty_results(),
    odk_output = list(),
    summary = list(),
    timestamp = Sys.time()
  )
  
  # Step 1: Validate file
  file_check <- validate_upload(xlsform_path, config)
  if (!file_check$valid) {
    results$success <- FALSE
    results$issues <- create_issue(
      id = 1,
      source = "odk",
      level = "error",
      message = file_check$message,
      rule_id = "file_validation"
    )
    return(results)
  }
  
  # Step 2: Read XLSForm
  tryCatch({
    results$xlsform_data <- read_xlsform(xlsform_path, config)
  }, error = function(e) {
    results$success <- FALSE
    results$issues <- create_issue(
      id = 1,
      source = "odk",
      level = "error",
      message = paste("Failed to read XLSForm:", e$message),
      rule_id = "file_read"
    )
    return(results)
  })
  
  if (is.null(results$xlsform_data)) {
    return(results)
  }
  
  # Step 3: Check XLSForm structure
  structure_check <- is_valid_xlsform(xlsform_path)
  if (!structure_check$valid) {
    results$success <- FALSE
    results$issues <- dplyr::bind_rows(
      results$issues,
      create_issue(
        id = 1,
        source = "odk",
        level = "error",
        message = structure_check$message,
        rule_id = "xlsform_structure"
      )
    )
    return(results)
  }
  
  all_issues <- create_empty_results()
  
  # Step 4: Run ODK validation (pyxform + ODK Validate)
  if (run_odk) {
    odk_issues <- run_odk_validation(xlsform_path, config)
    results$odk_output <- odk_issues$output
    
    if (nrow(odk_issues$issues) > 0) {
      all_issues <- dplyr::bind_rows(all_issues, odk_issues$issues)
    }
  }
  
  # Step 5: Run custom validation rules
  if (run_custom) {
    custom_issues <- run_custom_rules(results$xlsform_data$sheets)
    
    if (nrow(custom_issues) > 0) {
      all_issues <- dplyr::bind_rows(all_issues, custom_issues)
    }
  }
  
  # Step 6: Enhance issues with row numbers
  if (nrow(all_issues) > 0) {
    all_issues <- enhance_issues_with_rows(all_issues, results$xlsform_data$sheets)
  }
  
  # Step 7: Combine and sort results
  results$issues <- combine_results(all_issues)
  
  # Step 8: Generate summary
  results$summary <- summarize_results(results$issues)
  
  # Determine overall success
  results$success <- results$summary$errors == 0
  
  results
}

#' Run ODK validation (pyxform conversion + ODK Validate)
#' @param xlsform_path Path to XLSForm
#' @param config Configuration
#' @return List with issues and output
run_odk_validation <- function(xlsform_path, config = get_config()) {
  all_issues <- create_empty_results()
  output <- list(
    pyxform = list(),
    odk_validate = list()
  )
  
  # Step 1: Run pyxform (XLS -> XML)
  pyxform_result <- run_pyxform(xlsform_path, config = config)
  output$pyxform <- pyxform_result
  
  if (nrow(pyxform_result$issues) > 0) {
    all_issues <- dplyr::bind_rows(all_issues, pyxform_result$issues)
  }
  
  # Step 2: If conversion succeeded, run ODK Validate
  if (pyxform_result$success && !is.null(pyxform_result$xml_path)) {
    odk_result <- run_odk_validate(pyxform_result$xml_path, config = config)
    output$odk_validate <- odk_result
    
    if (nrow(odk_result$issues) > 0) {
      all_issues <- dplyr::bind_rows(all_issues, odk_result$issues)
    }
  }
  
  list(
    issues = all_issues,
    output = output
  )
}

#' Quick validation check (returns boolean)
#' @param xlsform_path Path to XLSForm
#' @param config Configuration
#' @return TRUE if form is valid, FALSE otherwise
is_form_valid <- function(xlsform_path, config = get_config()) {
  result <- validate_xlsform(xlsform_path, config = config)
  result$success && result$summary$errors == 0
}

#' Get validation errors only
#' @param xlsform_path Path to XLSForm
#' @param config Configuration
#' @return Tibble of error-level issues only
get_errors_only <- function(xlsform_path, config = get_config()) {
  result <- validate_xlsform(xlsform_path, config = config)
  result$issues[result$issues$level == "error", ]
}

#' Re-validate after corrections
#' @param xlsform_data Updated XLSForm data (with changes applied)
#' @param tracker Change tracker
#' @param config Configuration
#' @return Validation results
revalidate_xlsform <- function(xlsform_data, tracker, config = get_config()) {
  # Create temp file with changes
  download_info <- prepare_download(xlsform_data, tracker, config)
  
  # Run validation on temp file
  validate_xlsform(download_info$path, config = config)
}


