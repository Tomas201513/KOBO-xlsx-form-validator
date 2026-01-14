# Custom Rule: Cross-Sheet Reference Validation
# Validates that all list_name references in survey exist in the choices sheet.
# Also reports orphaned choice lists that are never referenced.

# =============================================================================
# check_cross_sheet_refs()
# - Validates select_one/select_multiple references:
#     * list_name in type column must exist in choices sheet
# - Reports orphaned choice lists (defined but never used)
# - Produces a log data frame with columns:
#     id, source, level, sheet, row, field, message, rule_id
# =============================================================================
check_cross_sheet_refs <- function(survey_df,
                                   choices_df,
                                   header_rows = 1L,
                                   sheet_name = "survey",
                                   report_orphans = TRUE,
                                   verbose = TRUE) {
  
  # ---------- Basic validation ----------
  if (!"type" %in% names(survey_df)) {
    if (verbose) message("No 'type' column found in survey.")
    return(list(valid = TRUE, log = data.frame(
      id=integer(0), source=character(0), level=character(0),
      sheet=character(0), row=integer(0), field=character(0),
      message=character(0), rule_id=character(0)
    )))
  }
  
  if (!"list_name" %in% names(choices_df)) {
    if (verbose) message("No 'list_name' column found in choices.")
    return(list(valid = TRUE, log = data.frame(
      id=integer(0), source=character(0), level=character(0),
      sheet=character(0), row=integer(0), field=character(0),
      message=character(0), rule_id=character(0)
    )))
  }
  
  # Normalize to character
  survey_df$type <- if (is.factor(survey_df$type)) as.character(survey_df$type) else survey_df$type
  choices_df$list_name <- if (is.factor(choices_df$list_name)) as.character(choices_df$list_name) else choices_df$list_name
  
  # ---------- Extract list_name from type ----------
  parse_list_name_from_type <- function(type_str) {
    if (is.na(type_str) || type_str == "") return(NA_character_)
    t <- gsub("\\s+", " ", trimws(type_str))
    if (grepl("^select_one\\b", t)) {
      # Handle select_one with or_other
      list_nm <- sub("^select_one\\s+", "", t)
      list_nm <- sub("\\s+or_other$", "", list_nm)
      return(list_nm)
    } else if (grepl("^select_multiple\\b", t)) {
      list_nm <- sub("^select_multiple\\s+", "", t)
      list_nm <- sub("\\s+or_other$", "", list_nm)
      return(list_nm)
    } else {
      return(NA_character_)
    }
  }
  
  # ---------- Get all unique list_names from choices ----------
  choices_list_names <- unique(choices_df$list_name[!is.na(choices_df$list_name) & choices_df$list_name != ""])
  
  # ---------- Logging ----------
  log_entries <- list()
  log_id <- 1L
  
  append_log <- function(level, row_idx, field, message, rule_id, sheet = sheet_name) {
    log_entries[[log_id]] <<- data.frame(
      id = log_id,
      source = "custom",
      level = level,
      sheet = sheet,
      row = as.integer(row_idx + header_rows),
      field = field,
      message = message,
      rule_id = rule_id,
      stringsAsFactors = FALSE
    )
    log_id <<- log_id + 1L
  }
  
  # ---------- Check each survey row for select_one/select_multiple ----------
  referenced_lists <- character(0)
  
  for (r in seq_len(nrow(survey_df))) {
    type_val <- survey_df$type[r]
    if (is.na(type_val) || type_val == "") next
    
    list_nm <- parse_list_name_from_type(type_val)
    if (is.na(list_nm) || list_nm == "") next
    
    # Track referenced list
    referenced_lists <- c(referenced_lists, list_nm)
    
    # Check if list_name exists in choices
    if (!(list_nm %in% choices_list_names)) {
      # Get question name for better message
      q_name <- if ("name" %in% names(survey_df)) survey_df$name[r] else paste("row", r)
      
      append_log(
        level = "error",
        row_idx = r,
        field = "type",
        message = sprintf("List '%s' referenced in question '%s' does not exist in choices sheet.", list_nm, q_name),
        rule_id = "R_LIST_NAME_MISSING"
      )
    }
  }
  
  # ---------- Check for orphaned choice lists ----------
  if (report_orphans) {
    referenced_lists <- unique(referenced_lists)
    orphaned_lists <- setdiff(choices_list_names, referenced_lists)
    
    if (length(orphaned_lists) > 0) {
      for (orphan in orphaned_lists) {
        # Find first row in choices where this list appears
        first_row <- which(choices_df$list_name == orphan)[1]
        
        append_log(
          level = "info",
          row_idx = first_row,
          field = "list_name",
          message = sprintf("Choice list '%s' is defined but never referenced in survey.", orphan),
          rule_id = "R_ORPHANED_CHOICE_LIST",
          sheet = "choices"
        )
      }
    }
  }
  
  # ---------- Finalize ----------
  log_df <- if (length(log_entries) == 0) {
    data.frame(id=integer(0), source=character(0), level=character(0),
               sheet=character(0), row=integer(0), field=character(0),
               message=character(0), rule_id=character(0))
  } else {
    do.call(rbind, log_entries)
  }
  
  all_ok <- nrow(log_df[log_df$level == "error", ]) == 0L
  
  if (verbose) {
    errors <- sum(log_df$level == "error")
    infos <- sum(log_df$level == "info")
    if (errors == 0 && infos == 0) {
      message("All cross-sheet references are valid.")
    } else {
      if (errors > 0) message(sprintf("%d missing list_name reference(s) found.", errors))
      if (infos > 0) message(sprintf("%d orphaned choice list(s) found.", infos))
    }
  }
  
  list(valid = all_ok, log = log_df)
}


# =============================================================================
# Wrapper for XLS-Validator rule system
# =============================================================================

#' Validate cross-sheet references (wrapper for XLS-Validator)
#' @param xlsform_data List containing survey, choices, settings data frames
#' @return tibble of validation issues following the schema
check_cross_sheet_refs_rule <- function(xlsform_data) {
  # Check if required sheets exist
  if (!"survey" %in% names(xlsform_data) || is.null(xlsform_data$survey)) {
    return(data.frame(
      id = integer(0), source = character(0), level = character(0),
      sheet = character(0), row = integer(0), field = character(0),
      message = character(0), rule_id = character(0), status = character(0)
    ))
  }
  
  if (!"choices" %in% names(xlsform_data) || is.null(xlsform_data$choices)) {
    return(data.frame(
      id = integer(0), source = character(0), level = character(0),
      sheet = character(0), row = integer(0), field = character(0),
      message = character(0), rule_id = character(0), status = character(0)
    ))
  }
  
  survey_df <- xlsform_data$survey
  choices_df <- xlsform_data$choices
  
  # Call the implementation
  result <- check_cross_sheet_refs(
    survey_df = survey_df,
    choices_df = choices_df,
    header_rows = 1L,
    sheet_name = "survey",
    report_orphans = TRUE,
    verbose = FALSE
  )
  
  log_df <- result$log
  
  # Add status column if missing
  if (nrow(log_df) > 0 && !"status" %in% names(log_df)) {
    log_df$status <- "open"
  }
  
  # Return empty with correct schema if no issues
  if (nrow(log_df) == 0) {
    return(data.frame(
      id = integer(0), source = character(0), level = character(0),
      sheet = character(0), row = integer(0), field = character(0),
      message = character(0), rule_id = character(0), status = character(0)
    ))
  }
  
  log_df
}

# Register rule
register_rule(
  rule_id = "cross_sheet_refs",
  rule_fn = check_cross_sheet_refs_rule,
  description = "Validate that select_one/select_multiple list_names exist in choices sheet",
  level = "error",
  enabled = TRUE
)




