# Custom Rule: Comprehensive Choice List Validation
# Validates choice lists across survey and choices sheets
# Uses the enhanced multi-sheet context from rule_registry

# =============================================================================
# check_choice_list_validation()
# - Validates that all referenced choice lists exist
# - Validates that choice names are unique within each list
# - Validates that choice labels are not empty
# - Checks for duplicate choice values within a list
# - Reports orphaned choice lists (defined but never used)
# =============================================================================

#' Comprehensive choice list validation
#' @param xlsform_data Rule context with survey and choices sheets
#' @return tibble of validation issues
check_choice_list_validation <- function(xlsform_data) {
  
  print("Running custom rule: choice_list_validation")
  
  issues <- data.frame(
    id = integer(0), source = character(0), level = character(0),
    sheet = character(0), row = integer(0), field = character(0),
    message = character(0), rule_id = character(0), status = character(0),
    stringsAsFactors = FALSE
  )
  
  # Get sheets
  survey <- if ("survey" %in% names(xlsform_data)) xlsform_data$survey else NULL
  choices <- if ("choices" %in% names(xlsform_data)) xlsform_data$choices else NULL
  
  if (is.null(survey) || is.null(choices)) {
    return(issues)
  }
  
  if (!"type" %in% names(survey)) return(issues)
  if (!"list_name" %in% names(choices)) return(issues)
  
  issue_id <- 1L
  
  # Helper to add issue
  add_issue <- function(level, sheet, row, field, message, rule_id) {
    issues <<- rbind(issues, data.frame(
      id = issue_id,
      source = "custom",
      level = level,
      sheet = sheet,
      row = as.integer(row),
      field = field,
      message = message,
      rule_id = rule_id,
      status = "open",
      stringsAsFactors = FALSE
    ))
    issue_id <<- issue_id + 1L
  }
  
  # Get all defined choice lists
  defined_lists <- unique(choices$list_name[!is.na(choices$list_name) & choices$list_name != ""])
  # Get all referenced choice lists from survey
  referenced_lists <- character(0)
  
  for (r in seq_len(nrow(survey))) {
    type_val <- survey$type[r]
    if (is.na(type_val) || type_val == "") next
    
    # Parse list name from select_one/select_multiple
    type_val <- gsub("\\s+", " ", trimws(type_val))
    list_name <- NA_character_
    
    if (grepl("^select_one\\b", type_val)) {
      list_name <- sub("^select_one\\s+", "", type_val)
      list_name <- sub("\\s+or_other$", "", list_name)
    } else if (grepl("^select_multiple\\b", type_val)) {
      list_name <- sub("^select_multiple\\s+", "", type_val)
      list_name <- sub("\\s+or_other$", "", list_name)
    }
    
    if (!is.na(list_name) && list_name != "") {
      referenced_lists <- c(referenced_lists, list_name)
      
      # Check if list exists
      if (!(list_name %in% defined_lists)) {
        q_name <- if ("name" %in% names(survey)) survey$name[r] else paste("row", r)
        add_issue(
          level = "error",
          sheet = "survey",
          row = r + 1,
          field = "type",
          message = sprintf("Choice list '%s' referenced in '%s' is not defined in choices sheet", 
                           list_name, q_name),
          rule_id = "R_UNDEFINED_CHOICE_LIST"
        )
      }
    }
  }
  
  referenced_lists <- unique(referenced_lists)
  
  # Check for duplicate choice names within each list
  for (list_name in defined_lists) {
    list_rows <- which(choices$list_name == list_name & !is.na(choices$list_name))
    
    if (length(list_rows) > 0 && "name" %in% names(choices)) {
      list_choices <- choices[list_rows, ]
      choice_names <- list_choices$name
      
      # Find duplicates
      dup_names <- choice_names[duplicated(choice_names) & !is.na(choice_names)]
      
      for (dup in unique(dup_names)) {
        dup_rows <- list_rows[choice_names == dup & !is.na(choice_names)]
        if (length(dup_rows) > 1) {
          # Report on second occurrence
          add_issue(
            level = "warning",
            sheet = "choices",
            row = dup_rows[2] + 1,
            field = "name",
            message = sprintf("Duplicate choice name '%s' in list '%s' (first at row %d)", 
                             dup, list_name, dup_rows[1] + 1),
            rule_id = "R_DUPLICATE_CHOICE_NAME"
          )
        }
      }
      
      # Check for empty choice names
      empty_rows <- list_rows[is.na(choice_names) | choice_names == ""]
      for (empty_row in empty_rows) {
        add_issue(
          level = "error",
          sheet = "choices",
          row = empty_row + 1,
          field = "name",
          message = sprintf("Empty choice name in list '%s'", list_name),
          rule_id = "R_EMPTY_CHOICE_NAME"
        )
      }
    }
    
    # Check for empty labels (if label column exists)
    label_cols <- names(choices)[grepl("^label", names(choices), ignore.case = TRUE)]
    if (length(label_cols) > 0 && length(list_rows) > 0) {
      for (label_col in label_cols[1]) {
        labels <- choices[[label_col]][list_rows]
        empty_label_rows <- list_rows[is.na(labels) | labels == ""]
        
        for (empty_row in empty_label_rows) {
          choice_name <- if ("name" %in% names(choices)) choices$name[empty_row] else "unknown"
          add_issue(
            level = "error",
            sheet = "choices",
            row = empty_row + 1,
            field = label_col,
            message = sprintf("Empty label for choice '%s' in list '%s'", choice_name, list_name),
            rule_id = "R_EMPTY_CHOICE_LABEL"
          )
        }
      }
    }
  }
  
  # Report orphaned choice lists (info level)
  orphaned_lists <- setdiff(defined_lists, referenced_lists)
  for (orphan in orphaned_lists) {
    first_row <- which(choices$list_name == orphan)[1]
    add_issue(
      level = "warning",
      sheet = "choices",
      row = first_row + 1,
      field = "list_name",
      message = sprintf("Choice list '%s' is defined but never used in survey", orphan),
      rule_id = "R_ORPHANED_CHOICE_LIST"
    )
  }
  
  issues
}

# Register rule
register_rule(
  rule_id = "choice_list_validation",
  rule_fn = check_choice_list_validation,
  description = "Comprehensive validation of choice lists (duplicates, empty names/labels, orphans)",
  level = "error",
  enabled = TRUE
)

