# Custom Rule Registry for XLS-Validator
# Manages registration and execution of custom validation rules

# Global registry of custom rules
.rule_registry <- new.env(parent = emptyenv())

#' Register a custom validation rule
#' @param rule_id Unique identifier for the rule
#' @param rule_fn Validation function(xlsform_data) -> tibble of issues
#' @param description Human-readable description
#' @param level Default severity level (error, warning, info)
#' @param enabled Whether rule is enabled by default
register_rule <- function(rule_id, rule_fn, description = "", level = "warning", enabled = TRUE) {
  .rule_registry[[rule_id]] <- list(
    id = rule_id,
    fn = rule_fn,
    description = description,
    level = level,
    enabled = enabled
  )
  invisible(TRUE)
}

#' Get all registered rules
#' @param enabled_only If TRUE, return only enabled rules
#' @return List of rule definitions
get_registered_rules <- function(enabled_only = TRUE) {
  rules <- as.list(.rule_registry)
  
  if (enabled_only) {
    rules <- Filter(function(r) r$enabled, rules)
  }
  
  rules
}

#' Enable or disable a rule
#' @param rule_id Rule identifier
#' @param enabled TRUE to enable, FALSE to disable
set_rule_enabled <- function(rule_id, enabled) {
  if (exists(rule_id, envir = .rule_registry)) {
    .rule_registry[[rule_id]]$enabled <- enabled
    invisible(TRUE)
  } else {
    warning(paste("Rule not found:", rule_id))
    invisible(FALSE)
  }
}

#' Run all registered custom rules
#' @param xlsform_data List of data frames from xlsform_reader (sheets or full xlsform_data)
#' @return Combined tibble of all custom rule issues
run_custom_rules <- function(xlsform_data) {
  rules <- get_registered_rules(enabled_only = TRUE)
  
  if (length(rules) == 0) {
    return(create_empty_results())
  }
  
  # Normalize input: ensure we have a consistent structure
  # Rules receive the sheets list, but we also provide full context if available
  rule_context <- normalize_rule_context(xlsform_data)
  
  all_issues <- create_empty_results()
  
  for (rule in rules) {
    tryCatch({
      # Pass the normalized context to rules
      issues <- rule$fn(rule_context)
      
      # Ensure source is set to 'custom'
      if (nrow(issues) > 0) {
        issues$source <- "custom"
        issues$rule_id <- rule$id
      }
      
      all_issues <- dplyr::bind_rows(all_issues, issues)
    }, error = function(e) {
      warning(paste("Error running rule", rule$id, ":", e$message))
    })
  }
  
  # Renumber IDs
  if (nrow(all_issues) > 0) {
    all_issues$id <- seq_len(nrow(all_issues))
  }
  
  all_issues
}

#' Normalize rule context for consistent rule execution
#' Ensures rules receive both individual sheets and full context
#' @param xlsform_data Input data (could be sheets list or full xlsform_data)
#' @return Normalized context with survey, choices, settings, and metadata
normalize_rule_context <- function(xlsform_data) {
  # If it's already the sheets list (legacy format)
  if ("survey" %in% names(xlsform_data) && is.data.frame(xlsform_data$survey)) {
    # Add metadata if not present
    context <- xlsform_data
    if (!"_meta" %in% names(context)) {
      context$`_meta` <- list(
        has_full_context = FALSE,
        available_sheets = names(xlsform_data)
      )
    }
    return(context)
  }
  
  # If it's full xlsform_data with $sheets
  if ("sheets" %in% names(xlsform_data)) {
    context <- xlsform_data$sheets
    context$`_meta` <- list(
      has_full_context = TRUE,
      file_path = xlsform_data$file_path,
      file_name = xlsform_data$file_name,
      name_index = xlsform_data$name_index,
      available_sheets = names(xlsform_data$sheets)
    )
    return(context)
  }
  
  # Fallback: return as-is
  xlsform_data
}

#' Get all field names from survey sheet
#' Helper for rules that need to validate field references
#' @param xlsform_data Rule context
#' @return Character vector of field names
get_all_field_names <- function(xlsform_data) {
  survey <- if ("survey" %in% names(xlsform_data)) xlsform_data$survey else NULL
  if (is.null(survey) || !"name" %in% names(survey)) {
    return(character())
  }
  
  names_col <- survey$name
  names_col <- names_col[!is.na(names_col) & nchar(trimws(names_col)) > 0]
  unique(names_col)
}

#' Get all choice list names from choices sheet
#' Helper for rules that need to validate list references
#' @param xlsform_data Rule context
#' @return Character vector of list names
get_all_list_names <- function(xlsform_data) {
  choices <- if ("choices" %in% names(xlsform_data)) xlsform_data$choices else NULL
  if (is.null(choices) || !"list_name" %in% names(choices)) {
    return(character())
  }
  
  list_names <- choices$list_name
  list_names <- list_names[!is.na(list_names) & nchar(trimws(list_names)) > 0]
  unique(list_names)
}

#' Get choice values for a specific list
#' Helper for rules that need to validate choice references
#' @param xlsform_data Rule context
#' @param list_name Name of the choice list
#' @return Character vector of choice names/values
get_choice_values <- function(xlsform_data, list_name) {
  choices <- if ("choices" %in% names(xlsform_data)) xlsform_data$choices else NULL
  if (is.null(choices) || !"list_name" %in% names(choices) || !"name" %in% names(choices)) {
    return(character())
  }
  
  list_choices <- choices[choices$list_name == list_name & !is.na(choices$list_name), ]
  if (nrow(list_choices) == 0) return(character())
  
  choice_names <- list_choices$name
  choice_names <- choice_names[!is.na(choice_names)]
  unique(choice_names)
}

#' List all rules with their status
#' @return Data frame with rule info
list_rules <- function() {
  rules <- as.list(.rule_registry)
  
  if (length(rules) == 0) {
    return(data.frame(
      id = character(),
      description = character(),
      level = character(),
      enabled = logical()
    ))
  }
  
  data.frame(
    id = sapply(rules, function(r) r$id),
    description = sapply(rules, function(r) r$description),
    level = sapply(rules, function(r) r$level),
    enabled = sapply(rules, function(r) r$enabled),
    stringsAsFactors = FALSE
  )
}

#' Clear all registered rules (useful for testing)
clear_rules <- function() {
  rm(list = ls(.rule_registry), envir = .rule_registry)
  invisible(TRUE)
}

# ============================================
# Auto-register all rules on source
# ============================================

# Source all rule files in custom_rules directory
.register_all_rules <- function() {
  rules_dir <- "R/custom_rules"
  if (!dir.exists(rules_dir)) return()
  
  rule_files <- list.files(
    rules_dir, 
    pattern = "\\.R$", 
    full.names = TRUE
  )
  
  # Exclude this file
  rule_files <- rule_files[!grepl("rule_registry\\.R$", rule_files)]
  
  for (f in rule_files) {
    tryCatch({
      source(f, local = TRUE)
    }, error = function(e) {
      warning(paste("Failed to source rule file:", f, "-", e$message))
    })
  }
}



