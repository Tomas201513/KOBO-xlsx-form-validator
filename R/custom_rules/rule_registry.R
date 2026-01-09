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
#' @param xlsform_data List of data frames from xlsform_reader
#' @return Combined tibble of all custom rule issues
run_custom_rules <- function(xlsform_data) {
  rules <- get_registered_rules(enabled_only = TRUE)
  
  if (length(rules) == 0) {
    return(create_empty_results())
  }
  
  all_issues <- create_empty_results()
  
  for (rule in rules) {
    tryCatch({
      issues <- rule$fn(xlsform_data)
      
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


