# ODK Expression Validator for XLS-Validator
# Lightweight parser for validating ODK XPath expressions
# Catches common errors before running pyxform

# =============================================================================
# Expression Validation Functions
# =============================================================================

#' Validate an ODK expression
#' @param expr Expression string to validate
#' @param available_fields Character vector of valid field names
#' @param available_lists Character vector of valid choice list names (for selected())
#' @return List with valid (logical), errors (character vector), warnings (character vector)
validate_expression <- function(expr, available_fields = character(), 
                                available_lists = character()) {
  result <- list(
    valid = TRUE,
    errors = character(),
    warnings = character()
  )
  
  if (is.na(expr) || nchar(trimws(expr)) == 0) {
    return(result)
  }
  
  # Check for balanced brackets and quotes
  bracket_check <- check_balanced_brackets(expr)
  if (!bracket_check$valid) {
    result$valid <- FALSE
    result$errors <- c(result$errors, bracket_check$message)
  }
  
  # Extract and validate field references
  field_refs <- extract_field_refs(expr)
  if (length(field_refs) > 0 && length(available_fields) > 0) {
    missing_fields <- setdiff(field_refs, available_fields)
    if (length(missing_fields) > 0) {
      result$valid <- FALSE
      result$errors <- c(result$errors, 
                        sprintf("Unknown field(s): %s", paste(missing_fields, collapse = ", ")))
    }
  }
  
  # Validate ODK function syntax
  func_check <- validate_odk_functions(expr, available_fields, available_lists)
  if (!func_check$valid) {
    result$valid <- FALSE
    result$errors <- c(result$errors, func_check$errors)
  }
  result$warnings <- c(result$warnings, func_check$warnings)
  
  # Check for common syntax errors
  syntax_check <- check_common_syntax_errors(expr)
  if (!syntax_check$valid) {
    result$valid <- FALSE
    result$errors <- c(result$errors, syntax_check$errors)
  }
  result$warnings <- c(result$warnings, syntax_check$warnings)
  
  result
}

#' Extract field references from expression (${field_name} syntax)
#' @param expr Expression string
#' @return Character vector of field names
extract_field_refs <- function(expr) {
  if (is.na(expr) || nchar(expr) == 0) {
    return(character())
  }
  
  # Match ${field_name} pattern
  matches <- regmatches(expr, gregexpr("\\$\\{([^}]+)\\}", expr))[[1]]
  
  if (length(matches) == 0) {
    return(character())
  }
  
  # Extract field names from matches
  field_names <- gsub("^\\$\\{|\\}$", "", matches)
  unique(field_names)
}

#' Check for balanced brackets, parentheses, and quotes
#' @param expr Expression string
#' @return List with valid (logical) and message (character)
check_balanced_brackets <- function(expr) {
  if (is.na(expr) || nchar(expr) == 0) {
    return(list(valid = TRUE, message = ""))
  }
  
  # Track bracket counts
  parens <- 0
  brackets <- 0
  braces <- 0
  single_quotes <- 0
  double_quotes <- 0
  
  chars <- strsplit(expr, "")[[1]]
  in_single_quote <- FALSE
  in_double_quote <- FALSE
  
  for (i in seq_along(chars)) {
    ch <- chars[i]
    
    # Handle quotes (toggle state)
    if (ch == "'" && !in_double_quote) {
      in_single_quote <- !in_single_quote
      single_quotes <- single_quotes + 1
    } else if (ch == '"' && !in_single_quote) {
      in_double_quote <- !in_double_quote
      double_quotes <- double_quotes + 1
    }
    
    # Only count brackets outside of quotes
    if (!in_single_quote && !in_double_quote) {
      if (ch == "(") parens <- parens + 1
      else if (ch == ")") parens <- parens - 1
      else if (ch == "[") brackets <- brackets + 1
      else if (ch == "]") brackets <- brackets - 1
      else if (ch == "{") braces <- braces + 1
      else if (ch == "}") braces <- braces - 1
      
      # Check for negative (unbalanced closing)
      if (parens < 0) {
        return(list(valid = FALSE, message = "Unmatched closing parenthesis ')'"))
      }
      if (brackets < 0) {
        return(list(valid = FALSE, message = "Unmatched closing bracket ']'"))
      }
      if (braces < 0) {
        return(list(valid = FALSE, message = "Unmatched closing brace '}'"))
      }
    }
  }
  
  # Check final counts
  if (parens != 0) {
    return(list(valid = FALSE, message = "Unbalanced parentheses"))
  }
  if (brackets != 0) {
    return(list(valid = FALSE, message = "Unbalanced brackets"))
  }
  if (braces != 0) {
    return(list(valid = FALSE, message = "Unbalanced braces"))
  }
  if (single_quotes %% 2 != 0) {
    return(list(valid = FALSE, message = "Unbalanced single quotes"))
  }
  if (double_quotes %% 2 != 0) {
    return(list(valid = FALSE, message = "Unbalanced double quotes"))
  }
  
  list(valid = TRUE, message = "")
}

#' Validate ODK-specific function syntax
#' @param expr Expression string
#' @param available_fields Character vector of valid field names
#' @param available_lists Character vector of valid choice list names
#' @return List with valid (logical), errors (character vector), warnings (character vector)
validate_odk_functions <- function(expr, available_fields = character(), 
                                   available_lists = character()) {
  result <- list(valid = TRUE, errors = character(), warnings = character())
  
  if (is.na(expr) || nchar(expr) == 0) {
    return(result)
  }
  
  # Check selected() function (exclude count-selected and other prefixed variants)
  selected_matches <- gregexpr("(?<!-)selected\\s*\\(([^)]+)\\)", expr, perl = TRUE)
  if (selected_matches[[1]][1] != -1) {
    selected_calls <- regmatches(expr, selected_matches)[[1]]
    for (call in selected_calls) {
      # Extract arguments
      args_str <- sub("^selected\\s*\\((.*)\\)$", "\\1", call)
      args <- strsplit(args_str, ",")[[1]]
      
      if (length(args) != 2) {
        result$valid <- FALSE
        result$errors <- c(result$errors, 
                          sprintf("selected() requires exactly 2 arguments, got %d in: %s", 
                                 length(args), call))
      } else {
        # Check if first argument is a valid field reference
        field_arg <- trimws(args[1])
        if (grepl("^\\$\\{", field_arg)) {
          field_name <- gsub("^\\$\\{|\\}$", "", field_arg)
          if (length(available_fields) > 0 && !field_name %in% available_fields) {
            result$warnings <- c(result$warnings,
                                sprintf("selected() references unknown field '%s'", field_name))
          }
        }
      }
    }
  }
  
  # Check count-selected() function
  count_selected_matches <- gregexpr("count-selected\\s*\\(([^)]+)\\)", expr)
  if (count_selected_matches[[1]][1] != -1) {
    count_calls <- regmatches(expr, count_selected_matches)[[1]]
    for (call in count_calls) {
      args_str <- sub("^count-selected\\s*\\((.*)\\)$", "\\1", call)
      args <- strsplit(args_str, ",")[[1]]
      
      if (length(args) != 1) {
        result$valid <- FALSE
        result$errors <- c(result$errors,
                          sprintf("count-selected() requires exactly 1 argument, got %d in: %s",
                                 length(args), call))
      }
    }
  }
  
  # Check if() function (XPath if)
  if_matches <- gregexpr("if\\s*\\(([^)]+)\\)", expr)
  if (if_matches[[1]][1] != -1) {
    if_calls <- regmatches(expr, if_matches)[[1]]
    for (call in if_calls) {
      args_str <- sub("^if\\s*\\((.*)\\)$", "\\1", call)
      # Simple check: should have at least 2 commas for 3 arguments
      comma_count <- length(gregexpr(",", args_str)[[1]])
      if (comma_count < 2 && !grepl("^-1$", as.character(gregexpr(",", args_str)[[1]][1]))) {
        result$warnings <- c(result$warnings,
                            sprintf("if() typically requires 3 arguments (condition, true, false): %s", call))
      }
    }
  }
  
  # Check coalesce() function
  coalesce_matches <- gregexpr("coalesce\\s*\\(([^)]+)\\)", expr)
  if (coalesce_matches[[1]][1] != -1) {
    coalesce_calls <- regmatches(expr, coalesce_matches)[[1]]
    for (call in coalesce_calls) {
      args_str <- sub("^coalesce\\s*\\((.*)\\)$", "\\1", call)
      args <- strsplit(args_str, ",")[[1]]
      
      if (length(args) < 2) {
        result$warnings <- c(result$warnings,
                            sprintf("coalesce() typically requires at least 2 arguments: %s", call))
      }
    }
  }
  
  result
}

#' Check for common syntax errors in ODK expressions
#' @param expr Expression string
#' @return List with valid (logical), errors (character vector), warnings (character vector)
check_common_syntax_errors <- function(expr) {
  result <- list(valid = TRUE, errors = character(), warnings = character())
  
  if (is.na(expr) || nchar(expr) == 0) {
    return(result)
  }
  
  # Check for spaces inside ${} (common error)
  if (grepl("\\$\\{\\s+|\\s+\\}", expr)) {
    result$warnings <- c(result$warnings,
                        "Spaces inside ${...} may cause issues")
  }
  
  # Check for = instead of == in comparisons (outside of assignments)
  # This is tricky because = is valid in some contexts
  if (grepl("[^=!<>]=[^=]", expr) && !grepl(":=", expr)) {
    # Check if it's likely a comparison context
    if (grepl("\\$\\{[^}]+\\}\\s*=\\s*['\"]", expr) || 
        grepl("\\$\\{[^}]+\\}\\s*=\\s*\\d", expr)) {
      result$warnings <- c(result$warnings,
                          "Single '=' may be intended as '==' for comparison")
    }
  }
  
  # Check for common typos in operators
  if (grepl("&&", expr)) {
    result$errors <- c(result$errors,
                      "Use 'and' instead of '&&' in ODK expressions")
    result$valid <- FALSE
  }
  
  if (grepl("\\|\\|", expr)) {
    result$errors <- c(result$errors,
                      "Use 'or' instead of '||' in ODK expressions")
    result$valid <- FALSE
  }
  
  # Check for != vs not()
  # != is valid but sometimes people use <> which isn't
  if (grepl("<>", expr)) {
    result$errors <- c(result$errors,
                      "Use '!=' instead of '<>' for not-equal comparison")
    result$valid <- FALSE
  }
  
  # Check for empty ${} references
  if (grepl("\\$\\{\\s*\\}", expr)) {
    result$errors <- c(result$errors,
                      "Empty field reference ${}")
    result$valid <- FALSE
  }
  
  # Check for unclosed ${
  if (grepl("\\$\\{[^}]*$", expr)) {
    result$errors <- c(result$errors,
                      "Unclosed field reference ${...}")
    result$valid <- FALSE
  }
  
  # Check for common function name typos
  common_typos <- list(
    "select\\(" = "selected(",
    "count_selected\\(" = "count-selected(",
    "count selected\\(" = "count-selected(",
    "jr:choice-name\\(" = "jr:choice-name(",
    "choice_name\\(" = "jr:choice-name("
  )
  
  for (typo in names(common_typos)) {
    if (grepl(typo, expr, ignore.case = TRUE)) {
      result$warnings <- c(result$warnings,
                          sprintf("Possible typo: did you mean '%s'?", common_typos[[typo]]))
    }
  }
  
  result
}

# =============================================================================
# Custom Rule: Expression Validation
# =============================================================================

#' Validate all expressions in survey sheet
#' @param xlsform_data Rule context with survey sheet
#' @return tibble of validation issues
check_expression_validation <- function(xlsform_data) {
  issues <- data.frame(
    id = integer(0), source = character(0), level = character(0),
    sheet = character(0), row = integer(0), field = character(0),
    message = character(0), rule_id = character(0), status = character(0),
    stringsAsFactors = FALSE
  )
  
  survey <- if ("survey" %in% names(xlsform_data)) xlsform_data$survey else NULL
  if (is.null(survey)) return(issues)
  
  # Get available field names for reference checking
  available_fields <- if ("name" %in% names(survey)) {
    survey$name[!is.na(survey$name) & survey$name != ""]
  } else {
    character()
  }
  
  # Get available choice lists
  choices <- if ("choices" %in% names(xlsform_data)) xlsform_data$choices else NULL
  available_lists <- if (!is.null(choices) && "list_name" %in% names(choices)) {
    unique(choices$list_name[!is.na(choices$list_name)])
  } else {
    character()
  }
  
  # Columns that contain expressions
  expression_cols <- c("relevant", "calculation", "constraint", "choice_filter", 
                       "required", "default", "repeat_count")
  expression_cols <- intersect(expression_cols, names(survey))
  
  issue_id <- 1L
  
  for (col in expression_cols) {
    for (r in seq_len(nrow(survey))) {
      expr <- survey[[col]][r]
      if (is.na(expr) || nchar(trimws(expr)) == 0) next
      
      # Validate expression
      validation <- validate_expression(expr, available_fields, available_lists)
      
      # Add errors
      for (error in validation$errors) {
        issues <- rbind(issues, data.frame(
          id = issue_id,
          source = "custom",
          level = "error",
          sheet = "survey",
          row = as.integer(r + 1),
          field = col,
          message = sprintf("Expression error: %s", error),
          rule_id = "R_EXPRESSION_ERROR",
          status = "open",
          stringsAsFactors = FALSE
        ))
        issue_id <- issue_id + 1L
      }
      
      # Add warnings
      for (warning in validation$warnings) {
        issues <- rbind(issues, data.frame(
          id = issue_id,
          source = "custom",
          level = "warning",
          sheet = "survey",
          row = as.integer(r + 1),
          field = col,
          message = sprintf("Expression warning: %s", warning),
          rule_id = "R_EXPRESSION_WARNING",
          status = "open",
          stringsAsFactors = FALSE
        ))
        issue_id <- issue_id + 1L
      }
    }
  }
  
  issues
}

# Register rule
register_rule(
  rule_id = "expression_validation",
  rule_fn = check_expression_validation,
  description = "Validate ODK expression syntax (balanced brackets, field references, function calls)",
  level = "error",
  enabled = TRUE
)

