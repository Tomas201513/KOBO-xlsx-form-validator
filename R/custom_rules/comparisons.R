# Custom Rule: Comparison Operators Validation
# Scans selected columns for comparisons: >, >=, <, <=
# Validates operand types and variable references.

# =============================================================================
# check_xlsform_comparisons()
# - Scans selected columns (default: relevant, calculation, constraint)
#   for comparisons: >, >=, <, <=
# - Validates operand types:
#     * Dot reference '.' -> current row's base type in df$type
#     * ${var} reference -> referenced row's base type in df$type
# - Allowed types are configurable (default: integer, decimal, date, calculate)
# - Returns a log with entries shaped as:
#     (id, source, level, sheet, row, field, message, rule_id)
# - No auto-fix (structural validation only)
# =============================================================================

# =============================================================================
# check_xlsform_comparisons()
# - Scans columns (default: relevant, calculation, constraint) for >, >=, <, <=
# - Validates operand types:
#     * Dot '.' -> current row's base type (from df$type)
#     * ${var}  -> referenced row's base type (where df$name == var)
# - Allowed types: integer, decimal, date, calculate (configurable)
# - Returns a structured log: (id, source, level, sheet, row, field, message, rule_id)
# =============================================================================
check_xlsform_comparisons <- function(df,
                                      cols = c("relevant", "calculation", "constraint"),
                                      allowed_types = c("integer","decimal","date","calculate"),
                                      sheet_name = "survey",
                                      header_rows = 1L,
                                      level = c("error","warning","info"),
                                      context_radius = 20L,
                                      require_past_reference = FALSE) {
  
  level <- match.arg(level)
  
  # --- Validate required columns ---
  missing_cols <- setdiff(c("type","name", cols), names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing columns in df: %s", paste(missing_cols, collapse = ", ")))
  }
  
  # Normalize types and names
  df$type <- if (is.factor(df$type)) as.character(df$type) else df$type
  df$name <- if (is.factor(df$name)) as.character(df$name) else df$name
  for (cl in cols) {
    df[[cl]] <- if (is.factor(df[[cl]])) as.character(df[[cl]]) else df[[cl]]
  }
  
  # Base type = first token of df$type (e.g., "integer", "select_one", "calculate", "date")
  base_type_of_row <- function(type_str) {
    if (is.na(type_str) || type_str == "") return(NA_character_)
    t <- gsub("\\s+", " ", trimws(type_str))
    sub("\\s+.*$", "", t)
  }
  
  # Context preview + caret pointer
  make_context <- function(s, start, end, radius = context_radius) {
    n <- nchar(s)
    left <- max(1L, start - radius)
    right <- min(n, end + radius)
    ctx <- substr(s, left, right)
    underline <- paste0(
      paste0(rep(" ", start - left), collapse = ""),
      paste0(rep("^", end - start + 1L), collapse = "")
    )
    list(context = ctx, pointer = underline, left_index = left, right_index = right)
  }
  
  # Comparison pattern: left operand, operator, right operand
  comparison_pattern <- "(\\.|\\$\\{[^}]+\\}|-?\\d+(?:\\.\\d+)?|\"[^\"]*\"|'[^']*')\\s*(>=|<=|>|<)\\s*(\\.|\\$\\{[^}]+\\}|-?\\d+(?:\\.\\d+)?|\"[^\"]*\"|'[^']*')"
  
  # Resolve operand: returns list(kind, ok_type, type, var_name, var_row)
  resolve_operand <- function(op, current_row_index, name_to_row) {
    op <- trimws(op)
    if (grepl("^-?\\d+(?:\\.\\d+)?$", op, perl = TRUE)) {
      return(list(kind = "literal_number", ok_type = TRUE, type = "literal_number", var_name = NA, var_row = NA))
    }
    if (grepl("^\"[^\"]*\"$", op, perl = TRUE) || grepl("^'[^']*'$", op, perl = TRUE)) {
      return(list(kind = "literal_string", ok_type = TRUE, type = "literal_string", var_name = NA, var_row = NA))
    }
    if (op == ".") {
      bt <- base_type_of_row(df$type[current_row_index])
      ok <- !is.na(bt) && tolower(bt) %in% tolower(allowed_types)
      return(list(kind = "dot", ok_type = ok, type = bt, var_name = NA, var_row = current_row_index))
    }
    if (grepl("^\\$\\{[^}]+\\}$", op, perl = TRUE)) {
      var <- sub("^\\$\\{", "", op); var <- sub("\\}$", "", var)
      var_row <- name_to_row[var]
      if (is.na(var_row)) {
        return(list(kind = "var", ok_type = FALSE, type = NA_character_, var_name = var, var_row = NA_integer_))
      }
      bt <- base_type_of_row(df$type[var_row])
      ok <- !is.na(bt) && tolower(bt) %in% tolower(allowed_types)
      return(list(kind = "var", ok_type = ok, type = bt, var_name = var, var_row = var_row))
    }
    list(kind = "unknown", ok_type = TRUE, type = NA_character_, var_name = NA, var_row = NA)
  }
  
  # --- Logging ---
  log_entries <- list(); log_id <- 1L
  append_log <- function(level, row_idx, field, message, rule_id) {
    log_entries[[log_id]] <<- data.frame(
      id      = log_id,
      source  = "custom",
      level   = level,
      sheet   = sheet_name,
      row     = as.integer(row_idx + header_rows),
      field   = field,
      message = message,
      rule_id = rule_id,
      stringsAsFactors = FALSE
    )
    log_id <<- log_id + 1L
  }
  
  name_to_row <- setNames(seq_len(nrow(df)), df$name)
  
  for (col in cols) {
    values <- df[[col]]
    for (r in seq_along(values)) {
      s <- values[r]
      if (is.na(s) || s == "") next
      
      m <- gregexpr(comparison_pattern, s, perl = TRUE)
      starts <- as.integer(m[[1]])
      lens <- attr(m[[1]], "match.length")
      matches <- regmatches(s, m)[[1]]
      
      if (length(starts) == 1L && starts[1] == -1L) next
      
      for (i in seq_along(matches)) {
        full  <- matches[i]
        left  <- sub(comparison_pattern, "\\1", full, perl = TRUE)
        oper  <- sub(comparison_pattern, "\\2", full, perl = TRUE)
        right <- sub(comparison_pattern, "\\3", full, perl = TRUE)
        start <- starts[i]; end <- start + lens[i] - 1L
        ctx <- make_context(s, start, end)
        
        L <- resolve_operand(left, r, name_to_row)
        R <- resolve_operand(right, r, name_to_row)
        
        # Missing referenced variables
        if (L$kind == "var" && is.na(L$var_row)) {
          msg <- sprintf(
            paste0("Comparison left operand variable missing at char %d-%d: %s\n",
                   "Left var '%s' not found in survey.\n",
                   "Context (chars %d-%d):\n%s\nNo auto-fix."),
            start, end, full,
            sub("^\\$\\{", "", sub("\\}$", "", left)),
            ctx$left_index, ctx$right_index, ctx$context, ctx$pointer
          )
          append_log("error", r, col, msg, "R_COMPARISON_LEFT_VAR_MISSING")
        }
        if (R$kind == "var" && is.na(R$var_row)) {
          msg <- sprintf(
            paste0("Comparison right operand variable missing at char %d-%d: %s\n",
                   "Right var '%s' not found in survey.\n",
                   "Context (chars %d-%d):\n%s\nNo auto-fix."),
            start, end, full,
            sub("^\\$\\{", "", sub("\\}$", "", right)),
            ctx$left_index, ctx$right_index, ctx$context, ctx$pointer
          )
          append_log("error", r, col, msg, "R_COMPARISON_RIGHT_VAR_MISSING")
        }
        
        # Optional: enforce that referenced var appears earlier than current row
        if (require_past_reference) {
          if (L$kind == "var" && !is.na(L$var_row) && L$var_row >= r) {
            msg <- sprintf(
              paste0("Comparison left reference to future question at char %d-%d: %s\n",
                     "Left var '%s' is defined at Excel row %d, referenced at row %d.\n",
                     "Context (chars %d-%d):\n%s\nNo auto-fix."),
              start, end, full,
              L$var_name, L$var_row + header_rows, r + header_rows,
              ctx$left_index, ctx$right_index, ctx$context, ctx$pointer
            )
            append_log(level, r, col, msg, "R_COMPARISON_REFERENCE_FUTURE_LEFT")
          }
          if (R$kind == "var" && !is.na(R$var_row) && R$var_row >= r) {
            msg <- sprintf(
              paste0("Comparison right reference to future question at char %d-%d: %s\n",
                     "Right var '%s' is defined at Excel row %d, referenced at row %d.\n",
                     "Context (chars %d-%d):\n%s\nNo auto-fix."),
              start, end, full,
              R$var_name, R$var_row + header_rows, r + header_rows,
              ctx$left_index, ctx$right_index, ctx$context, ctx$pointer
            )
            append_log(level, r, col, msg, "R_COMPARISON_REFERENCE_FUTURE_RIGHT")
          }
        }
        
        # Type validation: dot and var must be in allowed_types
        if (L$kind %in% c("dot","var") && !L$ok_type) {
          msg <- sprintf(
            paste0("Invalid type for comparison (left operand) at char %d-%d: %s\n",
                   "Left operand kind=%s, type='%s' (allowed: %s).\n",
                   "Context (chars %d-%d):\n%s\nNo auto-fix."),
            start, end, full,
            L$kind, ifelse(is.na(L$type), "NA", L$type), paste(allowed_types, collapse=", "),
            ctx$left_index, ctx$right_index, ctx$context, ctx$pointer
          )
          append_log(level, r, col, msg, "R_COMPARISON_LEFT_INVALID_TYPE")
        }
        if (R$kind %in% c("dot","var") && !R$ok_type) {
          msg <- sprintf(
            paste0("Invalid type for comparison (right operand) at char %d-%d: %s\n",
                   "Right operand kind=%s, type='%s' (allowed: %s).\n",
                   "Context (chars %d-%d):\n%s\nNo auto-fix."),
            start, end, full,
            R$kind, ifelse(is.na(R$type), "NA", R$type), paste(allowed_types, collapse=", "),
            ctx$left_index, ctx$right_index, ctx$context, ctx$pointer
          )
          append_log(level, r, col, msg, "R_COMPARISON_RIGHT_INVALID_TYPE")
        }
      }
    }
  }
  
  log_df <- if (length(log_entries) == 0) {
    data.frame(id=integer(0), source=character(0), level=character(0),
               sheet=character(0), row=integer(0), field=character(0),
               message=character(0), rule_id=character(0))
  } else {
    do.call(rbind, log_entries)
  }
  
  list(valid = nrow(log_df) == 0L, log = log_df)
}


# =============================================================================
# Wrapper for XLS-Validator rule system
# =============================================================================

#' Check comparison operators (wrapper for XLS-Validator)
#' @param xlsform_data List containing survey, choices, settings data frames
#' @return tibble of validation issues following the schema
check_comparisons_rule <- function(xlsform_data) {
  # Check if survey sheet exists
  if (!"survey" %in% names(xlsform_data) || is.null(xlsform_data$survey)) {
    return(data.frame(
      id = integer(0), source = character(0), level = character(0),
      sheet = character(0), row = integer(0), field = character(0),
      message = character(0), rule_id = character(0), status = character(0)
    ))
  }
  
  survey_df <- xlsform_data$survey
  
  # Check required columns
  if (!all(c("type", "name") %in% names(survey_df))) {
    return(data.frame(
      id = integer(0), source = character(0), level = character(0),
      sheet = character(0), row = integer(0), field = character(0),
      message = character(0), rule_id = character(0), status = character(0)
    ))
  }
  
  # Determine which columns to check (only those that exist)
  target_cols <- c("relevant", "calculation", "constraint")
  cols_to_check <- intersect(target_cols, names(survey_df))
  
  if (length(cols_to_check) == 0) {
    return(data.frame(
      id = integer(0), source = character(0), level = character(0),
      sheet = character(0), row = integer(0), field = character(0),
      message = character(0), rule_id = character(0), status = character(0)
    ))
  }
  
  # Call the implementation
  result <- check_xlsform_comparisons(
    df = survey_df,
    cols = cols_to_check,
    allowed_types = c("integer", "decimal", "date", "calculate"),
    sheet_name = "survey",
    header_rows = 1L,
    level = "error",
    context_radius = 20L,
    require_past_reference = FALSE
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
  rule_id = "comparisons",
  rule_fn = check_comparisons_rule,
  description = "Validate comparison operators (>, >=, <, <=) and operand types",
  level = "error",
  enabled = TRUE
)
