# Custom Rule: Selected Function Validation
# Validates selected(${var}, 'choice') expressions in XLSForm.
# Checks variable existence, order, type, and choice validity.
#
# Note: Space checking is handled by no_spaces_inside.R rule.
# This rule focuses only on selected() function validation.

# =============================================================================
# validate_selected_calls()
# - Validates selected(${var}, 'choice'):
#     * var exists and appears BEFORE the referencing row
#     * var type is select_one/select_multiple (extract list_name)
#     * choice exists in choices sheet under that list_name
# - Produces a log data frame with columns:
#     id, source, level, sheet, row, field, message, rule_id
# =============================================================================
validate_selected_calls <- function(survey_df,
                                    choices_df,
                                    cols = c("relevant", "calculation", "constraint", "choice_filter"),
                                    header_rows = 1L,
                                    sheet_name = "survey",
                                    verbose = TRUE) {
  
  # ---------- Basic validation ----------
  # Filter to only columns that exist
  cols <- intersect(cols, names(survey_df))
  if (length(cols) == 0) {
    if (verbose) message("No target columns found in survey.")
    return(list(valid = TRUE, log = data.frame(
      id=integer(0), source=character(0), level=character(0),
      sheet=character(0), row=integer(0), field=character(0),
      message=character(0), rule_id=character(0)
    )))
  }
  
  for (nm in c("list_name", "name")) {
    if (!nm %in% names(choices_df)) {
      stop("choices_df must contain columns: 'list_name' and 'name'")
    }
  }
  
  # Work on character copies
  survey_df[cols] <- lapply(survey_df[cols], function(x) {
    if (is.factor(x)) as.character(x) else as.character(x)
  })
  if ("type" %in% names(survey_df)) {
    survey_df$type <- if (is.factor(survey_df$type)) as.character(survey_df$type) else survey_df$type
  }
  if ("name" %in% names(survey_df)) {
    survey_df$name <- if (is.factor(survey_df$name)) as.character(survey_df$name) else survey_df$name
  }
  
  # ---------- selected(${var}, 'choice') extraction ----------
  extract_selected_calls <- function(s) {
    if (is.na(s) || s == "") return(data.frame(var=character(0), choice=character(0), match=character(0)))
    pattern <- "selected\\s*\\(\\s*\\$\\{\\s*([^}\\s]+)\\s*\\}\\s*,\\s*(['\"])([^'\"]+)\\2\\s*\\)"
    m <- gregexpr(pattern, s, perl = TRUE)
    reg <- regmatches(s, m)
    out <- data.frame(var=character(0), choice=character(0), match=character(0), stringsAsFactors = FALSE)
    if (length(reg) && length(reg[[1]]) > 0) {
      for (call in reg[[1]]) {
        var <- sub(pattern, "\\1", call, perl = TRUE)
        choice <- sub(pattern, "\\3", call, perl = TRUE)
        out <- rbind(out, data.frame(var = var, choice = choice, match = call, stringsAsFactors = FALSE))
      }
    }
    out
  }
  
  # Parse list_name from type (e.g., "select_one l_place" -> "l_place")
  parse_list_name_from_type <- function(type_str) {
    if (is.na(type_str) || type_str == "") return(NA_character_)
    t <- gsub("\\s+", " ", trimws(type_str))
    if (grepl("^select_one\\b", t)) {
      sub("^select_one\\s+", "", t)
    } else if (grepl("^select_multiple\\b", t)) {
      sub("^select_multiple\\s+", "", t)
    } else {
      NA_character_
    }
  }
  
  # ---------- Prepare lookups ----------
  df <- survey_df
  
  # Row index map for name -> first occurrence row
  name_to_row <- setNames(seq_len(nrow(df)), df$name)
  
  # Precompute each question's list_name if select_one/select_multiple
  list_name_by_name <- rep(NA_character_, nrow(df))
  if ("type" %in% names(df)) {
    list_name_by_name <- vapply(df$type, parse_list_name_from_type, character(1))
  }
  name_to_list <- setNames(list_name_by_name, df$name)
  
  # Choices lookup: set of "list_name|choice"
  key_choices <- paste(choices_df$list_name, choices_df$name, sep = "|")
  choices_set <- unique(key_choices)
  
  # ---------- Logging ----------
  log_entries <- list()
  log_id <- 1L
  
  append_log <- function(level, row_idx, field, message, rule_id) {
    log_entries[[log_id]] <<- data.frame(
      id = log_id,
      source = "custom",
      level = level,
      sheet = sheet_name,
      row = as.integer(row_idx + header_rows),
      field = field,
      message = message,
      rule_id = rule_id,
      stringsAsFactors = FALSE
    )
    log_id <<- log_id + 1L
  }
  
  # ---------- Scan columns for selected() calls ----------
  for (col in cols) {
    values <- df[[col]]
    for (r in seq_along(values)) {
      value <- values[r]
      if (is.na(value) || value == "") next
      
      # Extract and validate selected() calls
      calls <- extract_selected_calls(value)
      if (nrow(calls) > 0) {
        for (k in seq_len(nrow(calls))) {
          var <- calls$var[k]
          choice <- calls$choice[k]
          
          # Check: is var defined?
          var_row <- name_to_row[var]
          if (is.na(var_row)) {
            append_log("error", r, col,
                       sprintf("Question '%s' is not defined in survey.", var),
                       "R_SELECTED_VAR_MISSING")
            next
          }
          
          # Check: is var defined before this row?
          if (var_row >= r) {
            append_log("error", r, col,
                       sprintf("Question '%s' is defined at row %d, but referenced at row %d (must be earlier).",
                               var, var_row + header_rows, r + header_rows),
                       "R_SELECTED_REFERENCE_FUTURE")
          }
          
          # Check: is var a select_one or select_multiple?
          var_type <- if ("type" %in% names(df)) df$type[var_row] else NA_character_
          list_nm <- name_to_list[var]
          if (is.na(list_nm) || list_nm == "") {
            append_log("error", r, col,
                       sprintf("Question '%s' is not select_one/select_multiple (type='%s').", var, var_type),
                       "R_SELECTED_VAR_NOT_SELECT")
            next
          }
          
          # Check: does choice exist in choices sheet under that list_name?
          key <- paste(list_nm, choice, sep = "|")
          if (!(key %in% choices_set)) {
            append_log("warning", r, col,
                       sprintf("Choice '%s' not found under list_name '%s' in choices sheet.", choice, list_nm),
                       "R_SELECTED_CHOICE_MISSING")
          }
        }
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
  
  all_ok <- nrow(log_df) == 0L
  
  if (verbose) {
    if (all_ok) {
      message("All selected() calls are valid.")
    } else {
      message(sprintf("%d selected() validation issue(s) found.", nrow(log_df)))
    }
  }
  
  list(valid = all_ok, log = log_df)
}


# =============================================================================
# Wrapper for XLS-Validator rule system
# =============================================================================

#' Validate selected() function calls (wrapper for XLS-Validator)
#' @param xlsform_data List containing survey, choices, settings data frames
#' @return tibble of validation issues following the schema
check_selected_validation_rule <- function(xlsform_data) {
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
  
  # Check required columns in choices
  if (!all(c("list_name", "name") %in% names(choices_df))) {
    return(data.frame(
      id = integer(0), source = character(0), level = character(0),
      sheet = character(0), row = integer(0), field = character(0),
      message = character(0), rule_id = character(0), status = character(0)
    ))
  }
  
  # Columns to check for selected() calls
  target_cols <- c("relevant", "calculation", "constraint", "choice_filter")
  
  # Call the implementation
  result <- validate_selected_calls(
    survey_df = survey_df,
    choices_df = choices_df,
    cols = target_cols,
    header_rows = 1L,
    sheet_name = "survey",
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
  rule_id = "selected_validation",
  rule_fn = check_selected_validation_rule,
  description = "Validate selected() function calls: variable existence, order, type, and choice validity",
  level = "error",
  enabled = TRUE
)
