
# ============================================================================
# Rule: Validate all ${...} references (independent of selected())
# - Scans specified columns (default: relevant, calculation, constraint, choice_filter)
# - Extracts every ${var} reference
# - Validates:
#     * var exists in survey$name (excluding whitelisted meta/system names)
#     * var is defined BEFORE the referencing row (policy-aligned with your rules)
# - Optional: check var name pattern and warn if unusual
# Produces log entries with id, source, level, sheet, row, field, message, rule_id
# ============================================================================

validate_dollar_brace_refs <- function(survey_df,
                                       cols = c("relevant", "calculation", "constraint", "choice_filter"),
                                       header_rows = 1L,
                                       sheet_name = "survey",
                                       verbose = TRUE,
                                       # metadata/system names that are allowed in ${...}
                                       meta_names = c("start","end","today","deviceid","subscriberid","simserial",
                                                      "phonenumber","username","email","survey","starttime","endtime",
                                                      "_index","instanceID","instance_name"),
                                       # optional variable name pattern (warning if violated)
                                       enforce_name_pattern = TRUE,
                                       name_pattern = "^[A-Za-z][A-Za-z0-9_]*$"
) {
  # ---------- Helpers ----------
  clean_token <- function(x) {
    if (is.null(x)) return(NA_character_)
    x <- as.character(x)
    x <- gsub("\u00A0|\u2007|\u202F", " ", x, perl = TRUE)  # NBSP and friends -> space
    x <- trimws(gsub("\\s+", " ", x, perl = TRUE))
    x
  }
  normalize_expr <- function(x) {
    if (is.null(x)) return(NA_character_)
    x <- as.character(x)
    x <- gsub("\u2018|\u2019", "'", x, perl = TRUE)         # smart single quotes -> '
    x <- gsub("\u201C|\u201D", "\"", x, perl = TRUE)        # smart double quotes -> "
    x <- gsub("\u00A0|\u2007|\u202F", " ", x, perl = TRUE)  # NBSP etc -> space
    x
  }
  extract_var_refs <- function(s) {
    if (is.na(s) || s == "") return(character(0))
    patt <- "\\$\\{\\s*([^}\\s]+)\\s*\\}"
    m <- gregexpr(patt, s, perl = TRUE)
    reg <- regmatches(s, m)
    if (!length(reg) || length(reg[[1]]) == 0) return(character(0))
    refs <- vapply(reg[[1]], function(tok) clean_token(sub(patt, "\\1", tok, perl = TRUE)), character(1))
    unique(refs)
  }
  
  # ---------- Basic validation ----------
  cols <- intersect(cols, names(survey_df))
  if (length(cols) == 0) {
    if (verbose) message("No target columns found in survey for ${...} reference validation.")
    return(list(valid = TRUE, log = data.frame(
      id=integer(0), source=character(0), level=character(0),
      sheet=character(0), row=integer(0), field=character(0),
      message=character(0), rule_id=character(0)
    )))
  }
  
  # Normalize key columns and targets
  if ("name" %in% names(survey_df)) survey_df$name <- clean_token(survey_df$name)
  survey_df[cols] <- lapply(survey_df[cols], function(x) normalize_expr(if (is.factor(x)) as.character(x) else x))
  
  # Lookups
  df <- survey_df
  valid_names <- unique(na.omit(clean_token(df$name)))
  
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
  
  # ---------- Scan columns ----------
  for (col in cols) {
    values <- df[[col]]
    for (r in seq_along(values)) {
      value <- values[r]
      if (is.na(value) || value == "") next
      
      refs <- extract_var_refs(value)
      if (length(refs) == 0) next
      
      for (vr in refs) {
        # Skip metadata/system names
        if (vr %in% meta_names) next
        
        # Optional: name pattern check (warning)
        if (enforce_name_pattern && !grepl(name_pattern, vr)) {
          append_log("warning", r, col,
                     sprintf("Reference '%s' does not match the recommended name pattern '%s'.", vr, name_pattern),
                     "R_DOLLAR_REF_BADCASING")
        }
        
        # Existence
        if (!vr %in% valid_names) {
          append_log("error", r, col,
                     sprintf("Reference '%s' is not defined in survey.", vr),
                     "R_DOLLAR_REF_MISSING")
          next
        }
        
        # Order policy: must be defined before referencing row
        vr_row <- match(vr, df$name)
        if (is.na(vr_row)) {
          # Defensive: should not happen as vr %in% valid_names
          append_log("error", r, col,
                     sprintf("Reference '%s' could not be located in survey.", vr),
                     "R_DOLLAR_REF_NOT_LOCATABLE")
          next
        }
        
        if (vr_row > r) {
          append_log("error", r, col,
                     sprintf("Reference '%s' is defined at row %d, but referenced at row %d (must be earlier).",
                             vr, vr_row + header_rows, r + header_rows),
                     "R_DOLLAR_REF_FUTURE")
        } else if (vr_row == r) {
          append_log("warning", r, col,
                     sprintf("Reference '%s' is defined at row %d, and referenced at the same row %d (check if intentional).",
                             vr, vr_row + header_rows, r + header_rows),
                     "R_DOLLAR_REF_SAME_LINE")
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
    if (all_ok) message("All ${...} references are valid.")
    else message(sprintf("%d issue(s) found in ${...} references.", nrow(log_df)))
  }
  
  list(valid = all_ok, log = log_df)
}



# Create a fallback 'label' column from the first available label::<lang> column.
ensure_label_column <- function(survey_df) {
  if (!"label" %in% names(survey_df)) {
    # Find any label::<lang> columns
    label_cols <- grep("^label(::|\\:\\:)", names(survey_df), value = TRUE, perl = TRUE)
    if (length(label_cols) > 0) {
      # Take the first label::<lang> as 'label'
      survey_df$label <- survey_df[[label_cols[1]]]
    } else {
      # If no label columns at all, create an empty label column
      survey_df$label <- NA_character_
    }
  }
  survey_df
}


#' Validate ${...} references for a single field
#' @param xlsform_data List with $survey data frame
#' @param field One of "relevant", "calculation", "constraint", "choice_filter"
#' @param header_rows Number of header rows in the survey sheet
#' @param sheet_name Survey sheet name (default: "survey")
#' @return data.frame with issues (id, source, level, sheet, row, field, message, rule_id, status)
validate_dollar_refs_for_field <- function(xlsform_data,
                                           field = c("label", "relevant", "calculation", "constraint", "choice_filter"),
                                           header_rows = 1L,
                                           sheet_name = "survey") {
  field <- match.arg(field)
  
  if (!("survey" %in% names(xlsform_data)) || is.null(xlsform_data$survey)) {
    stop("xlsform_data$survey is missing.")
  }
  
  # survey_df <- xlsform_data$survey
  
  survey_df <- ensure_label_column(xlsform_data$survey)
  
  if (!(field %in% names(survey_df))) {
    return(data.frame(
      id = integer(0), source = character(0), level = character(0),
      sheet = character(0), row = integer(0), field = character(0),
      message = character(0), rule_id = character(0), status = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  res <- validate_dollar_brace_refs(
    survey_df = survey_df,
    cols = field,
    header_rows = header_rows,
    sheet_name = sheet_name,
    verbose = FALSE
  )
  
  log_df <- res$log
  if (nrow(log_df) > 0 && !"status" %in% names(log_df)) log_df$status <- "open"
  
  if (nrow(log_df) == 0L) {
    log_df <- data.frame(
      id = integer(0), source = character(0), level = character(0),
      sheet = character(0), row = integer(0), field = character(0),
      message = character(0), rule_id = character(0), status = character(0),
      stringsAsFactors = FALSE
    )
  }
  
  log_df
}




register_rule(
  rule_id = "dollar_brace_references",
  rule_fn = validate_dollar_refs_for_field,
  description = "Validate all ${...} references: existence and order, independent of selected()",
  level = "error",
  enabled = TRUE
)
