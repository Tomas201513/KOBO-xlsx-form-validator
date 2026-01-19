
validate_selected_calls <- function(survey_df,
                                    choices_df,
                                    cols = c("relevant", "calculation", "constraint", "choice_filter"),
                                    header_rows = 1L,
                                    sheet_name = "survey",
                                    verbose = TRUE) {
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
  extract_selected_calls <- function(s) {
    if (is.na(s) || s == "") {
      return(data.frame(var=character(0), choice=character(0), match=character(0), stringsAsFactors = FALSE))
    }
    # Case-insensitive selected(), handle both ' and " (already normalized)
    pattern <- "(?i)selected\\s*\\(\\s*\\$\\{\\s*([^}\\s]+)\\s*\\}\\s*,\\s*(['\"])([^'\"]+)\\2\\s*\\)"
    m <- gregexpr(pattern, s, perl = TRUE)
    reg <- regmatches(s, m)
    out <- data.frame(var=character(0), choice=character(0), match=character(0), stringsAsFactors = FALSE)
    if (length(reg) && length(reg[[1]]) > 0) {
      for (call in reg[[1]]) {
        var    <- clean_token(sub(pattern, "\\1", call, perl = TRUE))
        choice <- clean_token(sub(pattern, "\\3", call, perl = TRUE))
        out <- rbind(out, data.frame(var = var, choice = choice, match = call, stringsAsFactors = FALSE))
      }
    }
    out
  }
  # NEW: extract all ${...} variable references (regardless of function)
  extract_var_refs <- function(s) {
    if (is.na(s) || s == "") return(character(0))
    patt <- "\\$\\{\\s*([^}\\s]+)\\s*\\}"
    m <- gregexpr(patt, s, perl = TRUE)
    reg <- regmatches(s, m)
    if (!length(reg) || length(reg[[1]]) == 0) return(character(0))
    refs <- vapply(reg[[1]], function(tok) {
      clean_token(sub(patt, "\\1", tok, perl = TRUE))
    }, character(1))
    unique(refs)
  }
  parse_list_name_from_type <- function(type_str) {
    if (is.na(type_str) || type_str == "") return(NA_character_)
    t <- clean_token(type_str)
    if (grepl("^select_one\\b", t)) {
      clean_token(sub("^select_one\\s+", "", t))
    } else if (grepl("^select_multiple\\b", t)) {
      clean_token(sub("^select_multiple\\s+", "", t))
    } else {
      NA_character_
    }
  }
  
  # ---------- Basic validation ----------
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
  
  # Normalize core columns
  if ("type" %in% names(survey_df))  survey_df$type <- clean_token(survey_df$type)
  if ("name" %in% names(survey_df))  survey_df$name <- clean_token(survey_df$name)
  
  # Normalize target expression columns (quotes & spaces)
  survey_df[cols] <- lapply(survey_df[cols], function(x) normalize_expr(if (is.factor(x)) as.character(x) else x))
  
  # Normalize choices
  choices_df$list_name <- clean_token(choices_df$list_name)
  choices_df$name      <- clean_token(choices_df$name)
  
  # ---------- Prepare lookups ----------
  df <- survey_df
  valid_names <- unique(na.omit(clean_token(df$name)))
  list_name_by_name <- if ("type" %in% names(df)) vapply(df$type, parse_list_name_from_type, character(1)) else rep(NA_character_, nrow(df))
  
  key_choices <- paste(choices_df$list_name, choices_df$name, sep = "|")
  choices_set <- unique(key_choices)
  
  # Whitelist of common metadata/system fields often referenced with ${...}
  meta_names <- c("start","end","today","deviceid","subscriberid","simserial","phonenumber",
                  "username","email","survey","starttime","endtime","_index","instanceID")
  
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
  
  # ---------- Scan columns for selected() calls and generic ${...} refs ----------
  for (col in cols) {
    values <- df[[col]]
    for (r in seq_along(values)) {
      value <- values[r]
      if (is.na(value) || value == "") next
      
      # 1) Handle valid selected(${var}, 'choice') calls
      calls <- extract_selected_calls(value)
      selected_vars <- character(0)
      if (nrow(calls) > 0) {
        selected_vars <- unique(calls$var)
        for (k in seq_len(nrow(calls))) {
          var <- calls$var[k]
          choice <- calls$choice[k]
          
          # Check: is var defined?
          if (!var %in% valid_names) {
            # allow metadata names too? (selected(meta, 'x') is unlikely, still flag)
            append_log("error", r, col,
                       sprintf("Question '%s' is not defined in survey.", var),
                       "R_SELECTED_VAR_MISSING")
            next
          }
          var_row <- match(var, df$name) # first occurrence
          
          # Check: is var defined before this row?
          if (var_row > r) {
            append_log("error", r, col,
                       sprintf("Question '%s' is defined at row %d, but referenced at row %d (must be earlier).",
                               var, var_row + header_rows, r + header_rows),
                       "R_SELECTED_REFERENCE_FUTURE")
          }
          
          if (var_row == r) {
            append_log("warning", r, col,
                       sprintf("Question '%s' is defined at row %d, but referenced at row %d (Check if it is intentional).",
                               var, var_row + header_rows, r + header_rows),
                       "R_SELECTED_REFERENCE_SAME_LINE")
          }
          
          # Check: is var a select_one or select_multiple?
          var_type <- if ("type" %in% names(df)) df$type[var_row] else NA_character_
          list_nm  <- list_name_by_name[var_row]   # row-based!
          if (is.na(list_nm) || list_nm == "") {
            append_log("error", r, col,
                       sprintf("Question '%s' is not select_one/select_multiple (type='%s').", var, var_type),
                       "R_SELECTED_VAR_NOT_SELECT")
            # do not 'next' so we still check choice presence (will show missing under NA list_name)
          }
          
          # Check: does choice exist in choices sheet under that list_name?
          key <- paste(list_nm, choice, sep = "|")
          if (!(key %in% choices_set)) {
            append_log("error", r, col,
                       sprintf("Choice '%s' not found under list_name '%s' in choices sheet.", choice, list_nm),
                       "R_SELECTED_CHOICE_MISSING")
          }
        }
      }
      
      # 2) NEW: Validate ALL ${...} references (beyond selected())
      all_refs <- extract_var_refs(value)
      # Exclude those already validated via selected()
      other_refs <- setdiff(all_refs, selected_vars)
      if (length(other_refs) > 0) {
        for (vr in other_refs) {
          # Skip known metadata/system fields
          if (vr %in% meta_names) next
          
          # Existence
          if (!vr %in% valid_names) {
            append_log("error", r, col,
                       sprintf("Reference '%s' is not defined in survey.", vr),
                       "R_REF_VAR_MISSING")
            next
          }
          # Order
          vr_row <- match(vr, df$name)
          if (vr_row > r) {
            append_log("error", r, col,
                       sprintf("Reference '%s' is defined at row %d, but referenced at row %d (must be earlier).",
                               vr, vr_row + header_rows, r + header_rows),
                       "R_REF_REFERENCE_FUTURE")
          }
          if (vr_row == r) {
            append_log("warning", r, col,
                       sprintf("Reference '%s' is defined at row %d, but referenced at row %d (Check if it is intentional).",
                               vr, vr_row + header_rows, r + header_rows),
                       "R_REF_REFERENCE_SAME_LINE")
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
      message("All selected() calls and ${...} references are valid.")
    } else {
      message(sprintf("%d validation issue(s) found in selected() and ${...} references.", nrow(log_df)))
    }
  }
  
  list(valid = all_ok, log = log_df)
}



# =============================================================================
# Wrapper for XLS-Validator rule system
# =============================================================================

#' Run selected() validation for a single field
#' @param xlsform_data List with $survey and $choices data frames
#' @param field One of "relevant", "calculation", "constraint", "choice_filter"
#' @param header_rows Header rows count in the survey sheet (default 1)
#' @param sheet_name The name of the survey sheet (default "survey")
#' @return data.frame of validation issues (schema: id, source, level, sheet, row, field, message, rule_id, status)
validate_selected_for_field <- function(xlsform_data,
                                        field = c("relevant", "calculation", "constraint", "choice_filter"),
                                        header_rows = 1L,
                                        sheet_name = "survey") {
  field <- match.arg(field)
  
  # Ensure required sheets exist
  if (!("survey" %in% names(xlsform_data)) || is.null(xlsform_data$survey)) {
    stop("xlsform_data$survey is missing.")
  }
  if (!("choices" %in% names(xlsform_data)) || is.null(xlsform_data$choices)) {
    stop("xlsform_data$choices is missing.")
  }
  
  survey_df  <- xlsform_data$survey
  choices_df <- xlsform_data$choices
  
  # Ensure requested field exists in survey
  if (!(field %in% names(survey_df))) {
    # Return empty schema with correct columns
    return(data.frame(
      id = integer(0), source = character(0), level = character(0),
      sheet = character(0), row = integer(0), field = character(0),
      message = character(0), rule_id = character(0), status = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Call your implementation with only that column
  result <- validate_selected_calls(
    survey_df = survey_df,
    choices_df = choices_df,
    cols = field,                 # <--- single column here
    header_rows = header_rows,
    sheet_name = sheet_name,
    verbose = FALSE
  )
  
  log_df <- result$log
  if (nrow(log_df) > 0 && !"status" %in% names(log_df)) {
    log_df$status <- "open"
  }
  
  # Ensure schema if empty
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

# Register rule
register_rule(
  rule_id = "selected_validation",
  rule_fn = validate_selected_for_field,
  description = "Validate selected() function calls: variable existence, order, type, and choice validity",
  level = "error",
  enabled = TRUE
)
